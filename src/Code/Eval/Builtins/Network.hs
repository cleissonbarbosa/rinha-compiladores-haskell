module Code.Eval.Builtins.Network (networkBuiltins) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.QSem (newQSem, signalQSem, waitQSem)
import Control.Exception (finally)
import Control.Monad (forever)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Map as Map
import Data.Word (Word8)
import Network.Socket
  ( AddrInfo(..)
  , AddrInfoFlag(..)
  , ShutdownCmd(..)
  , Socket
  , SocketOption(..)
  , SocketType(Stream)
  , accept
  , addrAddress
  , addrFamily
  , addrProtocol
  , bind
  , close
  , connect
  , defaultHints
  , getAddrInfo
  , listen
  , setSocketOption
  , shutdown
  , socket
  , withSocketsDo
  )
import qualified Network.Socket.ByteString as NSB
import System.Timeout (timeout)

import Code.Eval.Builtins.IVF (classifyJsonBody, maxJsonBodyBytes)
import Code.Eval.Helpers
import Code.Eval.Runtime (applyClosedClosure)
import Code.Eval.Types

networkBuiltins :: Map.Map String ResultType -> [(String, ResultType)]
networkBuiltins baseEnv =
  [ ("net_listen", builtinNetListen)
  , ("net_listen_addr", builtinNetListenAddr)
  , ("net_accept", builtinNetAccept)
  , ("net_connect", builtinNetConnect)
  , ("net_connect_addr", builtinNetConnectAddr)
  , ("net_recv", builtinNetRecv)
  , ("net_send", builtinNetSend)
  , ("net_close", builtinNetClose)
  , ("net_shutdown", builtinNetShutdown)
  , ("net_serve", builtinNetServe)
  , ("net_proxy", builtinNetProxy)
  , ("http_read_request", builtinHttpReadRequest)
  , ("http_send_json", builtinHttpSendJson)
  , ("api_handle", builtinApiHandle baseEnv)
  ]

maxHttpBytes :: Int
maxHttpBytes = 256 * 1024

httpHeaderEnd :: BS.ByteString
httpHeaderEnd = BSC.pack "\r\n\r\n"

readyResponse :: BS.ByteString
readyResponse =
  BSC.pack "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\nConnection: keep-alive\r\nContent-Length: 14\r\n\r\n{\"ready\":true}"

notFoundResponse :: BS.ByteString
notFoundResponse =
  BSC.pack "HTTP/1.1 404 Not Found\r\nContent-Type: application/json\r\nConnection: keep-alive\r\nContent-Length: 21\r\n\r\n{\"error\":\"not_found\"}"

scoreResponses :: [BS.ByteString]
scoreResponses =
  map BSC.pack
    [ "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\nConnection: keep-alive\r\nContent-Length: 35\r\n\r\n{\"approved\":true,\"fraud_score\":0.0}"
    , "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\nConnection: keep-alive\r\nContent-Length: 35\r\n\r\n{\"approved\":true,\"fraud_score\":0.2}"
    , "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\nConnection: keep-alive\r\nContent-Length: 36\r\n\r\n{\"approved\":false,\"fraud_score\":0.4}"
    , "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\nConnection: keep-alive\r\nContent-Length: 36\r\n\r\n{\"approved\":false,\"fraud_score\":0.6}"
    , "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\nConnection: keep-alive\r\nContent-Length: 36\r\n\r\n{\"approved\":false,\"fraud_score\":0.8}"
    , "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\nConnection: keep-alive\r\nContent-Length: 36\r\n\r\n{\"approved\":false,\"fraud_score\":1.0}"
    ]

closeQuiet :: Socket -> IO ()
closeQuiet sock = close sock `catchAny` \_ -> return ()

shutdownQuiet :: Socket -> ShutdownCmd -> IO ()
shutdownQuiet sock cmd = shutdown sock cmd `catchAny` \_ -> return ()

setNoDelayQuiet :: Socket -> IO ()
setNoDelayQuiet sock = setSocketOption sock NoDelay 1 `catchAny` \_ -> return ()

listenOn :: String -> String -> IO Socket
listenOn host port = withSocketsDo $ do
  let hints = defaultHints { addrFlags = [AI_PASSIVE], addrSocketType = Stream }
  addrs <- getAddrInfo (Just hints) (if null host then Nothing else Just host) (Just port)
  let go [] = error ("net_listen: nao consegui escutar em " ++ host ++ ":" ++ port)
      go (addr:addrs') = do
        opened <- tryAny $ do
          sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
          setSocketOption sock ReuseAddr 1
          bind sock (addrAddress addr)
          listen sock 1024
          return sock
        case opened of
          Right sock -> return sock
          Left _ -> go addrs'
  go addrs

connectTo :: String -> String -> IO (Maybe Socket)
connectTo host port = withSocketsDo (attempt (6 :: Int))
  where
    attempt 0 = return Nothing
    attempt triesLeft = do
      let hints = defaultHints { addrSocketType = Stream }
      resolved <- tryAny (getAddrInfo (Just hints) (Just host) (Just port))
      connected <- case resolved of
        Left _ -> return Nothing
        Right addrs -> tryAddrs addrs
      case connected of
        Just sock -> return (Just sock)
        Nothing -> do
          threadDelay 10000
          attempt (triesLeft - 1)

    tryAddrs [] = return Nothing
    tryAddrs (addr:addrs') = do
      opened <- tryAny $ do
        sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
        setNoDelayQuiet sock
        connected <- timeout 200000 (connect sock (addrAddress addr))
        case connected of
          Just () -> return sock
          Nothing -> closeQuiet sock >> ioError (userError "net_connect: timeout")
      case opened of
        Right sock -> return (Just sock)
        Left _ -> tryAddrs addrs'

recvSocket :: Socket -> Int -> IO String
recvSocket sock bytes = do
  chunk <- NSB.recv sock bytes
  return (BSC.unpack chunk)

sendSocket :: Socket -> String -> IO Int
sendSocket sock value = do
  let bytes = BSC.pack value
  NSB.sendAll sock bytes
  return (BS.length bytes)

relaySocket :: Socket -> Socket -> Int -> IO ()
relaySocket src dst bytes =
  let loop = do
        chunk <- NSB.recv src bytes
        if BS.null chunk
          then shutdownQuiet dst ShutdownSend
          else NSB.sendAll dst chunk >> loop
  in loop `catchAny` \_ -> shutdownQuiet dst ShutdownSend

data ParsedHttp = ParsedHttp
  { parsedMethod :: !BS.ByteString
  , parsedPath :: !BS.ByteString
  , parsedBody :: !BS.ByteString
  , parsedConsumed :: !Int
  }

data HttpParseResult
  = HttpNeedMore
  | HttpBad
  | HttpParsed !ParsedHttp

wSpace, wTab, wLf, wCr, wQuote, wColon, wZero, wNine :: Word8
wSpace = 32
wTab = 9
wLf = 10
wCr = 13
wQuote = 34
wColon = 58
wZero = 48
wNine = 57

isDigitByte :: Word8 -> Bool
isDigitByte c = c >= wZero && c <= wNine

skipWsBS :: BS.ByteString -> BS.ByteString
skipWsBS = BS.dropWhile (\c -> c == wSpace || c == wTab || c == wLf || c == wCr)

lowerAscii :: Word8 -> Word8
lowerAscii c = if c >= 65 && c <= 90 then c + 32 else c

prefixIEq :: BS.ByteString -> BS.ByteString -> Bool
prefixIEq value prefix =
  BS.length value >= BS.length prefix && go 0
  where
    n = BS.length prefix
    go i
      | i >= n = True
      | otherwise =
          lowerAscii (BS.index value i) == lowerAscii (BS.index prefix i) &&
          go (i + 1)

dropLineEnd :: BS.ByteString -> BS.ByteString
dropLineEnd line =
  if not (BS.null line) && BS.last line == wCr
    then BS.init line
    else line

readPositiveIntBS :: BS.ByteString -> Int
readPositiveIntBS bytes = go (skipWsBS bytes) 0
  where
    go s acc
      | BS.null s = acc
      | isDigitByte (BS.head s) = go (BS.tail s) (acc * 10 + fromIntegral (BS.head s - wZero))
      | otherwise = acc

contentLengthOf :: BS.ByteString -> Int
contentLengthOf header = go (BSC.lines header)
  where
    key = BSC.pack "content-length:"
    go [] = 0
    go (raw:rest) =
      let line = dropLineEnd raw
      in if prefixIEq line key
           then readPositiveIntBS (BS.drop (BS.length key) line)
           else go rest

parseRequestLine :: BS.ByteString -> Maybe (BS.ByteString, BS.ByteString)
parseRequestLine header = do
  let line = BS.takeWhile (/= wCr) header
      (method, rest0) = BS.break (== wSpace) line
  if BS.null method || BS.null rest0
    then Nothing
    else do
      let rest1 = BS.dropWhile (== wSpace) (BS.drop 1 rest0)
          (path, rest2) = BS.break (== wSpace) rest1
      if BS.null path || BS.null rest2
        then Nothing
        else Just (method, path)

parseHttpBuffer :: BS.ByteString -> HttpParseResult
parseHttpBuffer buf =
  let (header, suffix) = BSC.breakSubstring httpHeaderEnd buf
  in if not (httpHeaderEnd `BS.isPrefixOf` suffix)
       then HttpNeedMore
       else
         case parseRequestLine header of
           Nothing -> HttpBad
           Just (method, path) ->
             let contentLength = contentLengthOf header
                 bodyStart = BS.length header + BS.length httpHeaderEnd
                 consumed = bodyStart + contentLength
             in if contentLength > maxJsonBodyBytes
                  then HttpBad
                  else if BS.length buf < consumed
                    then HttpNeedMore
                    else HttpParsed ParsedHttp
                      { parsedMethod = method
                      , parsedPath = path
                      , parsedBody = BS.take contentLength (BS.drop bodyStart buf)
                      , parsedConsumed = consumed
                      }

httpRequestClosed :: ResultType
httpRequestClosed =
  RecordResult (Map.fromList [("ok", BoolResult False)])

httpRequestResult :: ParsedHttp -> ResultType
httpRequestResult req =
  RecordResult
    (Map.fromList
      [ ("ok", BoolResult True)
      , ("method", StringResult (BSC.unpack (parsedMethod req)))
      , ("path", StringResult (BSC.unpack (parsedPath req)))
      , ("body", StringResult (BSC.unpack (parsedBody req)))
      ])

readHttpRequest :: Socket -> IO ResultType
readHttpRequest sock = loop BS.empty
  where
    loop buf =
      case parseHttpBuffer buf of
        HttpBad -> return httpRequestClosed
        HttpParsed req -> return (httpRequestResult req)
        HttpNeedMore ->
          if BS.length buf >= maxHttpBytes
            then return httpRequestClosed
            else do
              chunk <- NSB.recv sock 65536
              if BS.null chunk
                then return httpRequestClosed
                else loop (buf <> chunk)

statusLine :: Int -> BS.ByteString
statusLine code = case code of
  200 -> BSC.pack "HTTP/1.1 200 OK\r\n"
  404 -> BSC.pack "HTTP/1.1 404 Not Found\r\n"
  _ -> BSC.pack "HTTP/1.1 500 Internal Server Error\r\n"

sendJsonResponse :: Socket -> Int -> String -> IO Int
sendJsonResponse sock statusCode body = do
  let bodyBs = BSC.pack body
      header =
        statusLine statusCode
          <> BSC.pack "Content-Type: application/json\r\nConnection: keep-alive\r\nContent-Length: "
          <> BSC.pack (show (BS.length bodyBs))
          <> BSC.pack "\r\n\r\n"
      payload = header <> bodyBs
  NSB.sendAll sock payload
  return (BS.length payload)

respondApiRequest :: Map.Map String ResultType -> Socket -> ParsedHttp -> IO ()
respondApiRequest baseEnv sock req
  | parsedMethod req == BSC.pack "GET" && parsedPath req == BSC.pack "/ready" =
      NSB.sendAll sock readyResponse
  | parsedMethod req == BSC.pack "POST" && parsedPath req == BSC.pack "/fraud-score" = do
      score <- classifyJsonBody baseEnv (parsedBody req)
      NSB.sendAll sock (scoreResponses !! max 0 (min 5 score))
  | parsedMethod req == BSC.pack "POST" =
      NSB.sendAll sock (scoreResponses !! 5)
  | otherwise =
      NSB.sendAll sock notFoundResponse

handleApiSocket :: Map.Map String ResultType -> Socket -> IO ()
handleApiSocket baseEnv sock = loop BS.empty
  where
    loop buf =
      case parseHttpBuffer buf of
        HttpBad -> return ()
        HttpParsed req -> do
          respondApiRequest baseEnv sock req
          loop (BS.drop (parsedConsumed req) buf)
        HttpNeedMore ->
          if BS.length buf >= maxHttpBytes
            then return ()
            else do
              chunk <- NSB.recv sock 65536
              if BS.null chunk
                then return ()
                else loop (buf <> chunk)

builtinHttpReadRequest :: ResultType
builtinHttpReadRequest = NativeResult "http_read_request" $ \args -> case args of
  [SocketResult sock] ->
    readHttpRequest sock `catchAny` (\_ -> return rNil)
  _ -> error "http_read_request: esperado (socket)"

builtinHttpSendJson :: ResultType
builtinHttpSendJson = NativeResult "http_send_json" $ \args -> case args of
  [SocketResult sock, IntResult statusCode, StringResult body] ->
    (IntResult . toInteger <$> sendJsonResponse sock (fromInteger statusCode) body)
      `catchAny` (\_ -> return (IntResult 0))
  _ -> error "http_send_json: esperado (socket, int, string)"

builtinNetListen :: ResultType
builtinNetListen = NativeResult "net_listen" $ \args -> case args of
  [host, port] -> SocketResult <$> listenOn (asString host) (portOf port)
  _ -> error "net_listen: esperado (host, port)"

builtinNetListenAddr :: ResultType
builtinNetListenAddr = NativeResult "net_listen_addr" $ \args -> case args of
  [StringResult addr] ->
    case splitHostPort addr of
      Just (host, port) -> SocketResult <$> listenOn host port
      Nothing -> error ("net_listen_addr: endereco invalido " ++ addr)
  _ -> error "net_listen_addr: esperado (addr)"

builtinNetAccept :: ResultType
builtinNetAccept = NativeResult "net_accept" $ \args -> case args of
  [listener] -> do
    (client, _) <- accept (asSocket listener)
    setNoDelayQuiet client
    return (SocketResult client)
  _ -> error "net_accept: esperado (socket)"

builtinNetConnect :: ResultType
builtinNetConnect = NativeResult "net_connect" $ \args -> case args of
  [host, port] -> do
    connected <- connectTo (asString host) (portOf port)
    return (maybe rNil okSocket connected)
  _ -> error "net_connect: esperado (host, port)"

builtinNetConnectAddr :: ResultType
builtinNetConnectAddr = NativeResult "net_connect_addr" $ \args -> case args of
  [StringResult addr] ->
    case splitHostPort addr of
      Just (host, port) -> do
        connected <- connectTo host port
        return (maybe rNil okSocket connected)
      Nothing -> return rNil
  _ -> error "net_connect_addr: esperado (addr)"

builtinNetRecv :: ResultType
builtinNetRecv = NativeResult "net_recv" $ \args -> case args of
  [sock, IntResult maxBytesRaw] ->
    let maxBytes = max 1 (min (1024 * 1024) (fromInteger maxBytesRaw))
    in (StringResult <$> recvSocket (asSocket sock) maxBytes)
         `catchAny` (\_ -> return (StringResult ""))
  _ -> error "net_recv: esperado (socket, int)"

builtinNetSend :: ResultType
builtinNetSend = NativeResult "net_send" $ \args -> case args of
  [sock, StringResult value] ->
    (IntResult . toInteger <$> sendSocket (asSocket sock) value)
      `catchAny` (\_ -> return (IntResult 0))
  _ -> error "net_send: esperado (socket, string)"

builtinNetClose :: ResultType
builtinNetClose = NativeResult "net_close" $ \args -> case args of
  [sock] -> closeQuiet (asSocket sock) >> return (IntResult 0)
  _ -> error "net_close: esperado (socket)"

builtinNetShutdown :: ResultType
builtinNetShutdown = NativeResult "net_shutdown" $ \args -> case args of
  [sock, IntResult how] -> do
    let cmd = case how of
          0 -> ShutdownReceive
          1 -> ShutdownSend
          _ -> ShutdownBoth
    shutdownQuiet (asSocket sock) cmd
    return (IntResult 0)
  _ -> error "net_shutdown: esperado (socket, int)"

builtinNetServe :: ResultType
builtinNetServe = NativeResult "net_serve" $ \args -> case args of
  [SocketResult listener, ClosureResult handler] -> do
    limit <- envInt "RINHA_NET_MAX_THREADS" 64
    sem <- newQSem (max 1 limit)
    forever $ do
      waitQSem sem
      accepted <- tryAny (accept listener)
      case accepted of
        Left _ -> signalQSem sem
        Right (client, _) -> do
          setNoDelayQuiet client
          spawned <- tryAny $ forkIO $
            ((applyClosedClosure handler [SocketResult client] >> return ())
              `catchAny` (\_ -> return ()))
              `finally` (closeQuiet client >> signalQSem sem)
          case spawned of
            Right _ -> return ()
            Left _ -> closeQuiet client >> signalQSem sem
  _ -> error "net_serve: esperado (listener, handler)"

builtinNetProxy :: ResultType
builtinNetProxy = NativeResult "net_proxy" $ \args -> case args of
  [a, b, IntResult rawBytes] -> do
    let bytes = max 1 (min (1024 * 1024) (fromInteger rawBytes))
        left = asSocket a
        right = asSocket b
    done <- newEmptyMVar
    _ <- forkIO $ relaySocket left right bytes `finally` putMVar done ()
    _ <- forkIO $ relaySocket right left bytes `finally` putMVar done ()
    takeMVar done
    takeMVar done
    return (IntResult 0)
  _ -> error "net_proxy: esperado (socket, socket, int)"

builtinApiHandle :: Map.Map String ResultType -> ResultType
builtinApiHandle baseEnv = NativeResult "api_handle" $ \args -> case args of
  [SocketResult sock] ->
    (handleApiSocket baseEnv sock >> return (IntResult 0))
      `catchAny` (\_ -> return (IntResult 0))
  _ -> error "api_handle: esperado (socket)"