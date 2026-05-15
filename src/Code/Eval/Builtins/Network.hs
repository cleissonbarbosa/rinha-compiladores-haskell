-- |
-- Primitivos de socket TCP. O interpretador so expoe sockets crus: accept
-- loop, recv/send, connect e shutdown. Todo o protocolo HTTP (parse da
-- request, roteamento, montagem da resposta) vive em `rinha/server.rinha`.
module Code.Eval.Builtins.Network (networkBuiltins) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.QSem (newQSem, signalQSem, waitQSem)
import Control.Exception (finally)
import Control.Monad (forever)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
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

import Code.Eval.Helpers
import Code.Eval.Runtime (applyClosedClosure)
import Code.Eval.Types

networkBuiltins :: [(String, ResultType)]
networkBuiltins =
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
