{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Code.Eval.Builtins.IVF
  ( classifyJsonBody
  , ivfBuiltins
  , maxJsonBodyBytes
  ) where

import Control.Concurrent.MVar (MVar, modifyMVar, newMVar)
import Control.Monad (when)
import Data.Aeson (decode)
import Data.Array.IO (IOUArray, newArray, readArray, writeArray)
import Data.Array.Unboxed (UArray, array, (!))
import Data.Bits ((.&.), shiftL, (.|.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Unsafe as BSU
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Word (Word8, Word32)
import Foreign.C.String (CString, withCString)
import Foreign.C.Types (CInt(..), CLong(..), CSize(..))
import Foreign.ForeignPtr (newForeignPtr_)
import Foreign.Marshal.Array (withArray)
import Foreign.Ptr (Ptr, intPtrToPtr, nullPtr)
import GHC.Float (castWord32ToFloat)
import Parse ()
import System.Directory (doesFileExist)
import System.Environment (lookupEnv)
import System.IO (IOMode(ReadMode), hFileSize, withFile)
import System.IO.Unsafe (unsafePerformIO)
import Terms (File(..))

import Code.Eval.Helpers
import Code.Eval.Runtime (eval)
import Code.Eval.Types

foreign import ccall unsafe "open"
  c_open :: CString -> CInt -> IO CInt

foreign import ccall unsafe "close"
  c_close :: CInt -> IO CInt

foreign import ccall unsafe "mmap"
  c_mmap :: Ptr () -> CSize -> CInt -> CInt -> CInt -> CLong -> IO (Ptr Word8)

foreign import ccall unsafe "rinha_fast_ivf_warmup"
  c_fastIvfWarmup :: CString -> IO CInt

foreign import ccall unsafe "rinha_fast_ivf_score"
  c_fastIvfScore :: Ptr CInt -> CString -> CInt -> IO CInt

dim :: Int
dim = 14

rinhaScale :: Int
rinhaScale = 4096

q16Scale :: Int
q16Scale = 32767

maxCandidates :: Int
maxCandidates = 512

maxJsonBodyBytes :: Int
maxJsonBodyBytes = 64 * 1024

ivfMagic :: BS.ByteString
ivfMagic = BSC.pack "RIVF2026"

protRead :: CInt
protRead = 1

mapPrivate :: CInt
mapPrivate = 2

oReadOnly :: CInt
oReadOnly = 0

mapFailed :: Ptr Word8
mapFailed = intPtrToPtr (-1)

data IvfIndex = IvfIndex
  { idxNVecs :: !Int
  , idxNClusters :: !Int
  , idxNProbe :: !Int
  , idxDims :: !BS.ByteString
  , idxLabels :: !BS.ByteString
  , idxCentroids :: !(UArray Int Float)
  , idxBoundaries :: !(UArray Int Int)
  }

ivfRef :: MVar (Maybe IvfIndex)
ivfRef = unsafePerformIO (newMVar Nothing)
{-# NOINLINE ivfRef #-}

algoRef :: MVar (Maybe File)
algoRef = unsafePerformIO (newMVar Nothing)
{-# NOINLINE algoRef #-}

ivfBuiltins :: Map.Map String ResultType -> [(String, ResultType)]
ivfBuiltins baseEnv =
  [ ("fraud_score_json", builtinFraudScoreJson baseEnv)
  , ("ivf_query", builtinIvfQuery)
  , ("ivf_score", builtinIvfScore)
  , ("ivf_warmup", builtinIvfWarmup)
  ]

mmapFileReadOnly :: FilePath -> IO BS.ByteString
mmapFileReadOnly path = do
  rawSize <- withFile path ReadMode hFileSize
  let size = fromInteger rawSize
  when (size <= 0) $
    error ("mmap: arquivo vazio " ++ path)
  fd <- withCString path $ \cpath -> c_open cpath oReadOnly
  when (fd < 0) $
    error ("mmap: nao consegui abrir " ++ path)
  ptr <- c_mmap nullPtr (fromIntegral size) protRead mapPrivate fd 0
  _ <- c_close fd
  when (ptr == mapFailed) $
    error ("mmap: falha ao mapear " ++ path)
  fptr <- newForeignPtr_ ptr
  return (BSI.fromForeignPtr fptr 0 size)

fastIvfWarmup :: IO ()
fastIvfWarmup = do
  resources <- resourcesDir
  rc <- withCString resources c_fastIvfWarmup
  when (rc /= 0) $
    error "ivf_warmup: falha ao mapear indice"

fastIvfScore :: [Int] -> Int -> IO Int
fastIvfScore queryDims requestedProbe = do
  resources <- resourcesDir
  let probe = max 1 requestedProbe
      query = map fromIntegral (take dim (queryDims ++ repeat 0)) :: [CInt]
  score <- withArray query $ \ptr ->
    withCString resources $ \cresources ->
      c_fastIvfScore ptr cresources (fromIntegral probe)
  return (max 0 (min 5 (fromIntegral score)))

word32LE :: BS.ByteString -> Int -> Word32
word32LE bytes off =
  let b i = fromIntegral (BSU.unsafeIndex bytes (off + i)) :: Word32
  in b 0 .|. (b 1 `shiftL` 8) .|. (b 2 `shiftL` 16) .|. (b 3 `shiftL` 24)

float32LE :: BS.ByteString -> Int -> Float
float32LE bytes off = castWord32ToFloat (word32LE bytes off)

int16LEAt :: BS.ByteString -> Int -> Int
int16LEAt bytes off =
  let lo = fromIntegral (BSU.unsafeIndex bytes off) :: Int
      hi = fromIntegral (BSU.unsafeIndex bytes (off + 1)) :: Int
      raw = lo .|. (hi `shiftL` 8)
  in if raw >= 32768 then raw - 65536 else raw

loadIvfIndex :: IO IvfIndex
loadIvfIndex = do
  resources <- resourcesDir
  labels <- mmapFileReadOnly (resources ++ "/labels.bin")
  vectors <- mmapFileReadOnly (resources ++ "/vectors.bin")
  ivf <- mmapFileReadOnly (resources ++ "/ivf.bin")

  when (BS.length ivf < 28 || BS.take 8 ivf /= ivfMagic) $
    error "ivf_query: ivf.bin invalido"

  let hdrDim = fromIntegral (word32LE ivf 8) :: Int
      clusters = fromIntegral (word32LE ivf 12) :: Int
      probe = fromIntegral (word32LE ivf 16) :: Int
      indexN = fromIntegral (word32LE ivf 20) :: Int
      nVecs = BS.length labels
  when (hdrDim /= dim || indexN /= nVecs || clusters <= 0) $
    error "ivf_query: header ivf.bin invalido"
  when (BS.length vectors /= nVecs * dim * 2) $
    error "ivf_query: vectors.bin com tamanho invalido"

  let off = 28
      centroidBytes = clusters * dim * 4
      radiiBytes = clusters * 4
      boundaryBytes = (clusters + 1) * 4
      expected = off + centroidBytes + radiiBytes + boundaryBytes
  when (BS.length ivf /= expected) $
    error "ivf_query: payload ivf.bin invalido"

  let centroidSlice = BS.take centroidBytes (BS.drop off ivf)
      boundarySlice = BS.take boundaryBytes (BS.drop (off + centroidBytes + radiiBytes) ivf)
      centroidArr = array (0, clusters * dim - 1)
        [ (i, float32LE centroidSlice (i * 4)) | i <- [0 .. clusters * dim - 1] ]
      boundaryArr = array (0, clusters)
        [ (i, fromIntegral (word32LE boundarySlice (i * 4))) | i <- [0 .. clusters] ]

  return IvfIndex
    { idxNVecs = nVecs
    , idxNClusters = clusters
    , idxNProbe = if probe <= 0 then 1 else probe
    , idxDims = vectors
    , idxLabels = labels
    , idxCentroids = centroidArr
    , idxBoundaries = boundaryArr
    }

getIvfIndex :: IO IvfIndex
getIvfIndex =
  modifyMVar ivfRef $ \cached ->
    case cached of
      Just idx -> return (cached, idx)
      Nothing -> do
        idx <- loadIvfIndex
        return (Just idx, idx)

algoJsonCandidates :: IO [FilePath]
algoJsonCandidates = do
  configured <- lookupEnv "RINHA_ALGO_JSON"
  return $ case configured of
    Just path -> [path]
    Nothing -> ["/app/algo.json", "build/algo.json", "../build/algo.json", "algo.json"]

resolveAlgoJsonPath :: IO FilePath
resolveAlgoJsonPath = do
  candidates <- algoJsonCandidates
  go candidates
  where
    go [] = error "algo.rinha: nao encontrei algo.json; gere build/algo.json ou configure RINHA_ALGO_JSON"
    go (path:rest) = do
      exists <- doesFileExist path
      if exists then return path else go rest

loadAlgoProgram :: IO File
loadAlgoProgram = do
  path <- resolveAlgoJsonPath
  json <- BL.readFile path
  case decode json of
    Just ast -> return ast
    Nothing -> error ("algo.rinha: falha ao decodificar " ++ path)

getAlgoProgram :: IO File
getAlgoProgram =
  modifyMVar algoRef $ \cached ->
    case cached of
      Just fileAst -> return (cached, fileAst)
      Nothing -> do
        fileAst <- loadAlgoProgram
        return (Just fileAst, fileAst)

nestedInts :: Int -> ResultType -> [Int]
nestedInts 0 _ = []
nestedInts n (TupleResult (IntResult x) rest) = fromInteger x : nestedInts (n - 1) rest
nestedInts _ value = error ("ivf_query: vetor invalido " ++ show value)

dimAt :: IvfIndex -> Int -> Int -> Int
dimAt idx d v = int16LEAt (idxDims idx) (((d * idxNVecs idx) + v) * 2)

labelAt :: IvfIndex -> Int -> Int
labelAt idx v = if BSU.unsafeIndex (idxLabels idx) v == 0 then 0 else 1

boundaryAt :: IvfIndex -> Int -> Int
boundaryAt idx c = idxBoundaries idx ! c

centroidAt :: IvfIndex -> Int -> Int -> Float
centroidAt idx c d = idxCentroids idx ! ((c * dim) + d)

queryToQ16 :: [Int] -> [Float]
queryToQ16 = map (\v -> fromIntegral (v * q16Scale) / fromIntegral rinhaScale)

clusterDistance :: IvfIndex -> [Float] -> Int -> Float
clusterDistance idx query c = go 0 0
  where
    go d !acc
      | d >= dim = acc
      | otherwise =
          let diff = query !! d - centroidAt idx c d
          in go (d + 1) (acc + diff * diff)

vectorDistance :: IvfIndex -> [Float] -> Int -> Float
vectorDistance idx query v = go 0 0
  where
    go d !acc
      | d >= dim = acc
      | otherwise =
          let diff = query !! d - fromIntegral (dimAt idx d v)
          in go (d + 1) (acc + diff * diff)

toRinhaScale :: Int -> Int
toRinhaScale q16 =
  let v = (q16 * rinhaScale) `div` q16Scale
  in max (-rinhaScale) (min rinhaScale v)

candidateResult :: IvfIndex -> Int -> ResultType
candidateResult idx v =
  foldr TupleResult (IntResult (toInteger (labelAt idx v)))
    [ IntResult (toInteger (toRinhaScale (dimAt idx d v))) | d <- [0 .. dim - 1] ]

bestIvfClusters :: IvfIndex -> [Float] -> Int -> IO [Int]
bestIvfClusters idx query requestedProbe = do
  let probe = max 1 (min 32 requestedProbe)
  distances <- newArray (0, probe - 1) (1 / 0 :: Float) :: IO (IOUArray Int Float)
  ids <- newArray (0, probe - 1) (-1 :: Int) :: IO (IOUArray Int Int)
  countRef <- newIORef (0 :: Int)

  let findPos :: Int -> Int -> Float -> IO Int
      findPos pos count dist
        | pos >= count = return pos
        | otherwise = do
            current <- readArray distances pos
            if dist <= current
              then return pos
              else findPos (pos + 1) count dist

      shiftDown :: Int -> Int -> IO ()
      shiftDown slot pos
        | slot <= pos = return ()
        | otherwise = do
            prevDist <- readArray distances (slot - 1)
            prevId <- readArray ids (slot - 1)
            writeArray distances slot prevDist
            writeArray ids slot prevId
            shiftDown (slot - 1) pos

      insertCluster :: Int -> Float -> Int -> IO ()
      insertCluster count dist clusterId = do
        pos <- findPos 0 count dist
        let lastSlot = min count (probe - 1)
        shiftDown lastSlot pos
        writeArray distances pos dist
        writeArray ids pos clusterId
        when (count < probe) $
          writeIORef countRef (count + 1)

      consider :: Int -> IO ()
      consider clusterId = do
        let dist = clusterDistance idx query clusterId
        count <- readIORef countRef
        if count < probe
          then insertCluster count dist clusterId
          else do
            worst <- readArray distances (probe - 1)
            when (dist < worst) $
              insertCluster count dist clusterId

      loop :: Int -> IO ()
      loop clusterId
        | clusterId >= idxNClusters idx = return ()
        | otherwise = consider clusterId >> loop (clusterId + 1)

  loop 0
  count <- readIORef countRef
  mapM (readArray ids) [0 .. count - 1]

ivfTopIds :: IvfIndex -> [Float] -> Int -> [Int] -> IO [Int]
ivfTopIds idx query limit bestClusters = do
  distances <- newArray (0, limit - 1) (1 / 0 :: Float) :: IO (IOUArray Int Float)
  ids <- newArray (0, limit - 1) (-1 :: Int) :: IO (IOUArray Int Int)
  countRef <- newIORef (0 :: Int)

  let findPos :: Int -> Int -> Float -> IO Int
      findPos pos count dist
        | pos >= count = return pos
        | otherwise = do
            current <- readArray distances pos
            if dist <= current
              then return pos
              else findPos (pos + 1) count dist

      shiftDown :: Int -> Int -> IO ()
      shiftDown slot pos
        | slot <= pos = return ()
        | otherwise = do
            prevDist <- readArray distances (slot - 1)
            prevId <- readArray ids (slot - 1)
            writeArray distances slot prevDist
            writeArray ids slot prevId
            shiftDown (slot - 1) pos

      insertCandidate :: Int -> Float -> Int -> IO ()
      insertCandidate count dist vecId = do
        pos <- findPos 0 count dist
        let lastSlot = min count (limit - 1)
        shiftDown lastSlot pos
        writeArray distances pos dist
        writeArray ids pos vecId
        when (count < limit) $
          writeIORef countRef (count + 1)

      consider :: Int -> IO ()
      consider vecId = do
        let dist = vectorDistance idx query vecId
        count <- readIORef countRef
        if count < limit
          then insertCandidate count dist vecId
          else do
            worst <- readArray distances (limit - 1)
            when (dist < worst) $
              insertCandidate count dist vecId

      scanRange :: Int -> Int -> IO ()
      scanRange vecId end
        | vecId >= end = return ()
        | otherwise = consider vecId >> scanRange (vecId + 1) end

      scanCluster :: Int -> IO ()
      scanCluster c = scanRange (boundaryAt idx c) (boundaryAt idx (c + 1))

  mapM_ scanCluster bestClusters
  count <- readIORef countRef
  mapM (readArray ids) [0 .. count - 1]

ivfQuery :: IvfIndex -> [Int] -> Int -> Int -> IO ResultType
ivfQuery idx queryDims limit requestedProbe = do
  let query = queryToQ16 queryDims
  bestClusters <- bestIvfClusters idx query requestedProbe
  selected <- ivfTopIds idx query limit bestClusters
  return (foldr (rCons . candidateResult idx) rNil selected)

readNumMicros :: BS.ByteString -> Maybe Int
readNumMicros raw =
  let s0 = skipWsBS raw
      quoted = not (BS.null s0) && BS.head s0 == wQuote
      s1 = if quoted then BS.tail s0 else s0
      neg = not (BS.null s1) && BS.head s1 == wMinus
      s2 = if neg then BS.tail s1 else s1
      (wholeBytes, rest0) = BS.span isDigitByte s2
  in if BS.null wholeBytes
       then Nothing
       else
         let whole = BS.foldl' (\acc c -> acc * 10 + fromIntegral (c - wZero)) 0 wholeBytes
             (frac, _) =
               if not (BS.null rest0) && BS.head rest0 == wDot
                 then readFracMicros (BS.tail rest0) 0 0
                 else (0, rest0)
             signed = whole * 1000000 + frac
             value = if neg then negate signed else signed
         in Just value
  where
    readFracMicros s !acc !count
      | BS.null s =
          (acc * pow10i (6 - count), s)
      | isDigitByte (BS.head s) =
          if count < 6
            then readFracMicros (BS.tail s) (acc * 10 + fromIntegral (BS.head s - wZero)) (count + 1)
            else readFracMicros (BS.tail s) acc count
      | otherwise =
          (if count < 6 then acc * pow10i (6 - count) else acc, s)

pow10i :: Int -> Int
pow10i n
  | n <= 0 = 1
  | otherwise = 10 * pow10i (n - 1)

findAfter :: BS.ByteString -> BS.ByteString -> Maybe BS.ByteString
findAfter haystack needle =
  let (_, suffix) = BSC.breakSubstring needle haystack
  in if needle `BS.isPrefixOf` suffix
       then Just (BS.drop (BS.length needle) suffix)
       else Nothing

keyPatBS :: String -> BS.ByteString
keyPatBS keyName = BSC.pack ("\"" ++ keyName ++ "\"")

afterColonBS :: BS.ByteString -> BS.ByteString -> Maybe BS.ByteString
afterColonBS json key = do
  afterKey <- findAfter json key
  let afterKeyWs = skipWsBS afterKey
      (_, afterSeek) = BS.break (== wColon) afterKeyWs
  if BS.null afterSeek
    then Nothing
    else Just (skipWsBS (BS.tail afterSeek))

getNumMiBS :: BS.ByteString -> BS.ByteString -> Int -> Int
getNumMiBS json key fallback =
  fromMaybe fallback (afterColonBS json key >>= readNumMicros)

getBoolBS :: BS.ByteString -> BS.ByteString -> Int -> Int
getBoolBS json key fallback =
  case afterColonBS json key of
    Nothing -> fallback
    Just value
      | BS.null value -> fallback
      | BS.head value == 116 -> 1
      | otherwise -> 0

valueStringBS :: BS.ByteString -> BS.ByteString -> BS.ByteString
valueStringBS json key =
  case afterColonBS json key of
    Just value | not (BS.null value) && BS.head value == wQuote ->
      BS.takeWhile (/= wQuote) (BS.tail value)
    _ -> BS.empty

digitsValueBS :: BS.ByteString -> Int
digitsValueBS bytes = go bytes 0
  where
    go s !acc
      | BS.null s = acc
      | isDigitByte (BS.head s) = go (BS.tail s) (acc * 10 + fromIntegral (BS.head s - wZero))
      | otherwise = acc

digitAtBS :: BS.ByteString -> Int -> Int
digitAtBS bytes i =
  if i < BS.length bytes && isDigitByte (BS.index bytes i)
    then fromIntegral (BS.index bytes i - wZero)
    else 0

int2BS :: BS.ByteString -> Int -> Int
int2BS bytes i = digitAtBS bytes i * 10 + digitAtBS bytes (i + 1)

int4BS :: BS.ByteString -> Int -> Int
int4BS bytes i =
  digitAtBS bytes i * 1000 + digitAtBS bytes (i + 1) * 100 +
  digitAtBS bytes (i + 2) * 10 + digitAtBS bytes (i + 3)

daysFromCivilInt :: Int -> Int -> Int -> Int
daysFromCivilInt y0 m d =
  let y = if m < 3 then y0 - 1 else y0
      era = if y < 0 then (y - 399) `div` 400 else y `div` 400
      yoe = y - era * 400
      mp = if m > 2 then m - 3 else m + 9
      doy = (153 * mp + 2) `div` 5 + d - 1
      doe = yoe * 365 + yoe `div` 4 - yoe `div` 100 + doy
  in era * 146097 + doe - 719468

isoSecondsBS :: BS.ByteString -> Int
isoSecondsBS value =
  let y = int4BS value 0
      mo = int2BS value 5
      d = int2BS value 8
      h = int2BS value 11
      mi = int2BS value 14
      s = int2BS value 17
  in daysFromCivilInt y mo d * 86400 + h * 3600 + mi * 60 + s

mccRiskValue :: Int -> Int
mccRiskValue code =
  case code of
    5411 -> 614
    5812 -> 1229
    5912 -> 819
    5944 -> 1843
    7801 -> 3277
    7802 -> 3072
    7995 -> 3482
    4511 -> 1434
    5311 -> 1024
    5999 -> 2048
    _ -> 2048

classifyJsonBody :: Map.Map String ResultType -> BS.ByteString -> IO Int
classifyJsonBody baseEnv json
  | BS.null json || BS.length json > maxJsonBodyBytes = return 5
  | amountMi < 1 = return 5
  | otherwise = evalAlgoScore baseEnv algoScope
  where
    kTransaction = keyPatBS "transaction"
    kAmount = keyPatBS "amount"
    kInst = keyPatBS "installments"
    kAvg = keyPatBS "avg_amount"
    kTxCount = keyPatBS "tx_count_24h"
    kKmHome = keyPatBS "km_from_home"
    kKmCurr = keyPatBS "km_from_current"
    kOnline = keyPatBS "is_online"
    kPresent = keyPatBS "card_present"
    kMcc = keyPatBS "mcc"
    kReqAt = keyPatBS "requested_at"
    kTimestamp = keyPatBS "timestamp"
    kCustomer = keyPatBS "customer"
    kMerchant = keyPatBS "merchant"
    kTerminal = keyPatBS "terminal"
    kLastTx = keyPatBS "last_transaction"
    kId = keyPatBS "id"
    kKnown = keyPatBS "known_merchants"

    txJson = fromMaybe json (findAfter json kTransaction)
    custJson = fromMaybe json (findAfter json kCustomer)
    merchJson = fromMaybe json (findAfter json kMerchant)
    termJson = fromMaybe json (findAfter json kTerminal)

    amountMi = getNumMiBS txJson kAmount 0
    instMi = getNumMiBS txJson kInst 1000000
    custAvg0 = getNumMiBS custJson kAvg 0
    custAvgMi = if custAvg0 < 1 then amountMi else custAvg0
    merchAvg0 = getNumMiBS merchJson kAvg 0
    merchAvgMi = if merchAvg0 < 1 then amountMi else merchAvg0
    txCountMi = getNumMiBS custJson kTxCount 0
    kmHomeMi = getNumMiBS termJson kKmHome 0
    onlineV = getBoolBS termJson kOnline 0
    presentV = getBoolBS termJson kPresent 1
    mccCode = digitsValueBS (valueStringBS merchJson kMcc)
    mccV = mccRiskValue mccCode
    reqStr = valueStringBS txJson kReqAt
    reqHasDate = if BS.null reqStr then 0 else 1 :: Int
    reqHour = if reqHasDate == 1 then int2BS reqStr 11 else 12
    reqDays = if reqHasDate == 1 then daysFromCivilInt (int4BS reqStr 0) (int2BS reqStr 5) (int2BS reqStr 8) else 0
    reqDow = (reqDays + 3) - ((reqDays + 3) `div` 7) * 7
    reqSecs = if reqHasDate == 1 then isoSecondsBS reqStr else 0
    hasLast =
      case afterColonBS json kLastTx of
        Just value | not (BS.null value) && BS.head value /= 110 -> 1
        _ -> 0 :: Int
    lastStr = if hasLast == 1 then valueStringBS json kTimestamp else BS.empty
    lastSecs = if hasLast == 1 then if BS.null lastStr then reqSecs else isoSecondsBS lastStr else 0
    minutesLast = if hasLast == 1 then max 0 (reqSecs - lastSecs) `div` 60 else 0
    kmLastMi = if hasLast == 1 then getNumMiBS json kKmCurr 0 else 0
    merchIdStr = valueStringBS merchJson kId
    knownList = fromMaybe BS.empty (afterColonBS custJson kKnown)
    idInKnown = not (BS.null merchIdStr) && merchIdStr `BSC.isInfixOf` knownList
    unknownV = if idInKnown then 0 else 1
    algoScope = Map.fromList
      [ ("amount", rinhaInt (fromIntegral (amountMi `div` 10000)))
      , ("installments", rinhaInt (max 1 (fromIntegral (instMi `div` 1000000))))
      , ("cust_avg", rinhaInt (max 1 (fromIntegral (custAvgMi `div` 10000))))
      , ("hour", rinhaInt reqHour)
      , ("dow", rinhaInt reqDow)
      , ("has_last", rinhaInt hasLast)
      , ("minutes_last", rinhaInt minutesLast)
      , ("km_last", rinhaInt (max 0 (fromIntegral (kmLastMi `div` 1000))))
      , ("km_home", rinhaInt (max 0 (fromIntegral (kmHomeMi `div` 1000))))
      , ("tx_count", rinhaInt (max 0 (fromIntegral (txCountMi `div` 1000000))))
      , ("online", rinhaInt onlineV)
      , ("present", rinhaInt presentV)
      , ("unknown_m", rinhaInt unknownV)
      , ("mcc_risk", rinhaInt mccV)
      , ("merch_avg", rinhaInt (max 1 (fromIntegral (merchAvgMi `div` 10000))))
      ]

evalAlgoScore :: Map.Map String ResultType -> Map.Map String ResultType -> IO Int
evalAlgoScore baseEnv vars = do
  File _ expr _ <- getAlgoProgram
  result <- eval expr (Map.union vars baseEnv)
  case result of
    IntResult score -> return (max 0 (min 5 (fromInteger score)))
    other -> error ("algo.rinha: esperado score inteiro, recebeu " ++ show other)

skipWsBS :: BS.ByteString -> BS.ByteString
skipWsBS = BS.dropWhile (\c -> c == wSpace || c == wTab || c == wLf || c == wCr)

isDigitByte :: Word8 -> Bool
isDigitByte c = c >= wZero && c <= wNine

wSpace, wTab, wLf, wCr, wQuote, wColon, wDot, wMinus, wZero, wNine :: Word8
wSpace = 32
wTab = 9
wLf = 10
wCr = 13
wQuote = 34
wColon = 58
wDot = 46
wMinus = 45
wZero = 48
wNine = 57

builtinFraudScoreJson :: Map.Map String ResultType -> ResultType
builtinFraudScoreJson baseEnv = NativeResult "fraud_score_json" $ \args -> case args of
  [StringResult payload] ->
    IntResult . toInteger <$> classifyJsonBody baseEnv (BSC.pack payload)
  _ -> error "fraud_score_json: esperado (string)"

builtinIvfQuery :: ResultType
builtinIvfQuery = NativeResult "ivf_query" $ \args -> case args of
  [query, IntResult requestedLimit] -> do
    idx <- getIvfIndex
    probe <- envInt "RINHA_PROBE" (idxNProbe idx)
    let limit = max 1 (min maxCandidates (fromInteger requestedLimit))
    ivfQuery idx (nestedInts dim query) limit probe
  [query] -> do
    idx <- getIvfIndex
    probe <- envInt "RINHA_PROBE" (idxNProbe idx)
    ivfQuery idx (nestedInts dim query) 128 probe
  _ -> error "ivf_query: esperado (dvec, max_candidates)"

builtinIvfScore :: ResultType
builtinIvfScore = NativeResult "ivf_score" $ \args -> case args of
  [query] -> do
    probe <- envInt "RINHA_PROBE" 3
    IntResult . toInteger <$> fastIvfScore (nestedInts dim query) probe
  _ -> error "ivf_score: esperado (dvec)"

builtinIvfWarmup :: ResultType
builtinIvfWarmup = NativeResult "ivf_warmup" $ \args -> case args of
  [] -> fastIvfWarmup >> return (IntResult 0)
  _ -> error "ivf_warmup: esperado ()"