{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- |
-- Indice vetorial IVF nativo. Mantem apenas os primitivos que nao podem ser
-- escritos em Rinha: o mmap dos arquivos do indice e a recuperacao dos
-- candidatos mais proximos (`ivf_query`). A vetorizacao da transacao, o kNN
-- final e a politica de fraude vivem em `rinha/server.rinha`.
module Code.Eval.Builtins.IVF
  ( ivfBuiltins
  ) where

import Control.Concurrent.MVar (MVar, modifyMVar, newMVar)
import Control.Monad (when)
import Data.Array.IO (IOUArray, newArray, readArray, writeArray)
import Data.Array.Unboxed (UArray, array, (!))
import Data.Bits (shiftL, (.|.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Unsafe as BSU
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Word (Word8, Word32)
import Foreign.C.String (CString, withCString)
import Foreign.C.Types (CInt(..), CLong(..), CSize(..))
import Foreign.ForeignPtr (newForeignPtr_)
import Foreign.Ptr (Ptr, intPtrToPtr, nullPtr)
import GHC.Float (castWord32ToFloat)
import System.IO (IOMode(ReadMode), hFileSize, withFile)
import System.IO.Unsafe (unsafePerformIO)

import Code.Eval.Helpers
import Code.Eval.Types

foreign import ccall unsafe "open"
  c_open :: CString -> CInt -> IO CInt

foreign import ccall unsafe "close"
  c_close :: CInt -> IO CInt

foreign import ccall unsafe "mmap"
  c_mmap :: Ptr () -> CSize -> CInt -> CInt -> CInt -> CLong -> IO (Ptr Word8)

dim :: Int
dim = 14

rinhaScale :: Int
rinhaScale = 4096

q16Scale :: Int
q16Scale = 32767

maxCandidates :: Int
maxCandidates = 512

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

-- | Apenas dois builtins ficam nativos: a recuperacao de candidatos e o
-- aquecimento (mmap) do indice antes de aceitar trafego.
ivfBuiltins :: [(String, ResultType)]
ivfBuiltins =
  [ ("ivf_query", builtinIvfQuery)
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

-- | Forca o mmap/parse do indice antes de aceitar trafego, para que a
-- primeira request nao pague o custo de carga.
builtinIvfWarmup :: ResultType
builtinIvfWarmup = NativeResult "ivf_warmup" $ \args -> case args of
  [] -> getIvfIndex >> return (IntResult 0)
  _ -> error "ivf_warmup: esperado ()"
