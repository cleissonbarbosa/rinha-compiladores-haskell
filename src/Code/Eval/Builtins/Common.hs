module Code.Eval.Builtins.Common (commonBuiltins) where

import Control.Concurrent (forkIO)
import Data.Bits ((.&.))
import Data.IORef (atomicModifyIORef', newIORef)
import Data.Maybe (fromMaybe)
import System.Environment (lookupEnv)
import System.IO (hSetBinaryMode, stdin)

import Code.Eval.Helpers
import Code.Eval.Runtime (applyClosedClosure)
import Code.Eval.Types

commonBuiltins :: [(String, ResultType)]
commonBuiltins =
  [ ("read_input", builtinReadInput)
  , ("str_chars", builtinStrChars)
  , ("str_len", builtinStrLen)
  , ("str_byte", builtinStrByte)
  , ("str_from_chars", builtinStrFromChars)
  , ("env", builtinEnv)
  , ("env_int", builtinEnvInt)
  , ("spawn", builtinSpawn)
  , ("counter_new", builtinCounterNew)
  , ("counter_next", builtinCounterNext)
  ]

builtinReadInput :: ResultType
builtinReadInput = NativeResult "read_input" $ \_ -> do
  hSetBinaryMode stdin True
  contents <- getContents
  return (StringResult contents)

builtinStrChars :: ResultType
builtinStrChars = NativeResult "str_chars" $ \args -> case args of
  [StringResult s] ->
    return (foldr (\c acc -> rCons (IntResult (toInteger (fromEnum c))) acc) rNil s)
  _ -> error "str_chars: esperado (string)"

builtinStrLen :: ResultType
builtinStrLen = NativeResult "str_len" $ \args -> case args of
  [StringResult s] -> return (IntResult (toInteger (length s)))
  _ -> error "str_len: esperado (string)"

builtinStrByte :: ResultType
builtinStrByte = NativeResult "str_byte" $ \args -> case args of
  [IntResult n] -> return (StringResult [toEnum (fromInteger (n .&. 255))])
  _ -> error "str_byte: esperado (int)"

builtinStrFromChars :: ResultType
builtinStrFromChars = NativeResult "str_from_chars" $ \args -> case args of
  [chars] -> return (StringResult (go chars))
  _ -> error "str_from_chars: esperado (lista)"
  where
    go (TupleResult (IntResult 0) _) = []
    go (TupleResult (IntResult 1) (TupleResult (IntResult c) rest)) =
      toEnum (fromInteger (c .&. 255)) : go rest
    go other = error ("str_from_chars: lista invalida " ++ show other)

builtinEnv :: ResultType
builtinEnv = NativeResult "env" $ \args -> case args of
  [StringResult varName, StringResult fallback] -> do
    value <- lookupEnv varName
    return (StringResult (fromMaybe fallback value))
  _ -> error "env: esperado (string, string)"

builtinEnvInt :: ResultType
builtinEnvInt = NativeResult "env_int" $ \args -> case args of
  [StringResult varName, IntResult fallback] -> do
    value <- envInt varName (fromInteger fallback)
    return (IntResult (toInteger value))
  _ -> error "env_int: esperado (string, int)"

builtinSpawn :: ResultType
builtinSpawn = NativeResult "spawn" $ \args -> case args of
  [ClosureResult closure] -> do
    _ <- forkIO $ do
      _ <- applyClosedClosure closure [] `catchAny` \e -> print e >> return rNil
      return ()
    return (IntResult 0)
  _ -> error "spawn: esperado (closure)"

builtinCounterNew :: ResultType
builtinCounterNew = NativeResult "counter_new" $ \args -> case args of
  [IntResult n] -> CounterResult <$> newIORef n
  _ -> error "counter_new: esperado (int)"

builtinCounterNext :: ResultType
builtinCounterNext = NativeResult "counter_next" $ \args -> case args of
  [CounterResult ref] -> do
    n <- atomicModifyIORef' ref (\cur -> (cur + 1, cur))
    return (IntResult n)
  _ -> error "counter_next: esperado (counter)"