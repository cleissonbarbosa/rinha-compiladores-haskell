module Code.Eval.Helpers where

import Control.Exception (SomeException, catch, try)
import Data.Maybe (fromMaybe)
import Network.Socket (Socket)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

import Code.Eval.Types

rNil :: ResultType
rNil = TupleResult (IntResult 0) (IntResult 0)

rCons :: ResultType -> ResultType -> ResultType
rCons h t = TupleResult (IntResult 1) (TupleResult h t)

catchAny :: IO a -> (SomeException -> IO a) -> IO a
catchAny = catch

tryAny :: IO a -> IO (Either SomeException a)
tryAny = try

asInt :: ResultType -> Integer
asInt (IntResult n) = n
asInt v = error ("esperado inteiro, recebeu " ++ show v)

asString :: ResultType -> String
asString (StringResult s) = s
asString v = error ("esperado string, recebeu " ++ show v)

asSocket :: ResultType -> Socket
asSocket (SocketResult s) = s
asSocket v = error ("esperado socket, recebeu " ++ show v)

rinhaInt :: Int -> ResultType
rinhaInt = IntResult . toInteger

okSocket :: Socket -> ResultType
okSocket sock = TupleResult (IntResult 1) (SocketResult sock)

portOf :: ResultType -> String
portOf (StringResult s) = s
portOf (IntResult n) = show n
portOf v = error ("porta invalida: " ++ show v)

splitHostPort :: String -> Maybe (String, String)
splitHostPort value =
  let (portRev, restRev) = break (== ':') (reverse value)
  in if null restRev || null portRev
       then Nothing
       else Just (reverse (tail restRev), reverse portRev)

envInt :: String -> Int -> IO Int
envInt varName fallback = do
  raw <- lookupEnv varName
  return $ case raw >>= readMaybe of
    Just n | n > 0 -> n
    _ -> fallback

resourcesDir :: IO FilePath
resourcesDir = fromMaybe "resources" <$> lookupEnv "RESOURCES_DIR"