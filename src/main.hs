{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as B
import System.Environment

data AST = AST {
  name :: String,
  expression :: Expression,
  location :: Location
} deriving (Show)

data Expression = Print {
  printValue :: Value',
  printLocation :: Location
} deriving (Show)

data Value' = Str {
  strValue :: String,
  strLocation :: Location
} deriving (Show)

data Location = Location {
  start :: Int,
  end :: Int,
  filename :: String
} deriving (Show)

instance FromJSON AST where
  parseJSON (Object v) = AST <$>
                         v .: "name" <*>
                         v .: "expression" <*>
                         v .: "location"
  parseJSON _          = fail "Failed to parse AST"

instance FromJSON Expression where
  parseJSON (Object v) = Print <$>
                         v .: "value" <*>
                         v .: "location"
  parseJSON _          = fail "Failed to parse Expression"

instance FromJSON Value' where
  parseJSON (Object v) = Str <$>
                         v .: "value" <*>
                         v .: "location"
  parseJSON _          = fail "Failed to parse Value'"

instance FromJSON Location where
  parseJSON (Object v) = Location <$>
                         v .: "start" <*>
                         v .: "end" <*>
                         v .: "filename"
  parseJSON _          = fail "Failed to parse Location"

interpret :: AST -> IO ()
interpret (AST _ expr _) = case expr of
  Print value _ -> putStrLn (strValue value)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> do
      json <- B.readFile filename
      case decode json of
        Just ast -> interpret ast
        Nothing -> putStrLn "Failed to parse JSON"
    _ -> putStrLn "Usage: program <filename>"