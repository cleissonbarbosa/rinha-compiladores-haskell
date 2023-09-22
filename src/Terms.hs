module Terms where

data AST = AST {
  name :: String,
  expression :: Expression,
  location :: Location
} deriving (Show, Eq)

data Expression = Print {
  printValue :: Value',
  printLocation :: Location
} deriving (Show, Eq)

data Value' = Str {
  strValue :: String,
  strLocation :: Location
} deriving (Show, Eq)

data Location = Location {
  start :: Int,
  end :: Int,
  filename :: String
} deriving (Show, Eq)

-- data Term = AST' AST | Expression Expression | Value' Value' | Location' Location deriving (Show, Eq)