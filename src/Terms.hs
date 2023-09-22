module Terms where
  
data File = File {
  name :: String,
  expression :: Term,
  location :: Location
} deriving (Show, Eq)

data Varr = Varr {
  varText :: String,
  varLocation :: Location
} deriving (Show, Eq)

data Location = Location {
  start :: Integer,
  end :: Integer,
  filename :: String
} deriving (Show, Eq)

data BinaryOp = Add | Sub | Mul | Div | Mod | Eq | Neq | Lt | Gt | Leq | Geq | And | Or deriving (Enum, Show, Eq)

data Term = Let Varr Term Term Location
          | Binary Term BinaryOp Term Location
          | Int Integer Location
          | Bool Prelude.Bool Location
          | Str String Location
          | If Term Term Term Location
          | Call Term [Term] Location
          | Function [Varr] Term Location
          | Print Term Location
          | Var String Location
          deriving (Show, Eq)