module Code.Eval.Types where

import Data.IORef (IORef)
import qualified Data.Map as Map
import Network.Socket (Socket)
import Terms (Term, Varr, RecordField)

data Closure = Closure
  { params :: [Varr]
  , body :: Term
  , env :: Map.Map String ResultType
  }

instance Show Closure where
  show (Closure _ _ _) = "<#closure>"

data Void = Void
  deriving (Show, Eq)

data ResultType
  = Term Term
  | StringResult String
  | BoolResult Bool
  | ClosureResult Closure
  | IntResult Integer
  | TupleResult ResultType ResultType
  | RecordResult (Map.Map String ResultType)
  | NativeResult String ([ResultType] -> IO ResultType)
  | SocketResult Socket
  | CounterResult (IORef Integer)

instance Show ResultType where
  show (Term term) = show term
  show (StringResult str) = str
  show (BoolResult bool) = show bool
  show (ClosureResult closure) = show closure
  show (IntResult int) = show int
  show (TupleResult first second) = "(" ++ show first ++ ", " ++ show second ++ ")"
  show (RecordResult fields) = "<#record:" ++ show (Map.keys fields) ++ ">"
  show (NativeResult nativeName _) = "<#native:" ++ nativeName ++ ">"
  show (SocketResult _) = "<#socket>"
  show (CounterResult _) = "<#counter>"