module Eval where
    
import Terms(File(..), Term(..), Varr(..), BinaryOp(..))
import qualified Data.Map as Map
import Debug.Trace

data Closure = Closure {
    params :: [Varr],
    body :: Term,
    env ::  Map.Map String ResultType
}

instance Show Closure where
  show (Closure _ _ _) = "<#closure>"

data Void = Void {
}deriving (Show, Eq)


data ResultType = Term Term | StringResult String | BoolResult Bool | ClosureResult Closure | VoidResult Void | IntResult Integer | TupleResult ResultType ResultType

instance Show ResultType where
  show (Term term) = show term
  show (StringResult str) = str
  show (BoolResult bool) = show bool
  show (ClosureResult closure) = show closure
  show (VoidResult void) = show void
  show (IntResult int) = show int
  show (TupleResult first second) = "(" ++ (show first) ++ ", " ++ (show second) ++ ")"

eval :: Term -> Map.Map String ResultType -> IO (ResultType)
eval term scope = case term of
  Print value _ -> do
    v <- eval value scope
    case v of
      StringResult str -> putStrLn str >> return (VoidResult Void)
      IntResult int -> print int >> return (VoidResult Void)
      _ -> trace (show value) $ error "Invalid print value"
  Int intValue _ -> return (IntResult intValue)
  Str strValue _ -> return (StringResult strValue)
  Bool boolValue _ -> return (BoolResult boolValue)
  Function value param _ -> return (ClosureResult (Closure value param scope))
  Call callee arguments _ -> do
    closure <- eval callee scope
    case closure of
      ClosureResult (Closure clousureParams clousureBody _) -> do
        let paramNames = map (\(Varr text _) -> text) clousureParams
            argValues = mapM (\arg -> eval arg scope) arguments
        argResults <- argValues
        let new_scope = Map.fromList (zip paramNames argResults)
        eval clousureBody (Map.union new_scope scope)
      _ -> trace (show closure) $ error "Invalid closure"
  Var text _ -> do
    let varLookup = Map.lookup text scope
    case varLookup of
      Just value -> return value
      Nothing -> trace (show scope) $ error ("Invalid variable " ++ text)
  Binary lhs op rhs _ -> case op of
    Add -> do
      addLeft <- eval lhs scope
      addRight <- eval rhs scope
      case (addLeft, addRight) of
        (StringResult left, StringResult right) -> return (StringResult (left ++ right))
        (StringResult left, IntResult right) -> return (StringResult (left ++ (show right)))
        (IntResult left, StringResult right) -> return (StringResult ((show left) ++ right))
        (IntResult leftVal, IntResult rightVal) -> return (IntResult (leftVal + rightVal))
        _ -> error "Invalid addition"
    Sub -> do
      sleft <- eval lhs scope
      sright <- eval rhs scope
      case (sleft, sright) of
        (IntResult left, IntResult right) -> return (IntResult (left - right))
        _ -> error "Invalid subtraction"
    Lt -> do
      ltLeft <- eval lhs scope
      ltRight <- eval rhs scope
      case (ltLeft, ltRight) of
        (IntResult left, IntResult right) -> return (BoolResult (left < right))
        (StringResult left, StringResult right) -> return (BoolResult (left < right))
        (Term left, IntResult right) -> do
          leftEval <- eval left scope
          case leftEval of
            IntResult leftVal -> return (BoolResult (leftVal < right))
            _ -> error "Invalid less than"
        (IntResult left, Term right) -> do
          rightEval <- eval right scope
          case rightEval of
            IntResult rightVal -> return (BoolResult (left < rightVal))
            _ -> error "Invalid less than"
        _ -> error "Invalid less than"
    Eq -> do
      eqLeft <- eval lhs scope
      eqRight <- eval rhs scope
      case (eqLeft, eqRight) of
        (IntResult left, IntResult right) -> return (BoolResult (left == right))
        (StringResult left, StringResult right) -> return (BoolResult (left == right))
        (BoolResult left, BoolResult right) -> return (BoolResult (left == right))
        (Term left, Term right) -> do
          leftEval <- eval left scope
          rightEval <- eval right scope
          case (leftEval, rightEval) of
            (IntResult leftVal, IntResult rightVal) -> return (BoolResult (leftVal == rightVal))
            (StringResult leftVal, StringResult rightVal) -> return (BoolResult (leftVal == rightVal))
            (BoolResult leftVal, BoolResult rightVal) -> return (BoolResult (leftVal == rightVal))
            _ -> error "Invalid equality"
        (Term left, IntResult right) -> do
          leftEval <- eval left scope
          case leftEval of
            IntResult leftVal -> return (BoolResult (leftVal == right))
            _ -> error "Invalid equality"
        (IntResult left, Term right) -> do
          rightEval <- eval right scope
          case rightEval of
            IntResult rightVal -> return (BoolResult (left == rightVal))
            _ -> error "Invalid equality"
        _ -> error "Invalid equality"
    Or -> do
      orLeft <- eval lhs scope
      orRight <- eval rhs scope
      case (orLeft, orRight) of
        (BoolResult left, BoolResult right) -> return (BoolResult (left || right))
        (Term left, Term right) -> do
          leftEval <- eval left scope
          rightEval <- eval right scope
          case (leftEval, rightEval) of
            (BoolResult leftVal, BoolResult rightVal) -> return (BoolResult (leftVal || rightVal))
            _ -> error "Invalid or"
        _ -> error "Invalid or"
    _ -> error "Invalid binary operator"

  If condition ifThen ifOtherwise _ -> do
    cond <- eval condition scope
    case cond of
      BoolResult True -> eval ifThen scope
      BoolResult False -> eval ifOtherwise scope
      _ -> error "Invalid condition"
  Let lname value next _ -> do 
    v <- eval value scope
    letName <- case lname of
      Varr text _ -> return text
    new_scope <- (Map.insert letName v) <$> return scope
    eval next new_scope
  Tuple first second _ -> do
    firstEval <- eval first scope
    secondEval <- eval second scope
    return (TupleResult firstEval secondEval)
  First tuple _ -> do
    tupleEval <- eval tuple scope
    case tupleEval of
      TupleResult first _ -> return first
      _ -> error "Invalid first"
  Second tuple _ -> do
    tupleEval <- eval tuple scope
    case tupleEval of
      TupleResult _ second -> return second
      _ -> error "Invalid second"
  Error message _ _ -> error message

interpret :: File -> IO (ResultType)
interpret (File _ expr _) = eval expr Map.empty
