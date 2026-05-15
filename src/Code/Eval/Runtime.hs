module Code.Eval.Runtime
  ( applyClosure
  , applyClosedClosure
  , eval
  , evalBinaryOp
  , interpretWith
  ) where

import Debug.Trace (trace)
import qualified Data.Map as Map

import Code.Eval.Types
import Terms (BinaryOp(..), File(..), RecordField(..), Term(..), Varr(..))

applyClosure :: Closure -> [ResultType] -> Map.Map String ResultType -> IO ResultType
applyClosure (Closure closureParams closureBody closureEnv) argResults callerScope = do
  let paramNames = map (\(Varr text _) -> text) closureParams
      newScope = Map.union (Map.fromList (zip paramNames argResults)) closureEnv
  eval closureBody (Map.union newScope callerScope)

applyClosedClosure :: Closure -> [ResultType] -> IO ResultType
applyClosedClosure closure args = applyClosure closure args Map.empty

interpretWith :: Map.Map String ResultType -> File -> IO ResultType
interpretWith scope (File _ expr _) = eval expr scope

eval :: Term -> Map.Map String ResultType -> IO ResultType
eval term scope = case term of
  Print value _ -> do
    v <- eval value scope
    case v of
      StringResult str -> putStrLn str >> return (StringResult str)
      IntResult intValue -> print intValue >> return (IntResult intValue)
      anyValue -> print anyValue >> return anyValue
  Int intValue _ -> return (IntResult intValue)
  Str strValue _ -> return (StringResult strValue)
  Bool boolValue _ -> return (BoolResult boolValue)
  Function value param _ -> return (ClosureResult (Closure value param scope))
  Call callee arguments _ -> do
    closure <- eval callee scope
    case closure of
      ClosureResult (Closure closureParams closureBody closureEnv) -> do
        argResults <- mapM (\arg -> eval arg scope) arguments
        applyClosure (Closure closureParams closureBody closureEnv) argResults scope
      NativeResult _ nativeFn -> do
        argResults <- mapM (\arg -> eval arg scope) arguments
        nativeFn argResults
      _ -> trace (show closure) $ error "Invalid closure"
  Var text _ ->
    case Map.lookup text scope of
      Just value -> return value
      Nothing -> trace (show scope) $ error ("Invalid variable " ++ text)
  Binary lhs op rhs _ -> evalBinaryOp op lhs rhs scope
  If condition ifThen ifOtherwise _ -> do
    cond <- eval condition scope
    case cond of
      BoolResult True -> eval ifThen scope
      BoolResult False -> eval ifOtherwise scope
      _ -> error "Invalid condition"
  Let lname value next _ -> do
    v <- eval value scope
    let name = case lname of
          Varr text _ -> text
        newScope = Map.insert name v scope
    eval next newScope
  Tuple first second _ -> do
    firstEval <- eval first scope
    secondEval <- eval second scope
    return (TupleResult firstEval secondEval)
  Record fields _ -> do
    evaluated <- mapM (evalRecordField scope) fields
    return (RecordResult (Map.fromList evaluated))
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
  Field value (Varr name _) _ -> do
    valueEval <- eval value scope
    case valueEval of
      RecordResult fields ->
        case Map.lookup name fields of
          Just fieldValue -> return fieldValue
          Nothing -> error ("campo inexistente: " ++ name)
      _ -> error "Invalid field access"
  Error message _ _ -> error message

evalRecordField :: Map.Map String ResultType -> RecordField -> IO (String, ResultType)
evalRecordField scope (RecordField (Varr name _) value _) = do
  valueEval <- eval value scope
  return (name, valueEval)

evalBinaryOp :: BinaryOp -> Term -> Term -> Map.Map String ResultType -> IO ResultType
evalBinaryOp op lhs rhs scope = case op of
  Add -> do
    addLeft <- eval lhs scope
    addRight <- eval rhs scope
    case (addLeft, addRight) of
      (IntResult leftVal, IntResult rightVal) -> return (IntResult (leftVal + rightVal))
      (left, right) -> return (StringResult (show left ++ show right))
  Sub -> do
    subLeft <- eval lhs scope
    subRight <- eval rhs scope
    case (subLeft, subRight) of
      (IntResult left, IntResult right) -> return (IntResult (left - right))
      _ -> error "Invalid subtraction"
  Mul -> do
    mulLeft <- eval lhs scope
    mulRight <- eval rhs scope
    case (mulLeft, mulRight) of
      (IntResult left, IntResult right) -> return (IntResult (left * right))
      _ -> error "Invalid multiplication"
  Div -> do
    divLeft <- eval lhs scope
    divRight <- eval rhs scope
    case (divLeft, divRight) of
      (IntResult left, IntResult right) -> return (IntResult (left `div` right))
      _ -> error "Invalid division"
  Rem -> do
    remLeft <- eval lhs scope
    remRight <- eval rhs scope
    case (remLeft, remRight) of
      (IntResult left, IntResult right) -> return (IntResult (left `rem` right))
      _ -> error "Invalid remainder"
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
  Gt -> do
    gtLeft <- eval lhs scope
    gtRight <- eval rhs scope
    case (gtLeft, gtRight) of
      (IntResult left, IntResult right) -> return (BoolResult (left > right))
      (StringResult left, StringResult right) -> return (BoolResult (left > right))
      (Term left, IntResult right) -> do
        leftEval <- eval left scope
        case leftEval of
          IntResult leftVal -> return (BoolResult (leftVal > right))
          _ -> error "Invalid greater than"
      (IntResult left, Term right) -> do
        rightEval <- eval right scope
        case rightEval of
          IntResult rightVal -> return (BoolResult (left > rightVal))
          _ -> error "Invalid greater than"
      _ -> error "Invalid greater than"
  Lte -> do
    lteLeft <- eval lhs scope
    lteRight <- eval rhs scope
    case (lteLeft, lteRight) of
      (IntResult left, IntResult right) -> return (BoolResult (left <= right))
      (StringResult left, StringResult right) -> return (BoolResult (left <= right))
      (Term left, IntResult right) -> do
        leftEval <- eval left scope
        case leftEval of
          IntResult leftVal -> return (BoolResult (leftVal <= right))
          _ -> error "Invalid less than or equal"
      (IntResult left, Term right) -> do
        rightEval <- eval right scope
        case rightEval of
          IntResult rightVal -> return (BoolResult (left <= rightVal))
          _ -> error "Invalid less than or equal"
      _ -> error "Invalid less than or equal"
  Gte -> do
    gteLeft <- eval lhs scope
    gteRight <- eval rhs scope
    case (gteLeft, gteRight) of
      (IntResult left, IntResult right) -> return (BoolResult (left >= right))
      (StringResult left, StringResult right) -> return (BoolResult (left >= right))
      (Term left, IntResult right) -> do
        leftEval <- eval left scope
        case leftEval of
          IntResult leftVal -> return (BoolResult (leftVal >= right))
          _ -> error "Invalid greater than or equal"
      (IntResult left, Term right) -> do
        rightEval <- eval right scope
        case rightEval of
          IntResult rightVal -> return (BoolResult (left >= rightVal))
          _ -> error "Invalid greater than or equal"
      _ -> error "Invalid greater than or equal"
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
  Neq -> do
    neqLeft <- eval lhs scope
    neqRight <- eval rhs scope
    case (neqLeft, neqRight) of
      (IntResult left, IntResult right) -> return (BoolResult (left /= right))
      (StringResult left, StringResult right) -> return (BoolResult (left /= right))
      (BoolResult left, BoolResult right) -> return (BoolResult (left /= right))
      (Term left, Term right) -> do
        leftEval <- eval left scope
        rightEval <- eval right scope
        case (leftEval, rightEval) of
          (IntResult leftVal, IntResult rightVal) -> return (BoolResult (leftVal /= rightVal))
          (StringResult leftVal, StringResult rightVal) -> return (BoolResult (leftVal /= rightVal))
          (BoolResult leftVal, BoolResult rightVal) -> return (BoolResult (leftVal /= rightVal))
          _ -> error "Invalid equality"
      (Term left, IntResult right) -> do
        leftEval <- eval left scope
        case leftEval of
          IntResult leftVal -> return (BoolResult (leftVal /= right))
          _ -> error "Invalid equality"
      (IntResult left, Term right) -> do
        rightEval <- eval right scope
        case rightEval of
          IntResult rightVal -> return (BoolResult (left /= rightVal))
          _ -> error "Invalid equality"
      (StringResult left, IntResult right) -> return (BoolResult (left /= show right))
      (IntResult left, StringResult right) -> return (BoolResult (show left /= right))
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
  And -> do
    andLeft <- eval lhs scope
    andRight <- eval rhs scope
    case (andLeft, andRight) of
      (BoolResult left, BoolResult right) -> return (BoolResult (left && right))
      (Term left, Term right) -> do
        leftEval <- eval left scope
        rightEval <- eval right scope
        case (leftEval, rightEval) of
          (BoolResult leftVal, BoolResult rightVal) -> return (BoolResult (leftVal && rightVal))
          _ -> error "Invalid and"
      _ -> error "Invalid and"