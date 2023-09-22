module Eval where
    
import Terms

interpret :: AST -> IO ()
interpret (AST _ expr _) = case expr of
  Print value _ -> putStrLn (strValue value)