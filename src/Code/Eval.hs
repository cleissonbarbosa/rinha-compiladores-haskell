module Code.Eval (interpret) where

import Code.Eval.Builtins (defaultEnv)
import Code.Eval.Runtime (interpretWith)
import Code.Eval.Types (ResultType)
import Terms (File)

interpret :: File -> IO ResultType
interpret = interpretWith defaultEnv