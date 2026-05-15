module Code.Eval.Builtins (defaultEnv) where

import qualified Data.Map as Map

import Code.Eval.Builtins.Common (commonBuiltins)
import Code.Eval.Builtins.IVF (ivfBuiltins)
import Code.Eval.Builtins.Network (networkBuiltins)
import Code.Eval.Types (ResultType)

defaultEnv :: Map.Map String ResultType
defaultEnv = env
  where
    env = Map.fromList (commonBuiltins ++ networkBuiltins env ++ ivfBuiltins env)