module Common where

import Terms()

-- Exporte os tipos que são usados em Parse e Eval
exportedTypes :: [String]
exportedTypes = ["AST", "Expression", "Value'", "Location"]