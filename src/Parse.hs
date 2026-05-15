{-# LANGUAGE OverloadedStrings #-}

module Parse where

import Data.Aeson
import Terms(Term(..), File(..), Location(..), BinaryOp(..), Varr(..), RecordField(..))
import Debug.Trace

instance FromJSON File where
  parseJSON (Object v) = File <$>
                         v .: "name" <*>
                         v .: "expression" <*>
                         v .: "location"
  parseJSON _          = trace "Failed to parse AST" $ fail "Failed to parse AST"

instance FromJSON Term where
  parseJSON (Object v) = do
    kind <- v .: "kind"
    case (kind :: String) of
      "Print" -> Print <$> v .: "value" <*> v .: "location"
      "Let" -> Let <$> v .: "name" <*> v .: "value" <*> v .: "next" <*> v .: "location"
      "If" -> If <$> v .: "condition" <*> v .: "then" <*> v .: "otherwise" <*> v .: "location"
      "Function" -> Function <$> v .: "parameters" <*> v .: "value" <*> v .: "location"
      "Call" -> Call <$> v .: "callee" <*> v .: "arguments" <*> v .: "location"
      "Binary" -> Binary <$> v .: "lhs" <*> v .: "op" <*> v .: "rhs" <*> v .: "location"
      "Var" -> Var <$> v .: "text" <*> v .: "location"
      "Int" -> Terms.Int <$> v .: "value" <*> v .: "location"
      "Str" -> Str <$> v .: "value" <*> v .: "location"
      "Bool" -> Terms.Bool <$> v .: "value" <*> v .: "location"
      "First" -> First <$> v .: "value" <*> v .: "location"
      "Second" -> Second <$> v .: "value" <*> v .: "location"
      "Tuple" -> Tuple <$> v .: "first" <*> v .: "second" <*> v .: "location"
      "Record" -> Record <$> v .: "fields" <*> v .: "location"
      "Field" -> Field <$> v .: "value" <*> v .: "name" <*> v .: "location"
      _ -> trace "Invalid term type" $ fail "Invalid term type"

  parseJSON _ = trace "Failed to parse Term" $ fail "Failed to parse Term"

instance FromJSON Location where
  parseJSON (Object v) = Location <$>
                          v .: "start" <*>
                          v .: "end" <*>
                          v .: "filename"
  parseJSON _            = trace "Failed to parse Location" $ fail "Failed to parse Location"

instance FromJSON Varr where
  parseJSON (Object v) = Varr <$>
                          v .: "text" <*>
                          v .: "location"
  parseJSON _            = trace "Failed to parse Var" $ fail "Failed to parse Var"

instance FromJSON RecordField where
  parseJSON (Object v) = RecordField <$> v .: "name" <*> v .: "value" <*> v .: "location"
  parseJSON _ = trace "Failed to parse RecordField" $ fail "Failed to parse RecordField"

instance FromJSON BinaryOp where
  parseJSON (String v) = case v of
    "Add" -> return Add
    "Sub" -> return Sub
    "Mul" -> return Mul
    "Div" -> return Div
    "Rem" -> return Rem
    "Eq" -> return Eq
    "Neq" -> return Neq
    "Lt" -> return Lt
    "Gt" -> return Gt
    "Lte" -> return Lte
    "Gte" -> return Gte
    "And" -> return And
    "Or" -> return Or
    _ -> trace "Invalid BinaryOp" $ fail "Invalid BinaryOp"
  parseJSON _ = trace "Failed to parse BinaryOp" $ fail "Failed to parse BinaryOp"
