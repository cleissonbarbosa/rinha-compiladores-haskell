{-# LANGUAGE OverloadedStrings #-}

module Parse where

import Data.Aeson
import Terms(Term(..), File(..), Location(..), BinaryOp(..), Varr(..))
import qualified Data.Aeson.KeyMap as HM
import Debug.Trace

instance FromJSON File where
  parseJSON (Object v) = File <$>
                         v .: "name" <*>
                         v .: "expression" <*>
                         v .: "location"
  parseJSON _          = trace "Failed to parse AST" $ fail "Failed to parse AST"

instance FromJSON Term where
  parseJSON (Object v) = case HM.lookup "kind" v of
    Just "Print" -> Print <$> v .: "value" <*> v .: "location"
    Just "Let" -> Let <$> v .: "name" <*> v .: "value" <*> v .: "next" <*> v .: "location"
    Just "If" -> If <$> v .: "condition" <*> v .: "then" <*> v .: "otherwise" <*> v .: "location"
    Just "Function" -> Function <$> v .: "parameters" <*> v .: "value" <*> v .: "location"
    Just "Call" -> Call <$> v .: "callee" <*> v .: "arguments" <*> v .: "location"
    Just "Binary" -> Binary <$> v .: "lhs" <*> v .: "op" <*> v .: "rhs" <*> v .: "location"
    Just "Var" -> Var <$> v .: "text" <*> v .: "location"
    Just "Int" -> Terms.Int <$> v .: "value" <*> v .: "location"
    Just "Str" -> Str <$> v .: "value" <*> v .: "location"
    Just "Bool" -> Terms.Bool <$> v .: "value" <*> v .: "location"
    Just "First" -> First <$> v .: "value" <*> v .: "location"
    Just "Second" -> Second <$> v .: "value" <*> v .: "location"
    Just "Tuple" -> Tuple <$> v .: "first" <*> v .: "second" <*> v .: "location"
    -- print fail
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
