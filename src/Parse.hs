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
  parseJSON _          = fail "Failed to parse AST"

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
    "Mod" -> return Mod
    "Eq" -> return Eq
    "Neq" -> return Neq
    "Lt" -> return Lt
    "Gt" -> return Gt
    "Leq" -> return Leq
    "Geq" -> return Geq
    "And" -> return And
    "Or" -> return Or
    _ -> fail "Invalid BinaryOp"
  parseJSON _ = trace "Failed to parse BinaryOp" $ fail "Failed to parse BinaryOp"
