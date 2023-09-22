{-# LANGUAGE OverloadedStrings #-}

module Parse where

import Common ()
import Data.Aeson
import Terms
    ( AST(AST), Expression(Print), Location(Location), Value'(Str) )


instance FromJSON AST where
  parseJSON (Object v) = AST <$>
                         v .: "name" <*>
                         v .: "expression" <*>
                         v .: "location"
  parseJSON _          = fail "Failed to parse AST"

instance FromJSON Expression where
  parseJSON (Object v) = Print <$>
                         v .: "value" <*>
                         v .: "location"
  parseJSON _          = fail "Failed to parse Expression"

instance FromJSON Value' where
  parseJSON (Object v) = Str <$>
                         v .: "value" <*>
                         v .: "location"
  parseJSON _          = fail "Failed to parse Value'"

instance FromJSON Location where
  parseJSON (Object v) = Location <$>
                         v .: "start" <*>
                         v .: "end" <*>
                         v .: "filename"
  parseJSON _          = fail "Failed to parse Location"