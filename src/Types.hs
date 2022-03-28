{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Types (Note(..), Config(..)) where

import Data.Text ( Text )
import GHC.Generics
import Data.Yaml
import Control.Lens


data Note  = Note {
  _nIdentifier :: Text,
  _nDistFileName :: Text,
  _nRawContent :: Text,
  _nCompiledContent :: Text
} deriving (Show, Eq)


data Config = Config {
  cName :: Text,
  cIndex :: Text
} deriving (Show, Generic)

instance FromJSON Config where
  parseJSON (Object c) = Config
    <$> c .: "name"
    <*> c .: "index"