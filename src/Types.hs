{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Types (Note(..), Config(..)) where

import Data.Text ( Text )
import GHC.Generics
import Data.Yaml


data Note  = Note {
  nIdentifier :: Text,
  nRawContent :: Text,
  nCompiledContent :: Text
} deriving Show

data Config = Config {
  cName :: Text,
  cIndex :: Text
} deriving (Show, Generic)

instance FromJSON Config where
  parseJSON (Object c) = Config
    <$> c .: "name"
    <*> c .: "index"