module Types (Note(..)) where

import Data.Text ( Text )


data Note  = Note {
  nIdentifier :: Text,
  nRawContent :: Text,
  nCompiledContent :: Text
} deriving Show