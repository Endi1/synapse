{-# LANGUAGE OverloadedStrings #-}

module Parser (createConnections) where

import RIO.Text
import RIO.Text.Partial

createConnections :: String -> Text
createConnections content =
  let connections = parseConnections content
   in replaceConnections (pack content) $ Prelude.map pack connections

replaceConnections :: Text -> [Text] -> Text
replaceConnections = Prelude.foldr (\connection content -> replace ("[[" `append` connection `append` "]]") (synapseConnectionToMarkdownLink connection) content)

synapseConnectionToMarkdownLink :: Text -> Text
synapseConnectionToMarkdownLink connection = "[" `append` connection `append` "](" `append` "/" `append` connection `append` ")"

parseConnections :: String -> [String]
parseConnections [] = []
parseConnections (x : xs) = case x of
  '[' -> case openingBracket xs of
    (Just fn, s) -> fn : parseConnections s
    (Nothing, s) -> parseConnections s
  _ -> parseConnections xs

openingBracket :: String -> (Maybe String, String)
openingBracket [] = (Nothing, [])
openingBracket (x : xs) = case x of
  '[' -> fileName xs ""
  _ -> (Nothing, xs)

fileName :: String -> String -> (Maybe String, String)
fileName [] _ = (Nothing, [])
fileName (x : xs) fn = case x of
  ']' -> case closingBracket xs of
    (True, s) -> (Just fn, s)
    (False, s) -> (Nothing, s)
  _ -> fileName xs (fn ++ [x])

closingBracket :: String -> (Bool, String)
closingBracket [] = (False, [])
closingBracket (x : xs) = case x of
  ']' -> (True, xs)
  _ -> (False, xs)