{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Handlers (index, synapse) where

import Database.SQLite.Simple
import RIO (MonadIO (liftIO))
import RIO.List (headMaybe)
import RIO.Text (Text)
import Templates qualified
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Types (Synapse)
import Web.Scotty qualified as Scotty

index :: Scotty.ActionM ()
index =
  do
    rows <- liftIO getRows
    Scotty.html $ renderHtml $ Templates.index rows

synapse :: Text -> Scotty.ActionM ()
synapse name = do
  row <- liftIO $ getRow name
  case row of
    Nothing -> Scotty.text "Synapse could not be found"
    Just s -> Scotty.html $ renderHtml $ Templates.synapse s

getRows :: IO [Synapse]
getRows = do
  conn <- open "/home/endi/.synapse.db"
  query_ conn "SELECT id, name, content FROM synapses" :: IO [Synapse]

getRow :: Text -> IO (Maybe Synapse)
getRow name = do
  conn <- open "/home/endi/.synapse.db"
  rows <- query conn "SELECT id, name, content FROM synapses WHERE name=?" (Only name) :: IO [Synapse]
  return $ headMaybe rows