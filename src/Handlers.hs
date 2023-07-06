{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Handlers (index, synapse) where

import Database.SQLite.Simple
import RIO (MonadIO (liftIO))
import RIO.List (headMaybe)
import Templates qualified
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Types (Synapse)
import Web.Scotty qualified as Scotty

index :: Scotty.ActionM ()
index =
  do
    rows <- liftIO getRows
    Scotty.html $ renderHtml $ Templates.index rows

synapse :: Integer -> Scotty.ActionM ()
synapse rowID = do
  row <- liftIO $ getRow rowID
  case row of
    Nothing -> Scotty.text "Synapse could not be found"
    Just s -> Scotty.html $ renderHtml $ Templates.synapse s

getRows :: IO [Synapse]
getRows = do
  conn <- open "/home/endi/.synapse.db"
  query_ conn "SELECT id, name, content FROM synapses" :: IO [Synapse]

getRow :: Integer -> IO (Maybe Synapse)
getRow rowID = do
  conn <- open "/home/endi/.synapse.db"
  rows <- query conn "SELECT id, name, content FROM synapses WHERE id=?" (Only rowID) :: IO [Synapse]
  return $ headMaybe rows