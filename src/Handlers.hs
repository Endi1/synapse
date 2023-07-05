{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Handlers (index, synapse) where

import Database.SQLite.Simple
import RIO (Bifunctor (second), Int64, MonadIO (liftIO))
import RIO.Text.Lazy (Text, take)
import Templates qualified
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Web.Scotty qualified as Scotty

index :: Scotty.ActionM ()
index =
  do
    rows <- liftIO getRows
    Scotty.html $ renderHtml $ Templates.index (map (second (RIO.Text.Lazy.take 25)) rows)

synapse :: Integer -> Scotty.ActionM ()
synapse rowID = do
  row <- liftIO $ getRow rowID
  Scotty.html $ renderHtml $ Templates.synapse (head row)

getRows :: IO [(Int64, Text)]
getRows = do
  conn <- open "/home/endi/.synapse.db"
  query_ conn "SELECT * FROM synapses" :: IO [(Int64, Text)]

getRow :: Integer -> IO [(Int64, Text)]
getRow rowID = do
  conn <- open "/home/endi/.synapse.db"
  query conn "SELECT * FROM synapses WHERE id=?" (Only rowID) :: IO [(Int64, Text)]