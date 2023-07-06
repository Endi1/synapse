{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Commands (execute) where

import Database.SQLite.Simple qualified as DB
import Handlers qualified
import RIO (Int64)
import Types (Arguments, Command (..), command, options)
import Web.Scotty qualified as Scotty

execute :: Arguments -> IO ()
execute args = case command args of
  Import -> do
    insertedRowId <- import' $ options args
    print $ "Inserted with row ID: " ++ show insertedRowId
  Serve -> serve'

import' :: [String] -> IO Int64
import' args = do
  file <- readFile (head args)
  conn <- DB.open "/home/endi/.synapse.db"
  _ <- DB.execute conn "INSERT INTO synapses (content, name) VALUES (?, ?)" (file, head args)
  DB.lastInsertRowId conn

serve' :: IO ()
serve' =
  Scotty.scotty 3000 $ do
    Scotty.get "/" Handlers.index
    Scotty.get "/:id" $ do
      rowID <- Scotty.param "id"
      Handlers.synapse rowID