{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Commands (execute) where

import Database.SQLite.Simple qualified as DB
import Handlers qualified
import Parser (createConnections)
import RIO (Int64)
import RIO.List (headMaybe)
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
  file <- createConnections <$> readFile (head args)
  conn <- DB.open "/home/endi/.synapse.db"
  existing <- DB.query conn "SELECT id FROM synapses WHERE name=?" (DB.Only $ head args) :: IO [DB.Only Int64]
  case headMaybe existing of
    Just i -> do
      _ <- DB.execute conn "UPDATE synapses SET content=?" (DB.Only file)
      return $ DB.fromOnly i
    Nothing -> do
      _ <- DB.execute conn "INSERT INTO synapses (content, name) VALUES (?, ?)" (file, head args)
      DB.lastInsertRowId conn

serve' :: IO ()
serve' =
  Scotty.scotty 3000 $ do
    Scotty.get "/" Handlers.index
    Scotty.get "/:name" $ do
      name <- Scotty.param "name"
      Handlers.synapse name