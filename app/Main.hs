{-# LANGUAGE OverloadedStrings  #-}

module Main where
import Data.Text (Text, pack)
import System.Directory ( listDirectory )
import Data.Yaml (decodeFileEither, ParseException, prettyPrintParseException)
import Types ( Config, Synapse(..) )
import SynapseUtils (
  getMarkdownFiles,
  preprocess,
  writeNote,
  createSynapse
  )


run :: IO (Either Text Text)
run = do
  config <- decodeFileEither "synapse.yaml" :: IO (Either ParseException Config)
  case config of
    Left e -> do
      putStrLn "There was an issue while parsing the config file"
      return $ Left $ pack $ prettyPrintParseException e
    Right c -> do
      allFiles <- listDirectory "."
      noteFiles <- mapM (\fp -> do
                                    fileContent <- pack <$> readFile fp
                                    return (pack fp, fileContent)) (getMarkdownFiles allFiles)
      preprocess
      case createSynapse c noteFiles of
        Left e -> return $ Left e
        Right synapse -> do
          writeNote $ sIndexNote synapse
          mapM_ writeNote (sRestOfNotes synapse)
          return $ Right "Successfully compiled notes"


main :: IO ()
main = do
  runResult <- run
  case runResult of
    Left e -> print e
    Right s -> print s