{-# LANGUAGE OverloadedStrings  #-}
module Main where
import Data.Text (Text, pack, replace, append, unpack)
import qualified Data.Text.IO as DTIO
import System.Directory
import Text.Regex.TDFA
import CMarkGFM
import Control.Monad (when)
import Templates (noteTemplate)
import Text.Blaze.Html.Renderer.Text
import Data.Text.Lazy (toStrict)
import Types
import Paths_synapse
import Data.Yaml (decodeFileEither, ParseException, prettyPrintParseException)
import RIO (fromEither)


getMarkdownFiles :: IO [FilePath]
getMarkdownFiles = do
  dirContents <- listDirectory "."
  return $ filter regexMatch dirContents
  where
    regexMatch :: String -> Bool
    regexMatch f = (f :: String) =~ ("\\.md$" :: String)

getNoteIdentifier :: FilePath -> String
getNoteIdentifier fp =
  let (identifier, _, _, _) = (fp :: String) =~ ("\\.md$" :: String) :: (String, String, String, [String])
  in
    identifier

getNoteInnerLinkIdentifiers :: String -> [Text]
getNoteInnerLinkIdentifiers noteContent =
  let submatches = (noteContent :: String) =~ ("\\[\\[([a-zA-Z]+(([[:space:]]|_|:|-|\\.|,|')*[a-zA-Z]+)*)\\]\\]" :: String) :: [[String]]
  in
    map (pack . (!! 1)) submatches

createNoteInnerLinks :: [Text] -> Text -> Text
createNoteInnerLinks outerLinkIdentifiers noteContent
  = foldr
      (\ outerLinkIdentifier
         -> replace
              ("[[" `append` outerLinkIdentifier `append` "]]")
              ("<a href=\"" `append` outerLinkIdentifier `append` ".html\">" `append` outerLinkIdentifier `append` "</a>"))
      noteContent outerLinkIdentifiers

createNote :: FilePath -> IO Note
createNote filePath = do
  content <- readFile filePath
  let contentWithInnerLinks = createNoteInnerLinks (getNoteInnerLinkIdentifiers content) (pack content)
  return $ Note (pack $ getNoteIdentifier filePath) (pack content) (commonmarkToHtml [optUnsafe] [extTable] contentWithInnerLinks)

preprocess :: IO ()
preprocess = do
  distExists <- doesDirectoryExist "./dist"
  when distExists $ removeDirectoryRecursive "./dist"
  cssFileLocation <- getDataFileName "resources/pico.min.css"
  createDirectory "./dist"
  createDirectory "./dist/style"
  copyFile cssFileLocation "./dist/style/pico.min.css"

writeNote :: Note -> IO ()
writeNote note = DTIO.writeFile ("./dist/" ++ unpack (nIdentifier note) ++ ".html") (toStrict $ renderHtml $ noteTemplate note)

createIndex :: Config -> [Note] -> Either Text (IO [Note])
createIndex config notes = 
  let
    indexNoteList = filter (\n -> nIdentifier n == cIndex config) notes
  in
    case indexNoteList of
      [] -> Left $ "Index with name " `append` cIndex config `append` " not found"
      _ -> Right $ writeNote (head indexNoteList) >> return (filter (\n -> nIdentifier n /= cIndex config) notes)

run :: IO (Either Text Text)
run = do
  config <- decodeFileEither "synapse.yaml" :: IO (Either ParseException Config)
  case config of
    Left e -> do 
      putStrLn "There was an issue while parsing the config file"
      return $ Left $ pack $ prettyPrintParseException e
    Right c -> do
      markdownFiles <- getMarkdownFiles
      allFiles <- mapM createNote markdownFiles
      preprocess
      case createIndex c allFiles of
        Left e -> return $ Left e
        Right ns -> do 
          mapM_ writeNote =<< ns
          return $ Right "Successfully compiled notes"


main :: IO ()
main = do
  runResult <- run
  case runResult of
    Left e -> print e
    Right s -> print s