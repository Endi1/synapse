{-# LANGUAGE OverloadedStrings  #-}
module Main where
import Data.Text (Text, pack, replace, append, unpack)
import qualified Data.Text.IO as DTIO
import System.Directory
import Text.Regex.TDFA
import CMark
import Control.Monad (when)
import Templates (noteTemplate)
import Text.Blaze.Html.Renderer.Text
import Data.Text.Lazy (toStrict)
import Types
import Paths_synapse
import Data.Yaml (decodeFileEither, ParseException, prettyPrintParseException)


getMarkdownFiles :: IO [FilePath]
getMarkdownFiles = do
  dirContents <- listDirectory "."
  return $ filter regexMatch dirContents
  where
    regexMatch :: String -> Bool
    regexMatch f = (f :: String) =~ ("\\.md$" :: String)

getNoteIdentifier :: FilePath -> String
getNoteIdentifier fp =
  let (_, _, _, matches) = (fp :: String) =~ ("([a-zA-Z]+)\\.md$" :: String) :: (String, String, String, [String])
  in
    head matches

getNoteInnerLinkIdentifiers :: String -> [Text]
getNoteInnerLinkIdentifiers noteContent =
  let submatches = (noteContent :: String) =~ ("\\[\\[([a-zA-Z]+)\\]\\]" :: String) :: [[String]]
  in
    map (pack . (!! 1)) submatches

createNoteInnerLinks :: [Text] -> Text -> Text
createNoteInnerLinks outerLinkIdentifiers noteContent
  = foldr
      (\ outerLinkIdentifier
         -> replace
              ("[[" `append` outerLinkIdentifier `append` "]]")
              ("<a href='" `append` outerLinkIdentifier `append` ".html'>" `append` outerLinkIdentifier `append` "</a>"))
      noteContent outerLinkIdentifiers

createNote :: FilePath -> IO Note
createNote filePath = do
  content <- readFile filePath
  let contentWithInnerLinks = createNoteInnerLinks (getNoteInnerLinkIdentifiers content) (pack content)
  return $ Note (pack $ getNoteIdentifier filePath) (pack content) (commonmarkToHtml [optUnsafe] contentWithInnerLinks)

preprocess :: IO ()
preprocess = do
  distExists <- doesDirectoryExist "./dist"
  when distExists $ removeDirectoryRecursive "./dist"
  cssFileLocation <- getDataFileName "resources/style.css"
  createDirectory "./dist"
  createDirectory "./dist/style"
  copyFile cssFileLocation "./dist/style/style.css"

run :: IO ()
run = do
  markdownFiles <- getMarkdownFiles
  allNotes <- mapM createNote markdownFiles
  mapM_ writeNote allNotes
  config <- decodeFileEither "synapse.yaml" :: IO (Either ParseException Config)
  case config of
    Left e -> do 
      putStrLn "There was an issue while parsing the config file"
      print $ prettyPrintParseException e
    Right c -> print c
  where
    writeNote :: Note -> IO ()
    writeNote note = DTIO.writeFile ("./dist/" ++ unpack (nIdentifier note) ++ ".html") (toStrict $ renderHtml $ noteTemplate note)


main :: IO ()
main = run >> print "Successfully compiled notes"