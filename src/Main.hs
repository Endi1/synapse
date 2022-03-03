{-# LANGUAGE OverloadedStrings  #-}
module Main where
import Data.Text (Text, pack, replace, append)
import System.Directory
import Text.Regex.TDFA
import CMark

data Note  = Note {
  nIdentifier :: Text,
  nRawContent :: Text,
  nCompiledContent :: Text
} deriving Show

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

run :: IO [Note]
run = do
  markdownFiles <- getMarkdownFiles
  mapM createNote markdownFiles

main :: IO ()
main = do
  notes <- run
  print notes