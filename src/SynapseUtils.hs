{-# LANGUAGE OverloadedStrings #-}

module SynapseUtils (
  getNoteIdentifier,
  getMarkdownFiles,
  getNoteInnerLinkIdentifiers,
  createNoteInnerLinks,
  createNote,
  compileToMarkdown,
  preprocess,
  writeNote,
  createSynapse
) where
import Text.Regex.TDFA
import Control.Monad (when)
import System.Directory
import Data.Text (Text, pack, replace, append, unpack)
import Text.Blaze.Html.Renderer.Text
import Templates (noteTemplate)
import CMarkGFM
import Types
import Paths_synapse (getDataFileName)
import Data.Text.Internal.Builder (writeN)
import qualified Data.Text.IO as DTIO
import Data.Text.Lazy (toStrict)
import Control.Lens
import RIO.List

writeNote :: Note -> IO ()
writeNote note = DTIO.writeFile ("./dist/" ++ unpack (_nIdentifier note) ++ ".html") (toStrict $ renderHtml $ noteTemplate note)

preprocess :: IO ()
preprocess = do
  distExists <- doesDirectoryExist "./dist"
  when distExists $ removeDirectoryRecursive "./dist"
  cssFileLocation <- getDataFileName "resources/pico.min.css"
  createDirectory "./dist"
  createDirectory "./dist/style"
  copyFile cssFileLocation "./dist/style/pico.min.css"

createSynapse :: Config -> [File] -> Either Text Synapse
createSynapse config noteFiles =
  let
    restNotesFiles = filter (\(fp, _) -> fp /= cIndex config `append` ".md") noteFiles
    mIndexNote = find (\noteFile -> fst noteFile == cIndex config) noteFiles
  in
    case mIndexNote of 
      Nothing -> Left $ "File with name " `append` cIndex config `append` " not found"
      Just indexNote -> Right $ Synapse {
        sConfig = config, 
        sIndexNote = uncurry createNote indexNote & nDistFileName .~ "index",
        sRestOfNotes = map (uncurry createNote) restNotesFiles
      }

createNote :: Text -> Text -> Note
createNote filePath noteContent =
  let
    contentWithInnerLinks = createNoteInnerLinks (getNoteInnerLinkIdentifiers noteContent) noteContent
  in
    Note {
      _nIdentifier = getNoteIdentifier filePath,
      _nDistFileName = getNoteIdentifier filePath,
      _nRawContent = noteContent,
      _nCompiledContent = compileToMarkdown contentWithInnerLinks
    }

getNoteIdentifier :: Text -> Text
getNoteIdentifier fp =
  let (identifier, _, _, _) = (fp :: Text) =~ ("\\.md$" :: Text) :: (Text, Text, Text, [Text])
  in
    identifier

compileToMarkdown :: Text -> Text
compileToMarkdown = commonmarkToHtml [optUnsafe] [extTable]

getMarkdownFiles :: [FilePath] -> [FilePath]
getMarkdownFiles = filter regexMatch
  where
    regexMatch :: String -> Bool
    regexMatch f = (f :: String) =~ ("\\.md$" :: String)

getNoteInnerLinkIdentifiers :: Text -> [Text]
getNoteInnerLinkIdentifiers noteContent =
  let submatches = (noteContent :: Text) =~ ("\\[\\[([a-zA-Z]+(([[:space:]]|_|:|-|\\.|,|')*[a-zA-Z]+)*)\\]\\]" :: Text) :: [[Text]]
  in
    map (!! 1) submatches

-- | `createNoteInnerLinks` takes a list of note identifiers and creates inner links for them, inserting them in the noteContent and returning it
createNoteInnerLinks :: [Text] -> Text -> Text
createNoteInnerLinks outerLinkIdentifiers noteContent
  = foldr
      (\ outerLinkIdentifier
         -> replace
              ("[[" `append` outerLinkIdentifier `append` "]]")
              ("<a href=\"" `append` outerLinkIdentifier `append` ".html\">" `append` outerLinkIdentifier `append` "</a>"))
      noteContent outerLinkIdentifiers