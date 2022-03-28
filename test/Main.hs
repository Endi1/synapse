{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Data.Monoid
import Control.Monad
import Test.HUnit

import SynapseUtils ( getNoteIdentifier, getMarkdownFiles, getNoteInnerLinkIdentifiers, createNoteInnerLinks, createNote, compileToMarkdown )
import Types

testGetNoteIdentifier :: Assertion
testGetNoteIdentifier =
       let
              cases = [("noteid.md", "noteid"), ("note_id.md", "note_id"), ("89213.md", "89213")]
       in
              mapM_ (\ (param, result) -> assertEqual "" (getNoteIdentifier param) result ) cases

testGetMarkdownFiles :: Assertion
testGetMarkdownFiles = assertEqual ""
       (getMarkdownFiles ["first.md", "second.md", "third post.md", "fourth_post.md", "this should not be included"])
       ["first.md", "second.md", "third post.md", "fourth_post.md"]

testGetNoteInnerLinkIdentifiers :: Assertion
testGetNoteInnerLinkIdentifiers = assertEqual "" 
       (getNoteInnerLinkIdentifiers "This is an [[innerlink]]\nthis is [[another inner link]]\nand this is a [[third_inner_link]]")
       ["innerlink", "another inner link", "third_inner_link"]

testCreateNoteInnerLinks :: Assertion
testCreateNoteInnerLinks = assertEqual "" 
       (createNoteInnerLinks ["first", "second inner", "third_link", "fourth-link"] "This is [[first]] and\na [[second inner]] and a\n [[third_link]] and a [[fourth-link]]\n while [this is not](foobar)")
       "This is <a href=\"first.html\">first</a> and\na <a href=\"second inner.html\">second inner</a> and a\n <a href=\"third_link.html\">third link</a> and a [[fourth-link]]\n while [this is not](foobar)"

testCreateNote :: Assertion
testCreateNote = assertEqual ""
       (createNote "note.md" "This is the note body") 
       (Note {_nIdentifier="note", _nDistFileName = "note", _nRawContent="This is the note body", _nCompiledContent=compileToMarkdown "This is the note body"})

main :: IO ()
main = defaultMainWithOpts
       [
              testCase "getNoteIdentifier" testGetNoteIdentifier, 
              testCase "getMarkdownFiles" testGetMarkdownFiles, 
              testCase "getNoteInnerLinkIdentifiers" testGetNoteInnerLinkIdentifiers
       ]
       mempty