{-# LANGUAGE OverloadedStrings #-}

module Zk.NoteSpec (spec) where

import Test.Hspec
import Data.Maybe (isJust)
import Text.Regex (Regex, mkRegex, matchRegex)
import System.IO.Temp (emptySystemTempFile)
import Prelude

import Zk.Note
import Zk.Types

validFilenameRe :: Regex
validFilenameRe = mkRegex "^[a-z0-9_.-]+$"

isValidFilename :: String -> Bool
isValidFilename = isJust . matchRegex validFilenameRe

basicNoteFile :: FilePath
basicNoteFile = "test/fixtures/20210624_basic_note.md"

spec :: Spec
spec = do
  describe "Note" $ do
    describe "Tags" $ do
      it "retrieves tags from the header" $ do
        note <- fromPath basicNoteFile
        noteTags note `shouldContain` ["tag1", "tag2", "tag3"]
      it "retrieves tags from the text" $ do
        note <- fromPath basicNoteFile
        noteTags note `shouldContain` ["mollit", "delore"]
      it "retrieves all tags in the note" $ do
        note <- fromPath basicNoteFile
        noteTags note `shouldMatchList` ["tag1", "tag2", "tag3", "mollit", "delore"]
    describe "Metadata" $ do
      it "has the title in the title header field" $ do
        note <- fromPath basicNoteFile
        noteTitle note `shouldBe` "My cool note title"
      it "properly computes the filename" $ do
        pending
    describe "Utils" $ do
      it "filecase names are all lowercase" $ do
        filecase "Badfilename.md" `shouldSatisfy` isValidFilename
      it "filecase results should not contain illegal characters" $ do
        filecase "String with error/characters" `shouldSatisfy` isValidFilename
      it "filecase should not corrupt the extension" $ do
        filecase "note1.md" `shouldBe` "note1.md"



