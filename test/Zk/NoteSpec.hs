{-# LANGUAGE OverloadedStrings #-}

module Zk.NoteSpec (spec) where

import Test.Hspec
import Data.Maybe (isJust)
import Text.Regex (Regex, mkRegex, matchRegex)
import System.IO.Temp (emptySystemTempFile)
import System.FilePath (takeFileName)
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
        noteTags note `shouldContain` ["dolore", "mollit"]
      it "retrieves all tags in the note" $ do
        note <- fromPath basicNoteFile
        noteTags note `shouldMatchList` ["tag1", "tag2", "tag3", "mollit", "dolore"]
      it "avoids duplicate tags" $ do 
        pending
      it "parses all hashtags from a text block" $ do
        tagsFromText "here are #some #tags for #you" `shouldMatchList` ["some", "tags", "you"]
    describe "Tag cleaning" $ do
      it "strips the leading hash if needed" $ do
        cleanTag "#hello" `shouldBe` "hello"
      it "uppercase tags are lowercased" $ do 
        cleanTag "UpPeR" `shouldBe` "upper"
    describe "Metadata" $ do
      it "has the title in the title header field" $ do
        note <- fromPath basicNoteFile
        noteTitle note `shouldBe` "Basic note"
      it "properly computes the filename" $ do
        note <- fromPath basicNoteFile
        filename note `shouldBe` takeFileName basicNoteFile
    describe "Utils" $ do
      it "filecase names are all lowercase" $ do
        filecase "Badfilename.md" `shouldSatisfy` isValidFilename
      it "filecase results should not contain illegal characters" $ do
        filecase "String with error/characters" `shouldSatisfy` isValidFilename
      it "filecase should not corrupt the extension" $ do
        filecase "note1.md" `shouldBe` "note1.md"



