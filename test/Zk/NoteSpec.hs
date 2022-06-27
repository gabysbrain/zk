{-# LANGUAGE OverloadedStrings #-}

module Zk.NoteSpec (spec) where

import Test.Hspec
import qualified Data.Text as T
import System.IO.Temp (emptySystemTempFile)
import Prelude

import Zk.Note
import Zk.Types

basicNoteFile :: FilePath
basicNoteFile = "test/fixtures/basic_note.md"

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
        noteTags note `shouldBe` ["tag1", "tag2", "tag3", "mollit", "delore"]
    describe "Metadata" $ do
      it "has the title in the title header field" $ do
        note <- fromPath basicNoteFile
        noteTitle note `shouldBe` "My cool note title"

