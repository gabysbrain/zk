{-# LANGUAGE OverloadedStrings #-}

module Zk.ConfigSpec (spec) where

import Test.Hspec
import qualified Data.Text as T
import Filesystem.Path.CurrentOS (FilePath, fromText, encodeString)
import System.IO.Temp (emptySystemTempFile)
import Prelude hiding (FilePath)

import Zk.Config
import Zk.Types

configFile :: FilePath
configFile = fromText "test/fixtures/good.yaml"

badNotesDirFile :: FilePath
badNotesDirFile = fromText "test/fixtures/badnotedir.yaml"

missingConfigFile :: FilePath
missingConfigFile = fromText "test/fixtures/foo.yaml"

spec :: Spec
spec = do
  describe "Config" $ do
    it "reads a properly formatted config file" $ do
      f <- fromPath configFile
      return ()
    it "saves config properly" $ do
      tmp <- emptySystemTempFile "zk"
      let tmp' = fromText $ T.pack tmp
      let c = Config { notesDir = "foo" }
      toPath tmp' c
      -- FIXME: diff this file with a fixture
      return ()
    it "throws an error when notes dir is invalid" $ do
      -- FIXME: better exception checking
      fromPath badNotesDirFile `shouldThrow` anyException
    it "throws an error when file is missing" $ do
      -- FIXME: better exception checking
      fromPath missingConfigFile `shouldThrow` anyException

