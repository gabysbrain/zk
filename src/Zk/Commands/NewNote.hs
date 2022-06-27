{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Zk.Commands.NewNote where

import System.IO.Temp (withSystemTempFile, writeSystemTempFile)
import Control.Monad (unless, when)
import Control.Monad.Reader (ask)
import Control.Monad.Trans (liftIO)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import qualified Data.Text as T
import Text.RawString.QQ
import Prelude
import qualified System.Directory as Dir
import System.FilePath ((</>), (<.>))
import qualified System.Process as Proc

import Zk.Types

noteTmpl :: String
noteTmpl = [r|---
title: 
tags: 
...

|]

runNewNote :: Zk ()
runNewNote = do
  config <- ask
  liftIO $ newNote (notesDir config)

newNote :: FilePath -> IO ()
newNote notesDir = do
  absFolderPath <- Dir.canonicalizePath notesDir

  -- get the current date/time as a string
  time <- formatTime defaultTimeLocale "%Y%m%d%H%M%S" <$> getCurrentTime 

  -- create a temporary file and load it with the note template
  tmpFile <- writeSystemTempFile "zk.md" noteTmpl

  -- need some mod times to see if file changed
  createTime <- Dir.getModificationTime tmpFile

  -- change to the notes directory
  --Turtle.cd absFolderPath

  -- add markdown extension
  let noteFile = absFolderPath </> time <.> "md"

  -- create file (touch) (not needed)
  -- open vim with file
  -- FIXME: use editor variable or maybe config
  Proc.runCommand $ "vim " ++ tmpFile

  -- see if the note was edited, otherwise just delete the file
  modTime <- Dir.getModificationTime tmpFile
  if modTime > createTime
     then Dir.renameFile tmpFile noteFile
     else Dir.removeFile tmpFile

