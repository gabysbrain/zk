{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Zk.Commands.NewNote where

import System.IO.Temp (withSystemTempFile, writeSystemTempFile)
import Control.Monad (unless, when)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import qualified Data.Text as T
import Turtle ((<.>), (</>), FilePath)
import qualified Turtle
import Text.RawString.QQ
import Prelude hiding (FilePath)

noteTmpl :: String
noteTmpl = [r|---
title: 
tags: 
...

|]

newNote :: FilePath -> IO ()
newNote notesDir = do
  absFolderPath <- Turtle.realpath notesDir

  -- get the current date/time as a string
  time <- formatTime defaultTimeLocale "%Y%m%d%H%M%S" <$> getCurrentTime 

  -- create a temporary file and load it with the note template
  tmpFile <- Turtle.decodeString <$> writeSystemTempFile "zk.md" noteTmpl

  -- need some mod times to see if file changed
  createTime <- Turtle.datefile tmpFile

  -- change to the notes directory
  --Turtle.cd absFolderPath

  -- add markdown extension
  let noteFile = absFolderPath </> Turtle.fromString time <.> "md"
      noteFile' = show $ Turtle.format Turtle.fp noteFile

  -- create file (touch) (not needed)
  -- open vim with file
  let tmpFile' = show $ Turtle.format Turtle.fp tmpFile
  Turtle.shell (T.pack $ "vim " ++ tmpFile') Turtle.empty

  -- see if the note was edited, otherwise just delete the file
  modTime <- Turtle.datefile tmpFile
  if modTime > createTime
     then Turtle.mv tmpFile noteFile
     else Turtle.rm tmpFile

