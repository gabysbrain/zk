{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Zk.CmdRunner where

import Control.Monad (unless, when)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import qualified Data.Text as T
import Turtle ((<.>), (</>), FilePath)
import qualified Turtle
import Prelude hiding (FilePath)

import Zk.Commands.NewNote (newNote)

import Zk.Types

runCmd :: Cmd -> IO ()

runCmd CmdNew{..} = do
  dieIfFolderNotFound notesDir -- FIXME: maybe put in newNote
  newNote notesDir

runCmd _ = putStrLn "hello"

-- Check if a path is there and actually a folder
dieIfFolderNotFound :: FilePath -> IO ()
dieIfFolderNotFound path = do
  folderExists <- Turtle.testdir path
  fileExists <- Turtle.testfile path
  when fileExists (needFolderNotFileError path)
  unless folderExists (folderNotFoundError path)

folderNotFoundError :: FilePath -> IO ()
folderNotFoundError path = do
    --setErrorColor  
    let errorstr = T.pack ("unable to find folder: " ++ (show path)) 
    Turtle.die errorstr

-- error out that folder is required, but path points
-- to a file
needFolderNotFileError :: FilePath -> IO ()
needFolderNotFileError path = do
    --setErrorColor
    let errorstr = T.pack ("expected folder, not file: " ++ (show path)) 
    Turtle.die errorstr

