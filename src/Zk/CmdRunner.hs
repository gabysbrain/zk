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

import Zk.Types

runCmd :: Cmd -> IO ()

runCmd CmdNew{..} = do
  dieIfFolderNotFound notesDir
  absFolderPath <- Turtle.realpath notesDir
  -- change to the notes directory
  --Turtle.cd absFolderPath
  -- get the current date/time as a string
  time <- formatTime defaultTimeLocale "%Y%m%d%H%M%S" <$> getCurrentTime 

  -- add markdown extension
  let note = absFolderPath </> Turtle.fromString time <.> "md"
      noteTxt = show $ Turtle.format Turtle.fp note

  -- create file (touch) (not needed)
  -- open vim with file
  Turtle.shell (T.pack $ "vim " ++ noteTxt) Turtle.empty
  return ()

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

