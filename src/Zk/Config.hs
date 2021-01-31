{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Zk.Config where

import Control.Monad (unless, when)
import Data.Aeson.Types
import Data.Aeson.TH
import qualified Data.ByteString as B
import Data.Yaml
import qualified Data.Text as T
import Filesystem.Path.CurrentOS (FilePath, fromText, encodeString)
import qualified Turtle
import Prelude hiding (FilePath)

import Zk.Types

$(deriveJSON defaultOptions ''Config)

instance ToJSON FilePath where
  toJSON = toJSON . encodeString
  toEncoding = toEncoding . encodeString

instance FromJSON FilePath where
  parseJSON = withText "FilePath" (return . fromText)

fromPath :: FilePath -> IO Config
fromPath path = do
  -- FIXME: replace this with decodeEither' to better handle errors
  c <- decodeFileThrow $ encodeString path
  -- make sure the notes dir is valid
  dieIfFolderNotFound $ notesDir c
  return c

fromDefaultPath :: IO Config
fromDefaultPath = fromPath "~/.config/zk/config.yaml" -- FIXME: use OS config path

toPath :: FilePath -> Config -> IO ()
toPath path = encodeFile (encodeString path)

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

