{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Zk.Config where

import Control.Monad (unless, when, void)
import Data.Aeson.Types
import Data.Aeson.TH
import qualified Data.ByteString as B
import Data.Yaml
import qualified Data.Text as T
import System.FilePath (takeDirectory)
import System.Directory (XdgDirectory(XdgConfig)
                        , createDirectoryIfMissing
                        , getXdgDirectory
                        , doesFileExist
                        , doesDirectoryExist)
import Prelude 

import Zk.Types

$(deriveJSON defaultOptions ''Config)

--instance ToJSON FilePath where
  --toJSON = toJSON . encodeString
  --toEncoding = toEncoding . encodeString

--instance FromJSON FilePath where
  --parseJSON = withText "FilePath" (return . fromText)

defaultPath :: IO FilePath
defaultPath = getXdgDirectory XdgConfig "zk/config.yaml"

fromPath :: FilePath -> IO Config
fromPath path = do
  -- FIXME: replace this with decodeEither' to better handle errors
  c <- decodeFileThrow path
  -- make sure the notes dir is valid
  dieIfFolderNotFound $ notesDir c
  return c

fromDefaultPath :: IO Config
fromDefaultPath = defaultPath >>= fromPath 

toPath :: FilePath -> Config -> IO ()
toPath path config = do
  -- need to create the folder path
  createDirectoryIfMissing True $ takeDirectory path
  encodeFile path config

toDefaultPath :: Config -> IO ()
toDefaultPath config = do
  p <- defaultPath 
  toPath p config 

-- Check if a path is there and actually a folder
dieIfFolderNotFound :: FilePath -> IO ()
dieIfFolderNotFound path = do
  folderExists <- doesDirectoryExist path
  fileExists <- doesFileExist path
  when fileExists (needFolderNotFileError path)
  unless folderExists (folderNotFoundError path)

folderNotFoundError :: FilePath -> IO ()
folderNotFoundError path = do
    --setErrorColor  
    let errorstr = "unable to find folder: " ++ (show path)
    void $ ioError $ userError errorstr

-- error out that folder is required, but path points
-- to a file
needFolderNotFileError :: FilePath -> IO ()
needFolderNotFileError path = do
    --setErrorColor
    let errorstr = "expected folder, not file: " ++ (show path)
    void $ ioError $ userError errorstr

