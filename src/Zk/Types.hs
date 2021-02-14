{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Zk.Types where

import Filesystem.Path.CurrentOS (FilePath, fromText)
import Data.Default (Default, def)
import Data.Functor (Functor)
import Control.Applicative (Applicative)
import Control.Monad.Reader (ReaderT, MonadReader)
import Control.Monad.IO.Class (MonadIO)
import Prelude hiding (FilePath)
import GHC.Generics 

newtype Zk a = MyZk {
    runMyZk :: ReaderT Config IO a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadReader Config)

data Cmd
  = CmdInit { showHelp :: Bool }
  | CmdNew {
      cmdNotesDir :: FilePath
    }
  deriving (Show)

newtype Config
  = Config {
      notesDir :: FilePath
    }
  deriving (Show, Generic)

instance Default Config where
  def = Config $ fromText "~/Sync/Notes/"

