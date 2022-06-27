{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Zk.CmdRunner where

import Control.Monad (unless, when)
import Control.Monad.Reader (runReaderT)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import qualified Data.Text as T
import qualified Zk.Config as Config
import Prelude hiding (FilePath)

import Zk.Commands.NewNote (runNewNote)
import Zk.Commands.Init (runInit)

import Zk.Types

-- not Zk because we may not have the config (in case of init)
runCmd :: Cmd -> IO ()
runCmd CmdNew{..}  = runApp runNewNote
runCmd CmdInit{..} = runInit

runApp :: Zk a -> IO a
runApp app = do
  config <- Config.fromDefaultPath
  runReaderT (runMyZk app) config

