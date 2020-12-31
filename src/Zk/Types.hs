module Zk.Types where

import Filesystem.Path.CurrentOS (FilePath)
import Prelude hiding (FilePath)

data Cmd
  = CmdInit {
      notesDir :: FilePath
    }
  | CmdNew {
      notesDir :: FilePath
    }
  deriving (Show)

