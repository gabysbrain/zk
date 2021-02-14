{-# LANGUAGE OverloadedStrings #-}

module Zk.Commands.Init where

import Control.Monad.Trans (liftIO)
import qualified Data.Text as T
import Data.Default (def)
import Data.Text (Text, strip)
import Data.Text.IO (putStr, putStrLn, getLine)
import Turtle ((<.>), (</>), FilePath, fromText)
import qualified Turtle
import System.IO (hFlush, stdout)
import Prelude hiding (FilePath, putStr, putStrLn, getLine)

import Zk.Types
import Zk.Config (toDefaultPath)

runInit :: IO ()
runInit = initConfig

initConfig :: IO ()
initConfig = do
  let config = def
  -- location of notes dir
  nd <- askVal "Notes directory" 
               (Right . fromText) 
               (Just $ notesDir config)
  toDefaultPath $ config { notesDir=nd }

askVal :: Show a => Text -> (Text -> Either Text a) -> Maybe a -> IO a
askVal q parser (Just defVal) = do
  putStr $ q <> " [" <> T.pack (show defVal) <> "] "
  hFlush stdout
  v <- getLine
  if isBlank v
     then return defVal
     else case parser v of
            Left err -> do
              putStrLn err
              askVal q parser (Just defVal) -- keep asking
            Right v' -> return v'

isBlank :: Text -> Bool
isBlank txt = strip txt == ""

  {-
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
-}

