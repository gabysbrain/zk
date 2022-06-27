{-# LANGUAGE OverloadedStrings #-}

module Zk.Commands.Init where

import Control.Monad.Trans (liftIO)
import qualified Data.Text as T
import Data.Default (def)
import Data.Text (Text, strip)
import Data.Text.IO (putStr, putStrLn, getLine)
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
               (Right . T.unpack) 
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

