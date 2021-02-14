{-# LANGUAGE OverloadedStrings #-}

module Zk.CmdParser where

import Filesystem.Path.CurrentOS as Path
import Options.Applicative ( ReadM, Parser, ParserInfo
                           , argument, command, customExecParser, metavar
                           , help, helper, info, fullDesc, progDesc, prefs
                           , readerError, showHelpOnError, str, subparser, value
                           , switch, long, short )
import qualified Data.Text as T
import Prelude hiding (FilePath)

import Zk.Types 

myExecParser :: ParserInfo a -> IO a
myExecParser = customExecParser (prefs showHelpOnError)

zkDesc :: String
zkDesc = zkHeader -- TODO: more descriptive some day

zkHeader :: String
zkHeader = "Zettelkasten-style note management"

parseCommand :: Parser Cmd
parseCommand = subparser $
  -- initialize repository
  -- FIXME: abstract each block to a function
     (command "init" $ info (helper <*> parseInitCommand)
                            (fullDesc <> progDesc "initialize a note repository"))
  <> (command "new"  $ info (helper <*> parseNewCommand)
                            (fullDesc <> progDesc "create a new note"))

parseInitCommand :: Parser Cmd
parseInitCommand = CmdInit <$> showHelpParser

parseNewCommand :: Parser Cmd
parseNewCommand = CmdNew <$> notesPathParser

showHelpParser :: Parser Bool
showHelpParser = switch (long "help" <> short 'h' <> help "Show help")

-- FIXME: remove default path maybe?
notesPathParser :: Parser FilePath
notesPathParser = argument
  (str >>= readFolderPath)
  (value "./" <> metavar "NOTESPATH" <> help "folder path for notes")

readFolderPath :: String -> ReadM FilePath
readFolderPath s = do
  let path = Path.fromText (T.pack s)
  if Path.valid path
     then return path
     else readerError ("invalid path: " ++ show path)





