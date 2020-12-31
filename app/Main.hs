{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE RecordWildCards #-}

module Main where

import Options.Applicative ( header, helper, info, fullDesc, progDesc )
import Zk.CmdParser (myExecParser, parseCommand, zkDesc, zkHeader)
import Zk.CmdRunner (runCmd)

main :: IO ()
main = do
  command <- myExecParser $ info (helper <*> parseCommand)
               (fullDesc <> progDesc zkDesc <> header zkHeader)

  runCmd command

