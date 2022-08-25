module Main where

import Test.Hspec
import Test.Hspec.Runner
import Test.Hspec.Formatters
import qualified Spec
import JUnitXml (xmlformat)

import Control.Monad (unless)
import System.Exit (exitFailure)
import System.Environment (getArgs)
import System.IO (IOMode(WriteMode), openFile)

consoleConfig = defaultConfig
  { configColorMode = ColorAlways
  , configFormatter = Just checks
  --, configFormatter = Just JUnit.xmlformat
  }

ciConfig f = defaultConfig
  { configFormat = Just $ \_ -> return $ xmlformat f
  , configFormatter = Nothing
  }

main :: IO ()
main = do
  confSpec <- configSpec
  --config <- getArgs >>= readConfig consoleConfig
  config <- getArgs >>= readConfig confSpec
  (config', specForest) <- evalSpec config Spec.spec
  result <- runSpecForest specForest config'
  evaluateResult result

-- TODO: switch congfig depending on whether or not we're in a CI environment
configSpec :: IO Config
configSpec = do
  file <- openFile "testresults.xml" WriteMode
  return $ ciConfig file

