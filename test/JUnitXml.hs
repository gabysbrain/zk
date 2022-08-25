module JUnitXml where

import Test.Hspec
import Test.Hspec.Core.Format
import Test.Hspec.Runner (Path)
import Control.Monad (unless)
import Data.List (intercalate, unwords)
import Data.Maybe (fromMaybe)
import Data.Functor ((<&>))
import System.IO (Handle, hPutStrLn, hClose, hFlush)

xmlformat :: Handle -> Format
xmlformat f Started = do 
  hPutStrLn f "<?xml version='1.0' encoding='UTF-8'?>"
  hPutStrLn f $ openTag "testsuites" [("name", "tests")]
xmlformat f (Done _) = do
  hPutStrLn f $ closeTag "testsuites"
  hFlush f
xmlformat f (GroupStarted (groupDescs, req)) = do
  hPutStrLn f $ openTag "testsuite" [("name", req)]
xmlformat f (GroupDone _) = do
  hPutStrLn f $ closeTag "testsuite"
xmlformat f (ItemStarted path) = do
  return () -- wait for item done, it has more info
xmlformat f (ItemDone path item) = do
  hPutStrLn f $ openTestCase path
  logItem f item
  hPutStrLn f closeTestCase

logItem :: Handle -> Item -> IO ()
logItem f (Item {itemResult=Success}) = return ()
logItem f (Item {itemResult=(Pending _ reason)}) =
  hPutStrLn f $ singleTag "skipped" [("message", fromMaybe "No reason given" reason)]
logItem f (Item {itemResult=(Failure _ reason)}) =
  hPutStrLn f $ singleTag "failure" [("message", show reason)]

openTestCase :: Path -> String
openTestCase path@(xs,x) = openTag "testcase" [("name", x), ("id", intercalate "." xs)]

closeTestCase :: String
closeTestCase = closeTag "testcase"

openTag :: String -> [(String,String)] -> String
openTag tag [] = "<" ++ tag ++ ">"
openTag tag attrs = "<" ++ tag ++ " " ++ unwords attrStrs ++ ">"
  where
  attrStrs = attrs <&> \(name, val) -> name ++ "='" ++ val ++ "'"

closeTag :: String -> String
closeTag tag = "</" ++ tag ++ ">"

singleTag :: String -> [(String,String)] -> String
singleTag tag attrs = openTag tag attrs ++ closeTag tag

