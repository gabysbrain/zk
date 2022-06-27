{-# LANGUAGE OverloadedStrings #-}

module Zk.Note where

import Control.Monad (unless, when, void)
import Data.Aeson.Types
import Data.Aeson.TH
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Yaml
import Prelude 

import Debug.Trace

import Zk.Types

-- This header may get promoted to Zk.Types one day but right now it's just
-- for internal yaml parsing reasons
data NoteHeader = NoteHeader
  { hdrTitle :: String
  , hdrTags :: [String]
  } deriving (Show)

instance FromJSON NoteHeader where
  parseJSON (Object v) = do
    title <- v .: "title"
    tagStr <- v .: "tags"
    return $ NoteHeader title (words tagStr)

fromPath :: FilePath -> IO Note
fromPath path = do
  rawData <- B.readFile path

  -- try to get the note and see if we have some error
  -- TODO: there should be a MonadT Note type which encapsulates errors and IO
  case Zk.Note.parse rawData of
    Left err -> ioError $ userError err
    Right note -> return note

-- verify a note matches the proper format
parse :: ByteString -> Either String Note
parse rawText = do
  let rawText' = B.strip rawText -- we're just reading, not editing so stripping any whitespace won't change anything
  let fileLines = B.lines rawText'

  -- make sure we start with a header
  unless (head fileLines == "---") (Left "note has no header")

  let (yamlData, content') = break (== "...") $ tail fileLines -- end of header
  traceM $ B.unpack $ B.unlines yamlData

  -- check that the header was properly closed (probably ok if there's no content)
  unless (head content' == "...") (Left "note header not properly closed")

  -- parse header yaml
  traceM $ B.unpack $ B.unlines yamlData
  header <- either (Left . show) Right $ decodeEither' (B.unlines yamlData)

  -- TODO: get tags from content

  return $ Note { noteTitle = hdrTitle header
                , noteTags = hdrTags header
                , noteCreationDate = "" -- We can fill these later
                , noteModificationDate = ""
                , noteContent = B.unpack $ B.unlines $ tail content' -- first element of content is the end of header marker
                }


--fromFileName :: String -> Config -> IO Note


