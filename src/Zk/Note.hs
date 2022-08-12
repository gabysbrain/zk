{-# LANGUAGE OverloadedStrings #-}

module Zk.Note where

import Control.Monad (unless, when, void)
import Data.Aeson.Types
import Data.Aeson.TH
import Data.ByteString (ByteString)
import Data.Char (toLower)
import Data.List.Split (splitOn)
import qualified Data.ByteString.Char8 as B
import System.FilePath (takeFileName)
import Data.Yaml
import Text.Regex (Regex, mkRegex, subRegex, matchRegexAll)
import Prelude 

import Debug.Trace

import Zk.Types

invalidFilenameRe :: Regex
invalidFilenameRe = mkRegex "[^A-Za-z0-9._-]"

tagRe :: Regex
tagRe = mkRegex "#[^[:space:]]+"

-- This header may get promoted to Zk.Types one day but right now it's just
-- for internal yaml parsing reasons
data NoteHeader = NoteHeader
  { hdrTitle :: String
  , hdrTags :: [String]
  } deriving (Show)

instance FromJSON NoteHeader where
  parseJSON (Object v) = do
    title <- v .: "title"
    tags <- v .: "tags"
    return $ NoteHeader title tags

note :: String -> [String] -> String -> Note
note ttl tags content =
  Note { noteTitle = ttl
       , noteTags = allTags
       , noteCreationDate = "" -- We can fill these later
       , noteContent = content
       }
  where
  allTags = tags ++ tagsFromText content

fromPath :: FilePath -> IO Note
fromPath path = do
  rawData <- B.readFile path

  let fn = takeFileName path
  let createDate = head $ splitOn "_" fn

  -- try to get the note and see if we have some error
  -- TODO: there should be a MonadT Note type which encapsulates errors and IO
  case Zk.Note.parse rawData of
    Left err -> ioError $ userError err
    Right note -> return $ note {noteCreationDate=createDate}

-- verify a note matches the proper format
parse :: ByteString -> Either String Note
parse rawText = do
  let rawText' = B.strip rawText -- we're just reading, not editing so 
                                 -- stripping any whitespace won't 
                                 -- change anything
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
  -- first element of content is the end of header marker
  let content = B.unpack $ B.unlines $ tail content' 

  return $ note (hdrTitle header) (hdrTags header) content

filename :: Note -> String
filename Note {noteTitle = ttl, noteCreationDate = dt} = dt ++ "_" ++ filecaseTitle ++ ".md"
  where filecaseTitle = filecase ttl

-- FIXME: This might need to be part of a Utils module
filecase :: String -> String
filecase fn = toLower <$> subRegex invalidFilenameRe fn "_"

tagsFromText :: String -> [String]
tagsFromText str = cleanTag <$> findAllMatches tagRe str

cleanTag :: String -> String
cleanTag s@(init:rest) = toLower <$> if init == '#' then rest else s

findAllMatches :: Regex -> String -> [String]
findAllMatches re "" = []
findAllMatches re str = 
  case matchRegexAll re str of
    Nothing -> []
    Just (_, match, rest, _) -> match : findAllMatches re rest


