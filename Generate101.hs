module Main where

import Data.Aeson
import qualified Data.ByteString.Lazy as BS

import OneOhOne

-- IMPORTANT: Always add new talks at the top of the list!
-- The use of reverse then makes sure that the old entries
-- get to keep their old id numbers that are zipped in

talks :: IO [(Int, Talk)]
talks = do
  f <- BS.readFile "_101.json"
  case eitherDecode' f of
    Left err -> error err
    Right ts -> return $ reverse $ zip [(0::Int)..] $ reverse ts

{-
-- aeson-pretty in .cabal file
-- {-# LANGUAGE OverloadedStrings #-}
-- import Data.Aeson.Encode.Pretty
prettyEncode :: [Talk] -> FilePath -> IO ()
prettyEncode ts file = do
  let ord = ["tag", "date", "speaker", "institute", "speakerurl", "insturl", "title", "url", "abstract", "location", "locationurl", "description", "material"]
  let cfg = defConfig {confIndent = Spaces 2, confCompare = keyOrder ord}
  BS.writeFile file $ encodePretty' cfg ts
-}

main = do
  ts <- talks
  generateRSS  ts "msp101.rss"
  generateICS  ts "msp101.ics"
  generateHTML ts "msp101.html"
