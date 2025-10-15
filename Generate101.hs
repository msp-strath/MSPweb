module Main where

import Data.Yaml
import qualified Data.ByteString as BS

import OneOhOne

-- IMPORTANT: Always add new talks at the top of the list!
-- The use of reverse then makes sure that the old entries
-- get to keep their old id numbers that are zipped in

talksFromFile :: IO (Usual,[(Int, Talk)])
talksFromFile = do
  f <- BS.readFile "101.yaml"
  case decodeEither' f of
    Left err -> error (show err)
    Right (OneOhOneData usual ts) -> return (usual, reverse $ zip [(0::Int)..] $ reverse ts)

main :: IO ()
main = do
  (usual,ts) <- talksFromFile
  let ts' = filter (not . cancelled . snd) ts
  generateRSS  ts' "msp101.rss"
  generateICS  ts' "msp101.ics"
  generateHTML usual ts' "msp101.html"
  generateSnippet ts' "_snippets/next101.html"
 where cancelled CancelledTalk{} = True
       cancelled _               = False
