module Main where

import Data.Time (UTCTime(..), showGregorian)
import System.FilePath 


-- Generate web pages, calendars and a RSS feed for MSP 101 from data in 
-- a text file

-- Extra material, such as slides, source code, ...
type Material = (FilePath, String) -- ^ path and description

data Talk = Talk {date :: UTCTime,
                  year :: Int,      -- ^ academic year
                  speaker :: String,
                  institute :: String,
                  speakerurl :: String,
                  insturl :: String,
                  title :: String,
                  abstract :: String,
                  location :: String,
                  material :: [Material]
                 }
  deriving (Show, Read, Eq)

generateRSS :: [Talk]
            -> FilePath -- ^ Output path
            -> IO ()
generateRSS ts out = do
  let content = concatMap processEntry ts
      header = unlines ["<?xml version='1.0' encoding='ISO-8859-1'?>",
                        "<rss version='2.0' xmlns:atom='http://www.w3.org/2005/Atom'>",
                        "<channel>",
                        "<title>MSP101</title>",
                        "<link>http://msp.cis.strath.ac.uk/msp101.html</link>",
                        "<description>MSP101 is an ongoing series of informal talks given on Wednesday mornings by visiting academics or members of the MSP group.</description>",
                        "<language>en-gb</language>"]
      footer = unlines ["</channel>", "</rss>"]
  writeFile out (header ++ content ++ footer)
    where processEntry (Talk date year speaker inst speakerurl insturl title abstract location material) 
            = let rsstitle = (showGregorian $ utctDay date) ++ ": " ++ speaker ++ (if (null inst) then "" else (" (" ++ inst ++ ")"))
                  abstract = ""
                  desc = ""
              in
               unlines ["<item>", 
                        "<title" ++ rsstitle ++ "</title>",
                        "<description><![CDATA[" ++ desc ++ "]]></description>",
                        "<guid>" ++ (show date) ++ "</guid>",
                        "</item>"]

main = do
  talkStr <- readFile "101.txt" --TODO
  let talks = read talkStr
  generateRSS talks "msp101.rss"
