{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Main where

import Prelude hiding (div)
import GHC.Generics
import Data.Yaml

import Data.List
import Data.Maybe
import Data.Function

import Data.Default (def)

-- import Data.ByteString.Lazy (ByteString)
-- import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Text (Text)
import qualified Data.Text as T

import Control.Applicative
import Control.Monad
import Data.Foldable

import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Status
import Network.TLS (defaultParamsClient, clientShared, clientSupported, sharedCAStore, supportedCiphers, supportedExtendedMainSecret, EMSMode(..))
import Network.TLS.Extra (ciphersuite_default)
import Network.Connection (TLSSettings(..))
import System.X509 (getSystemCertificateStore)

import Data.Time
import Data.Time.Format

import Text.Feed.Types
import Text.Feed.Import
import Text.Feed.Query

import Html

------------------------------------------------------------------------------
-- number of days to cut off old news
oldCutoff :: Integer
oldCutoff = 365 * 5
------------------------------------------------------------------------------


data NewsDate
  = OneDate Day
  | DateRange Day Day
  deriving (Show, Eq)

startDate :: NewsDate -> Day
startDate (OneDate d) = d
startDate (DateRange s e) = s

newsDateToHTML :: NewsDate -> HTML
newsDateToHTML (OneDate day) = formatTime defaultTimeLocale "%e %B %Y" day
newsDateToHTML (DateRange start end) =
  let (sy, sm, sd) = toGregorian start
      (ey, em, ed) = toGregorian end
      wholeMonths = sd == 1 && ed == gregorianMonthLength ey em
  in case (sy == ey, sm == em, sd == ed, wholeMonths) of
    (True, True, True, _) -> newsDateToHTML (OneDate start)
    (True, True, False, True)   -> fmt "%B %Y" end
    (True, True, False, False)  -> fmt "%e" start ++ dash ++ fmt "%e %B %Y" end
    (True, False, False, True)  -> fmt "%B" start ++ dash ++ fmt "%B %Y" end
    (True, False, False, False) -> fmt "%e %B" start ++ dash ++ fmt "%e %B %Y" end
    (_, _, _, True)             -> fmt "%B %Y" start ++ dash ++ fmt "%B %Y" end
    _                           -> fmt "%e %B %Y" start ++ dash ++ fmt "%e %B %Y" end
  where fmt = formatTime defaultTimeLocale
        dash = " &ndash; "

instance Ord NewsDate where
  compare = compare `on` startDate

data News = News
  { date :: NewsDate
  , desc :: String
  }
  deriving (Show, Eq)

instance FromJSON News where
  parseJSON = withObject "News" $ \ v -> do
    desc <- v .: "description"
    date <- single v <|> range v
    pure (News date desc)
    where
      single v = do
        date <- v .: "date" >>= parseTimeM True defaultTimeLocale "%e %B %Y"
        pure (OneDate date)
      range  v = do
        start <- v .: "start" >>= parseTimeM True defaultTimeLocale "%e %B %Y"
        end <- v .: "end" >>= parseTimeM True defaultTimeLocale "%e %B %Y"
        pure (DateRange start end)


newsToHTML :: News -> HTML
newsToHTML n = ddt (newsDateToHTML (date n), desc n)

generateNews :: FilePath -> IO (HTML            -- current news
                               , [(Year, HTML)] -- old news, grouped by year
                               )
generateNews file = do
  now <- getCurrentTime
  f <- BS.readFile file
  case decodeEither' f of
    Left err ->
      error (show err)
    Right news -> do
      let (old, new) = partition (\ x -> date x < OneDate (addDays (-oldCutoff) (utctDay now))) news
      let oldGrouped = groupBy (\ a b -> startYear a == startYear b) old
      pure (dlist new, map (\ x -> (startYear (head x), dlist x)) oldGrouped)
    where
      startYear x = let (y,_,_) = toGregorian (startDate (date x)) in y
      dlist [] = ""
      dlist x = tag "dl" (unlines (map newsToHTML x))

------------------------------------------------------------------------------
pubsRSS :: Request
pubsRSS = parseRequest_ "https://pureportal.strath.ac.uk/en/organisations/mathematically-structured-programming/publications/?format=rss"

getPureRSS :: Int -- number of entries
           -> IO [Item]
getPureRSS n = do
  certificateStore <- getSystemCertificateStore
  let tlsSettings = TLSSettings $
        (defaultParamsClient
                          (show $ host pubsRSS)
                          (BS8.pack $ show $ port pubsRSS))
          { clientSupported = def { supportedCiphers =
                                      ciphersuite_default
                                  , supportedExtendedMainSecret =
                                      AllowEMS
                                  }
          , clientShared = def { sharedCAStore = certificateStore }
          }
  let tlsManagerSettings = mkManagerSettings tlsSettings Nothing
  manager <- newManager tlsManagerSettings
  res <- httpLbs pubsRSS manager
  case statusIsSuccessful (responseStatus res) of
    False -> pure []
    True -> do
      case parseFeedSource (responseBody res) of
        Nothing -> pure []
        Just feed -> pure $ take n $ reverse $ sortOn (getItemPublishDate :: Item -> Maybe (Maybe UTCTime)) (feedItems feed)


itemToHTML :: Item -> Maybe HTML
itemToHTML i = do
  -- title <- T.unpack <$> getItemTitle i
  -- link <- T.unpack <$> getItemLink i
  -- title and link are included in the description
  desc <- T.unpack <$> getItemDescription i
  pure desc

printRSStitle :: Item -> IO ()
printRSStitle i = do
  case (getItemTitle i, getItemDate i) of
    (Just t, Just d) -> putStrLn (T.unpack d ++ ": " ++ T.unpack t)
    _ -> pure ()


main :: IO ()
main = do

  rss <- getPureRSS 5
  let header = "### default.html(section.news=current)\n<!-- DO NOT EDIT THIS FILE DIRECTLY — EDIT _news.yaml AND RUN GenerateNews.hs INSTEAD -->"
  let items = catMaybes $ map itemToHTML rss
  when (not $ null items) $ do
    putStrLn "\n===================\nRecent publications\n==================="
    traverse_ printRSStitle rss
    putStrLn ""
  let pubs = if (null items) then "" else
        div "recent-pubs" $
        (h2 "Recent MSP publications") ++
        (ulist items) ++
        p ("See the " ++ (anchor "https://pureportal.strath.ac.uk/en/organisations/mathematically-structured-programming" "MSP PURE page") ++ " for a full list of recent papers, grants, etc.")
  (newNews, oldNews) <- generateNews "_news.yaml"
  let news = h2 "News" ++ newNews ++ if null oldNews then "" else h3 "Older News" ++ p ("See here for " ++ anchor "old-news.html" "older news" ++ ".")
  let oldPage = h2 "Older news" ++ concatMap (\ (y, ds) -> h3 (show y) ++ ds) oldNews
  writeFile "news.html" (header ++ pubs ++ news)
  if null oldNews then pure () else writeFile "old-news.html" (header ++ oldPage)
