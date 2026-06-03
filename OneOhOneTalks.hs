{-# LANGUAGE DeriveGeneric, OverloadedStrings, LambdaCase, TupleSections, RecordWildCards #-}
module OneOhOneTalks where

import Data.List
import Data.Maybe
import Data.Ord
import Data.Time
import Data.Yaml
import qualified Data.ByteString as BS

import GHC.Generics

--  MSP 101 data stored in a yaml format

data Usual
  = Usual
  { usualTime :: TimeOfDay
  , usualDay :: String
  , usualRoom :: String
  , usualBuilding :: String
  } deriving (Eq, Show, Generic)

instance FromJSON Usual

-- Extra material, such as slides, source code, ...
data Material = Link { address :: String,
                       linkDescription :: String }
              | PDF  { slideName :: FilePath,
                       comment :: Maybe String}
              | Whiteboard { dirName :: FilePath }
              | File { path :: FilePath,
                       fileDescription :: String }
  deriving (Show, Read, Eq, Generic)

instance FromJSON Material

data Talk = Talk {
                   date :: UTCTime,
                   speaker :: String,
                   institute :: String,
                   speakerurl :: String,
                   insturl :: String,
                   title :: String,
                   abstract :: String,
                   location :: String,
                   material :: [Material]
                 }

          | SpecialEvent {
                           date :: UTCTime,
                           endDate :: UTCTime,
                           title :: String,
                           url :: String,
                           location :: String,
                           locationurl :: String,
                           description :: String
                         }
          | DepartmentalSeminar {
                                  date :: UTCTime,
                                  speaker :: String,
                                  institute :: String,
                                  speakerurl :: String,
                                  insturl :: String,
                                  title :: String,
                                  abstract :: String,
                                  location :: String
                                }
          | BasicTalk {
                   date :: UTCTime,
                   speaker :: String,
                   institute :: String,
                   speakerurl :: String,
                   insturl :: String,
                   title :: String,
                   abstract :: String,
                   location :: String,
                   material :: [Material]
                 }
          -- we want to keep cancelled talks in the input file in
          -- order to not shift indices; easiest way is to just change
          -- the tag
          | CancelledTalk {
                   date :: UTCTime,
                   speaker :: String,
                   institute :: String,
                   speakerurl :: String,
                   insturl :: String,
                   title :: String,
                   abstract :: String,
                   location :: String,
                   material :: [Material]
                 }
  deriving (Show, Read, Eq, Generic)

instance FromJSON Talk

data OneOhOneData
  = OneOhOneData
  { usual :: Usual
  , talks :: [Talk]
  } deriving (Show, Eq, Generic)

instance FromJSON OneOhOneData

nextTalk :: [(Int, Talk)] -> IO (Maybe (Int, Talk))
nextTalk talks = do
  now <- fmap zonedTimeToUTC getZonedTime --getCurrentTime
  pure $ listToMaybe (sortBy (comparing $ date . snd) $ filter (\(i,x) -> date x > now && isTalk x) talks)
  where
    isTalk SpecialEvent{} = False
    isTalk CancelledTalk{} = False
    isTalk _ = True

talksFromFile :: IO (Usual,[(Int, Talk)])
talksFromFile = do
  f <- BS.readFile "101.yaml"
  case decodeEither' f of
    Left err -> error (show err)
    Right (OneOhOneData usual ts) -> return (usual, reverse $ zip [(0::Int)..] $ reverse ts)
