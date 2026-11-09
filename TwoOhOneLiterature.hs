{-# LANGUAGE DeriveGeneric, OverloadedStrings, LambdaCase, TupleSections, RecordWildCards #-}
module TwoOhOneLiterature where

import Control.Applicative
import Data.List
import Data.Maybe
import Data.Ord
import Data.Time
import Data.Yaml
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

import GHC.Generics

import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Status
import Network.HTTP.Types.Header (hContentType)

data Usual
  = Usual
  { usualTime :: TimeOfDay
  , usualDay :: String
  , usualRoom :: String
  , usualBuilding :: String
  } deriving (Eq, Show, Generic)
instance FromJSON Usual

data Tag = Read | ToRead | Reading
  deriving (Show, Read, Eq, Generic)
instance FromJSON Tag

data Material = File { path :: FilePath,
                       description :: Maybe String }
              | Link { url :: String,
                       description :: Maybe String }
  deriving (Show, Read, Eq, Generic)
instance FromJSON Material

data Literature = Literature
  { 
    tag :: Tag,
    title :: String,
    author :: Maybe String,
    authors :: Maybe [String],
    material :: Maybe [Material],
    date :: Maybe UTCTime,
    dates :: Maybe [UTCTime],
    semester :: Maybe String,
    suggestedBy :: Maybe String
  }
  deriving (Show, Read, Eq, Generic)
instance FromJSON Literature

data TwoOhOneData
  = TwoOhOneData
  { usual :: Usual
  , literature :: [Literature]
  } deriving (Show, Eq, Generic)
instance FromJSON TwoOhOneData

litsFromFile :: IO (Usual,[(Int,Literature)])
litsFromFile = do
  f <- BS.readFile "201.yaml"
  case decodeEither' f of
    Left err -> error (show err)
    Right (TwoOhOneData usual lits) -> return (usual, reverse $ zip [(0::Int)..] $ reverse lits)

