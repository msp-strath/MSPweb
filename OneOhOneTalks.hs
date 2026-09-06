{-# LANGUAGE DeriveGeneric, OverloadedStrings, LambdaCase, TupleSections, RecordWildCards #-}
module OneOhOneTalks where

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

import People

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
                   material :: [Material],
                   speakerimage :: Maybe String
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
                                  location :: String,
                                  speakerimage :: Maybe String
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
                   material :: [Material],
                   speakerimage :: Maybe String
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
                   material :: [Material],
                   speakerimage :: Maybe String
                 }
  deriving (Show, Read, Eq, Generic)

instance FromJSON Talk

data OneOhOneData
  = OneOhOneData
  { usual :: Usual
  , talks :: [Talk]
  } deriving (Show, Eq, Generic)

instance FromJSON OneOhOneData

isTalk :: Talk -> Bool
isTalk SpecialEvent{} = False
isTalk CancelledTalk{} = False
isTalk _ = True

nextTalk :: [(Int, Talk)] -> IO (Maybe (Int, Talk))
nextTalk talks = do
  now <- fmap zonedTimeToUTC getZonedTime --getCurrentTime
  pure $ listToMaybe (sortBy (comparing $ date . snd) $ filter (\(i,x) -> date x > now && isTalk x) talks)

talksFromFile :: IO (Usual,[(Int, Talk)])
talksFromFile = do
  f <- BS.readFile "101.yaml"
  case decodeEither' f of
    Left err -> error (show err)
    Right (OneOhOneData usual ts) -> return (usual, reverse $ zip [(0::Int)..] $ reverse ts)

findImage :: String -> IO (Maybe FilePath)
findImage nom = do
  msp <- people <$> readPeopleFile "people.yaml"
  case [ ident x | x <- msp, name x == nom] of
    (idnt:_) -> imageFromIdent idnt
    _ -> pure Nothing

-- Precondition: isTalk t == True
resolveSpeakerImage :: String -- speaker
                    -> Maybe String -- possible speaker iamge
                    -> Maybe FilePath -- do we want to download a remote image, and to where?
                    -> IO (Maybe FilePath)
resolveSpeakerImage speaker speakerImage mtempfile = case speakerImage of
  Nothing -> findImage speaker
  Just "no" -> pure Nothing
  Just url -> case mtempfile of
    Nothing -> pure (Just url)
    Just tempfile -> do
      manager <- newManager tlsManagerSettings
      request <- parseRequest url
      response <- httpLbs request manager
      case statusIsSuccessful (responseStatus response) of
        False -> do
          putStr $ "Warning: could not download speaker image from URL '" ++ url ++ "': " ++ show (statusCode (responseStatus response)) ++ " "
          BS.putStr (statusMessage (responseStatus response))
          putStrLn ""
          pure Nothing
        True -> do
          ext <- case lookup hContentType (responseHeaders response) of
            Just "image/gif" -> pure ".gif"
            Just "image/jpeg" -> pure ".jpg"
            Just "image/png" -> pure ".png"
            Just "image/tiff " -> pure ".tiff"
            Just other -> do
              putStrLn "Warning: unknown speaker image content type '"
              BS.putStr other
              putStrLn "'"
              pure ""
            Nothing -> do
              putStrLn "Warning: No speaker image content type reported"
              pure ""
          LBS.writeFile (tempfile ++ ext) (responseBody response)
          pure (Just (tempfile ++ ext))
