{-# LANGUAGE DeriveGeneric, OverloadedStrings, RecordWildCards #-}
module TwoOhOne where

import System.Directory
import System.IO

import Data.List
import Data.Maybe
import Data.Time
import Data.Yaml
import qualified Data.ByteString as BS
import GHC.Generics

import Html hiding (tag)

------------------------------------------------------
-- Parse --

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
              | DOI { doi :: String }
  deriving (Show, Read, Eq, Generic)
-- TODO: check if links are still live in JSON parsing of urls
instance FromJSON Material

data Literature = Literature
  { 
    tag :: Tag,
    title :: String,
    -- TODO: implement more sophisticated JSON parser for authors to merge author and authors fields together
    author :: Maybe String,
    -- TODO: the Maybe [a] parsers are so the auto generated FromJSON instance doesn't warn on missing authors/material/dates fields, since the standard [a] FromJSON instance errors on missing fields
    authors :: Maybe [String],
    material :: Maybe [Material],
    -- TODO: implement more sophisticated JSON parser for dates to merge date and dates fields together
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

------------------------------------------------------
-- HTML generation --

splsZulipHTML = anchor "https://spls.zulipchat.com"
                       "SPLS Zulipchat"
uosZulipHTML = anchor "https://spls.zulipchat.com/#narrow/stream/227686-UoS/topic/MSP.20201.20study.20group"
                      "#UoSstream"

firstJust :: [Maybe a] -> Maybe a
firstJust = listToMaybe . catMaybes

formatUTCDate :: UTCTime -> String
formatUTCDate = formatTime defaultTimeLocale "%Y-%m-%d"

header :: Usual
       -> String
header Usual{..} = 
  let formattedTime = let f s = formatTime defaultTimeLocale s usualTime in
                      f "%l" ++
                      (if todMin usualTime /= 0 then f ".%M" else "") ++
                      f "%P"
  in
  unlines
    [ "### default.html",
      h2 "MSP201 Study Group",
      p "MSP201 is a series of study groups, aimed at PhD students and other academics interested in MSP topics. We typically choose either a book to read over the course of a semester, or a paper to read over one or two sessions. The study group usually happens in person, but we might occasionally set up a Zoom link upon request.",
      p $ bold "How to join?" ++ " The study group is coordinated on " ++ splsZulipHTML ++ ", in the " ++ uosZulipHTML ++ ". Everyone is welcome to join and suggest new topics!",
      p $ "MSP201 is usually " ++ usualDay ++ " " ++ formattedTime ++ " in room " ++ usualRoom ++ " in " ++ usualBuilding ++ "."
    ]

litHTML :: (Int,Literature)
        -> String
litHTML (i, Literature{..}) =
  let formatDates dts =
        details (length dts <= 3) "Dates" 
                (ulist $ formatUTCDate <$> dts)

      formatMaterial :: Material -> HTML
      formatMaterial (File{..}) = anchor path (fromMaybe "File" description)
      formatMaterial (Link{..}) = anchor url (fromMaybe "Link" description)
      formatMaterial (DOI{..}) = anchor ("https://doi.org/" ++ doi) doi

      formatMaterials ms =
          details False "Material"
          (ulist $ formatMaterial <$> ms)

      formattedAuthors = (" by " ++) <$> firstJust [
          author,
          intercalate ", " <$> authors
        ]
  in
  ddtWithID i
    (bold title ++ fromMaybe "" formattedAuthors)
    (unlines . catMaybes $
      [ Html.div "" . ("Suggested by: " ++) <$> suggestedBy,
        firstJust [
          Html.div "" . formatUTCDate <$> date,
          formatDates <$> dates,
          Html.div "" <$> semester
        ],
        formatMaterials <$> material
      ])

section :: String -> Tag -> [(Int,Literature)] -> HTML
section header t lits = unlines [
  h3 header,
  dl $ unlines $ litHTML <$>
    filter (\(i,x) -> tag x == t) lits ]

generateHTML :: Usual
             -> [(Int,Literature)]
             -> FilePath
             -> IO ()
generateHTML usual lits out = do
  let currentlyReading = section "Currently Reading" Reading lits 
  let toRead = section "Planned to Read" ToRead lits
  -- TODO: hide Read papers in the future
  let read = section "Previously Read" Read lits
  writeFile out $ unlines [header usual, currentlyReading, toRead, read]

