{-# LANGUAGE DeriveGeneric, OverloadedStrings, RecordWildCards #-}
module TwoOhOne where

import System.Directory
import System.IO

import Control.Applicative

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

instance FromJSON Material where
  parseJSON = withObject "Material" $ \o ->
    fileparser o <|> linkparser o <|> doiparser o
    where
      fileparser o = do
        path <- o .: "file"
        desc <- o .:? "description"
        pure (File path desc)
      linkparser o = do
        url <- o .: "url"
        desc <- o .:? "description"
        pure (Link url desc)
      doiparser o = DOI <$> o .: "doi"

data ReadingDates = Dates [UTCTime] | Semester String
  deriving (Show, Read, Eq, Generic)

data Literature = Literature
  {
    tag :: Tag,
    title :: String,
    authors :: [String],
    material :: Maybe [Material],
    dates :: ReadingDates,
    suggestedBy :: Maybe String
  }
  deriving (Show, Read, Eq, Generic)

instance FromJSON Literature where
  parseJSON = withObject "Literature" $ \o -> Literature <$>
    o .: "status" <*>
    o .: "title" <*>
    authorparser o  <*>
    o .: "material" <*>
    dateparser o <*>
    o .:? "suggestedBy"
   where
     authorparser o =
       (o .: "author" >>= \ a -> pure [a]) <|>
       (o .: "authors") <|>
       (pure [])
     dateparser o =
       (o .: "date" >>= \ d -> pure (Dates [d])) <|>
       (Dates <$> o .: "dates") <|>
       (Semester <$> o .: "semester") <|>
       pure (Dates [])



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

formatUTCDate :: UTCTime -> String
formatUTCDate = formatTime defaultTimeLocale "%A %e %B %Y"

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
  let formatDates :: ReadingDates -> Maybe HTML
      formatDates (Dates []) = Nothing
      formatDates (Dates [date]) = Just $ Html.div "" (formatUTCDate date)
      formatDates (Dates dts) = Just $
        details (length dts <= 3) "Dates"
                (ulist $ formatUTCDate <$> dts)
      formatDates (Semester semester) = Just $ Html.div "" semester


      formatMaterial :: Material -> HTML
      formatMaterial (File{..}) = anchor path (fromMaybe "File" description)
      formatMaterial (Link{..}) = anchor url (fromMaybe "Link" description)
      formatMaterial (DOI{..}) = anchor ("https://doi.org/" ++ doi) doi

      formatMaterials ms =
          details False "Material"
          (ulist $ formatMaterial <$> ms)

      formattedAuthors = if null authors then "" else " by " ++ (intercalate ", " authors)
  in
  ddtWithID i
    (bold title ++ formattedAuthors)
    (unlines . catMaybes $
      [ Html.div "suggestedBy" . ("Suggested by: " ++) <$> suggestedBy,
        formatDates dates,
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
  let toRead = section "Planning to Read" ToRead lits
  -- TODO: hide Read papers in the future
  let read = section "Previously Read" Read lits
  writeFile out $ unlines [header usual, currentlyReading, toRead, read]
