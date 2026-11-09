{-# LANGUAGE DeriveGeneric, OverloadedStrings, LambdaCase, TupleSections, RecordWildCards #-}
module TwoOhOne where

import Data.Time
import System.Directory
import System.IO

import Data.List
import Data.Maybe

import Html hiding (tag)
import TwoOhOneLiterature

splsZulipHTML = anchor "https://spls.zulipchat.com"
                       "SPLS Zulipchat"
uosZulipHTML = anchor "https://spls.zulipchat.com/#narrow/stream/227686-UoS/topic/MSP.20201.20study.20group"
                      "#UoSstream"

firstJust :: [Maybe a] -> Maybe a
firstJust [] = Nothing
firstJust (Nothing : xs) = firstJust xs
firstJust (Just a : _) = Just a

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
      p $ "MSP201 is a series of study groups, aimed at PhD students and other academics interested in MSP topics. We typically choose either a book to read over the course of a semester, or a paper to read over one or two sessions. The study group usually happens in person, but we might occasionally set up a Zoom link upon request.",
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
  -- TODO: what to do with ToRead papers in the past?
  -- TODO: what to do with Read papers in the future?
  let currentlyReading = section "Currently Reading" Reading lits 
  let toRead = section "Planned to Read" ToRead lits
  let read = section "Previously Read" Read lits
  writeFile out $ unlines [header usual, currentlyReading, toRead, read]

