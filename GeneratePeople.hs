{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Main where

import Prelude hiding (div, span)
import GHC.Generics
import Data.Yaml
import Data.Functor
import Data.List hiding (span)
import Data.Maybe
import qualified Data.ByteString as BS

import Control.Monad

import System.Directory
import System.FilePath

import Html
import Markdown

type Markdown = String

data Status = Academic | PhDStudent | PhDStaff | Research | PhDFinished | Alum
  deriving (Show, Eq, Generic)

instance FromJSON Status where
  parseJSON (String "academic") = pure Academic
  parseJSON (String "phd-student") = pure PhDStudent
  parseJSON (String "research") = pure Research
  parseJSON (String "phd-finished") = pure PhDFinished
  parseJSON (String "phd-staff") = pure PhDStaff
  parseJSON (String "alum") = pure Alum
  parseJSON _ = fail "invalid status"


data LinkRelationship
  = HomePage
  | Pure
  | Thesis
  | Staff
  deriving (Show, Eq, Generic)

instance FromJSON LinkRelationship where
  parseJSON (String "homepage") = pure HomePage
  parseJSON (String "staff") = pure Staff
  parseJSON (String "pure") = pure Pure
  parseJSON (String "thesis") = pure Thesis
  parseJSON _ = fail "invalid link type"


data Link =
  Link { href  :: String
       , rel   :: LinkRelationship
--       , linkTitle :: Maybe String
       }
  deriving (Show, Eq, Generic)

instance FromJSON Link

data Person = Person
  { firstname :: String
  , lastname :: String
  , ident :: String
  , pronouns :: Maybe String
  , title :: Maybe String
  , status :: Status
  , picture :: Maybe String
  , email :: Maybe String
  , webpage :: Maybe String
  , links :: Maybe [Link]
  , description :: Markdown
  , phdTopics :: Maybe [Markdown]    -- Only relevant for status == Academic
  } deriving (Show, Eq, Generic)

name :: Person -> String
name p = firstname p ++ " " ++ lastname p

instance FromJSON Person

hasStatus :: Status -> Person -> Bool
hasStatus s p = (==) s (status p)

currentMember :: Person -> Bool
currentMember p = case status p of
  Academic -> True
  Research -> True
  PhDStaff -> True
  PhDStudent -> True
  PhDFinished -> False
  Alum -> False

data MSP
  = MSP
  { preamble :: Markdown
  , people   :: [Person]
  } deriving (Show,Eq,Generic)

instance FromJSON MSP

data MSPGrouped
  = MSPGrouped
  {
    academic :: [Person]
  , research :: [Person]
  , student  :: [Person]
  , alumni   :: [Person]

  }
------------------------------------------------------------------------------

linkToHTML :: Link -> HTML
linkToHTML link = case rel link of
  HomePage -> anchor (href link) "homepage"
  Staff    -> anchor ("http://www.strath.ac.uk/staff/" ++ href link) "Staff page"
  Pure     -> anchor (href link) "Staff page (pure)"
  Thesis   -> anchor (href link) "PhD Thesis"

statusToHTML :: Status -> HTML
statusToHTML Academic = "Academic staff"
statusToHTML Research = "Research staff"
statusToHTML PhDStudent = "PhD student"
statusToHTML PhDStaff = "PhD Student & Teaching Staff"
statusToHTML PhDFinished = "Alumnus (PhD)"
statusToHTML Alum = "Alumus"

personToHTML :: Person -> IO HTML
personToHTML person = do
  let nom = maybe "" (++" ") (title person) ++ name person
  let prnouns = maybe "" (\ x -> " " ++ span "grayish" ("(" ++ x ++ ")")) (pronouns person)
  let links = intersperse " or email " $ catMaybes [homepage person, maillink person]
  desc <- translateMarkdown (description person ++ concatStop links)
  image <- imageFromIdent (ident person) (currentMember person)
  let body = concat [strong nom, prnouns, desc]
  pure $ div "person" $ concat $ catMaybes
    [ (div "person-image" . img (Just "border-radius: 20%; height: 100px;") (name person)) <$> image
    , pure (div "person-description" body)
    ]
    where
      maillink :: Person -> Maybe HTML
      maillink person | hasStatus Academic person
        = fmap emailToHTML (email person)
      maillink person | otherwise = Nothing

      homepage :: Person -> Maybe HTML
      homepage person = fmap (\ w -> "See " ++ anchor w (firstname person ++ "'s webpage")) (webpage person)

      imageFromIdent :: String -> Bool -> IO (Maybe String)
      imageFromIdent _ False = pure Nothing
      imageFromIdent ident _ = do
        images <- listDirectory "images/people/"
        let candidates = [ image | image <- images, dropExtensions image == ident]
        case candidates of
          (path:_) -> pure (Just $ "images/people/" </> path)
          _ -> do
            putStrLn $ "Warning: did not find an image for '" ++ ident ++ "'"
            pure (Just "images/people/placeholder.jpg")

      concatStop :: [String] -> String
      concatStop [] = ""
      concatStop xs = concat $ xs ++ ["."]

peopleToHTML :: HTML -> [Person] -> IO HTML
peopleToHTML _ [] = pure ""
peopleToHTML title (p:ps) = do
  content <- traverse personToHTML (p:ps)
  pure $ unlines (h3 title : content)


groupMSP :: [Person] -> MSPGrouped
groupMSP
  = foldl bucket (MSPGrouped [] [] [] [])

  where
    bucket :: MSPGrouped -> Person -> MSPGrouped
    bucket g p =
      case status p of
        Academic     -> g { academic = academic g ++ [p] }
        PhDStudent   -> g { student  = student  g ++ [p] }
        PhDStaff     -> g { student  = student  g ++ [p] }
        Research     -> g { research = research g ++ [p] }
        PhDFinished  -> g { alumni   = alumni   g ++ [p] }
        Alum         -> g { alumni   = alumni   g ++ [p] }

generatePeople :: FilePath -> IO HTML
generatePeople file = do
  f <- BS.readFile file
  case decodeEither' f of
    Left err ->
      error (show err)
    Right input -> do
      let msp = groupMSP (people input)
      concat <$> sequence
        [ pure $ h2 "People"
        , translateMarkdown (preamble input)
        , peopleToHTML "Academic Staff" (academic msp)
        , peopleToHTML "Research Staff" (research msp)
        , peopleToHTML "PhD Students" (student msp)
        , peopleToHTML "Alumni" (alumni msp)
        ]

------------------------------------------------------------------------------
main :: IO ()
main = do
  people <- generatePeople "people.yaml"
  let header = "### default.html(section.people=current,headtags=<link rel='stylesheet' href='{{rootPath}}css/pure.css' type='text/css'>)\n<!-- DO NOT EDIT THIS FILE DIRECTLY — EDIT people.yaml AND RUN GeneratePeople.hs INSTEAD -->"
  writeFile "people.html" (header ++ people)
