{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Main where

import Prelude hiding (div)
import GHC.Generics
import Data.Yaml
import Data.List
import qualified Data.ByteString as BS

import Html

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
  { name :: String
  , ident :: Maybe String
  , pronouns :: Maybe String
  , title :: Maybe String
  , status :: Status
  , picture :: Maybe String
  , email :: Maybe String
  , links :: Maybe [Link]
  , description :: Markdown
  , phdTopics :: Maybe [Markdown]    -- Only relevant for status == Academic
  } deriving (Show, Eq, Generic)

instance FromJSON Person

data MSP
  = MSP
  { preamble :: Markdown
  , academic :: [Person]
  , research :: Maybe [Person]
  , student  :: [Person]
  , graduate :: [Person]
  , alumni   :: [Person]
  } deriving (Show,Eq,Generic)

instance FromJSON MSP
------------------------------------------------------------------------------

linkToHTML :: Link -> HTML
linkToHTML link = case rel link of
  HomePage -> anchor (href link) "homepage"
  Staff    -> anchor (href link) "Staff page"
  Pure     -> anchor (href link) "Staff page (pure)"
  Thesis   -> anchor (href link) "PhD Thesis"

statusToHTML :: Status -> HTML
statusToHTML Academic = "Academic staff"
statusToHTML Research = "Research staff"
statusToHTML PhDStudent = "PhD student"
statusToHTML PhDStaff = "PhD Student & Teaching Staff"
statusToHTML PhDFinished = "Alumnus (PhD)"
statusToHTML Alum = "Alumus"

personToHTML :: Person -> HTML
personToHTML person =
  div "card"
    (concat [ maybe "" (\fname -> img ("people-pics/" ++ fname)) (picture person)
            , h5 (maybe "" (++" ") (title person) ++ name person)
--            , p (statusToHTML (status person))
            , p (description person)
            -- pronouns
            , maybe "" emailToHTML (email person)
            , maybe "" (ulist . map linkToHTML) (links person)
            -- PhD topics
            ])

mspToHTML :: [Person] -> HTML -> HTML
mspToHTML ps title
  = unlines
  $ (h3 title)
  : map personToHTML ps

------------------------------------------------------------------------------
main :: IO ()
main = do
  f <- BS.readFile "people.yaml"
  case decodeEither' f of
    Left err ->
      error (show err)
    Right msp -> do
      putStrLn (preamble msp)
      putStrLn (mspToHTML (academic msp) "Academic Staff")
      putStrLn (maybe "" (\ps -> mspToHTML ps "Research Staff") (research msp))
      putStrLn (mspToHTML (student msp) "PhD Students")
      putStrLn (mspToHTML (graduate msp) "Graduates")
      putStrLn (mspToHTML (alumni msp) "Alumni")
--      putStrLn (unlines (map personToHTML (people members)))
