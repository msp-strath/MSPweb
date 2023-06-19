{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Main where

import Prelude hiding (div)
import GHC.Generics
import Data.Yaml
import qualified Data.ByteString as BS

type Markdown = String

data Status = Academic | PhDStudent | Research | PhDFinished | Alum
  deriving (Show, Eq, Generic)

instance FromJSON Status where
  parseJSON (String "academic") = pure Academic
  parseJSON (String "phd-student") = pure PhDStudent
  parseJSON (String "research") = pure Research
  parseJSON (String "phd-finished") = pure PhDFinished
  parseJSON (String "alum") = pure Alum
  parseJSON _ = fail "invalid status"


data LinkRelationship
  = HomePage
  | Staff
  | Pure
  | Thesis
  deriving (Show, Eq, Generic)

instance FromJSON LinkRelationship where
  parseJSON (String "homepage") = pure HomePage
  parseJSON (String "uni") = pure Uni
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

data Person =
  Person { name :: String
         , ident :: Maybe String
         , pronouns :: Maybe String
         , title :: Maybe String
         , status :: Status
         , picture :: Maybe String
         , email :: Maybe String
         , links :: Maybe [Link]
         , description :: Markdown
         , phdTopics :: Maybe [Markdown]    -- Only relevant for status == Academic
         }
  deriving (Show, Eq, Generic)

instance FromJSON Person

------------------------------------------------------------------------------
type HTML = String

div :: String -> HTML -> HTML
div klass content =
  "<div class=\"" ++ klass ++ "\">" ++ content ++ "</div>"

h5 :: HTML -> HTML
h5 content = "<h5>" ++ content ++ "</h5>"

p :: HTML -> HTML
p content = "<p>" ++ content ++ "</p>"

ulist :: [HTML] -> HTML
ulist items = "<ul>" ++ concatMap (\item -> "<li>" ++ item ++ "</li>") items ++ "</ul>"

anchor :: String -> HTML -> HTML
anchor url content = "<a href=\"" ++ url ++ "\">" ++ content ++ "</a>"

img :: String -> HTML
img url = "<img src=\"" ++ url ++ "\">"

linkToHTML :: Link -> HTML
linkToHTML link = case rel link of
  HomePage -> anchor (href link) "homepage"
  Uni      -> anchor (href link) "Staff page"
  Pure     -> anchor (href link) "Staff page (pure)"
  Thesis   -> anchor (href link) "PhD Thesis"

emailToHTML :: String -> HTML
emailToHTML emailAddr =
  anchor ("mailto:" ++ emailAddr) ("Email: " ++ emailAddr)

statusToHTML :: Status -> HTML
statusToHTML Academic = "Academic staff"
statusToHTML Research = "Research staff"
statusToHTML PhDStudent = "PhD student"
statusToHTML PhDFinished = "Alumnus (PhD)"
statusToHTML Alum = "Alumus"

personToHTML :: Person -> HTML
personToHTML person =
  div "card"
    (concat [ maybe "" (\fname -> img ("people-pics/" ++ fname)) (picture person)
            , h5 (maybe "" (++" ") (title person) ++ name person)
            , p (statusToHTML (status person))
            , p (description person)
            -- pronouns
            , maybe "" emailToHTML (email person)
            , maybe "" (ulist . map linkToHTML) (links person)
            -- PhD topics
            ])

------------------------------------------------------------------------------
main :: IO ()
main = do
  f <- BS.readFile "people.yaml"
  case decodeEither' f of
    Left err ->
      error (show err)
    Right people ->
      putStrLn (unlines (map personToHTML people))
