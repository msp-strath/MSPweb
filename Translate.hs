module Translate where

import Prelude hiding (div, span)

import Data.List (isSuffixOf, intersperse)
import Data.Maybe

import Text.Pandoc
import Data.Text (pack, unpack)

import Html
import People

knownSuffixes = [".html", ".md", ".typ"]

outputExtension :: String -> String
outputExtension ".md" = ".html"
outputExtension ".typ" = ".html"
outputExtension x = x

translateFormat :: String -> String -> IO String
translateFormat ".md" = translateMarkdown
translateFormat ".typ" = translateTypst
translateFormat _ = return

translateMarkdown :: String -> IO String
translateMarkdown s =
  fmap unpack $ handleError =<< runIO (writeHtml5String def  =<< (readMarkdown (def { readerExtensions = phpMarkdownExtraExtensions }) $ pack s))

translateTypst :: String -> IO String
translateTypst s =
  fmap unpack $ handleError =<< runIO (writeHtml5String (def { writerMathMethod = MathML }) =<< (readTypst def $ pack s))

----------------------------------------------------------------------------

linkToHTML :: Link -> HTML
linkToHTML link = case rel link of
  HomePage -> anchor (href link) "homepage"
  Staff    -> anchor ("http://www.strath.ac.uk/staff/" ++ href link) "Staff page"
  Pure     -> anchor ("https://pureportal.strath.ac.uk/en/persons/" ++ href link) "Staff page (pure)"
  Thesis   -> anchor (href link) "PhD Thesis"

statusToHTML :: Status -> HTML
statusToHTML Academic = "Academic staff"
statusToHTML Research = "Research staff"
statusToHTML PhDStudent = "PhD student"
statusToHTML PhDStaff = "PhD Student & Teaching Staff"
statusToHTML PhDFinished = "Alumnus (PhD)"
statusToHTML Alum = "Alumus"
statusToHTML Honorary = "Honorary Research Fellow"

personToHTML :: Person -> IO HTML
personToHTML person = do
  let nom = maybe "" (++" ") (label person) ++ name person
  let prnouns = maybe "" (\ x -> " " ++ span "grayish" ("(" ++ x ++ ")")) (pronouns person)
  let links = intersperse " or email " $ catMaybes [homepage person, maillink person]
  desc <- translateMarkdown (description person ++ concatStop links)
  image <- case currentMember person of
    True -> do
      cand <- imageFromIdent (ident person)
      pure (Just (maybe "images/people/placeholder.jpg" id cand))
    False -> pure Nothing
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

      concatStop :: [String] -> String
      concatStop [] = ""
      concatStop xs = concat $ xs ++ ["."]

peopleToHTML :: HTML -> [Person] -> IO HTML
peopleToHTML _ [] = pure ""
peopleToHTML title (p:ps) = do
  content <- traverse personToHTML (p:ps)
  pure
    $ unlines
      [ h3 title
      , div "people" $ unlines content
      ]
