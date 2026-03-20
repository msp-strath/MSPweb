{-# LANGUAGE LambdaCase, OverloadedStrings, DeriveGeneric, RecordWildCards #-}
module Main where

import Control.Monad
import Control.Monad.IO.Class

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B
import Data.Char
import Data.Maybe
import Data.Time
import Data.Text (Text, pack, unpack)
import qualified Data.Text.Lazy as LT
import Data.Yaml

import GHC.Generics

import Network.Mail.SMTP hiding (htmlPart)
import Network.Mail.Mime (plainPart, htmlPart)
import Network.HTTP.Req
import Network.HTTP.Client.MultipartFormData

import System.Directory
import System.FilePath
import System.Exit
import System.IO
import System.IO.Temp
import System.Process

import Html
import OneOhOne

expandLocation :: String -> String
expandLocation room =
  case lookup (takeWhile isAlpha room) codes of
    Just building -> building ++ " room " ++ room
    Nothing -> room
  where
    codes = [ ("AB", "Robertson Wing")
            , ("AL", "181 St James Road")
            , ("AQ", "Lord Todd Building")
            , ("AT", "Alexander Turnbull Building")
            , ("BH", "Barony Hall")
            , ("CL", "Collins Building")
            , ("CU", "Curran Building (Library)")
            , ("CW", "Cathedral Street Wing")
            , ("DW", "Sir William Duncan Wing")
            , ("GH", "Graham Hills Building")
            , ("HD", "Henry Dyer Building")
            , ("HL", "Kelvin Hydrodynamics Laboratory")
            , ("HW", "Hamnett Wing")
            , ("A", "John Anderson Building")
            , ("JW", "James Weir Building")
            , ("LH", "Lord Hope Building")
            , ("LT", "Livingstone Tower")
            , ("MC", "McCance Building")
            , ("RC", "Royal College Building")
            , ("H", "Strathclyde Sport")
            , ("SP", "St Paul's Chaplaincy Centre")
            , ("SW", "Stenhouse Wing (Business School")
            , ("TC", "Technology Innovation Centre")
            , ("TG", "Thomas Graham Building")
            , ("TL", "Learning and Teaching Building")
            , ("UC", "University Centre")
            , ("WC", "Wolfson Centre")
            ]

emailTemplate :: String -- speaker
              -> String -- affiliation
              -> String -- title
              -> String -- abstract
              -> String -- location
              -> UTCTime -- when
              -> String -- online URL
              -> String -- announcer
              -> (String, String, String) -- subject, body, htmlBody
emailTemplate speaker affiliation title abstract location time online announcer  =
  (subject, body, htmlBody)
   where
    shortTime = formatTime defaultTimeLocale "%-l%P %a %-e/%-m" time
    longTime = formatTime defaultTimeLocale "%A %-e %B, %H:%M" time
    subject = "[MSP101] " ++ speaker ++ ": " ++ title ++ " (" ++ shortTime ++ ", " ++ location ++ ")"
    blurb = "Dear all,\n\nWe have another MSP101 seminar coming up. Hope to see you there!\n"
    htmlBlurb = "<p>Hi newsletter editor,</p><p>Please find details below for the upcoming MSP group seminar. All welcome!</p>"
    body = unlines $
      [ blurb
      , "Best wishes,\n" ++ announcer
      , ""
      , "Date, time and place:\n  " ++ longTime ++ ", " ++ expandLocation location
      , ""
      , if null online then "" else ("Online attendance:\n  " ++ online ++ "\n")
      , "Speaker:\n  " ++ speaker ++ (if null affiliation then "" else " (" ++ affiliation ++ ")")
      , ""
      , "Title:\n  " ++ title
      , ""
      , "Abstract:\n  " ++ abstract
      , "MSP101 Feeds:"
      , "  Web: http://msp.cis.strath.ac.uk/msp101.html"
      , "  RSS: http://msp.cis.strath.ac.uk/msp101.rss"
      , " iCal: http://msp.cis.strath.ac.uk/msp101.ics"
      , ""
      ]
    htmlBody = unlines $
      [ htmlBlurb
      , "<p>Best wishes,<br>" ++ announcer ++ "</p>"
      , tag "p" $ (tag "b" "Date, time and place:") <++> longTime ++ ", " ++ expandLocation location
      , if null online then "" else (tag "p" $ (tag "b" "Online attendance:") <++> link online)
      , tag "p" $ (tag "b" "Speaker:") <++> speaker ++ (if null affiliation then "" else " (" ++ affiliation ++ ")")
      , tag "p" $ (tag "b" "Title:") <++> title
      , tag "p" $ (tag "b" "Abstract:") <++> abstract
      ]


zulipTemplate :: String -- speaker
              -> String -- affiliation
              -> String -- title
              -> String -- abstract
              -> String -- location
              -> UTCTime -- when
              -> String -- online URL
              -> LT.Text
zulipTemplate speaker affiliation title abstract location time online = LT.pack $ unlines $
      [ "**Date and time:** " ++ formatTime defaultTimeLocale "%A %-e %B, %H:%M" time
      , "**Venue**: " ++  expandLocation location
      , "**Online**: " ++ online
      , "**Speaker:** " ++ speaker ++ (if null affiliation then "" else " (" ++ affiliation ++ ")")
      , "**Title:** " ++ title
      , "**Abstract:**\n```quote\n" ++ abstract ++ "\n```"
      , ""
      , "**MSP101 Feeds:** [Web](http://msp.cis.strath.ac.uk/msp101.html), [RSS](http://msp.cis.strath.ac.uk/msp101.rss), [iCal](http://msp.cis.strath.ac.uk/msp101.ics)"
      ]

mastodonTemplate :: String -- speaker
                 -> String -- affiliation
                 -> String -- title
                 -> String -- location
                 -> UTCTime -- when
                 -> Text
mastodonTemplate speaker affiliation title location time = pack $ unlines $
  [ "On " ++ formatTime defaultTimeLocale "%A %-e %B at %H:%M" time ++ ", " ++ speaker ++ " will give a talk in the #MSP101 seminar entitled"
  , ""
  , "> " ++ title
  , ""
  , "The talk will take place in " ++ expandLocation location ++ "."
  , ""
  , "More details can be found on the @spli@mastodon.scot Zulip and at https://msp.cis.strath.ac.uk/msp101.html ."
  ]

imageTemplate :: String -- speaker
              -> String -- affiliation
              -> String -- title
              -> String -- location
              -> UTCTime -- when
              -> (String, B.ByteString) -- (tex, alt text)
imageTemplate speaker affiliation title location time = (tex, alt) where
  tex = unlines $
    [ "\\documentclass[colour=random]{mspadvert}"
    , "\\renewcommand{\\conferenceHook}{MSP101 Seminar}"
    , "\\title{" ++ title ++ "}"
    , "\\date{" ++ formatTime defaultTimeLocale "%A %-e %B %Y" time ++ "}"
    , "\\author{" ++ speaker ++ "}"
    , "\\institute{" ++ affiliation ++ "}"
    , "\\begin{document}"
    , "\\maketitle"
    , "\\end{document}"
    ]
  alt = B.pack $ unlines $
    [ "MSP 101 seminar announcement."
    , ""
    , "Title: " ++ title
    , "Date: " ++ formatTime defaultTimeLocale "%A %-e %B %Y" time
    , "Speaker: " ++ speaker
    , "Affiliation: " ++ affiliation
    ]

data AnnounceSettings = AnnounceSettings
  { emailAnnouncer :: String
  , announcer :: String
  , announcerShort :: String
  , smtpPassword :: String
  --
  , onlineURL :: String
  --
  , zulipChannelEmail :: String
  --
  , mastodonClientKey :: String
  , mastodonClientSecret :: String
  , mastodonAccesstoken :: String

  }
  deriving (Generic)

instance FromJSON AnnounceSettings
instance ToJSON AnnounceSettings


readSettings :: IO AnnounceSettings
readSettings = do
  doesFileExist "_announcesettings.yaml" >>= \case
    False -> do
      BS.writeFile "_announcesettings.yaml" (encode empty)
      putStrLn "File '_announcesettings.yaml' does not exist, creating stub. Edit and try again."
      exitFailure
    True -> do
      f <- BS.readFile "_announcesettings.yaml"
      case decodeEither' f of
        Left err -> error (show err)
        Right s -> pure s
  where
    empty = AnnounceSettings "" "" "" "" "" "" "" "" ""

confirm :: String -> IO Bool
confirm msg = do
  putStr $ msg ++ " "
  input <- getChar
  putStrLn ""
  case toUpper input of
    'Y' -> pure True
    'N' -> pure False
    _ -> confirm msg

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  AnnounceSettings{..} <- readSettings
  let smtpServer = "mail-eu.smtp2go.com"
  let username = "msp101announcer"
  let from = Address (Just (pack announcer)) (pack emailAnnouncer)
  (usual,ts) <- talksFromFile
  nextTalk ts >>= \case
    Nothing -> putStrLn "No upcoming talk to announce!"
    Just (i, t) -> do

      putStrLn $ "NEXT TALK:\n\n" ++ formatTime defaultTimeLocale "%-e %B %Y" (date t) ++ " " ++ speaker t ++ ": " ++ (title t)
      putStrLn ""
      doit <- confirm "Continue (y/n)?"
      when (not doit) $ exitSuccess

      -- Announce on mailing list and departmental newsletter
      let (subject, body, htmlBody) = emailTemplate (speaker t) (institute t) (title t) (abstract t) (location t) (date t) onlineURL announcerShort

      putStrLn "==== Mailing list ======================================"
      putStrLn ("Subject: " ++ subject)
      putStrLn body
      putStrLn "========================================================"
      doit <- confirm "Announce to mailing list (y/n)?"
      when doit $ sendMailWithLoginTLS smtpServer username smtpPassword (simpleMail from [Address Nothing "msp-interest@lists.strath.ac.uk"]  [] [] (pack subject) [plainPart (LT.pack body)])

      putStrLn "==== Departmental newsletter ==========================="
      putStrLn ("Subject: " ++ subject)
      putStrLn htmlBody
      putStrLn "========================================================"
      doit <- confirm "Announce to departmental newsletter? (y/n)?"
      when doit $ sendMailWithLoginTLS smtpServer username smtpPassword (simpleMail from [Address Nothing "cis-newsletter@strath.ac.uk"]  [] [] (pack subject) [plainPart (LT.pack body)])

      -- Announce on Zulip
      let zulipBody = zulipTemplate (speaker t) (institute t) (title t) (abstract t) (location t) (date t) onlineURL
      putStrLn "==== Zulip ============================================="
      putStrLn (LT.unpack zulipBody)
      putStrLn "========================================================"
      doit <- confirm "Announce to Zulip (y/n)?"
      when doit $ sendMailWithLoginTLS smtpServer username smtpPassword (simpleMail from [Address Nothing (pack zulipChannelEmail)]  [] [] "MSP 101" [plainPart zulipBody])

      -- Announce on Mastodon
      let fediBody = mastodonTemplate (speaker t) (institute t) (title t) (location t) (date t)
      putStrLn "==== Mastodon =========================================="
      putStrLn (unpack fediBody)
      putStrLn "========================================================"
      doit <- confirm "Announce to Mastodon (y/n)?"
      when doit $ do
        let (imageTex, altText) = imageTemplate (speaker t) (institute t) (title t) (location t) (date t)
        withTempDirectory "_msp-conference-advert" "msp-ad" $ \ dir -> do
          forM_ ["mspadvert.cls"
                , "strathclyde-colours.sty"
                , "strathclyde-tikz.sty"
                , "msp-background.png"
                , "msp.png"
                , "strath_science.jpg"]
            (\ x -> makeAbsolute (dir </> ".." </> x) >>= flip copyFile (dir </> x))

          writeFile (dir </> "ad.tex") imageTex
          withCurrentDirectory dir $ do
            callProcess "pdflatex" ["ad.tex"]
            callProcess "pdflatex" ["ad.tex"]
            callProcess "pdftoppm" ["-png", "ad.pdf", "ad"]
            runReq defaultHttpConfig $ do
              let headers = header "Authorization" (B.pack $ "Bearer " ++ mastodonAccesstoken)
              file <- reqBodyMultipart [partFile "file" "ad-1.png", partBS "description" altText]
              uploadReq <- req POST (https "mastodon.acm.org" /: "api" /: "v2" /: "media" ) file jsonResponse headers
              -- liftIO $ print (responseBody uploadReq :: Value)
              -- liftIO $ putStrLn "====="
              case parseEither (withObject "response" $ \ o -> o .: "id") (responseBody uploadReq :: Value) of
                Left err -> error (show err)
                Right i -> do
                  let params = "status" =: fediBody
                               <> "media_ids[]" =: (i :: Text)
                  postReq <- req POST (https "mastodon.acm.org" /: "api" /: "v1" /: "statuses" ) (ReqBodyUrlEnc params) bsResponse headers
                  --liftIO $ BS.putStr (responseBody postReq)
                  pure ()
