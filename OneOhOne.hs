{-# LANGUAGE DeriveGeneric, OverloadedStrings, LambdaCase, TupleSections #-}
module OneOhOne where

import Data.Time
import System.Directory
import System.IO

import Data.List
import Data.Maybe
import Data.Ord
import Data.Foldable
import Control.Arrow
import Control.Monad

import Data.Array((!))
import Text.Regex.PCRE

import GHC.Generics
import Data.Aeson

import qualified Data.Text.Lazy as LT
import qualified Data.Text as T

import qualified Data.ByteString.Lazy as LBS

import Data.Map (Map)
import qualified Data.Map as Map

import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Status

import Data.XML.Types
import Text.Feed.Types
import Text.RSS.Syntax
import Text.RSS.Export
import Text.Feed.Import
import Text.Feed.Query

-- constants
usualTime :: TimeOfDay
usualTime = TimeOfDay 13 0 0

usualDay :: String
usualDay = "Friday"

usualRoom :: String
usualRoom = "LT210"

usualBuilding :: String
usualBuilding = "Livingstone Tower"

-- HTML utils

nl2br :: String -> String
nl2br [] = []
nl2br ('\n':xs) = "<br/>\n" ++ nl2br xs
nl2br (x:xs) = x:(nl2br xs)

createLink :: String -> String -> String
createLink "" name = name
createLink ref name = "<a href='" ++ ref ++ "'>" ++ name ++ "</a>"

createLinkAnchor :: String -> String -> String
createLinkAnchor "" name = name
createLinkAnchor ref name = "<a href='" ++ ref ++ "' class='hoverlink'>" ++ name ++ "</a>"

bracket :: String -> String
bracket str = if null str then "" else " (" ++ str ++ ")"

-- Text utils

wordwrap :: Int -> String -> String
wordwrap maxlen = wrap_ 0 . words where
        wrap_ _ [] = ""
        wrap_ pos (w:ws)
                -- at line start: put down the word no matter what
                | pos == 0, lw > maxlen = take maxlen w ++ "\n " ++ wrap_ 0 (drop maxlen w:ws)
                | pos == 0 = w ++ wrap_ lw ws
                | pos + lw + 1 > maxlen = "\n  " ++ wrap_ 0 (w:ws)
                | otherwise = " " ++ w ++ wrap_ (pos + lw + 1) ws
                where lw = length w

-- Text.Regex.PCRE does not implement subRegex, so we copy
-- the code from Text.Regex (which does not handle non-greedy matches)
-- (a module system for Haskell, anyone?)
subRegex :: Regex                          -- ^ Search pattern
         -> String                         -- ^ Input string
         -> String                         -- ^ Replacement text
         -> String                         -- ^ Output string
subRegex _ "" _ = ""
subRegex regexp inp repl =
  let compile _i str [] = \ _m ->  (str++)
      compile i str (("\\",(off,len)):rest) =
        let i' = off+len
            pre = take (off-i) str
            str' = drop (i'-i) str
        in if null str' then \ _m -> (pre ++) . ('\\':)
             else \  m -> (pre ++) . ('\\' :) . compile i' str' rest m
      compile i str ((xstr,(off,len)):rest) =
        let i' = off+len
            pre = take (off-i) str
            str' = drop (i'-i) str
            x = read xstr
        in if null str' then \ m -> (pre++) . ((fst (m!x))++)
             else \ m -> (pre++) . ((fst (m!x))++) . compile i' str' rest m
      compiled :: MatchText String -> String -> String
      compiled = compile 0 repl findrefs where
        -- bre matches a backslash then capture either a backslash or some digits
        bre :: Regex
        bre = makeRegex ("\\\\(\\\\|[0-9]+)" :: String)
        findrefs = map (\m -> (fst (m!1),snd (m!0))) (matchAllText bre repl)
      go _i str [] = str
      go i str (m:ms) =
        let (_,(off,len)) = m!0
            i' = off+len
            pre = take (off-i) str
            str' = drop (i'-i) str
        in if null str' then pre ++ (compiled m "")
             else pre ++ (compiled m (go i' str' ms))
  in go 0 inp (matchAllText regexp inp)

-- quick and dirty regexp translation of some HTML to its textual representation
html2text :: String -> String
html2text s = foldl' (\ t (p , r) -> subRegex (makeRegex p) t r) s $
                    [("<em>(.*?)</em>", "*\\1*"),          -- bold
                     ("<strong>(.*?)</strong>", "*\\1*"),
                     ("<b>(.*?)</b>", "*\\1*"),
                     ("<i>(.*?)</i>", "/\\1/"),            -- italic
                     ("<dfn>(.*?)</dfn>", "/\\1/"),
                     ("<q>(.*?)</q>", "\"\\1\""),          -- quoted
                     ("<br/>|<br>", "\n"),                 -- line breaks
                     ("<p>", ""),          -- paragraph breaks; remove opening tag
                     ("</p>", "\n\n"),     --   turn closing tag into double linebreaks
                     ("<a\\s+href\\s*=\\s*[\"'](.*?)[\"']>\\1</a>", "\\1"), -- links
                     ("<a\\s+href\\s*=\\s*[\"'](.*?)[\"']>(.*?)</a>", "\\2 (\\1)"),
                     ("<ul>", "\n"),                       -- lists
                     ("</ul>", ""),
                     ("<li>(.*?)</li>", "* \\1\n"),
                     ("<div\\s+class\\s*=\\s*[\"']centered[\"']>(.*)</div>", "     \\1     \n"),  -- centered text
                     ("&nbsp;", " "),                 -- escape characters
                     ("&ndash;", "--"),
                     ("&mdash;", "--"),
                     ("&sup2;", "^2"),
                     ("&sup3;", "^3"),
                     ("&frac12;", "1/2"),
                     ("&ouml;", "ö"),
                     ("&auml;", "ä"),
                     ("&aring;", "å"),
                     ("&lt;", "<"),                   -- math
                     ("&gt;", ">"),
                     ("&amp;", "&"),
                     ("&and;", "and"),
                     ("&or;", "or"),
                     ("&isin;", "in"),
                     ("&notin;", "not in"),
                     ("&not;", "not"),
                     ("&empty;", "empty"),
                     ("&forall;", "forall"),
                     ("&exist;", "exist"),
                     ("&rarr;", "->"),
                     ("&larr;", "<-"),
                     ("&harr;", "<->"),
                     ("&rArr;", "=>"),
                     ("&lArr;", "<="),
                     ("&hArr;", "<=>")
                     ] ++
                     [ ("&" ++ letter ++ ";", letter) | -- greek letters
                       letter <- [ "Gamma", "Delta", "Theta", "Lambda",
                                   "Xi", "Pi", "Sigma", "Phi", "Psi",
                                   "Omega", "alpha", "beta", "gamma",
                                   "delta", "epsilon", "zeta", "eta",
                                   "theta", "iota", "kappa", "lambda",
                                   "mu", "nu", "xi", "pi", "rho", "sigma",
                                   "tau", "phi", "psi", "omega" ] ]


-- Generate web pages, calendars and a RSS feed for MSP 101 from data in
-- a text file

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
instance ToJSON Material

data Talk = Talk {
                   date :: UTCTime,
                   speaker :: String,
                   institute :: String,
                   speakerurl :: String,
                   insturl :: String,
                   title :: String,
                   abstract :: String,
                   location :: String,
                   material :: [Material]
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
                                  location :: String
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
                   material :: [Material]
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
                   material :: [Material]
                 }
  deriving (Show, Read, Eq, Generic)

instance FromJSON Talk
instance ToJSON Talk



generateRSS :: [(Int,Talk)]
            -> FilePath -- ^ Output path
            -> IO ()
generateRSS ts out = do

  file <- doesFileExist out >>= \case
    True -> pure (Just out)
    False -> do
      -- try to download last version from website
      manager <- newManager tlsManagerSettings
      res <- httpLbs (parseRequest_ "https://msp.cis.strath.ac.uk/msp101.rss") manager
      case statusIsSuccessful (responseStatus res) of
        False -> pure Nothing
        True -> do
          LBS.writeFile out (responseBody res)
          pure (Just out)
  oldItemsMap <- case file of
    Just file -> parseFeedFromFile file >>= \case
      Just f -> let is = getFeedItems f
                in pure $ Map.fromList $  concatMap (\ x -> maybeToList ((,x) . snd <$> getItemId x)) is
      _ -> pure Map.empty
    _ -> pure Map.empty
  now <- T.pack . formatTime defaultTimeLocale "%a, %_d %b %Y %H:%M:%S %z" <$> getZonedTime
  let channel = (nullChannel "MSP101 Seminar" "https://msp.cis.strath.ac.uk/msp101.rss")
                  { rssDescription = "MSP101 is an ongoing series of informal talks by visiting academics or members of the MSP group."
                  , rssLanguage = Just "en-gb"
                  , rssLastUpdate = Just now
                  , rssChannelOther = [Element "atom:link" [("href", ["https://msp.cis.strath.ac.uk/msp101.rss"]), ("rel", ["self"]), ("type", ["application/rss+xml"])] []]
                  , rssItems = map (processEntry now oldItemsMap) ts
                  }
  let feed = (nullRSS "MSP101 Seminar" "https://msp.cis.strath.ac.uk/msp101.rss")
               { rssAttrs = [("xmlns:atom",["http://www.w3.org/2005/Atom"])]
               , rssChannel = channel }
  case textRSS feed of
    Just t -> writeFile out (LT.unpack t)
    Nothing -> putStrLn "Error: Could not generate RSS feed!"
  where
    gatherData :: Talk -> (String, String)
    gatherData (Talk date speaker inst speakerurl insturl title abstract location material)
      = let rsstitle = showGregorian (utctDay date) ++ ": " ++ speaker ++ bracket inst
            abstr = if null abstract then "" else "<p><b>Abstract</b><br/><br/>" ++  nl2br abstract ++ "</p>"
            desc = concat ["<h2>" ++ createLink speakerurl speaker ++ bracket (createLink insturl inst) ++ "</h2>",
                            "<h2>" ++ title ++ "</h2>",
                            abstr,
                            "<p><b>" ++ show date ++ "<br/>" ++ location ++ "</b><br/></p>"]
        in (rsstitle, desc)
    gatherData (DepartmentalSeminar date speaker inst speakerurl insturl title abstract location)
      = let rsstitle = showGregorian (utctDay date) ++ " Departmental seminar: " ++ speaker ++ bracket inst
            abstr = if null abstract then "" else "<p><b>Abstract</b><br/><br/>" ++  nl2br abstract ++ "</p>"
            desc = concat ["<h2>" ++ createLink speakerurl speaker ++ bracket (createLink insturl inst) ++ "</h2>",
                            "<h2>" ++ title ++ "</h2>",
                            abstr,
                            "<p><b>" ++ show date ++ "<br/>" ++ location ++ "</b><br/>"]
        in (rsstitle, desc)
    gatherData (SpecialEvent date endDate title url location locationurl description)
      = let rsstitle = showGregorian (utctDay date) ++ ": " ++ title
            abstr = if null description then "" else "<p>" ++  nl2br description ++ "</p>"
            desc = concat ["<h2>" ++ createLink url title ++ bracket location ++ "</h2>",
                            "<h2>" ++ title ++ "</h2>",
                            abstr,
                            "<p><b>" ++ show date ++ "<br/>" ++ createLink locationurl location ++ "</b><br/></p>"]
        in (rsstitle, desc)
    gatherData (BasicTalk date speaker inst speakerurl insturl title abstract location material)
      = gatherData (Talk date speaker inst speakerurl insturl ("MSP 101: " ++ title) abstract location material) -- for now

    processEntry :: T.Text -> Map T.Text Item -> (Int, Talk) -> RSSItem
    processEntry now is (i,x) =
      let (rsstitle, desc) = gatherData x
          guid = T.pack ("http://msp.cis.strath.ac.uk/msp101.html#" ++ show i)
          itemBarTime = (nullItem (T.pack rsstitle)) { rssItemDescription = Just (T.pack desc), rssItemGuid = Just (nullPermaGuid guid) }
          time = case Map.lookup guid is of
            Just item@(Text.Feed.Types.RSSItem ri) | Just oldTime <-getItemPublishDateString item -> if equalRSSItems ri itemBarTime then oldTime else now
            _ -> now
      in itemBarTime { rssItemPubDate = Just time }
    equalRSSItems :: RSSItem -> RSSItem -> Bool
    equalRSSItems a b = rssItemTitle a == rssItemTitle b && rssItemDescription a == rssItemDescription b


generateICS :: [(Int,Talk)]
            -> FilePath -- ^ Output path
            -> IO ()
generateICS ts out = do
  now <- getZonedTime
  let content = concatMap (processEntry now) ts
      header = unlines ["BEGIN:VCALENDAR", "VERSION:2.0", "PRODID:-//MSP//MSP101 v1.0//EN",
                        "X-WR-CALNAME: MSP101",
                        "X-WR-CALDESC: MSP101 seminar series",
                        "BEGIN:VTIMEZONE",
                        "TZID:Europe/London",
                        "LAST-MODIFIED:20230407T050750Z",
                        "TZURL:https://www.tzurl.org/zoneinfo-outlook/Europe/London",
                        "X-LIC-LOCATION:Europe/London",
                        "BEGIN:DAYLIGHT",
                        "TZNAME:BST",
                        "TZOFFSETFROM:+0000",
                        "TZOFFSETTO:+0100",
                        "DTSTART:19700329T010000",
                        "RRULE:FREQ=YEARLY;BYMONTH=3;BYDAY=-1SU",
                        "END:DAYLIGHT",
                        "BEGIN:STANDARD",
                        "TZNAME:GMT",
                        "TZOFFSETFROM:+0100",
                        "TZOFFSETTO:+0000",
                        "DTSTART:19701025T020000",
                        "RRULE:FREQ=YEARLY;BYMONTH=10;BYDAY=-1SU",
                        "END:STANDARD",
                        "END:VTIMEZONE"
                       ]
      footer = unlines ["END:VCALENDAR"]
  writefileCRLF out (header ++ content ++ footer)
    where writefileCRLF fp txt = withFile fp WriteMode (\ h -> do hSetNewlineMode h (NewlineMode CRLF CRLF); hPutStr h txt)

          gatherData :: Talk -> (String, UTCTime, Maybe UTCTime, String, String, String)
          gatherData (Talk date speaker inst speakerurl insturl title abstract location material)
            = let desc = unlines ["Speaker: " ++ speaker ++ " " ++ bracket inst, "Title: " ++ title ++ "\n", abstract]
              in (desc, date, Nothing, location, title, "")
          gatherData (DepartmentalSeminar date speaker inst speakerurl insturl title abstract location)
            = let desc = unlines ["Speaker: " ++ speaker ++ " " ++ bracket inst, "Title: " ++ title ++ "\n", abstract]
              in (desc, date, Nothing, location, title, "Departmental seminar: ")
          gatherData (SpecialEvent date endDate title url location locationurl description)
            = (description, date, Just endDate, location, title, "Event: ")
          gatherData (BasicTalk date speaker inst speakerurl insturl title abstract location material)
            = gatherData (Talk date speaker inst speakerurl insturl ("MSP 101: " ++ title) abstract location material) -- for now

          escape :: String -> String
          escape [] = []
          escape ('\\':xs) = "\\\\" ++ escape xs
          escape ('\n':xs) = "\\n" ++ escape xs
          escape (';':' ':xs) = "\\; " ++ escape xs
          escape (',':' ':xs) = "\\, " ++ escape xs
          escape (x:xs) = x:escape xs
          processEntry now (i,x)
            = let (desc, date, endDate, location, title, kindEvent) = gatherData x
                  end = fromMaybe (addUTCTime (60*60::NominalDiffTime) date) endDate
                  t = escape . html2text
              in
                  unlines ["BEGIN:VEVENT",
                           "DTSTAMP;TZID=Europe/London:" ++ formatTime defaultTimeLocale "%Y%m%dT%H%M%S" now,
                           "DTSTART;TZID=Europe/London:" ++ formatTime defaultTimeLocale "%Y%m%dT%H%M%S" date,
                           "DTEND;TZID=Europe/London:" ++ formatTime defaultTimeLocale "%Y%m%dT%H%M%S" end,
                           wordwrap 73 $ "LOCATION:" ++ t location,
                           wordwrap 73 $ "SUMMARY:" ++ t (kindEvent ++ title),
                           wordwrap 73 $ "DESCRIPTION:" ++ t desc,
                           "UID:" ++ show i,
                           "END:VEVENT"]

generateHTML :: [(Int,Talk)]
            -> FilePath -- ^ Output path
            -> IO ()
generateHTML ts out = do
  now <- fmap zonedTimeToUTC getZonedTime --getCurrentTime
  let (previousTalks, upcomingTalks) = sortBy (flip $ comparing $ date . snd) *** sortBy (comparing $ date . snd) $ partition (\(i,x) -> date x < now) ts
  -- Some talk statistics for stdout
  when (not $ null upcomingTalks) $ putStrLn "\n==============\nUpcoming talks\n=============="
  mapM putStrLn (map (\ x -> show (date x) ++ ": " ++ (title x) ++ (bracket $ (case x of SpecialEvent{} -> "" ; _ -> speaker x))) $ map snd upcomingTalks)
  putStrLn $ "\n(" ++ (show $ length previousTalks) ++ " previous talks.)\n"

  let upcoming = if null upcomingTalks then "" else unlines ["<h2>Upcoming talks</h2>",
                                                             "<dl>", concatMap (processEntry True) upcomingTalks, "</dl>"]

      previous = if null previousTalks then "" else unlines ["<h2>List of previous talks</h2>",
                                                             "<dl>", concatMap (processEntry False) previousTalks, "</dl>"]


      formattedTime = let f s = formatTime defaultTimeLocale s usualTime in
                      f "%l" ++
                      (if todMin usualTime /= 0 then f ".%M" else "") ++
                      f "%P"
      header = unlines ["### default.html(section.msp101=current,headtags=<link rel='alternate' type='application/rss+xml' title='MSP101 seminars RSS feed' href='/msp101.rss'/>)",
                        "<!-- DO NOT EDIT THIS FILE DIRECTLY — EDIT _101.json AND RUN Generate101.hs INSTEAD -->",
                        "<h2>MSP101 Seminar</h2>",
                        "<p>MSP101 is an ongoing series of informal talks by visiting academics or members of the MSP group. The talks are usually " ++ usualDay ++ " " ++ formattedTime ++ " in room " ++ usualRoom ++ " in " ++ usualBuilding ++ ". They are announced on the <a href='http://lists.strath.ac.uk/mailman/listinfo/msp-interest'>msp-interest</a> mailing-list. The list of talks is also available as a <a type='application/rss+xml' href='/msp101.rss'><img src='/images/feed-icon-14x14.png' alt='feed icon'>RSS feed</a> and as a <a href='msp101.ics'>calendar file</a>. <b>The MSP 101 seminar is now hybrid, with both in-person attendance in room " ++ usualRoom ++ ", and online via Zoom (the link can be found on the msp-interest mailing list and the SPLS Zulipchat).</b></p>"]
  writeFile out (header ++ upcoming ++ previous)
    where
          entryBlock b i dt abst nMat mat = unlines $
                   [ "  <dt id='" ++ show i ++ "'>" ++ dt ++ "</dt>"
                   , "  <dd>"]
                   ++
                   (if null abst then [] else
                     [ "    <details" ++ (if b then " open" else "") ++ ">"
                     , "      <summary><b>Abstract</b></summary>"
                     , "      " ++ abst
                     , "    </details>"
                     ])
                   ++
                   (if null mat then [] else
                     (if null abst then id else (("" :) . ("" :)))
                     [ "    <details" ++ (if nMat <= 5 then " open" else "") ++ ">"
                     , "      <summary><b>Material</b></summary>"
                     , "      " ++ mat
                     , "    </details>"
                     ])
                   ++
                   [ "  </dd>"]

          processEntry b (i,(Talk date speaker inst speakerurl insturl title abstract location material))
            = let time = if utctDayTime date == timeOfDayToTime usualTime
                           then createLinkAnchor ('#':show i) (showGregorian $ utctDay date)
                           else let fmt = \ str -> formatTime defaultTimeLocale str date in
                                createLinkAnchor ('#':show i) (fmt "%F") ++ fmt ", %R"
                  place = if location == usualRoom then "" else (bracket location)
                  person = if null inst then (createLink speakerurl speaker)
                                        else (createLink speakerurl speaker) ++ ", " ++ (createLink insturl inst)
                  dt = time ++ place ++ ": " ++ title ++ (bracket person)
                  pMat (Link url desc) = createLink url desc
                  pMat (PDF file Nothing) = createLink ("101/slides/" ++ file) "Slides"
                  pMat (PDF file (Just com)) = (createLink ("101/slides/" ++ file) "Slides") ++ " " ++ (bracket com)
                  pMat (File file desc) = createLink ("101/files/" ++ file) desc
                  pMat (Whiteboard dir) = createLink ("101/wb/" ++ dir) "Whiteboard photos"
                  mat = if null material then ""
                          else
                           "<ul>" ++
                           (concatMap (\ x -> "<li>" ++ (pMat x) ++ "</li>")
                                      material) ++ "</ul>"
              in entryBlock b i dt (nl2br abstract) (length material) (nl2br mat)
          processEntry b (i,(DepartmentalSeminar date speaker inst speakerurl insturl title abstract location))
            = let fmt = \ str -> formatTime defaultTimeLocale str date
                  time = createLinkAnchor ('#':show i) (fmt "%Y-%m-%d") ++ fmt ", %H:%M"
                  place = bracket location
                  person = if null inst then (createLink speakerurl speaker)
                                        else (createLink speakerurl speaker) ++ ", " ++ (createLink insturl inst)
                  dt = time ++ " " ++ "Departmental seminar" ++ " " ++ place ++ ": " ++ title ++ (bracket person)
              in entryBlock b i dt (nl2br abstract) 0 ""
          processEntry b (i,(SpecialEvent date endDate title url location locationurl description))
            = let time = createLinkAnchor ('#':show i) (formatTime defaultTimeLocale "%Y-%m-%d" date)
                  dt = time ++ ": " ++ (createLink url title)
                                    ++ (bracket (createLink locationurl location))
              in entryBlock b i dt (nl2br description) 0 ""
          processEntry b (i,(BasicTalk date speaker inst speakerurl insturl title abstract location material))
            = processEntry b (i,(Talk date speaker inst speakerurl insturl ("MSP 101: " ++ title) abstract location material)) -- for now
