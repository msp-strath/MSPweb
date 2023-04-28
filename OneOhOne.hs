{-# LANGUAGE DeriveGeneric #-}
module OneOhOne where

import Data.Time

import Data.List
import Data.Ord
import Data.Foldable
import Control.Arrow
import Control.Monad

import Data.Array((!))
import Text.Regex.PCRE

import GHC.Generics
import Data.Aeson

-- constants
usualTime :: TimeOfDay
usualTime = TimeOfDay 15 0 0

usualDay :: String
usualDay = "Friday"

-- HTML utils

nl2br :: String -> String
nl2br [] = []
nl2br ('\n':xs) = "<br/>\n" ++ nl2br xs
nl2br (x:xs) = x:(nl2br xs)

createLink :: String -> String -> String
createLink [] name = name
createLink ref name = "<a href='" ++ ref ++ "'>" ++ name ++ "</a>"

bracket :: String -> String
bracket str = if null str then "" else " (" ++ str ++ ")"

wordwrap :: Int -> String -> String -> String
wordwrap maxlen div = (wrap_ 0) . words where
        wrap_ _ [] = ""
        wrap_ pos (w:ws)
                -- at line start: put down the word no matter what
                | pos == 0 = w ++ wrap_ (pos + lw) ws
                | pos + lw + 1 > maxlen = div ++ wrap_ 0 (w:ws)
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
        bre = makeRegex "\\\\(\\\\|[0-9]+)" :: Regex
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
                     ("<br/>|<br>", "\n"),                 -- line breaks
                     ("<p>(.*?)</p>", "\\1\n\n"),          -- paragraph breaks
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
  let content = concatMap processEntry ts
      header = unlines ["<?xml version='1.0' encoding='ISO-8859-1'?>",
                        "<rss version='2.0' xmlns:atom='http://www.w3.org/2005/Atom'>",
                        " <channel>",
                        "  <atom:link href='http://msp.cis.strath.ac.uk/msp101.rss' rel='self' type='application/rss+xml' />",
                        "  <title>MSP101</title>",
                        "  <link>http://msp.cis.strath.ac.uk/msp101.html</link>",
                        "  <description>MSP101 is an ongoing series of informal talks by visiting academics or members of the MSP group.</description>",
                        "  <language>en-gb</language>"]
      footer = unlines [" </channel>", "</rss>"]
  writeFile out (header ++ content ++ footer)
    where processEntry (i,(Talk date speaker inst speakerurl insturl title abstract location material))
            = let rsstitle = (showGregorian $ utctDay date) ++ ": " ++ speaker ++ bracket inst
                  abstr = if (null abstract) then "" else "<p><b>Abstract</b><br/><br/>" ++  (nl2br abstract) ++ "</p>"
                  desc = unlines ["<h2>" ++ (createLink speakerurl speaker) ++ (bracket (createLink insturl inst)) ++ "</h2>",
                                  "<h2>" ++ title ++ "</h2>",
                                  abstr,
                                  "<b>" ++ (show date) ++ "<br/>" ++ location ++ "</b><br/>"]
              in
               unlines ["  <item>",
                        "   <title>" ++ rsstitle ++ "</title>",
                        "   <description><![CDATA[" ++ desc ++ "]]></description>",
                        "   <guid isPermaLink='true'>http://msp.cis.strath.ac.uk/msp101.html#" ++ (show i) ++ "</guid>",
                        "  </item>"]
          processEntry (i,(DepartmentalSeminar date speaker inst speakerurl insturl title abstract location))
            = let rsstitle = (showGregorian $ utctDay date) ++ " Departmental seminar " ++ ": " ++ speaker ++ bracket inst
                  abstr = if (null abstract) then "" else "<p><b>Abstract</b><br/><br/>" ++  (nl2br abstract) ++ "</p>"
                  desc = unlines ["<h2>" ++ (createLink speakerurl speaker) ++ (bracket (createLink insturl inst)) ++ "</h2>",
                                  "<h2>" ++ title ++ "</h2>",
                                  abstr,
                                  "<b>" ++ (show date) ++ "<br/>" ++ location ++ "</b><br/>"]
              in
               unlines ["  <item>",
                        "   <title>" ++ rsstitle ++ "</title>",
                        "   <description><![CDATA[" ++ desc ++ "]]></description>",
                        "   <guid isPermaLink='true'>http://msp.cis.strath.ac.uk/msp101.html#" ++ (show i) ++ "</guid>",
                        "  </item>"]
          processEntry (i,(SpecialEvent date title url location locationurl description))
            = let rsstitle = (showGregorian $ utctDay date) ++ ": " ++ title
                  abstr = if (null description) then "" else "<p>" ++  (nl2br description) ++ "</p>"
                  desc = unlines ["<h2>" ++ (createLink url title) ++ (bracket location) ++ "</h2>",
                                  "<h2>" ++ title ++ "</h2>",
                                  abstr,
                                  "<b>" ++ (show date) ++ "<br/>" ++ (createLink locationurl location) ++ "</b><br/>"]
              in
               unlines ["  <item>",
                        "   <title>" ++ rsstitle ++ "</title>",
                        "   <description><![CDATA[" ++ desc ++ "]]></description>",
                        "   <guid isPermaLink='true'>http://msp.cis.strath.ac.uk/msp101.html#" ++ (show i) ++ "</guid>",
                        "  </item>"]
          processEntry (i,(BasicTalk date speaker inst speakerurl insturl title abstract location material)) = processEntry (i, (Talk date speaker inst speakerurl insturl ("MSP 101: " ++ title) abstract location material)) -- for now


generateICS :: [(Int,Talk)]
            -> FilePath -- ^ Output path
            -> IO ()
generateICS ts out = do
  now <- getZonedTime
  let content = concatMap (processEntry now) ts
      header = unlines ["BEGIN:VCALENDAR", "VERSION:2.0", "PRODID:-//MSP//MSP101 v1.0//EN",
                        "X-WR-CALNAME: MSP101",
                        "X-WR-CALDESC: MSP101 seminar series"]
      footer = unlines ["END:VCALENDAR"]
  writeFile out (header ++ content ++ footer)
    where gatherData (Talk date speaker inst speakerurl insturl title abstract location material)
            = let desc = unlines ["Speaker: " ++ speaker ++ " " ++ (bracket inst), "Title: " ++ title ++ "\n", abstract]
                  end = addUTCTime (60*60::NominalDiffTime) date
              in (desc, end, date, location, title, "")
          gatherData (DepartmentalSeminar date speaker inst speakerurl insturl title abstract location)
            = let desc = unlines ["Speaker: " ++ speaker ++ " " ++ (bracket inst), "Title: " ++ title ++ "\n", abstract]
                  end = addUTCTime (60*60::NominalDiffTime) date
              in (desc, end, date, location, title, "Departmental seminar: ")
          gatherData (SpecialEvent date title url location locationurl description)
            = let end = addUTCTime (60*60::NominalDiffTime) date
              in (description, end, date, location, title, "Event: ")
          gatherData (BasicTalk date speaker inst speakerurl insturl title abstract location material) = gatherData (Talk date speaker inst speakerurl insturl ("MSP 101: " ++ title) abstract location material) -- for now
          escape :: String -> String
          escape [] = []
          escape ('\\':xs) = "\\\\" ++ (escape xs)
          escape ('\n':xs) = "\\n" ++ (escape xs)
          escape (';':' ':xs) = "\\; " ++ (escape xs)
          escape (',':' ':xs) = "\\, " ++ (escape xs)
          escape (x:xs) = x:(escape xs)
          processEntry now (i,x)
            = let (desc, end, date, location, title, kindEvent) = gatherData x
                  t = escape . html2text
              in
                  unlines ["BEGIN:VEVENT",
                           "DTSTAMP;TZID=Europe/London:" ++ (formatTime defaultTimeLocale "%Y%m%dT%H%M%S" now),
                           "DTSTART;TZID=Europe/London:" ++ (formatTime defaultTimeLocale "%Y%m%dT%H%M%S" date),
                           "DTEND;TZID=Europe/London:" ++ (formatTime defaultTimeLocale "%Y%m%dT%H%M%S" $ end),
                           "LOCATION:" ++ (t location),
                           wordwrap 73 "\n  " $ "SUMMARY:" ++ (t $ kindEvent ++ title),
                           wordwrap 73 "\n  " $ "DESCRIPTION:" ++ (t $ desc),
                           "UID:" ++ (show i),
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
                                                             "<dl>", concatMap processEntry upcomingTalks, "</dl>"]

      previous = if null previousTalks then "" else unlines ["<h2>List of previous talks</h2>",
                                                             "<dl>", concatMap processEntry previousTalks, "</dl>"]


      formattedTime = let f s = formatTime defaultTimeLocale s usualTime in
                      f "%l" ++
                      (if todMin usualTime /= 0 then f ".%M" else "") ++
                      f "%P"
      header = unlines ["### default.html(section.msp101=current,headtags=<link rel='alternate' type='application/rss+xml' title='MSP101 seminars RSS feed' href='/msp101.rss'/>)",
                        "<!-- DO NOT EDIT THIS FILE DIRECTLY — EDIT _101.json AND RUN Generate101.hs INSTEAD -->",
                        "<h2>MSP101</h2>",
                        "<p>MSP101 is an ongoing series of informal talks by visiting academics or members of the MSP group. The talks are usually " ++ usualDay ++ " " ++ formattedTime ++ " in room LT711 in Livingstone Tower. They are announced on the <a href='https://lists.cis.strath.ac.uk/mailman/listinfo/msp-interest'>msp-interest</a> mailing-list. The list of talks is also available as a <a type='application/rss+xml' href='/msp101.rss'><img src='/images/feed-icon-14x14.png' alt='feed icon'>RSS feed</a> and as a <a href='msp101.ics'>calendar file</a>. <b>The MSP 101 seminar is now hybrid, with both in-person attendance in room LT711, and online via Zoom (the link can be found on the msp-interest mailing list and the SPLS Zulipchat).</b></p>"]
  writeFile out (header ++ upcoming ++ previous)
    where processEntry (i,(Talk date speaker inst speakerurl insturl title abstract location material))
            = let time = if utctDayTime date == timeOfDayToTime usualTime then (showGregorian $ utctDay date) else (formatTime defaultTimeLocale "%F, %R" date)
                  place = if location == "LT711" then "" else (bracket location)
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
                           (if null abstract then "" else "\n\n") ++
                           "<b>Material</b><ul>" ++
                           (concatMap (\ x -> "<li>" ++ (pMat x) ++ "</li>")
                                      material) ++ "</ul>"
              in
                 unlines ["  <dt id='" ++ (show i) ++ "'>" ++ dt ++ "</dt>",
                          "    <dd>" ++ (nl2br abstract)
                                     ++ (nl2br mat) ++ "</dd>"]
          processEntry (i,(DepartmentalSeminar date speaker inst speakerurl insturl title abstract location))
            = let time = formatTime defaultTimeLocale "%Y-%m-%d, %H:%M" date
                  place = bracket location
                  person = if null inst then (createLink speakerurl speaker)
                                        else (createLink speakerurl speaker) ++ ", " ++ (createLink insturl inst)
                  dt = time ++ " " ++ "Departmental seminar" ++ " " ++ place ++ ": " ++ title ++ (bracket person)
              in
                 unlines ["  <dt id='" ++ (show i) ++ "'>" ++ dt ++ "</dt>",
                          "    <dd>" ++ (nl2br abstract) ++ "</dd>"]
          processEntry (i,(SpecialEvent date title url location locationurl description))
            = let time = formatTime defaultTimeLocale "%Y-%m-%d" date
                  dt = time ++ ": " ++ (createLink url title)
                                    ++ (bracket (createLink locationurl location))
              in
                 unlines ["  <dt id='" ++ (show i) ++ "'>" ++ dt ++ "</dt>",
                          "    <dd>" ++ (nl2br description) ++ "</dd>"]
          processEntry (i,(BasicTalk date speaker inst speakerurl insturl title abstract location material)) = processEntry (i,(Talk date speaker inst speakerurl insturl ("MSP 101: " ++ title) abstract location material)) -- for now
