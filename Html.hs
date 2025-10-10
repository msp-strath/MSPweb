module Html where

type HTML = String

tag :: String -> HTML -> HTML
tag t content = "<" ++ t ++ ">" ++ content ++ "</" ++ t ++ ">"

div :: String -> HTML -> HTML
div klass content =
  "<div class=\"" ++ klass ++ "\">" ++ content ++ "</div>"

hn :: Int -> HTML -> HTML
hn n = tag ("h" ++ show n)

h5 :: HTML -> HTML
h5 = hn 5

h3 :: HTML -> HTML
h3 = hn 3

h2 :: HTML -> HTML
h2 = hn 2

p :: HTML -> HTML
p = tag "p"

ulist :: [HTML] -> HTML
ulist items = "<ul>" ++ concatMap (\item -> "<li>" ++ item ++ "</li>") items ++ "</ul>"

item :: HTML -> HTML
item = tag "li"

dlist :: [(HTML,HTML)] -> HTML
dlist items
  = tag "dl"
  $ concatMap ddt items

ddt :: (HTML, HTML) -> HTML
ddt (term, desc)
  = concat [tag "dt" term, tag "dd" desc]

anchor :: String -> HTML -> HTML
anchor url content = "<a href=\"" ++ url ++ "\">" ++ content ++ "</a>"

img :: String -> HTML
img url = "<img src=\"" ++ url ++ "\">"

emailToHTML :: String -> HTML
emailToHTML emailAddr =
  anchor ("mailto:" ++ emailAddr) ("Email: " ++ emailAddr)

details :: HTML -> HTML -> HTML
details title content
  = tag "details"
      $ concat
      [ tag "summary" title
      , content
      ]
