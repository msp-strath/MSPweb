module Html where

type HTML = String

tag :: String -> HTML -> HTML
tag t content = "<" ++ t ++ ">" ++ content ++ "</" ++ t ++ ">"

div :: String -> HTML -> HTML
div klass content =
  "<div class=\"" ++ klass ++ "\">" ++ content ++ "</div>"

span :: String -> HTML -> HTML
span klass content =
  "<span class=\"" ++ klass ++ "\">" ++ content ++ "</span>"


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

strong :: HTML -> HTML
strong = tag "strong"

ddt :: (HTML, HTML) -> HTML
ddt (term, desc) = "<dt>" ++ term ++ "</dt><dd>" ++ desc ++ "</dd>"

anchor :: String -> HTML -> HTML
anchor url content = "<a href=\"" ++ url ++ "\">" ++ content ++ "</a>"

img :: Maybe String -> String -> String -> HTML
img  style url alt = "<img src=\"" ++ url ++ "\"" ++ maybe "" h style ++ "alt=\"" ++ alt ++ "\">"
 where
  h :: String -> String
  h x = " style=\"" ++ x ++ "\" "

emailToHTML :: String -> HTML
emailToHTML emailAddr =
  anchor ("mailto:" ++ emailAddr) emailAddr
