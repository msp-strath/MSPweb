module Html where

type HTML = String

(<++>) :: HTML -> HTML -> HTML
x <++> y = x ++ " " ++ y

tag :: String -> HTML -> HTML
tag t content = "<" ++ t ++ ">" ++ content ++ "</" ++ t ++ ">"

div :: String -> HTML -> HTML
div klass content =
  "<div" ++ kl klass ++ ">" ++ content ++ "</div>"
  where
    kl "" = ""
    kl x = " class=\"" ++ klass ++ "\""

span :: String -> HTML -> HTML
span klass content =
  "<span" ++ kl klass ++ ">" ++ content ++ "</span>"
  where
    kl "" = ""
    kl x = " class=\"" ++ klass ++ "\""


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

italic :: HTML -> HTML
italic = tag "i"

bold :: HTML -> HTML
bold = tag "strong"

ulist :: [HTML] -> HTML
ulist items = "<ul>" ++ concatMap (\item -> "<li>" ++ item ++ "</li>") items ++ "</ul>"

strong :: HTML -> HTML
strong = tag "strong"

ddt :: (HTML, HTML) -> HTML
ddt (term, desc) = "<dt>" ++ term ++ "</dt><dd>" ++ desc ++ "</dd>"

ddtWithID :: Int -> HTML -> HTML -> HTML
ddtWithID i term desc = "<dt id='" ++ show i ++ "'>" ++ term ++ "</dt><dd>" ++ desc ++ "</dd>"

dl :: HTML -> HTML
dl = tag "dl"

anchor :: String -> HTML -> HTML
anchor url content = "<a href=\"" ++ url ++ "\">" ++ content ++ "</a>"

link :: String -> HTML
link url = anchor url url

img :: Maybe String -> String -> String -> HTML
img  style alt url = "<img src=\"" ++ url ++ "\"" ++ maybe "" h style ++ "alt=\"" ++ alt ++ "\">"
 where
  h :: String -> String
  h x = " style=\"" ++ x ++ "\" "

emailToHTML :: String -> HTML
emailToHTML emailAddr =
  anchor ("mailto:" ++ emailAddr) emailAddr

details :: Bool -> HTML -> HTML -> HTML
details isOpen summary body = unlines [
   "<details" ++ (if isOpen then " open" else "") ++ ">",
   "<summary>" ++ summary ++ "</summary>",
   body,
   "</details>"
  ]

-- HTML utils

nl2br :: String -> String
nl2br [] = []
nl2br ('\n':xs) = "<br>\n" ++ nl2br xs
nl2br (x:xs) = x:(nl2br xs)

createLink :: String -> String -> String
createLink "" name = name
createLink ref name = anchor ref name

createLinkAnchor :: String -> String -> String
createLinkAnchor "" name = name
createLinkAnchor ref name = "<a href='" ++ ref ++ "' class='hoverlink'>" ++ name ++ "</a>"

bracket :: String -> String
bracket str = if null str then "" else " (" ++ str ++ ")"
