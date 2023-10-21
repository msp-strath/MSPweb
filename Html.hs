module Html where

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

emailToHTML :: String -> HTML
emailToHTML emailAddr =
  anchor ("mailto:" ++ emailAddr) ("Email: " ++ emailAddr)
