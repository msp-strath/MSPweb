module Translate where

import Data.List (isSuffixOf)

import Text.Pandoc
import Data.Text (pack, unpack)

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
  fmap unpack $ handleError =<< runIO (writeHtml5String (def { writerHTMLMathMethod = MathML }) =<< (readTypst def $ pack s))
