module Markdown where

import Text.Pandoc
import Data.Text (pack, unpack)

translateMarkdown :: String -> IO String
translateMarkdown s =
  fmap unpack $ handleError =<< runIO (writeHtml5String def  =<< (readMarkdown (def { readerExtensions = phpMarkdownExtraExtensions }) $ pack s))
