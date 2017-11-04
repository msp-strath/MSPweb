module Main where

import Data.Functor ((<$>))
import Data.List (isSuffixOf, isPrefixOf)
import Data.Maybe (catMaybes)
import Data.Traversable (traverse)
import Control.Monad (mapM_)
import System.FilePath (combine, takeFileName, takeExtension, dropExtension)
import System.Directory ( getDirectoryContents, doesDirectoryExist
                        , doesFileExist, getCurrentDirectory
                        , createDirectoryIfMissing, copyFile )

import Text.Pandoc

{-
-- Cheapskate
import Cheapskate
import Text.Blaze.Html
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Data.Text.Lazy as TL (toStrict)
import Data.Text (pack, unpack)

-- CMark
-- import Data.Text (pack, unpack)
-- import CMark (commonmarkToHtml, optSmart)
-}

{----------------------------------------------------------------------------}
(</>) :: FilePath -> FilePath -> FilePath
(</>) = combine

{----------------------------------------------------------------------------}
-- | Rose trees for representing files and directories. Each of the
-- names in the tree are relative to the entries above them.
data FileTree
    = File FilePath
    -- ^ The name of a file
    | Dir  FilePath [FileTree]
    -- ^ A reference to a directory with its entries
    deriving Show

-- | Classification of the objects in the filesystem.
data EntryClass
    = IsFile
    | IsDirectory
    | Unknown
    deriving Show

translateMarkdown :: String -> String
translateMarkdown =  writeHtmlString def . handleError . readMarkdown def
--translateMarkdown = unpack . TL.toStrict . renderHtml . toHtml . markdown def . pack -- cheapskate
--translateMarkdown = unpack . commonmarkToHtml [optSmart] . pack -- CMark


-- | Determine whether a path refers to a file, a directory, or
-- unknown.
classifyPath :: FilePath -> IO EntryClass
classifyPath path = do
  isFile <- doesFileExist path
  isDirectory <- doesDirectoryExist path
  return (if isFile then IsFile
          else if isDirectory then IsDirectory
               else Unknown)

-- | Determine when an entry in a directory is to be used as a source
-- file for generating the web site. Anything that starts with an
-- underscore ("_") or a dot ("."), is sandwiched between "#", or ends
-- with "~" or ".hs" (unless a 101 attachment) is not considered
-- relevant.
relevantEntry :: FilePath -- ^ current base directory
              -> FilePath -- ^ input root
              -> FilePath -- ^ current file
              -> Bool
relevantEntry _ _ "."     = False
relevantEntry _ _ ".."    = False
relevantEntry _ _ ('_':_) = False
relevantEntry _ _ ('.':_) = False
relevantEntry _ _ s | "#" `isPrefixOf` s && "#" `isSuffixOf` s  = False
relevantEntry _ _ s | "~"   `isSuffixOf` s = False
-- Keep all attached non-junk 101 files
relevantEntry base root _ | base == root </> "101" </> "files" = True
-- these should be below the 101 check!
relevantEntry _ _         "dist"      = False
relevantEntry _ _         "Makefile"  = False
relevantEntry _ _         "README.md" = False
relevantEntry _ _ s | ".cabal" `isSuffixOf` s = False
relevantEntry _ _ s | ".hs" `isSuffixOf` s = False
relevantEntry _ _ _ = True

-- | Recursively scan the given directory to gather all the relevant
-- entries in a directory. Relevant entries are decided by the
-- 'relevantEntry' function above.
scanDirectory :: FilePath -- ^ current base directory
              -> FilePath -- ^ input root
              -> FilePath -- ^ current path
              -> IO [FileTree]
scanDirectory base inputRoot path = do
  all <- getDirectoryContents path
  l <- filter (relevantEntry base inputRoot) <$> getDirectoryContents path
  catMaybes <$> traverse (scanEntry (base </> path) inputRoot . (path </>)) l

-- | Examine a pathname and return the appropriate kind of
-- 'FileTree'. 'Nothing' is returned if 'classifyPath' is unable to
-- determine whether the pathname represents a file or a directory.
scanEntry :: FilePath -- ^ current base directory
          -> FilePath -- ^ input root
          -> FilePath -- ^ current path
          -> IO (Maybe FileTree)
scanEntry base inputRoot path = do
  classification <- classifyPath path
  case classification of
    IsFile ->
        return (Just (File (takeFileName path)))
    IsDirectory ->
        Just <$> (Dir (takeFileName path) <$> scanDirectory base inputRoot path)
    Unknown ->
        return Nothing

{----------------------------------------------------------------------------}
-- ^ Replace all occurences of the strings "{{varname}}" in the first
-- argument with the string associated with "varname" in the second
-- argument. If there is no string associated with "varname", then
-- "{{varname}}" is replaced by the empty string.
substitute :: String
           -> [(String,String)]
           -> String
substitute template env = loop template []
    where
      loop []           acc = reverse acc
      loop ('{':'{':cs) acc = getVar cs [] acc
      loop (c:cs)       acc = loop cs (c:acc)

      getVar []           varAcc acc = reverse (varAcc ++ "{{" ++ acc)
      getVar ('}':'}':cs) varAcc acc =
          let varNm = reverse varAcc in
          case lookup varNm env of
            Nothing  -> loop cs acc
            Just val -> loop cs (reverse val ++ acc)
      getVar (c:cs) varAcc acc = getVar cs (c:varAcc) acc

-- | Check the input string to determine if it starts with a header of
-- the form "### <template-name> (var1=value1,var2=value2)\n". If it
-- does, then @Left (<template-name>, [(var1,value1),(var2,value2)],
-- rest-of-string)@ is returned. Otherwise, @Right input@ is returned,
-- where @input@ is the original input.
parseHeader :: String
            -> Either (String, [(String,String)], String) String
parseHeader file =
    case break (=='\n') file of
      ('#':'#':'#':header, rest) ->
          let (template, header') =
                  break (\c -> c==' ' || c=='(') (dropWhile (==' ') header)
              getEnv []      env = reverse env
              getEnv (')':_) env = reverse env
              getEnv cs      env =
                  let (x,y) = break (\c -> c==',' || c==')') cs
                      (v,a) = break (=='=') x
                  in getEnv (drop 1 y) ((v,drop 1 a):env)
              env = getEnv (dropWhile (\c -> c==' ' || c=='(') header') []
              body = dropWhile (\c -> c==' ' || c=='\n' || c=='\t') rest
          in
          Left (template, env, body)
      _ ->
          Right file

{----------------------------------------------------------------------------}
-- | Scan all the entries in the given list of file trees. For
-- directories, the corresponding directory is created in the output
-- root. For files whose names do not end in '.html', the file is
-- copied over to the output tree. For files whose names end in
-- '.html', the file is checked for a header (using 'parseHeader'). If
-- a header is found, then the named template file is used to generate
-- the output, otherwise the file is copied to the output.
generateFiles :: FilePath -- ^ Input root
              -> FilePath -- ^ Output root
              -> [FileTree] -- ^ Entries to process
              -> IO ()
generateFiles inputRoot outputRoot tree = do
  createDirectoryIfMissing True outputRoot
  mapM_ (processTree "" "") tree
    where
      processTree rootPath path (File name) =
        let inputPath  = inputRoot </> path </> name
            outputExt = case takeExtension name of
                          ".md" -> ".html"
                          _     -> takeExtension name
            outputPath = outputRoot </> path </> (dropExtension name) ++ outputExt
            knownSuffixes = [".html", ".md"] in
        if any (\ x -> x `isSuffixOf` name) knownSuffixes then do
          result <- parseHeader <$> readFile inputPath
          let translate = if  ".md" `isSuffixOf` name then translateMarkdown else id
          body <- case result of
                Left (template, baseEnv, body) -> do
                  let templatePath = inputRoot </> "_templates" </> template
                  templateBody <- readFile templatePath
                  let env = ("rootPath",rootPath):("content", translate body):baseEnv
                  return (substitute templateBody env)
                Right body ->
                  return $ translate body
          writeFile outputPath $ body
        else
            copyFile inputPath outputPath

      processTree rootPath path (Dir name entries) = do
        let outputPath = outputRoot </> path </> name
        createDirectoryIfMissing False outputPath
        mapM_ (processTree (rootPath </> "../") (path </> name)) entries

-- | Main function. Scan the current directory for relevant input
-- files, and generate the corresponding output in '_build/'.
main :: IO ()
main = do
  inputRoot <- getCurrentDirectory
  let outputRoot = inputRoot </> "_build"
  inputFiles <- scanDirectory inputRoot inputRoot inputRoot
  generateFiles inputRoot outputRoot inputFiles
