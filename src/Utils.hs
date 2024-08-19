module Utils where

-- TODO: use http://hackage.haskell.org/package/managed instead of turtle

-- TODO
-- dont use system-filepath (Filesystem.Path module, good lib, turtle is using it,         FilePath is just record)
-- dont use filepath        (System.FilePath module, bad lib,  directory-tree is using it, FilePath is just String)
-- use https://hackage.haskell.org/package/path-io-1.6.0/docs/Path-IO.html walkDirAccumRel

-- TODO
-- use https://hackage.haskell.org/package/recursion-schemes

-- TODO
-- use https://github.com/luke-clifton/shh

-- import qualified Filesystem.Path.CurrentOS
import Options.Applicative
import Options.Applicative.NonEmpty ()
import "protolude" Protolude hiding (find)
import qualified "turtle" Turtle
import "turtle" Turtle ((</>))
import qualified "directory" System.Directory
import qualified "filepath" System.FilePath
-- import qualified "system-filepath" Filesystem.Path
import "base" Data.String (String)
import qualified "base" Data.String as String
import qualified "base" Data.List as List
import qualified Data.List.Index as List
import qualified "text" Data.Text as Text
import qualified Data.Text.IO
import qualified Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import qualified "directory-tree" System.Directory.Tree
import "directory-tree" System.Directory.Tree (DirTree (..), AnchoredDirTree (..))
import qualified "cases" Cases
import "non-empty-text" Data.NonEmptyText (NonEmptyText)
import qualified "non-empty-text" Data.NonEmptyText as NonEmptyText
import System.IO (stdout, stderr)

import Control.Concurrent.Async
import Control.Monad.Writer
import ModuleName
import UpdateModuleName

filterDirTreeByFilename :: (String -> Bool) -> DirTree a -> Bool
filterDirTreeByFilename _ (Dir ('.':_) _) = False
filterDirTreeByFilename pred (File n _) = pred n
filterDirTreeByFilename _ _ = True

dirTreeContent :: DirTree a -> IO [a]
dirTreeContent (Failed name err) = Turtle.die $ "Dir tree error: filename " <> show name <> ", error " <> show err
dirTreeContent (File fileName a) = pure [a]
dirTreeContent (Dir dirName contents) = do
  output :: [[a]] <- traverse dirTreeContent contents
  pure $ join output

anyCaseToCamelCase :: Text -> Text
anyCaseToCamelCase = Cases.process Cases.title Cases.camel -- first letter is always upper

-- Utility function to ensure a path ends with a slash
-- ensureTrailingSlash :: Text -> Text
-- ensureTrailingSlash p = if Text.last p == '/' then p else p <> "/"
ensureTrailingSlash :: Turtle.FilePath -> Turtle.FilePath
ensureTrailingSlash p = if not (null p) && List.last p == '/' then p else p ++ "/"

stringToDir :: Turtle.FilePath -> Turtle.FilePath
stringToDir = toS . ensureTrailingSlash

-- Convert a directory path to include a trailing slash
parseDir :: ReadM Turtle.FilePath
parseDir = fmap (toS . stringToDir) str

-- | Splits the given Text into a tuple based on the first occurrence of the delimiter.
--   The result is a tuple where the first element is the part before the delimiter,
--   and the second element is the part after the delimiter.
splitOn :: Text -> Text -> (Text, Text)
splitOn delimiter text =
  let (before, rest) = Text.breakOn delimiter text
   in case Text.stripPrefix delimiter rest of
        Just after -> (before, after)
        Nothing -> (before, rest) -- Shouldn't happen because breakOn guarantees the delimiter is there if `rest` is non-empty.

-- Example:
-- baseDir - /home/srghma/projects/purescript-halogen-nextjs/app/
-- filePath - /home/srghma/projects/purescript-halogen-nextjs/app/Nextjs/Pages/Buttons.purs
-- output - ["Nextjs","Pages","Buttons"]
--
-- Example 2:
-- baseDir - /home/srghma/projects/purescript-halogen-nextjs/app/
-- filePath - /home/srghma/projects/purescript-halogen-nextjs/app/Nextjs/Pages/Buttons/CSS.module.css
-- output - ["Nextjs","Pages","Buttons","CSS"]
fullPathToModuleName :: Turtle.FilePath -> Turtle.FilePath -> IO ModuleName
fullPathToModuleName baseDir fullPath = do
  -- Strip the base directory from the full path
  fullPathWithoutBase :: Turtle.FilePath <-
    maybe
    (die $ "Cannot strip baseDir " <> show baseDir <> " from path " <> show fullPath)
    pure
    $ Turtle.stripPrefix baseDir fullPath
  -- Split the remaining path into directories and remove extensions
  let parts :: [Text] = fmap (toS . stripSuffix "/")
                                  . Turtle.splitDirectories
                                  . System.FilePath.dropExtensions
                                  $ fullPathWithoutBase

  case toModuleName_arrayOfString parts of
    Left e -> Turtle.die . Text.intercalate " " $
      ["In directory"
      , toS baseDir
      , "cannot convert path"
      , toS fullPath
      , "to module name. Error: "
      , moduleNameError_print e]
    Right x -> pure x

appendIfNotAlreadySuffix :: Eq a => [a] -> [a] -> [a]
appendIfNotAlreadySuffix suffix target =
  if suffix `List.isSuffixOf` target
     then target
     else target ++ suffix

stripSuffix :: Eq a => [a] -> [a] -> [a]
stripSuffix suffix target =
  if suffix `List.isSuffixOf` target
     then List.reverse . List.drop (List.length suffix) $ List.reverse target
     else target

isPursFile :: FilePath -> Bool
isPursFile n = System.FilePath.takeExtensions n == ".purs"

isCssModuleFile n
  = System.FilePath.takeExtensions n == ".module.css"
  || System.FilePath.takeExtensions n == ".module.scss"

findFilesWith :: (Turtle.FilePath -> Bool) -> Turtle.FilePath -> IO [Turtle.FilePath]
findFilesWith f baseDir = do
  -- contains absolute path inside
  _base :/ (dirTree :: DirTree FilePath) <- System.Directory.Tree.readDirectoryWith return baseDir

  let (dirTreeWithPursFiles :: DirTree FilePath) =
        System.Directory.Tree.filterDir
          (filterDirTreeByFilename f)
          dirTree

  filePaths :: [Turtle.FilePath] <- dirTreeContent dirTreeWithPursFiles

  pure filePaths

ensureDirsExist :: NonEmpty Turtle.FilePath -> IO ()
ensureDirsExist filepaths = do
  -- Check if directories exist
  results <- forM filepaths $ \path -> do
    exists <- System.Directory.doesDirectoryExist path
    return (path, exists)
  -- Filter out non-existing directories
  let nonExistingDirs = nonEmpty . mapMaybe (\(path, exists) -> if not exists then Just path else Nothing) $ toList results
  -- If there are non-existing directories, terminate with an error message
  case nonExistingDirs of
    Nothing -> pure ()
    Just nonExistingDirs' -> die $ "The following directories do not exist:\n" <> Text.unlines (toList $ map toS nonExistingDirs')


wrapInQuotes :: Text.Text -> Text.Text
wrapInQuotes t = "\"" <> t <> "\""
