{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}

module Main where

-- TODO: use http://hackage.haskell.org/package/managed instead of turtle

-- TODO
-- dont use system-filepath (Filesystem.Path module, good lib, turtle is using it,         FilePath is just record)
-- dont use filepath        (System.FilePath module, bad lib,  directory-tree is using it, FilePath is just String)
-- use https://hackage.haskell.org/package/path-io-1.6.0/docs/Path-IO.html walkDirAccumRel

-- TODO
-- use https://hackage.haskell.org/package/recursion-schemes

-- import qualified Filesystem.Path.CurrentOS
import Options.Applicative
import Options.Applicative.NonEmpty
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
import qualified Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import qualified "directory-tree" System.Directory.Tree
import "directory-tree" System.Directory.Tree (DirTree (..), AnchoredDirTree (..))
import qualified "cases" Cases
import Control.Concurrent.Async
import UpdateModuleName
import Control.Monad.Writer

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

data AppOptions = AppOptions
  { appOptions_projectRoots :: [Turtle.FilePath] -- to [(Nothing, filepath ++ "/src", true), [(Just testPathToModule, filepath ++ "/test", false)]]
  , appOptions_srcDirs      :: [Turtle.FilePath] -- to (Nothing, filepath, true)
  , appOptions_testDirs     :: [Turtle.FilePath] -- to (Just testPathToModule, filepath, true)
  , appOptions_customDirs   :: [(PathToModule, Turtle.FilePath)] -- to (Just pathToModule, filepath, true)
  }

data DirectoryWithPursFilesConfig = DirectoryWithPursFilesConfig
  { directoryWithPursFilesConfig_prependPathToModule :: Maybe PathToModule
  , directoryWithPursFilesConfig_pathToDirectory     :: Turtle.FilePath
  , directoryWithPursFilesConfig_required            :: Bool
  }

appOptionsToDirectoryWithPursFilesConfig :: AppOptions -> Maybe (NonEmpty DirectoryWithPursFilesConfig)
appOptionsToDirectoryWithPursFilesConfig opts =
  let
    -- Helper function to create configs for source and test directories
    createConfig :: Maybe PathToModule -> Turtle.FilePath -> Bool -> DirectoryWithPursFilesConfig
    createConfig maybeModulePath path required =
      DirectoryWithPursFilesConfig
        { directoryWithPursFilesConfig_prependPathToModule = maybeModulePath
        , directoryWithPursFilesConfig_pathToDirectory = path
        , directoryWithPursFilesConfig_required = required
        }
    -- Create configs from project roots
    rootConfigs = concatMap (\rootPath ->
      [ createConfig Nothing (rootPath </> "src") True
      , createConfig (Just testPathToModule) (rootPath </> "test") False
      ]) (appOptions_projectRoots opts)
    -- Create configs from source directories
    srcConfigs = map (\srcDir -> createConfig Nothing srcDir True) (appOptions_srcDirs opts)
    -- Create configs from test directories
    testConfigs = map (\testDir -> createConfig (Just testPathToModule) testDir True) (appOptions_testDirs opts)
    -- Create configs from custom directories
    customConfigs = map (\(pathToModule, dir) -> createConfig (Just pathToModule) dir True) (appOptions_customDirs opts)
    allConfigs = rootConfigs ++ srcConfigs ++ testConfigs ++ customConfigs
  in NonEmpty.nonEmpty allConfigs

-- Convert a directory path to include a trailing slash
parseDir :: ReadM Turtle.FilePath
parseDir = fmap (toS . ensureTrailingSlash) str
  where
    -- Utility function to ensure a path ends with a slash
    ensureTrailingSlash :: Text -> Text
    ensureTrailingSlash p = if Text.last p == '/' then p else p <> "/"

-- `--root ./myproj` SHOULD EQUAL TO `--src ./myproj/src --test ./myproj/test` SHOULD EQUAL TO `--src ./myproj/src --custom Test ./myproj/test`
appOptionsParser :: Parser AppOptions
appOptionsParser = AppOptions
  <$> many (option parseDir (long "root" <> short 'r' <> metavar "DIRECTORY" <> help "Base dir with two directories - src/ and test/, containg .purs files. Can pass multiple -r" ))
  <*> many (option parseDir (long "src" <> short 's' <> metavar "DIRECTORY" <> help "Source directory."))
  <*> many (option parseDir (long "test" <> short 't' <> metavar "DIRECTORY" <> help "Test directory."))
  <*> many customDirOption
  where
    customDirOption :: Parser (PathToModule, Turtle.FilePath)
    customDirOption = (,) <$> (argument parsePathToModule (metavar "MODULE" <> help "'Foo.Bar'")) <*> argument parseDir (metavar "DIRECTORY" <> help "Custom module directory.")


appOptionsParserInfo :: ParserInfo AppOptions
appOptionsParserInfo = info (appOptionsParser <**> helper)
  ( fullDesc
  <> progDesc "Adds or updates `module Foo.Bar` based on path. IF .purs file is in test/ THEN `module Test.Foo.Bar` IFELSE "
  <> header "Update module name in directory recursively" )

-- Example:
-- baseDir - /home/srghma/projects/purescript-halogen-nextjs/app/
-- filePath - /home/srghma/projects/purescript-halogen-nextjs/app/Nextjs/Pages/Buttons/purs.purs
-- output - ["Nextjs","Pages","Buttons","purs"]

-- | Converts a full file path to a module path by stripping the base directory
-- | and transforming the remaining path into a list of module segments.
fullPathToPathToModule :: Turtle.FilePath -> Turtle.FilePath -> IO PathToModule
fullPathToPathToModule baseDir fullPath = do
  -- Strip the base directory from the full path
  fullPathWithoutBase :: Turtle.FilePath <- maybe (die $ "Cannot strip baseDir " <> show baseDir <> " from path " <> show fullPath) pure $ Turtle.stripPrefix baseDir fullPath
  -- Split the remaining path into directories and remove extensions
  let modulePathWithoutRoot :: [Text] = fmap (toS . stripSuffix "/" . Turtle.encodeString)
                                  . Turtle.splitDirectories
                                  . System.FilePath.dropExtensions
                                  $ fullPathWithoutBase
  -- Ensure the module path is non-empty
  modulePathWithoutRoot' :: NonEmpty Text <- maybe
    (Turtle.die $ "Module path should be nonEmpty for baseDir: " <> show baseDir <> ", fullPath: " <> show fullPath)
    pure
    (NonEmpty.nonEmpty modulePathWithoutRoot)
  -- Return the module path as a NonEmpty list of Text
  pure (PathToModule modulePathWithoutRoot')

stripSuffix :: Eq a => [a] -> [a] -> [a]
stripSuffix suffix target =
  if List.isSuffixOf suffix target
     then List.reverse $ List.drop (List.length suffix) $ List.reverse target
     else target

withConcurrentPutStrLn :: (Text -> IO ()) -> IO ()
withConcurrentPutStrLn f = do
  mutex <- newMVar ()
  let putStrLn' = withMVar mutex . const . putStrLn @Text
  f putStrLn'

data FindPursFilesError
  = FindPursFilesError_DirectoryDoesntExist Turtle.FilePath

findPursFiles :: Turtle.FilePath -> IO [Turtle.FilePath]
findPursFiles baseDir = do
  -- contains absolute path inside
  _base :/ (dirTree :: DirTree FilePath) <- System.Directory.Tree.readDirectoryWith return (Turtle.encodeString baseDir)

  let (dirTreeWithPursFiles :: DirTree FilePath) =
        System.Directory.Tree.filterDir
          (filterDirTreeByFilename (\n -> System.FilePath.takeExtensions n == ".purs"))
          dirTree

  filePaths :: [Turtle.FilePath] <- map Turtle.decodeString <$> dirTreeContent dirTreeWithPursFiles

  pure filePaths

ensureDirsExist :: NonEmpty Turtle.FilePath -> IO ()
ensureDirsExist filepaths = do
  -- Check if directories exist
  results <- forM filepaths $ \path -> do
    exists <- System.Directory.doesDirectoryExist path
    return (path, exists)
  -- Filter out non-existing directories
  let nonExistingDirs = mapMaybe (\(path, exists) -> if not exists then Just path else Nothing) results
  -- If there are non-existing directories, terminate with an error message
  case nonExistingDirs of
    Nothing -> pure ()
    Just nonExistingDirs' -> die $ "The following directories do not exist:\n" <> (Text.unlines $ map Turtle.encodeString nonExistingDirs')

main :: IO ()
main = do
  appOptions <- execParser appOptionsParserInfo

  directoryWithPursFilesConfig :: NonEmpty DirectoryWithPursFilesConfig <-
    case appOptionsToDirectoryWithPursFilesConfig appOptions of
      Nothing -> Turtle.die "You didnt provide any input"
      Just d -> pure d

  let
    dirsThatRequiredToBePresent :: Maybe (NonEmpty Turtle.FilePath) =
      mapMaybe
      (\config ->
        if directoryWithPursFilesConfig_required config
           then Just directoryWithPursFilesConfig_pathToDirectory config
           else Nothing)
      directoryWithPursFilesConfig

  maybe ensureDirsExist pure dirsThatRequiredToBePresent

  baseDirsWithPursFiles :: NonEmpty (Turtle.FilePath, [Turtle.FilePath]) <-
    forConcurrently directoryWithPursFilesConfig \config -> do
      let baseDir = directoryWithPursFilesConfig_pathToDirectory config
      files <- findPursFiles baseDir
      pure $ (baseDir, files)

  -- putStrLn $ "baseDir " <> Turtle.encodeString baseDir

  withConcurrentPutStrLn \concurrentPutStrLn ->
    forConcurrently_ baseDirsWithPursFiles \((baseDir, filePaths)) ->
      forConcurrently_ filePaths \filePath -> do
        let
          log x = tell [x]

          action :: WriterT [Text] IO ()
          action = do
            log $ toS $ "processing " <> Turtle.encodeString filePath

            fileContent <- liftIO $ Turtle.readTextFile filePath

            pathToModule <- liftIO $ fullPathToPathToModule baseDir filePath

            case updateModuleName fileContent pathToModule of
              UpdateModuleNameOutput__NothingChanged -> log $ "  nothing changed"
              UpdateModuleNameOutput__Error errorMessage -> log $ "  error: " <> errorMessage
              UpdateModuleNameOutput__Updated newFileContent -> do
                liftIO $ Turtle.writeTextFile filePath newFileContent
                log $ "  updated module name to \"" <> printPathToModule pathToModule <> "\""

        execWriterT action >>= (concurrentPutStrLn . Text.intercalate "\n")
