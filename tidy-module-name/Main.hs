module Main where

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
import qualified Data.Text.IO
import qualified Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import qualified "directory-tree" System.Directory.Tree
import "directory-tree" System.Directory.Tree (DirTree (..), AnchoredDirTree (..))
import qualified "cases" Cases
import "non-empty-text" Data.NonEmptyText (NonEmptyText)
import qualified "non-empty-text" Data.NonEmptyText as NonEmptyText
import qualified Text.Colour

import Control.Concurrent.Async
import ModuleName
import Logger
import UpdateModuleName
import Utils
import AppOptions
import Control.Monad.Writer
import Text.Colour.Chunk (Chunk)
import Data.String (fromString)
import Options.Applicative.Help.Pretty (Doc, vcat)

appOptionsParserInfo :: ParserInfo GlobalAndCommandOptions
appOptionsParserInfo = info (globalAndCommandOptionsParser <**> helper)
  ( fullDesc
  <> progDescDoc (Just $ vcat
        [ "./src/Foo/Bar.purs -> module Foo.Bar"
        , "./test/Foo/Bar.purs -> module Test.Foo.Bar"
        ])
  <> header "Finds .purs files and updates their module name" )

-- Log

-- newtype Program = Program { unProgram ::

main :: IO ()
main = do
  globalAndCommandOptions <- execParser appOptionsParserInfo

  let
    color = globalAndCommandOptions_color globalAndCommandOptions
    commandShort = commandOptionsToCommandShort $ globalAndCommandOptions_command globalAndCommandOptions
    targetDirectoriesOptions = commandOptionsToTargetDirectoriesOptions $ globalAndCommandOptions_command globalAndCommandOptions

  logger <- makeLogger color

  directoryConfig :: NonEmpty DirectoryConfig <- targetDirectoriesOptionsToDirectoryConfigOrCwd targetDirectoriesOptions

  let
    dirsThatRequiredToBePresent :: Maybe (NonEmpty Turtle.FilePath) = collectRequiredDirs directoryConfig

  mapM_ ensureDirsExist dirsThatRequiredToBePresent

  -- traceM $ show directoryConfig

  baseDirs :: NonEmpty (Turtle.FilePath, [Turtle.FilePath], Maybe ModuleName) <-
    forConcurrently directoryConfig \config -> do
      let baseDir = directoryConfig_pathToDirectory config
      files <- findFilesWith isPursFile baseDir
      pure (ensureTrailingSlash baseDir, files, directoryConfig_prependModuleName config)
  -- traceM $ show baseDirs

  -- putStrLn $ "baseDir " <> Turtle.encodeString baseDir

  withConcurrentLogger logger \concurrentLogger ->
    forConcurrently_ baseDirs \(baseDir, filePaths, maybePrependModuleName) ->
      forConcurrently_ filePaths \filePath -> do
        let
          log :: [Chunk] -> WriterT [[Chunk]] IO ()
          log x = tell [x]
          logAndCollectFatalErrorToReportInTheEnd x = tell [x]
          logAndCollectCheckErrorToReportInTheEnd x = tell [x]

          action :: WriterT [[Chunk]] IO ()
          action = do
            log [ Text.Colour.fore Text.Colour.green "processing "
                , Text.Colour.fore Text.Colour.yellow (fromString $ filePath)
                ]

            fileContent <- liftIO $ Data.Text.IO.readFile filePath

            moduleName_fromPath <- liftIO $ fullPathToModuleName baseDir filePath

            let moduleName_expected = prependMaybeModuleName maybePrependModuleName moduleName_fromPath

            let
              logModuleNameOutputError e =
                logAndCollectFatalErrorToReportInTheEnd
                  [ Text.Colour.fore Text.Colour.red "  error: "
                  , Text.Colour.fore Text.Colour.brightRed $ Text.Colour.chunk
                      case e of
                        ModuleNameOutputError__CannotParseModuleNameInsideOfFile moduleLine
                          -> "cannot parse module name " <> moduleLine
                        ModuleNameOutputError__ImpossibleErrorLineWithIndexNotFound
                          -> "impossible case, line with index not found"
                  ]

            case commandShort of
              Command_FormatInPlace -> do
                case updateModuleName fileContent moduleName_expected of
                  UpdateModuleNameOutput__NothingChanged -> log [Text.Colour.fore Text.Colour.blue "  nothing changed"]
                  UpdateModuleNameOutput__Error e -> logModuleNameOutputError e
                  UpdateModuleNameOutput__Updated newFileContent -> do
                    liftIO $ Data.Text.IO.writeFile filePath newFileContent
                    log
                      [ Text.Colour.fore Text.Colour.yellow "  updated module name to \"",
                        Text.Colour.fore Text.Colour.cyan (Text.Colour.chunk $ printModuleName moduleName_expected),
                        Text.Colour.fore Text.Colour.yellow "\""
                      ]
              Command_Check ->
                case checkModuleName fileContent moduleName_expected of
                  CheckModuleNameOutput__NothingChanged -> log [Text.Colour.fore Text.Colour.blue "  is up to date"]
                  CheckModuleNameOutput__Error e -> logModuleNameOutputError e
                  CheckModuleNameOutput__ActualDoesntExistExpectedShouldBeAdded expectedModuleName ->
                    logAndCollectCheckErrorToReportInTheEnd
                      [ Text.Colour.fore Text.Colour.red "  module name is not present in a file, "
                      , Text.Colour.fore Text.Colour.cyan (Text.Colour.chunk $ wrapInQuotes expectedModuleName)
                      , Text.Colour.fore Text.Colour.red " should be added"
                      ]
                  CheckModuleNameOutput__ActualShouldBeUpdatedToExpected actualModuleName expectedModuleName ->
                    logAndCollectCheckErrorToReportInTheEnd
                      [ Text.Colour.fore Text.Colour.red "  module name "
                      , Text.Colour.fore Text.Colour.cyan (Text.Colour.chunk $ wrapInQuotes actualModuleName)
                      , Text.Colour.fore Text.Colour.red " should be updated to "
                      , Text.Colour.fore Text.Colour.cyan (Text.Colour.chunk $ wrapInQuotes expectedModuleName)
                      ]
        execWriterT action >>= (concurrentLogger Log . Text.Colour.unlinesChunks)
