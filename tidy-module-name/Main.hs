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

  baseDirs :: NonEmpty (Turtle.FilePath, [Turtle.FilePath]) <-
    forConcurrently directoryConfig \config -> do
      let baseDir = directoryConfig_pathToDirectory config
      files <- findFilesWith isPursFile baseDir
      pure (baseDir, files)

  -- putStrLn $ "baseDir " <> Turtle.encodeString baseDir

  withConcurrentLogger logger \concurrentLogger ->
    forConcurrently_ baseDirs \(baseDir, filePaths) ->
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

            moduleName <- liftIO $ fullPathToModuleName baseDir filePath

            case updateModuleName fileContent moduleName of
              UpdateModuleNameOutput__NothingChanged ->
                case commandShort of
                  Command_FormatInPlace -> do
                    log [Text.Colour.fore Text.Colour.blue "  nothing changed"]
                  Command_Check ->
                    log [Text.Colour.fore Text.Colour.blue "  is up to date"]
              UpdateModuleNameOutput__Error error ->
                let
                  errorMessage =
                    case error of
                      UpdateModuleNameOutputError__CannotParseModuleNameInsideOfFile moduleLine
                        -> "cannot parse module name " <> moduleLine
                      UpdateModuleNameOutputError__ImpossibleErrorLineWithIndexNotFound
                        -> "impossible case, line with index not found"
                in
                  logAndCollectFatalErrorToReportInTheEnd
                    [ Text.Colour.fore Text.Colour.red "  error: "
                    , Text.Colour.fore Text.Colour.brightRed (Text.Colour.chunk errorMessage)
                    ]
              UpdateModuleNameOutput__Updated newFileContent ->
                case commandShort of
                  Command_FormatInPlace -> do
                    liftIO $ Data.Text.IO.writeFile filePath newFileContent
                    log
                      [ Text.Colour.fore Text.Colour.yellow "  updated module name to \"",
                        Text.Colour.fore Text.Colour.cyan (Text.Colour.chunk $ printModuleName moduleName),
                        Text.Colour.fore Text.Colour.yellow "\""
                      ]
                  Command_Check ->
                    logAndCollectCheckErrorToReportInTheEnd
                      [ Text.Colour.fore Text.Colour.red "  module name should be updated to \"",
                        Text.Colour.fore Text.Colour.cyan (Text.Colour.chunk $ printModuleName moduleName),
                        Text.Colour.fore Text.Colour.yellow "\""
                      ]

        execWriterT action >>= (concurrentLogger Log . Text.Colour.unlinesChunks)