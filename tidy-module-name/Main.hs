module Main where

import           Options.Applicative
import           Options.Applicative.NonEmpty
import           "protolude" Protolude                  hiding (find)
import qualified "directory" System.Directory
import qualified "filepath" System.FilePath
import           "turtle" Turtle                        ((</>))
import qualified "turtle" Turtle
-- import qualified "system-filepath" Filesystem.Path
import qualified "cases" Cases
import qualified "base" Data.List                       as List
import qualified Data.List.Index                        as List
import qualified Data.List.NonEmpty                     (NonEmpty)
import qualified Data.List.NonEmpty                     as NonEmpty
import           "non-empty-text" Data.NonEmptyText     (NonEmptyText)
import qualified "non-empty-text" Data.NonEmptyText     as NonEmptyText
import           "base" Data.String                     (String)
import qualified "base" Data.String                     as String
import qualified "text" Data.Text                       as Text
import qualified Data.Text.IO
import           "directory-tree" System.Directory.Tree
    ( AnchoredDirTree (..)
    , DirTree (..)
    )
import qualified "directory-tree" System.Directory.Tree
import qualified Text.Colour

import           AppOptions
import           Control.Concurrent.Async
import           Control.Monad.Writer
import           Data.String                            (fromString)
import           Logger
import           ModuleName
import           Options.Applicative.Help.Pretty        (Doc, vcat)
import           Text.Colour.Chunk                      (Chunk)
import           UpdateModuleName
import           Utils

appOptionsParserInfo :: ParserInfo GlobalAndCommandOptions
appOptionsParserInfo = info (globalAndCommandOptionsParser <**> helper <**> simpleVersioner "v1.2.3")
  ( fullDesc
  <> progDescDoc (Just $ vcat
        [ "./src/Foo/Bar.purs -> module Foo.Bar"
        , "./test/Foo/Bar.purs -> module Test.Foo.Bar"
        ])
  <> header "Finds .purs files and updates their module name" )

type Program a = WriterT ([[Chunk]], Any) IO a

programLog :: [Chunk] -> Program ()
programLog x = tell ([x], Any False)

programLogAndDieInTheEnd :: [Chunk] -> Program ()
programLogAndDieInTheEnd x = tell ([x], Any True)

programLogModuleNameOutputError :: ModuleNameOutputError -> Program ()
programLogModuleNameOutputError e =
  programLogAndDieInTheEnd
    [ Text.Colour.fore Text.Colour.red "  error: ",
      Text.Colour.fore Text.Colour.brightRed $ Text.Colour.chunk
        case e of
          ModuleNameOutputError__CannotParseModuleNameInsideOfFile moduleLine ->
            "cannot parse module name " <> moduleLine
          ModuleNameOutputError__ImpossibleErrorLineWithIndexNotFound ->
            "impossible case, line with index not found"
    ]

program :: CommandShort -> Turtle.FilePath -> Turtle.FilePath -> Maybe ModuleName -> Program ()
program commandShort baseDir filePath maybePrependModuleName = do
  programLog
    [ Text.Colour.fore Text.Colour.green "processing ",
      Text.Colour.fore Text.Colour.yellow (fromString $ filePath)
    ]

  fileContent <- liftIO $ Data.Text.IO.readFile filePath

  moduleName_fromPath <- liftIO $ fullPathToModuleName baseDir filePath

  let moduleName_expected = prependMaybeModuleName maybePrependModuleName moduleName_fromPath

  case commandShort of
    Command_FormatInPlace -> do
      case updateModuleName fileContent moduleName_expected of
        UpdateModuleNameOutput__NothingChanged -> programLog [Text.Colour.fore Text.Colour.blue "  nothing changed"]
        UpdateModuleNameOutput__Error e -> programLogModuleNameOutputError e
        UpdateModuleNameOutput__Updated newFileContent -> do
          liftIO $ Data.Text.IO.writeFile filePath newFileContent
          programLog
            [ Text.Colour.fore Text.Colour.yellow "  updated module name to ",
              Text.Colour.fore Text.Colour.cyan (Text.Colour.chunk . wrapInQuotes $ printModuleName moduleName_expected)
            ]
    Command_Check ->
      case checkModuleName fileContent moduleName_expected of
        CheckModuleNameOutput__NothingChanged -> programLog [Text.Colour.fore Text.Colour.blue "  is up to date"]
        CheckModuleNameOutput__Error e -> programLogModuleNameOutputError e
        CheckModuleNameOutput__ActualDoesntExistExpectedShouldBeAdded expectedModuleName ->
          programLogAndDieInTheEnd
            [ Text.Colour.fore Text.Colour.red "  module name is not present in a file, ",
              Text.Colour.fore Text.Colour.cyan (Text.Colour.chunk $ wrapInQuotes expectedModuleName),
              Text.Colour.fore Text.Colour.red " should be added"
            ]
        CheckModuleNameOutput__ActualShouldBeUpdatedToExpected actualModuleName expectedModuleName ->
          programLogAndDieInTheEnd
            [ Text.Colour.fore Text.Colour.red "  module name ",
              Text.Colour.fore Text.Colour.cyan (Text.Colour.chunk $ wrapInQuotes actualModuleName),
              Text.Colour.fore Text.Colour.red " should be updated to ",
              Text.Colour.fore Text.Colour.cyan (Text.Colour.chunk $ wrapInQuotes expectedModuleName)
            ]

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

  withConcurrentLogger logger \concurrentLogger -> do
    (anys :: NonEmpty Any) <-
      forConcurrently baseDirs \(baseDir, filePaths, maybePrependModuleName) -> do
        (anys :: [Any]) <-
          forConcurrently filePaths \filePath -> do
            (logs, die) <- execWriterT (program commandShort baseDir filePath maybePrependModuleName)
            concurrentLogger Log . Text.Colour.unlinesChunks $ logs
            pure die
        pure $ mconcat anys
    let die = getAny $ mconcat $ NonEmpty.toList $ anys
    if die then exitWith (ExitFailure 1) else pure ()
