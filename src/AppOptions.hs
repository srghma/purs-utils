module AppOptions where

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
import Text.Colour (TerminalCapabilities(..))
import System.IO (stdin, stdout)
import qualified Text.Colour
import qualified Text.Colour.Capabilities as Text.Colour
import qualified Text.Colour.Capabilities.FromEnv as Text.Colour
import Control.Concurrent.Async
import Control.Monad.Writer
import ModuleName
import UpdateModuleName
import Utils
import qualified GHC.Float as Turtle
import Text.Colour.Chunk (Chunk)

------------------------------------------------------------------------------------

collectRequiredDirs :: NonEmpty DirectoryConfig -> Maybe (NonEmpty Turtle.FilePath)
collectRequiredDirs directoryConfigs =
      nonEmpty $
      mapMaybe
      (\config ->
        if directoryConfig_required config
           then Just $ directoryConfig_pathToDirectory config
           else Nothing)
      (toList directoryConfigs)

------------------------------------------------------------------------------------

data DirectoryConfig = DirectoryConfig
  { directoryConfig_prependModuleName :: Maybe ModuleName
  , directoryConfig_pathToDirectory   :: Turtle.FilePath
  , directoryConfig_required          :: Bool
  }
  deriving (Show, Eq)

rootConfig :: Turtle.FilePath -> NonEmpty DirectoryConfig
rootConfig rootPath =
  (DirectoryConfig Nothing (rootPath </> "src") True)
  NonEmpty.:|
  [ DirectoryConfig (Just testModuleName) (rootPath </> "test") False
  ]

-- Create configs from project roots
rootConfigs :: [Turtle.FilePath] -> [DirectoryConfig]
rootConfigs = concatMap (NonEmpty.toList . rootConfig)

targetDirectoriesOptionsToDirectoryConfigOrCwd :: TargetDirectoriesOptions -> IO (NonEmpty DirectoryConfig)
targetDirectoriesOptionsToDirectoryConfigOrCwd x =
  case targetDirectoriesOptionsToDirectoryConfig x of
    Just x -> pure x
    Nothing -> do
      (cwd :: Turtle.FilePath) <- Turtle.pwd
      pure $ rootConfig cwd

targetDirectoriesOptionsToDirectoryConfig :: TargetDirectoriesOptions -> Maybe (NonEmpty DirectoryConfig)
targetDirectoriesOptionsToDirectoryConfig x =
  case x of
    TargetDirectoriesOptions_Flagged opts -> targetDirectoriesOptionsFlaggedToDirectoryConfig opts
    TargetDirectoriesOptions_Positional arrayFilePath -> NonEmpty.nonEmpty $ rootConfigs arrayFilePath

targetDirectoriesOptionsFlaggedToDirectoryConfig :: TargetDirectoriesOptionsFlagged -> Maybe (NonEmpty DirectoryConfig)
targetDirectoriesOptionsFlaggedToDirectoryConfig opts =
  let
    -- Create configs from source directories
    srcConfigs = map (\srcDir -> DirectoryConfig Nothing srcDir True) (targetDirectoriesOptionsFlagged_srcDirs opts)
    -- Create configs from test directories
    testConfigs = map (\testDir -> DirectoryConfig (Just testModuleName) testDir True) (targetDirectoriesOptionsFlagged_testDirs opts)
    -- Create configs from custom directories
    customConfigs = map (\(CustomOption moduleName dir) -> DirectoryConfig (Just moduleName) dir True) (targetDirectoriesOptionsFlagged_customDirs opts)
    allConfigs = rootConfigs (targetDirectoriesOptionsFlagged_projectRoots opts) ++ srcConfigs ++ testConfigs ++ customConfigs
  in NonEmpty.nonEmpty allConfigs

---------------------------------------------------------------------------------------------------------------------------------------

data CustomOption = CustomOption ModuleName Turtle.FilePath
  deriving (Show, Eq)

parseCustomOption :: ReadM CustomOption
parseCustomOption = do
  str <- str
  let (arg1, arg2) = splitOn "=" str
  case (toModuleName_stringSeparatedWithDots arg1, stringToDir (toS arg2)) of
    (Left e, _) -> readerError . toS $ moduleNameError_print e
    (Right val1, val2) -> return $ CustomOption val1 val2

---------------------------------------------------------------------------------------------------------------------------------------

data TargetDirectoriesOptionsFlagged = TargetDirectoriesOptionsFlagged
  { targetDirectoriesOptionsFlagged_projectRoots :: [Turtle.FilePath] -- to [(Nothing, filepath ++ "/src", true), [(Just testModuleName, filepath ++ "/test", false)]]
  , targetDirectoriesOptionsFlagged_srcDirs      :: [Turtle.FilePath] -- to (Nothing, filepath, true)
  , targetDirectoriesOptionsFlagged_testDirs     :: [Turtle.FilePath] -- to (Just testModuleName, filepath, true)
  , targetDirectoriesOptionsFlagged_customDirs   :: [CustomOption] -- to (Just moduleName, filepath, true)
  }
  deriving (Show, Eq)

-- `program --root ./myproj`
-- EQUAL TO `program --src ./myproj/src --test ./myproj/test`
-- EQUAL TO `program --src ./myproj/src --custom Test=./myproj/test`

-- can pass also `program ./myproj ./myproj2` (interpreted as `program --root ./myproj --root ./myproj2`)

-- or just `program` (interpreted as cwd)

targetDirectoriesOptionsFlaggedParser :: Parser TargetDirectoriesOptionsFlagged
targetDirectoriesOptionsFlaggedParser = TargetDirectoriesOptionsFlagged
  <$> many (option parseDir (long "root" <> short 'r' <> metavar "DIRECTORY" <> help "Base dir with two directories - src/ and test/. Can pass multiple -r" ))
  <*> many (option parseDir (long "src" <> short 's' <> metavar "DIRECTORY" <> help "Source directory."))
  <*> many (option parseDir (long "test" <> short 't' <> metavar "DIRECTORY" <> help "Test directory."))
  <*> many (option parseCustomOption (long "custom" <> short 'c'))

---------------------------------------------------------------------------------------------------------------------------------------

data TargetDirectoriesOptions
  = TargetDirectoriesOptions_Flagged TargetDirectoriesOptionsFlagged
  | TargetDirectoriesOptions_Positional [Turtle.FilePath]
  deriving (Show, Eq)

targetDirectoriesOptionsPositionalParser :: Parser [FilePath]
targetDirectoriesOptionsPositionalParser = many (argument parseDir (metavar "DIRECTORY" <> help "Positional arguments treated as --root directories."))

targetDirectoriesOptionsParser :: Parser TargetDirectoriesOptions
targetDirectoriesOptionsParser
  = TargetDirectoriesOptions_Flagged <$> targetDirectoriesOptionsFlaggedParser
  <|> TargetDirectoriesOptions_Positional <$> targetDirectoriesOptionsPositionalParser

---------------------------------------------------------------------------------------------------------------------------------------

data CommandOptions
  = CommandOptions_FormatInPlace TargetDirectoriesOptions
  | CommandOptions_Check TargetDirectoriesOptions
  deriving (Show, Eq)

commandOptionsParser :: Parser CommandOptions
commandOptionsParser =
  subparser
     ( command "format-in-place" (info (CommandOptions_FormatInPlace <$> targetDirectoriesOptionsParser) (progDesc "Update files"))
    <> command "check" (info (CommandOptions_Check <$> targetDirectoriesOptionsParser) (progDesc "Throw if files are not updated"))
     )

---------------------------------------------------------------------------------------------------------------------------------------

data ColorOption
  = ColorOption_Auto
  -- | ColorOption_Always
  | ColorOption_Never
  deriving (Show, Eq)

parseColorOption :: ReadM ColorOption
parseColorOption = eitherReader $ \arg ->
  case arg of
    "auto"   -> Right ColorOption_Auto
    -- "always" -> Right ColorOption_Always
    "never"  -> Right ColorOption_Never
    _        -> Left $ "Invalid value for --color: " ++ arg ++ ". Expected one of: auto, always, never."

colorOptionParser :: Parser ColorOption
colorOptionParser = option parseColorOption
  ( long "color"
  <> short 'c'
  <> metavar "when"
  <> help "When to use colors [default: auto] [possible values: auto, never]"
  <> value ColorOption_Auto -- Default value
  <> showDefaultWith showColorOption
  )

showColorOption :: ColorOption -> String
showColorOption ColorOption_Auto   = "auto"
-- showColorOption ColorOption_Always = "always"
showColorOption ColorOption_Never  = "never"

data GlobalAndCommandOptions = GlobalAndCommandOptions
  { globalAndCommandOptions_color :: ColorOption
  , globalAndCommandOptions_command :: CommandOptions
  }
  deriving (Show, Eq)

globalAndCommandOptionsParser :: Parser GlobalAndCommandOptions
globalAndCommandOptionsParser = GlobalAndCommandOptions
  <$> colorOptionParser
  <*> commandOptionsParser

-------------------------

data CommandShort
  = Command_FormatInPlace
  | Command_Check
  deriving (Show, Eq)

commandOptionsToCommandShort :: CommandOptions -> CommandShort
commandOptionsToCommandShort (CommandOptions_FormatInPlace _) = Command_FormatInPlace
commandOptionsToCommandShort (CommandOptions_Check _) = Command_Check

commandOptionsToTargetDirectoriesOptions :: CommandOptions -> TargetDirectoriesOptions
commandOptionsToTargetDirectoriesOptions (CommandOptions_FormatInPlace x) = x
commandOptionsToTargetDirectoriesOptions (CommandOptions_Check x) = x
