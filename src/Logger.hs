module Logger where

-- import qualified "system-filepath" Filesystem.Path

import Control.Concurrent.Async
import Control.Monad.Writer
import qualified Data.List.Index as List
import qualified Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text.IO
import qualified GHC.Float as Turtle
import ModuleName
import Options.Applicative
import Options.Applicative.NonEmpty
import System.IO (stdin, stdout)
import Text.Colour (TerminalCapabilities (..))
import qualified Text.Colour
import qualified Text.Colour.Capabilities as Text.Colour
import qualified Text.Colour.Capabilities.FromEnv as Text.Colour
import Text.Colour.Chunk (Chunk)
import UpdateModuleName
import Utils
import qualified "base" Data.List as List
import "base" Data.String (String)
import qualified "base" Data.String as String
import qualified "cases" Cases
import qualified "directory" System.Directory
import "directory-tree" System.Directory.Tree (AnchoredDirTree (..), DirTree (..))
import qualified "directory-tree" System.Directory.Tree
import qualified "filepath" System.FilePath
import "non-empty-text" Data.NonEmptyText (NonEmptyText)
import qualified "non-empty-text" Data.NonEmptyText as NonEmptyText
import "protolude" Protolude hiding (find)
import qualified "text" Data.Text as Text
import "turtle" Turtle ((</>))
import qualified "turtle" Turtle

import AppOptions

colorOptionToTerminalCapabilities :: ColorOption -> IO (TerminalCapabilities, TerminalCapabilities)
colorOptionToTerminalCapabilities ColorOption_Auto = do
  outCap <- Text.Colour.getTerminalCapabilitiesFromHandle stdout
  errCap <- Text.Colour.getTerminalCapabilitiesFromHandle stderr
  pure (outCap, errCap)
colorOptionToTerminalCapabilities ColorOption_Never = pure (WithoutColours, WithoutColours)

data LogAction = Log | Die

type Logger io = LogAction -> [Chunk] -> io ()

-- https://github.com/chshersh/iris/issues/75
makeLoggerFromTuple ::
  forall io.
  (MonadIO io) =>
  (TerminalCapabilities, TerminalCapabilities) ->
  Logger io
makeLoggerFromTuple (outCap, _) Log text = liftIO $ Text.Colour.hPutChunksLocaleWith outCap stdout text
makeLoggerFromTuple (_, errCap) Die text = do
  liftIO $ Text.Colour.hPutChunksLocaleWith errCap stderr text
  liftIO $ exitWith (ExitFailure 1)

makeLogger ::
  forall io.
  (MonadIO io) =>
  ColorOption ->
  io (Logger io)
makeLogger x =
  fmap makeLoggerFromTuple $ liftIO $ colorOptionToTerminalCapabilities x

withConcurrentLogger :: Logger IO -> (Logger IO -> IO ()) -> IO ()
withConcurrentLogger logger f = do
  mutex <- newMVar ()
  f $ \action text -> withMVar mutex $ \_ -> logger action text
