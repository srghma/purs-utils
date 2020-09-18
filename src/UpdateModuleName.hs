module UpdateModuleName where

import "protolude" Protolude
import "text" Data.Text hiding (map)
import Text.Regex.Base
import Text.RE.PCRE.Text
import Data.String.QQ
import qualified Data.Text as Text
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty

newtype PathToModule = PathToModule { unPathToModule :: NonEmpty Text }
  deriving (Show)

printPathToModule pathToModule = Text.intercalate "." $ NonEmpty.toList $ unPathToModule pathToModule

startsWithModuleWord :: Text -> Bool
startsWithModuleWord line = Text.words line & Protolude.head & \h ->
  case h of
    Nothing -> False
    Just h' -> h' == "module"

data UpdateModuleNameOutput
  = UpdateModuleNameOutput__NothingChanged
  | UpdateModuleNameOutput__Error Text
  | UpdateModuleNameOutput__Updated Text
  deriving (Show, Eq)

updateModuleName :: Text -> PathToModule -> UpdateModuleNameOutput
updateModuleName content expectedPathToModule = do
  let content' = Text.lines content
  let lineWithModule :: Maybe Int = List.findIndex startsWithModuleWord content'

  let printedExpectedModuleName = printPathToModule expectedPathToModule

  case lineWithModule of
    Nothing -> UpdateModuleNameOutput__Updated $ Text.unlines
      [ "module " <> printedExpectedModuleName <> " where"
      , content
      ]
    Just index ->
      case List.splitAt index content' of
        (linesBeforeModule, moduleLine : linesAfterModule) ->
          case Text.words moduleLine of
            "module" : actualModuleName : moduleLineTail ->
              if actualModuleName == printedExpectedModuleName
                 then UpdateModuleNameOutput__NothingChanged
                 else UpdateModuleNameOutput__Updated $ Text.unlines $
                    linesBeforeModule <>
                    [ Text.unwords $ [ "module", printedExpectedModuleName ] <> moduleLineTail
                    ] <> linesAfterModule
            _ -> UpdateModuleNameOutput__Error $ "Cannot parse module name: " <> moduleLine
        _ -> UpdateModuleNameOutput__Error "Impossible: line with index not found"
