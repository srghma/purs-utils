module UpdateModuleName where

import "protolude" Protolude
import "text" Data.Text (Text)
import qualified "text" Data.Text as Text
import "non-empty-text" Data.NonEmptyText (NonEmptyText)
import qualified "non-empty-text" Data.NonEmptyText as NonEmptyText
import Text.Regex.Base
import Text.RE.PCRE.Text
import Data.String.QQ
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import Options.Applicative
import Data.Char as Char

newtype PathToModule = PathToModule { unPathToModule :: NonEmpty NonEmptyText }
  deriving (Show)

testPathToModule :: PathToModule
testPathToModule = PathToModule $ NonEmpty.singleton $ NonEmptyText.new 'T' "est"

-- String to PathToModule
-- "Foo" to ["Foo"]
-- "Foo.Bar" to ["Foo", "Bar"]
-- "foo.Bar" to error "the $input is invalid, all module names should start with uppercase letter"
parsePathToModule :: ReadM PathToModule
parsePathToModule = eitherReader $ \s ->
  let parts = Text.splitOn "." (Text.pack s)
      (maybeNonEmptyParts :: Maybe (NonEmpty NonEmptyText)) = join $ fmap NonEmpty.nonEmpty $ traverse NonEmptyText.fromText parts
  in case maybeNonEmptyParts of
        Just neParts
          | all isValidModulePart neParts ->
              Right $ PathToModule neParts
          | otherwise ->
              Left $ "Invalid module name: " ++ s ++ ". Each part must start with an uppercase letter and be alphanumeric."
        Nothing -> Left "Module name cannot be empty."
  where
    isValidModulePart :: NonEmptyText -> Bool
    isValidModulePart part = Char.isUpper (NonEmptyText.head part) && all Char.isAlphaNum (Text.unpack (NonEmptyText.toText part))

printPathToModule :: PathToModule -> Text
printPathToModule pathToModule = Text.intercalate "." $ fmap NonEmptyText.toText $ NonEmpty.toList $ unPathToModule pathToModule

startsWithModuleWord :: Text -> Bool
startsWithModuleWord line = case Protolude.head (Text.words line) of
  Nothing -> False
  Just h -> h == "module"

data UpdateModuleNameOutput
  = UpdateModuleNameOutput__NothingChanged
  | UpdateModuleNameOutput__Error Text
  | UpdateModuleNameOutput__Updated Text
  deriving (Show, Eq)

updateModuleName :: Text -> PathToModule -> UpdateModuleNameOutput
updateModuleName content expectedPathToModule = do
  let contentLines = Text.lines content
  let lineWithModule :: Maybe Int = List.findIndex startsWithModuleWord contentLines

  let printedExpectedModuleName = printPathToModule expectedPathToModule

  case lineWithModule of
    Nothing -> UpdateModuleNameOutput__Updated $ Text.unlines
      [ "module " <> printedExpectedModuleName <> " where"
      , content
      ]
    Just index ->
      case List.splitAt index contentLines of
        (linesBeforeModule, moduleLine : linesAfterModule) ->
          case Text.words moduleLine of
            "module" : actualModuleName : moduleLineTail ->
              if actualModuleName == printedExpectedModuleName
                 then UpdateModuleNameOutput__NothingChanged
                 else UpdateModuleNameOutput__Updated $ Text.unlines $
                    linesBeforeModule <>
                    [ Text.unwords $ ["module", printedExpectedModuleName] <> moduleLineTail
                    ] <> linesAfterModule
            _ -> UpdateModuleNameOutput__Error $ "Cannot parse module name: " <> moduleLine
        _ -> UpdateModuleNameOutput__Error "Impossible: line with index not found"
