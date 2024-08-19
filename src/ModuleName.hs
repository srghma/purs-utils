module ModuleName where

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

newtype ModuleName = ModuleName { unModuleName :: NonEmpty NonEmptyText }
  deriving (Show)

testModuleName :: ModuleName
testModuleName = ModuleName . NonEmpty.singleton $ NonEmptyText.new 'T' "est"

data ModuleNameError
  = ModuleNameError_Empty
  | ModuleNameError_ShouldBeUppercaseAndAlphanumeric (NonEmpty NonEmptyText)

moduleNameError_print :: ModuleNameError -> Text
moduleNameError_print ModuleNameError_Empty = "Module name cannot be empty."
moduleNameError_print (ModuleNameError_ShouldBeUppercaseAndAlphanumeric original)
  = "Each part must start with an uppercase letter and be alphanumeric. Invalid module name: "
  <> "\"" <> printModuleName (ModuleName original) <> "\""

isValidModulePart :: NonEmptyText -> Bool
isValidModulePart part = Char.isUpper (NonEmptyText.head part) && all Char.isAlphaNum (Text.unpack (NonEmptyText.toText part))

toModuleName_arrayOfString :: (ConvertText s Text) => [s] -> Either ModuleNameError ModuleName
toModuleName_arrayOfString textArray =
  let (maybeNonEmptyParts :: Maybe (NonEmpty NonEmptyText)) =
        ((NonEmpty.nonEmpty =<<) $ traverse (NonEmptyText.fromText . toS) textArray)
  in case maybeNonEmptyParts of
        Just neParts
          | all isValidModulePart neParts ->
              Right $ ModuleName neParts
          | otherwise ->
              Left $ ModuleNameError_ShouldBeUppercaseAndAlphanumeric neParts
        Nothing -> Left ModuleNameError_Empty

toModuleName_stringSeparatedWith :: (ConvertText s Text) => Text -> s -> Either ModuleNameError ModuleName
toModuleName_stringSeparatedWith separator s = toModuleName_arrayOfString $ Text.splitOn separator (toS s)

toModuleName_stringSeparatedWithDots :: (ConvertText s Text) => s -> Either ModuleNameError ModuleName
toModuleName_stringSeparatedWithDots = toModuleName_stringSeparatedWith "."

-- String to ModuleName
-- "Foo" to ["Foo"]
-- "Foo.Bar" to ["Foo", "Bar"]
-- "foo.Bar" to error "the $input is invalid, all module names should start with uppercase letter"
parseModuleName :: ReadM ModuleName
parseModuleName = eitherReader $ \s ->
  case toModuleName_stringSeparatedWithDots s of
    Left e -> Left . toS $ moduleNameError_print e
    Right x -> Right x

printModuleName :: ModuleName -> Text
printModuleName moduleName = Text.intercalate "." . fmap NonEmptyText.toText $ NonEmpty.toList (unModuleName moduleName)
