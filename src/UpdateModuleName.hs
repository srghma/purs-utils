module UpdateModuleName where

import "protolude" Protolude
import "text" Data.Text hiding (map)
import Text.Regex.Base
import Text.RE.PCRE.Text
import Data.String.QQ
import qualified Data.Text as Text
import qualified Data.List as List

newtype PathToModule = PathToModule { unPathToModule :: NonEmpty Text }
  deriving (Show)

updateModuleName :: Text -> PathToModule -> Maybe Text
updateModuleName = undefined
