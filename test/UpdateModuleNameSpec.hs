module UpdateModuleNameSpec where

import           Protolude

import           Test.Hspec

import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.List as List
import Data.String.QQ

import Text.Regex.Base
import Text.RE.PCRE.Text
import UpdateModuleName
import Control.Arrow

spec :: Spec
spec = do
  it "no changes" $ do
    let
      content :: Text
      content = [s|
module Data.Percent (Percent, calcPercent, unsafePercent, toNumber) where

newtype Percent = Percent Number
|]
      expectedPathToModule = PathToModule ("Data" :| ["Percent"])

    updateModuleName content expectedPathToModule `shouldBe` UpdateModuleNameOutput__NothingChanged
  it "changes" $ do
    let
      content :: Text
      content = [s|
module Data.Percent (Percent, calcPercent, unsafePercent, toNumber) where

newtype Percent = Percent Number
|]

      newContent :: Text
      newContent = [s|
module UpdatedData.Percent (Percent, calcPercent, unsafePercent, toNumber) where

newtype Percent = Percent Number
|]
      expectedPathToModule = PathToModule ("UpdatedData" :| ["Percent"])

    updateModuleName content expectedPathToModule `shouldBe` UpdateModuleNameOutput__Updated newContent
  it "changes 2" $ do
    let
      content :: Text
      content = [s|
module Data.Percent

newtype Percent = Percent Number
|]

      newContent :: Text
      newContent = [s|
module UpdatedData.Percent

newtype Percent = Percent Number
|]

      expectedPathToModule = PathToModule ("UpdatedData" :| ["Percent"])

    updateModuleName content expectedPathToModule `shouldBe` UpdateModuleNameOutput__Updated newContent
