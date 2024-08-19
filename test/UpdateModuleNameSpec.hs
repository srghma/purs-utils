module UpdateModuleNameSpec where

import           Protolude

import           Test.Hspec

import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.List as List
import Data.String.QQ
import "non-empty-text" Data.NonEmptyText (NonEmptyText)
import qualified "non-empty-text" Data.NonEmptyText as NonEmptyText

import Text.Regex.Base
import Text.RE.PCRE.Text
import Control.Arrow
import ModuleName
import UpdateModuleName

dataPercent = ModuleName (NonEmptyText.new 'D' "ata" :| [NonEmptyText.new 'P' "ercent"])

updatedDataPercent = ModuleName (NonEmptyText.new 'U' "pdatedData" :| [NonEmptyText.new 'P' "ercent"])

spec :: Spec
spec = do
  it "no changes" $ do
    let
      content :: Text
      content = [s|
module Data.Percent (Percent, calcPercent, unsafePercent, toNumber) where

newtype Percent = Percent Number
|]

    updateModuleName content dataPercent `shouldBe` UpdateModuleNameOutput__NothingChanged
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

    updateModuleName content updatedDataPercent `shouldBe` UpdateModuleNameOutput__Updated newContent
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



    updateModuleName content updatedDataPercent `shouldBe` UpdateModuleNameOutput__Updated newContent
  it "changes 3" $ do
    let
      content :: Text
      content = [s|
module Data.Percent
  ( Percent
  , calcPercent
  , unsafePercent
  , toNumber
  ) where

newtype Percent = Percent Number
|]

      newContent :: Text
      newContent = [s|
module UpdatedData.Percent
  ( Percent
  , calcPercent
  , unsafePercent
  , toNumber
  ) where

newtype Percent = Percent Number
|]

    updateModuleName content updatedDataPercent `shouldBe` UpdateModuleNameOutput__Updated newContent
