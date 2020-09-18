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

content :: Text
content = [s|
module Data.Percent (Percent, calcPercent, unsafePercent, toNumber) where

newtype Percent = Percent Number
|]

shouldBeModuleName = PathToModule ("Data" :| ["Percent"])

spec :: Spec
spec = do
  it "UpdateModuleName" $ do
    updateModuleName content shouldBeModuleName `shouldBe` Nothing
