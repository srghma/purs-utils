module CssContentToTypes
( cssContentToTypes
) where

import qualified Data.List             as List
import           Data.String.QQ
import qualified Data.Text             as Text
import           "text" Data.Text      hiding (map)
import           Language.Scss.Parser
import           "protolude" Protolude
import           Text.Megaparsec       (parseMaybe)
import           Text.RE.PCRE.Text
import           Text.Regex.Base

-- [".myButton",".myButton",".myButton2","#myButton3","#myButton4",".myButton5",".classInsideClass",".classInsideClass2",,".classWithBefore2Pre",".classWithBefore2:before",".classWithBefore3:before",".classWithBefore3Post",".classInOneLine","#idInOneLine",".myButton3"]

-- from ".classWithBefore1:before" to "classWithBefore1"

-- e.g. ".myButton3 > a" to ".myButton3"
extractClassesAndIds :: Text -> [Text]
extractClassesAndIds = List.filter (\t -> Text.isPrefixOf "#" t || Text.isPrefixOf "." t) . Text.words

extractClassOrId :: Text -> Maybe Text
extractClassOrId css = (join . map (`atMay` 1)) . flip atMay 0 $ (css =~ [re|[\.#]([\w\-]+)|] :: [[Text]])

valueToSelectors :: Value -> [Text]
valueToSelectors (Selector selector values) = selector <> join (map valueToSelectors values)
valueToSelectors (AtRule name body values) = join $ map valueToSelectors values
valueToSelectors _ = []

cssContentToTypes :: Text -> [Text]
cssContentToTypes cssContent = ordNub (mapMaybe extractClassOrId (join . map extractClassesAndIds $ (either (const []) (join . map valueToSelectors) $ parse cssContent)))
