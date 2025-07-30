module Midas.Mappings.Utils.Inheritance (inheritValue, typeMapping) where

import CommonImports
import qualified Data.Text as T
import qualified Vocabularies.PHAROS as P
import Pharos.CidocCrm.Patterns.Appellations
import Pharos.Utils.CsvHashTable (lookupValues)

-- MIDAS value inheritance logic
-- This function traverses up the XML tree to find the first non-empty value for the given XPath expression.
-- It stops if it encounters a value that is "---", which indicates that it should not inherit further.
-- If it finds a valid value, it returns that value; otherwise, it continues to traverse up the parent nodes.
-- If no valid value is found, it returns an empty list.
inheritValue :: XPathExpr -> XMLNode -> [T.Text]
inheritValue elemXpath node = go (xmlNodeParent node)
  where
    go Nothing = []
    go (Just parentNode) =
      let values = map nodeText $ evaluateXPathExpr parentNode elemXpath
          filteredValues = filter (\v -> not (T.null v) && v /= T.pack "---") values
      in if T.pack "---" `elem` values
         then [] -- Stop if "---" is present
         else if null filteredValues
              then go (xmlNodeParent parentNode) -- Continue up if no valid values found
              else filteredValues -- Return valid values

typeMapping property elemXPathExpr e55 reconciliationTable = [
    elemXPathExpr @> (
      when (not_ $ equals [x|normalize-space(text())|] "---") (
        [x|tokenize(text(), '(&)|(/)|(:)')|] @> (
          property ---> (E55, templateUri  ("vocab/" <> localName e55 <> "/{type}") [("type", [x|text()|])]) ==> [
            P2 --> (E55, e55),
            appellation_0_1 P.preferred_name [x|text()|],
            [x|.|] @> (\node -> sameAsMapping reconciliationTable $  nodeText node)
          ]
        )        
      )
    ),
    when (not_ $ exists elemXPathExpr) (
      [x|.|] @> \node ->
        map (\val -> property --> (E55, templateUri "vocab/classification/{type}" [("type", val)])) (inheritValue elemXPathExpr node)
    )
  ]

-- Reconciliation Caches
sameAsMapping table value =
  case lookupValues table [T.takeWhile (/= '?') value] of
    [Just exactMatch, _, _, _, _] -> [SameAs ---> (E55, constUri exactMatch) ==> [P71i --> (E32, constUri "http://vocab.getty.edu/aat/")]]
    [_, Just broadMatch, _, _, _] -> broaderMapping broadMatch
    [_, _, _, Just closeMatch, _] -> broaderMapping closeMatch
    [_, _, _, _, Just relatedMatch] -> broaderMapping relatedMatch

    -- if there is no mach then we mark current E55 as belonging to pharos types vocabulary
    _ -> [P71i --> (E32, constUri "https://artresearch.net/resource/pharos/vocab/types")]

broaderMapping match = [
    P127 ---> (E55, constUri m) ==> [
      P71i --> (E32, constUri "http://vocab.getty.edu/aat/") 
    ] | m <- T.splitOn (T.pack ";") match, not (T.null m)
  ] ++ [
    P71i --> (E32, constUri "https://artresearch.net/resource/pharos/vocab/types")
  ]