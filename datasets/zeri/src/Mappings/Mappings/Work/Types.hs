module Mappings.Mappings.Work.Types where

import CommonImports
import qualified Vocabularies.PHAROS as P
import Pharos.CidocCrm.Patterns.Appellations

import qualified Data.Text as T
import Pharos.Utils.CsvHashTable (LocationReconciliationHash, lookupLocation, lookupValues, loadLocationReconciliationHash)
import GHC.IO (unsafePerformIO)

types :: [PathTree E22_]
types = [
    [x|PARAGRAFO[@etichetta="OBJECT"]/OGTD|] @> (
      P2 ---> (E55, templateUri "vocab/object_type/{type}" [("type", [x|text()|])]) ==> [
        P2 --> (E55, P.object_type),
        appellation_0_1 P.preferred_name [x|text()|],
        [x|.|] @> (\node -> ogtdSameAsMapping $ nodeText node)
      ]
    ),
    [x|PARAGRAFO[@etichetta="OBJECT"]/OGTT|] @> (
      P2 ---> (E55, templateUri "vocab/typology/{type}" [("type", [x|text()|])]) ==> [
        P2 --> (E55, P.object_type),
        appellation_0_1 P.preferred_name [x|text()|],
        [x|.|] @> (\node -> ogttSameAsMapping $ nodeText node)
      ]
    )
  ]

-- Reconciliation Caches
ogtdSameAsMapping value =
  case lookupValues ogtdTable [value] of
    [Just exactMatch, _] -> [SameAs ---> (E55, constUri exactMatch)  ==> [P71i --> (E32, constUri "http://vocab.getty.edu/aat/")]]
    [_, Just broadMatch] | not $ T.null broadMatch -> [
        P127 ---> (E55, constUri broadMatch) ==> [P71i --> (E32, constUri "http://vocab.getty.edu/aat/")],
        P71i --> (E32, constUri "http://vocab.getty.edu/aat/")
      ]
    -- if there is no mach then we mark current E55 as belonging to pharos types vocabulary
    _ -> [P71i --> (E32, constUri "https://artresearch.net/resource/pharos/vocab/types")]

ogttSameAsMapping value =
  case lookupValues ogttTable [value] of
    [Just exactMatch] -> [SameAs ---> (E55, constUri exactMatch) ==> [P71i --> (E32, constUri "http://vocab.getty.edu/aat/")]]
    _ -> [P71i --> (E32, constUri "https://artresearch.net/resource/pharos/vocab/types")]

ogtdTable :: LocationReconciliationHash
ogtdTable = unsafePerformIO $ loadLocationReconciliationHash
    "datasets/zeri/resources/types/ogtd.csv"
    ["value"]
    ["exactMatch", "broadMatch"]
{-# NOINLINE ogtdTable #-}

ogttTable :: LocationReconciliationHash
ogttTable = unsafePerformIO $ loadLocationReconciliationHash
    "datasets/zeri/resources/types/ogtt.csv"
    ["value"]
    ["exactMatch"]
{-# NOINLINE ogttTable #-}