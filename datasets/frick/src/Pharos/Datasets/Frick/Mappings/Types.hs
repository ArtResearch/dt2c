module Pharos.Datasets.Frick.Mappings.Types where

import CommonImports
import qualified Vocabularies.PHAROS as P
import Pharos.CidocCrm.Patterns.Appellations

import qualified Data.Text as T
import Pharos.Utils.CsvHashTable (LocationReconciliationHash, lookupLocation, lookupValues, loadLocationReconciliationHash)
import GHC.IO (unsafePerformIO)

types :: [PathTree E22_]
types = [
    [x|datafield[@tag='300']/subfield[@code='a']/marc-value()|] @> (
      P2 ---> (E55, templateUri "vocab/physical_description/{type}" [("type", [x|text()|])]) ==> [
        P2 --> (E55, P.object_type),
        appellation_0_1 P.preferred_name [x|text()|],
        [x|.|] @> (\node -> sameAsMapping a300Table $ nodeText node)
      ]
    )
  ]


-- Reconciliation Caches
sameAsMapping table value =
  case lookupValues table [value] of
    [Just exactMatch, _] -> [SameAs ---> (E55, constUri exactMatch)  ==> [P71i --> (E32, constUri "http://vocab.getty.edu/aat/")]]
    [_, Just broadMatch] | not $ T.null broadMatch -> [
        P127 ---> (E55, constUri broadMatch) ==> [P71i --> (E32, constUri "http://vocab.getty.edu/aat/")],
        P71i --> (E32, constUri "http://vocab.getty.edu/aat/")
      ]
    -- if there is no mach then we mark current E55 as belonging to pharos types vocabulary
    _ -> [P71i --> (E32, constUri "https://artresearch.net/resource/pharos/vocab/types")]

a300Table :: LocationReconciliationHash
a300Table = unsafePerformIO $ loadLocationReconciliationHash
    "datasets/frick/resources/types/a300_a.csv"
    ["value"]
    ["exactMatch", "broadMatch"]
{-# NOINLINE a300Table #-}