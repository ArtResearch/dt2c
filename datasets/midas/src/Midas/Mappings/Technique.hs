module Midas.Mappings.Technique where

import CommonImports
import qualified Vocabularies.PHAROS as P
import Midas.Mappings.Vocabulary
import Midas.Mappings.Utils.Inheritance

import Pharos.Utils.CsvHashTable (LocationReconciliationHash, loadLocationReconciliationHash)
import GHC.IO (unsafePerformIO)

technique :: [PathTree E22_]
technique = [
    P108i ---> (E12, relativeUri "/production") ==> 
      typeMapping P32 [x|a5300|] P.material a5300Table
  ]

a5300Table :: LocationReconciliationHash
a5300Table = unsafePerformIO $ loadLocationReconciliationHash
    "datasets/midas/resources/types/a5300_enriched.csv"
    ["value"]
    ["exactMatch", "broadMatch", "narrowMatch", "closeMatch", "relatedMatch"]
{-# NOINLINE a5300Table #-}
