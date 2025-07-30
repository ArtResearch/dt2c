module Midas.Mappings.Material where

import CommonImports
import qualified Vocabularies.PHAROS as P
import Midas.Mappings.Vocabulary
import Midas.Mappings.Utils.Inheritance

import Pharos.Utils.CsvHashTable (LocationReconciliationHash, loadLocationReconciliationHash)
import GHC.IO (unsafePerformIO)

materials :: [PathTree E22_]
materials = 
  typeMapping P45 [x|a5260|] P.material a5260Table

a5260Table :: LocationReconciliationHash
a5260Table = unsafePerformIO $ loadLocationReconciliationHash
    "datasets/midas/resources/types/a5260_enriched.csv"
    ["value"]
    ["exactMatch", "broadMatch", "narrowMatch", "closeMatch", "relatedMatch"]
{-# NOINLINE a5260Table #-}
