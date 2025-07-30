module Midas.Mappings.Types where

import CommonImports
import Midas.Mappings.Vocabulary
import Midas.Mappings.Utils.Inheritance

import qualified Data.Text as T
import Pharos.Utils.CsvHashTable (LocationReconciliationHash, loadLocationReconciliationHash)
import GHC.IO (unsafePerformIO)

types :: [PathTree E22_]
types = 
  typeMapping P2 [x|a5220|] classification a5220Table ++
  typeMapping P2 [x|a5226|] art a5226Table ++
  typeMapping P2 [x|a5230|] object_type a5230Table ++
  typeMapping P2 [x|a5240|] formtyp a5240Table

a5220Table :: LocationReconciliationHash
a5220Table = unsafePerformIO $ loadLocationReconciliationHash
    "datasets/midas/resources/types/a5220_enriched.csv"
    ["value"]
    ["exactMatch", "broadMatch", "narrowMatch", "closeMatch", "relatedMatch"]
{-# NOINLINE a5220Table #-}

a5226Table :: LocationReconciliationHash
a5226Table = unsafePerformIO $ loadLocationReconciliationHash
    "datasets/midas/resources/types/a5226_enriched.csv"
    [T.pack "value"]
    ["exactMatch", "broadMatch", "narrowMatch", "closeMatch", "relatedMatch"]
{-# NOINLINE a5226Table #-}

a5230Table :: LocationReconciliationHash
a5230Table = unsafePerformIO $ loadLocationReconciliationHash
    "datasets/midas/resources/types/a5230_enriched.csv"
    [T.pack "value"]
    ["exactMatch", "broadMatch", "narrowMatch", "closeMatch", "relatedMatch"]
{-# NOINLINE a5230Table #-}

a5240Table :: LocationReconciliationHash
a5240Table = unsafePerformIO $ loadLocationReconciliationHash
    "datasets/midas/resources/types/a5240_enriched.csv"
    [T.pack "value"]
    ["exactMatch", "broadMatch", "narrowMatch", "closeMatch", "relatedMatch"]
{-# NOINLINE a5240Table #-}
