{-# LANGUAGE FlexibleContexts #-}

module Mappings.Mappings.Work.Production where

import CommonImports
import Mappings.Mappings.Artist (artistTrees)
import Mappings.NamedUris (production)
import Mappings.Mappings.Work.Dates (mapProductionDate)

-- | Zeri production mapping
productionLinks :: PathTree E22_
productionLinks =
    P108i ---> (E12, typedNamed production (relativeUri "/production")) ==>
        artistTrees ++ [
            dateLinks
        ]

-- production date
dateLinks :: PathTree E12_
dateLinks = [x|PARAGRAFO[@etichetta='DATING']|] @> mapProductionDate
