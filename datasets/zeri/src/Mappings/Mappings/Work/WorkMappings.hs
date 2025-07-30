module Mappings.Mappings.Work.WorkMappings where

import Prelude
import CommonImports
import qualified Vocabularies.AAT as AAT
import Mappings.Mappings.Work.Identifiers
import Mappings.Mappings.Work.Titles
import Mappings.Mappings.Work.MaterialTechnique
import Mappings.Mappings.Work.Production
import Data.Text (Text)
import Mappings.NamedUris (work)
import Mappings.Mappings.Work.Location (location)
import Mappings.Mappings.Work.Photo (photoLink)
import Mappings.Mappings.Work.Subjects (subjectLink)
import Mappings.Mappings.Work.Types (types)

import qualified Data.Text as T
import Pharos.Utils.CsvHashTable (LocationReconciliationHash, lookupLocation, lookupValue, loadLocationReconciliationHash)
import GHC.IO (unsafePerformIO)

baseMapping :: D E22_
baseMapping = D E22 [x|/RISULTATI/SCHEDA|]
    ( typedNamed work (templateUri "work/{work_id}" [("work_id", [x|@sercdoa|])] ))

-- Base type links that are common to all mappings
baseTypeLinks = [
    P2 --> (E55, AAT.workOfArt),
    P70i --> (E31, constUri "https://artresearch.net/resource/e31/zeri"),
    ObjectSameAs --> (E22, uriFn [x|@sercdoa|] reconciledWorkUri)
  ]

zeriBaseUri :: Maybe Text
zeriBaseUri = Just "https://artresearch.net/resource/zeri/"

-- Full mapping combining all components
workMapping :: Mapping 'E22_
workMapping = 
  baseMapping +> 
    baseTypeLinks ++ 
    identifiers ++
    types ++
      [  
          titles,
          materialTechniqueLinks,
          productionLinks,
          location,
          photoLink,
          subjectLink
      ]

-- Artist Reconciliation Cache
reconciledWorkUri objectId = unsafePerformIO $ lookupLocation workTable [objectId]
{-# NOINLINE reconciledWorkUri #-}

workTable :: LocationReconciliationHash
workTable = unsafePerformIO $ loadLocationReconciliationHash
    "datasets/zeri/resources/objects/reconciliations.csv"
    [T.pack "id"]
    [T.pack "uri"]
{-# NOINLINE workTable #-}

