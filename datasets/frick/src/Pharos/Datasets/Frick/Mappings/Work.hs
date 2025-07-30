module Pharos.Datasets.Frick.Mappings.Work  where

import Prelude
import CommonImports
import qualified Vocabularies.AAT as AAT
import qualified Pharos.Datasets.Frick.Mappings.Vocabulary as FrickMeta
import Data.Text (Text)
import Pharos.Datasets.Frick.Mappings.Identifiers
import Pharos.Datasets.Frick.Mappings.Titles
import Pharos.Datasets.Frick.Mappings.NamedUris (work)
import Pharos.Datasets.Frick.Mappings.Repositories (repositoryLinks)
import Pharos.Datasets.Frick.Mappings.Photo (photoLinks)
import Pharos.Datasets.Frick.Mappings.Artist (productionLinks)
import Pharos.Datasets.Frick.Mappings.Subjects (subjects)
import Pharos.Datasets.Frick.Mappings.Types (types)

import qualified Data.Text as T
import Pharos.Utils.CsvHashTable (LocationReconciliationHash, lookupLocation, lookupValue, loadLocationReconciliationHash)
import GHC.IO (unsafePerformIO)

baseMapping :: D E22_
baseMapping = D E22 [x|/record|]
    (typedNamed work ( templateUri "work/{work_id}" [("work_id", [x|controlfield[@tag='001']|])] ))

-- Base type links that are common to all mappings
baseTypeLinks = [
    P2 --> (E55, AAT.workOfArt),
    P70i --> (E31, constUri "https://artresearch.net/resource/e31/frick"),

    -- we use legacy Frick URIs for reconciliation
    -- because the data is comming from the old system
    [x|datafield[@tag='910']/subfield[@code='a']|] @> (
      ObjectSameAs --> (E22, uriFn [x|@sercdoa|] reconciledWorkUri)
    )
  ]

frickBaseUri :: Maybe Text
frickBaseUri = Just "https://artresearch.net/resource/frick/"

-- Full mapping combining all components
workMapping :: Mapping 'E22_
workMapping =
  baseMapping +>
    baseTypeLinks ++
    identifiers ++
    titles ++
    productionLinks ++
    repositoryLinks ++
    photoLinks ++ 
    subjects ++
    types

-- Artist Reconciliation Cache
reconciledWorkUri objectId = unsafePerformIO $ lookupLocation workTable [objectId]
{-# NOINLINE reconciledWorkUri #-}

workTable :: LocationReconciliationHash
workTable = unsafePerformIO $ loadLocationReconciliationHash
    "datasets/frick/resources/objects/reconciliations.csv"
    [T.pack "id"]
    [T.pack "uri"]
{-# NOINLINE workTable #-}