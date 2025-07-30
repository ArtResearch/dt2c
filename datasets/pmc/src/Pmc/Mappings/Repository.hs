module Pmc.Mappings.Repository where

import CommonImports
import qualified Vocabularies.PHAROS as P
import Pharos.CidocCrm.Patterns (appellation_0_1)
import Pmc.Mappings.NamedUris (work)

import qualified Data.Text as T
import System.IO.Unsafe (unsafePerformIO)
import Pharos.Utils.CsvHashTable (LocationReconciliationHash, loadLocationReconciliationHash, lookupLocation)


repositoryLinks :: [PathTree E22_]
repositoryLinks = [repositoryLink]

repositoryLink =
  [x|lido:descriptiveMetadata/lido:objectIdentificationWrap/lido:repositoryWrap/lido:repositorySet|] @> [
    when (not_ $ exists [x|lido:repositoryLocation/lido:namePlaceSet|]) (
      [x|lido:repositoryName/lido:legalBodyName/lido:appellationValue|] @> standaloneRepository P50
    ),

    [x|lido:repositoryLocation/lido:namePlaceSet|] @> (
      P55 ---> (E53, templateUri "place/{name}" [("name", [x|lido:appellationValue/text()|])]) ==> [
        P2 --> (E55, P.geographical_entity),
        [x|../../lido:repositoryName/lido:legalBodyName/lido:appellationValue|] @> repositoryAtPlace P74i,

        SameAs --> (E53, uriFn [x|lido:appellationValue/text()|] reconciledPlaceUri),
        appellation_0_1 P.preferred_name [x|lido:appellationValue/text()|]
      ]
    )
  ]

standaloneRepository p = repository p (templateUri "repository/{id}" [("id", [x|text()|])])

repositoryAtPlace p = repository p (relativeUriT "/repository/{id}" [("id", [x|text()|])])

repository p g =
  p ---> (E39, g) ==> [
    P2 --> (E55, P.repository),
    appellation_0_1 P.preferred_name [x|text()|],
    P50i --> (E22, typedNamedUri work),

    -- repository reconciliation
    SameAs --> (E39, uriMultiExprFn [
        [x|../../../lido:repositoryLocation/lido:namePlaceSet/lido:appellationValue/text()|], 
        [x|text()|]
      ] reconciledRepositoryUri)
  ]

-- Place Reconciliation Cache
reconciledPlaceUri key = unsafePerformIO $ lookupLocation placeTable [key]
{-# NOINLINE reconciledPlaceUri #-}

placeTable :: LocationReconciliationHash
placeTable = unsafePerformIO $ loadLocationReconciliationHash
    "datasets/pmc/resources/places.csv"
    [T.pack "place"]
    [T.pack "tgn_uri", T.pack "wikidata_uri"]
{-# NOINLINE placeTable #-}

-- Repository Reconciliation Cache
reconciledRepositoryUri = unsafePerformIO . lookupLocation repositoryTable
{-# NOINLINE reconciledRepositoryUri #-}

repositoryTable :: LocationReconciliationHash
repositoryTable = unsafePerformIO $ loadLocationReconciliationHash
    "datasets/pmc/resources/actors/repositories.csv"
    [T.pack "city", T.pack "name"]
    [T.pack "wikidata_uri"]
{-# NOINLINE repositoryTable #-}