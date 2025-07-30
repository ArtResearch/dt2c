module Midas.Mappings.Repository where

import qualified Data.Text as T
import Data.Maybe
import CommonImports
import qualified Vocabularies.PHAROS as P
import Pharos.CidocCrm.Patterns.Appellations
import Midas.Mappings.NamedUris (work)
import MidasGeo.Authority.Geo (placeId, lookupPlaceReconciliation)
import Pharos.Utils.CsvHashTable (LocationReconciliationHash, lookupLocation, loadLocationReconciliationHash)
import GHC.IO (unsafePerformIO)

repositoryLinks :: [PathTree E22_]
repositoryLinks = [repositoryLink]

repositoryLink :: PathTree E22_
repositoryLink =
  [x|aob28|] @> (
    when (equals [x|@modifier|] "Verwalter") (
      [x|.|] @> [
        [x|a2864|] @> repositoryWithPlace,
        when (not_ $ exists [x|a2864|]) (
          repository P50
        )
      ]
    )
  )

repositoryWithPlace node = 
    let (maybeGeoId, geoUri) = placeId [x|../a28na/text()|] [x|text()|] node
    in
      (
        P55 ---> (E53, geoUri) ==> [
          P2 --> (E55, P.geographical_entity),

          -- geo_id based reconciliation
          SameAs --> (E53, uriFn [x|text()|] (lookupPlaceReconciliation maybeGeoId)),

          [x|../a2900|] @> repository P74i,

          -- if there is no geo id then we need to generate appellation for the place here
          -- otherwise we get it from the geo authority mapping
          if isNothing maybeGeoId then
            appellation_0_1 P.preferred_name [x|normalize-space(text())|]
          else NullTree
        ]
      )

repository p =
  p ---> (E39, relativeUriT "/repository/{id}" [("id", [x|text()|])]) ==> [
    P2 --> (E55, P.repository),
    appellation_0_1 P.preferred_name [x|normalize-space(text())|],
    P50i --> (E22, typedNamedUri work),
    SameAs --> (E39, uriMultiExprFn [
        [x|../a2864/text()|], 
        [x|text()|]
      ] reconciledRepositoryUri)
  ]

-- Repository Reconciliation Cache
reconciledRepositoryUri = unsafePerformIO . lookupLocation repositoryTable
{-# NOINLINE reconciledRepositoryUri #-}

repositoryTable :: LocationReconciliationHash
repositoryTable = unsafePerformIO $ loadLocationReconciliationHash
    "datasets/midas/resources/actors/repositories.csv"
    [T.pack "city", T.pack "name"]
    [T.pack "wikidata_uri"]
{-# NOINLINE repositoryTable #-}