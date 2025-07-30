{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Mappings.Mappings.Work.Location where

import CommonImports
import qualified Vocabularies.PHAROS as P

import qualified Data.Text as T
import Data.List (find)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv as Csv
import qualified Data.Vector as V
import qualified Data.Vector.Hashtables as HT
import qualified Data.Vector.Mutable as VM
import Control.Monad.Primitive (PrimState)
import System.IO.Unsafe (unsafePerformIO)
import GHC.Generics (Generic)
import Mappings.Vocabulary
import Pharos.CidocCrm.Patterns (appellation_0_1)
import Mappings.NamedUris (work, place)
import Data.Maybe (fromMaybe)
import Pharos.Utils.CsvHashTable (LocationReconciliationHash, loadLocationReconciliationHash, lookupLocation, resolveFilePath)


-- At the moment we support the following comination of places:
--
-- PVCS (Country)
-- ├── PVCR (Region/Federal State)
-- │   ├── PVCP (District)
-- │   │   ├── PVCC (Town/Municipality)
-- │   │   │   └── PVCL (Village)
-- │   │   └── PVCL (Village)
-- │   └── PVCC (Town/Municipality)
-- │       └── PVCL (Village)
-- └── PVCC (Town/Municipality)
--
-- The deepest entity is specified as E22 Work location using P55i_currently_holds.
location :: PathTree E22_
location =
  [x|PARAGRAFO[@etichetta="LOCATION"]|] @> [
    -- Repository
    [x|LDCN|] @> repository,

    -- we back-link the most specific place as a location of the work using P55i_currently_holds
    -- we do this only if LDCN (Repository) is not specified, otherwise repository location is
    -- the location of the work
    when (not_ $ exists [x|LDCN|]) (placeLink workLink)
  ]

placeLink entityLink =
  -- Country
  [x|PVCS|] @> (
    P0 ---> (E53, typedNamed place (templateUri ("place/" <> localName P.country <> "/{location_id}") [("location_id", [x|text()|])])) ==> [
      P2 --> (E55, P.country),
      placeName,

      -- country reconciliation
      SameAs --> (E53, uriFn [x|normalize-space(text())|] reconciledCountryUri),

      -- Region/Federal State (PVCR)
      [x|../PVCR|] @> (
        P89i ---> (E53, typedNamed place (relativePlaceUri Mappings.Vocabulary.region)) ==> [
          P2 --> (E55, Mappings.Vocabulary.region),
          placeName,

          -- region reconciliation
          SameAs --> (E53, uriMultiExprFn [[x|../PVCS/text()|], [x|text()|]] reconciledRegionUri),


          -- District (PVCP)
          [x|../PVCP|] @> (
              P89i ---> (E53, typedNamed place (relativePlaceUri district)) ==> [
                P2 --> (E55, district),
                placeName,

                SameAs --> (E53, uriMultiExprFn [[x|../PVCS/text()|], [x|../PVCR/text()|], [x|text()|]] reconciledDistrictUri),

                -- Town/Municipality (PVCC)
                townLink,

                -- When we have a Village (PVCL) without a Town (PVCC)
                when (not_ $ exists [x|../PVCC|]) villageLink,

                -- if there is no Village (PVCL) and Town/Municipality (PVCC) then 
                -- make District the current location of the work
                when (and_ [ not_ $ exists [x|../PVCC|], not_ $ exists [x|../PVCL|]]) entityLink
              ]
            ),

          -- When we have a Town/Municipality (PVCC) without a District (PVCP)
          when (not_ $ exists [x|../PVCP|]) townLink,

          -- if there is no District (PVPC) and Town/Municipality (PVCC) then 
          -- make Region/Federal State (PVCR) a current location of the work
          when (and_ [not_$ exists [x|../PVCP|], not_ $ exists [x|../PVCC|]]) entityLink
        ]),

      -- When we have a Town/Municipality (PVCC) without a Region/Federal State (PVCR)
      when (not_ $ exists [x|../PVCR|]) townLink,

      -- if there is no Region/Federal State (PVCR), District (PVCP) and Town/Municipality (PVCC) then 
      -- make Country (PVCS) a current location of the work
      when (and_ [not_ $ exists [x|../PVCR|], not_ $ exists [x|../PVCP|], not_ $ exists [x|../PVCC|]]) entityLink
    ]
  )
  where
    -- Common URI generation for place levels
    relativePlaceUri typ = relativeUriT ("/" <> localName typ <> "/{location_id}") [("location_id", [x|normalize-space(text())|])]

    -- Common P1_is_identified_by (appellation) pattern for place names
    placeName = appellation_0_1 P.preferred_name [x|normalize-space(text())|]

    -- Village (PVCL)
    villageLink =
      [x|../PVCL|] @> (
        P89i ---> (E53, typedNamed place (relativePlaceUri village)) ==> [
          P2 --> (E55, village),

          placeName,

          -- village reconciliation
          SameAs --> (E53, uriMultiExprFn [[x|../PVCS/text()|], [x|../PVCR/text()|], [x|../PVCP/text()|], [x|../PVCC/text()|], [x|text()|]] reconciledVillageUri),

          -- make Village (PVCL) a current location of the work
          entityLink
        ]
      )

    -- Town/Municipality (PVCC)
    townLink :: PathTree E53_
    townLink =
      [x|../PVCC|] @> (
        P89i ---> (E53, typedNamed place (relativePlaceUri town)) ==> [
          P2 --> (E55, town),
          placeName,

          -- city reconciliation
          SameAs --> (E53, uriMultiExprFn [[x|../PVCS/text()|], [x|../PVCR/text()|], [x|../PVCP/text()|], [x|text()|]] reconciledCityUri),

          -- Town can contain a Village (PVCL)
          villageLink,

          -- if there is no Villag (PVCL) then 
          -- make Town/Municipality (PVCC) a current location of the work
          when (not_ $ exists [x|../PVCL|]) entityLink
        ]
      )

repository :: XMLNode -> PathTree E22_
repository node =
  let
      repositoryValue = nodeText node
      (maybeType1, maybeType2) = fromMaybe (Nothing, Nothing) (lookupLocationData repositoryValue)
  in case (maybeType1, maybeType2) of
    -- museum/public gallery/cultural institution
    -- e.g 
    -- Anglican Foundation of Garden City
    -- Galleria degli Uffizi
    (Just "M", Nothing) -> [x|../.|] @> repositoryInstitution repositoryValue

    -- art dealer/ antiquarian/ antiques gallery
    (Just "A", Nothing) -> [x|../.|] @> repositoryActor repositoryValue

    -- Auction house
    (Just "AH", Nothing) -> [x|../.|] @> repositoryActor repositoryValue

    -- Collection (intended as a group of objects propriety o a private individual, not a physical place)
    (Just "C", Nothing) -> [x|../.|] @> repositoryCollection repositoryValue


    -- building (churches, palaces, castles...)
    -- e.g:
    -- Arbury Hall
    -- Cathédrale de Notre-Dame
    --
    -- we back-link location occupied by the building as location of the work using P55i_currently_holds
    (Just "B", Nothing) -> [x|../.|] @>
      placeLink (
        P0 ---> (E22, relativeUriT "/building/{x}" [("x", repositoryValue)]) ==> [
          P2 --> (E55, P.built_work),
          appellation_0_1 P.preferred_name repositoryValue,

          -- generate building label + the closest geographical location
          P1 ---> (E41, relativeUri "/appellation/pharos_preferred_name") ==> [
            P2 --> (E55, P.pharos_preferred_name),
            P190 --> literalMultiExprFn
              [[x|../PVCL/text()|], [x|../PVCC/text()|], [x|../PVCP/text()|], [x|../PVCR/text()|], [x|../PVCS/text()|]]
              -- find the first non empty location, and if it exists, prepend it to the name
              (maybe repositoryValue (\loc -> repositoryValue <> ", " <> loc) . find (not . T.null))
          ],

          -- link work to the building
          P46 --> (E22, typedNamedUri work),

          P156 ---> (E53, relativeUri "/place") ==> [
            P89 --> (E53, typedNamedUri place),

            -- place of the building is the current location of the work
            P55i --> (E22, typedNamedUri work)
          ]
        ]
      )

    --building repositoryValue workLink


    -- building in a building
    -- e.g:
    -- Abbazia delle Tre Fontane --> Chiesa di S. Paolo
    (Just "B", Just "B") ->
      let (firstBuildingName, secondBuildingName) = parseTwoRepositoryTypes repositoryValue
      in [x|../.|] @>
        placeLink (
          P0 ---> (E22, relativeUriT "/building/{x}" [("x", firstBuildingName)]) ==> [
            P2 --> (E55, P.built_work),
            appellation_0_1 P.preferred_name firstBuildingName,

            -- generate building label + the closest geographical location
            P1 ---> (E41, relativeUri "/appellation/pharos_preferred_name") ==> [
              P2 --> (E55, P.pharos_preferred_name),
              P190 --> literalMultiExprFn
                [[x|../PVCL/text()|], [x|../PVCC/text()|], [x|../PVCP/text()|], [x|../PVCR/text()|], [x|../PVCS/text()|]]
                -- find the first non empty location, and if it exists, prepend it to the name
                (maybe firstBuildingName (\loc -> firstBuildingName <> ", " <> loc) . find (not . T.null))
            ],

            P46 ---> (E22, relativeUriT "/part/{name}" [("name", secondBuildingName)]) ==> [
              P2 --> (E55, P.built_work_part),
              appellation_0_1 P.preferred_name secondBuildingName,

              -- link work to the building part
              P46 --> (E22, typedNamedUri work),

              P156 --> (E53, relativeUri "/place")
            ],

            P156 ---> (E53, relativeUri "/place") ==> [
              P89 --> (E53, typedNamedUri place),
              P89i --> (E53, relativeUriT "/part/{name}/place" [("name", secondBuildingName)])
            ]
          ]
        )

    _ -> NullTree

repositoryInstitution = repositoryActor_ repository_institution
repositoryActor = repositoryActor_ repository_actor

repositoryActor_ ogranizationType name =
  placeLink (
    P74i ---> (E39,  relativeUriT ("/actor/" <> localName ogranizationType <> "/{x}") [("x", name)]) ==> [
      P2 --> (E55, ogranizationType),
      P50i --> (E22, typedNamedUri work),
      appellation_0_1 P.preferred_name name,

      -- repository reconciliation
      SameAs --> (E53, uriMultiExprFn [
          [x|../PVCS/text()|],
          [x|../PVCR/text()|],
          [x|../PVCP/text()|],
          [x|../PVCC/text()|],
          [x|../PVCL/text()|],
          [x|../LDCN/text()|]
        ] reconciledRepositoryUri)
    ]
  )

repositoryCollection name =
  placeLink (
    P55i ---> (E22, relativeUriT "/collection/{x}" [("x", name)]) ==> [
      P2 --> (E55, repository_actor),
      P46 --> (E22, typedNamedUri work),
      appellation_0_1 P.preferred_name name,

      -- generate collection label + the closest geographical location
      P1 ---> (E41, relativeUri "/appellation/pharos_preferred_name") ==> [
        P2 --> (E55, P.pharos_preferred_name),
        P190 --> literalMultiExprFn
          [[x|../PVCL/text()|], [x|../PVCC/text()|], [x|../PVCP/text()|], [x|../PVCR/text()|], [x|../PVCS/text()|]]
          -- find the first non empty location, and if it exists, prepend it to the name
          (maybe name (\loc -> name <> ", " <> loc) . find (not . T.null))
      ]
    ]
  )


workLink = P55i --> (E22, typedNamedUri work)

-- Helper function to split a text by the first comma and strip whitespace from both parts.
-- Returns (stripped_part_before_comma, stripped_part_after_comma)
-- If no comma, second part is empty. If text before/after comma is only whitespace, it becomes empty.
parseTwoRepositoryTypes :: T.Text -> (T.Text, T.Text)
parseTwoRepositoryTypes inputText =
  let (rawFirst, rawRestWithComma) = T.breakOn "," inputText
      firstValue = T.strip rawFirst
      -- If rawRestWithComma is empty, it means no comma was found.
      -- If it's not empty, it starts with the comma. T.tail removes it.
      secondValue = if T.null rawRestWithComma
                         then T.empty
                         else T.strip (T.tail rawRestWithComma)
  in (firstValue, secondValue)

-- City Reconciliation Cache
reconciledCountryUri key = unsafePerformIO $ lookupLocation countryTable [key]
{-# NOINLINE reconciledCountryUri #-}

countryTable :: LocationReconciliationHash
countryTable = unsafePerformIO $ loadLocationReconciliationHash
    "datasets/zeri/resources/places/countries.csv"
    [T.pack "country"]
    [T.pack "tgn_uri", T.pack "wikidata_uri"]
{-# NOINLINE countryTable #-}

-- Region Reconciliation Cache
reconciledRegionUri = unsafePerformIO . lookupLocation regionTable
{-# NOINLINE reconciledRegionUri #-}

regionTable :: LocationReconciliationHash
regionTable = unsafePerformIO $ loadLocationReconciliationHash
    "datasets/zeri/resources/places/regions.csv"
    [T.pack "country", T.pack "region"]
    [T.pack "tgn_uri", T.pack "wikidata_uri"]
{-# NOINLINE regionTable #-}

-- District Reconciliation Cache
reconciledDistrictUri = unsafePerformIO . lookupLocation districtTable
{-# NOINLINE reconciledDistrictUri #-}

districtTable :: LocationReconciliationHash
districtTable = unsafePerformIO $ loadLocationReconciliationHash
    "datasets/zeri/resources/places/districts.csv"
    [T.pack "country", T.pack "region", T.pack "district"]
    [T.pack "tgn_uri", T.pack "wikidata_uri"]
{-# NOINLINE districtTable #-}

-- City/Town Reconciliation Cache
reconciledCityUri = unsafePerformIO . lookupLocation cityTable
{-# NOINLINE reconciledCityUri #-}

cityTable :: LocationReconciliationHash
cityTable = unsafePerformIO $ loadLocationReconciliationHash
    "datasets/zeri/resources/places/cities.csv"
    [T.pack "country", T.pack "region", T.pack "district", T.pack "city"]
    [T.pack "tgn_uri", T.pack "wikidata_uri"]
{-# NOINLINE cityTable #-}

-- Village Reconciliation Cache
reconciledVillageUri = unsafePerformIO . lookupLocation villageTable
{-# NOINLINE reconciledVillageUri #-}

villageTable :: LocationReconciliationHash
villageTable = unsafePerformIO $ loadLocationReconciliationHash
    "datasets/zeri/resources/places/villages.csv"
    [T.pack "country", T.pack "region", T.pack "district", T.pack "city", T.pack "village"]
    [T.pack "tgn_uri", T.pack "wikidata_uri"]
{-# NOINLINE villageTable #-}

-- Repository Reconciliation Cache
reconciledRepositoryUri = unsafePerformIO . lookupLocation repositoryTable
{-# NOINLINE reconciledRepositoryUri #-}

repositoryTable :: LocationReconciliationHash
repositoryTable = unsafePerformIO $ loadLocationReconciliationHash
    "datasets/zeri/resources/actors/repositories.csv"
    [T.pack "country", T.pack "region", T.pack "district", T.pack "city", T.pack "village", T.pack "name"]
    [T.pack "wikidata_uri"]
{-# NOINLINE repositoryTable #-}

-- Location Type Cache
-- Define the record structure for Cassava, matching CSV columns
-- We are interested in the first three columns: LDCN/PRCD, Type 1, Type 2
data LocationRow = LocationRow
  { lrKey    :: T.Text
  , lrType1  :: Maybe T.Text
  , lrType2  :: Maybe T.Text
  } deriving (Show, Generic)


-- Tell Cassava how to map CSV columns to LocationRow fields
-- We only care about the first 3 columns.
instance Csv.FromRecord LocationRow where
  parseRecord v
    | V.length v >= 3 = LocationRow <$> v Csv..! 0 <*> v Csv..! 1 <*> v Csv..! 2
    | otherwise       = fail $ "CSV row does not have enough columns (expected at least 3, got " ++ show (V.length v) ++ ")"

type LocationCacheValue = (Maybe T.Text, Maybe T.Text)
type LocationHashTable = HT.Dictionary (PrimState IO) VM.MVector T.Text VM.MVector LocationCacheValue

-- Load Location data from CSV into a hashtable
loadLocationData :: FilePath -> IO LocationHashTable
loadLocationData filePath = do
  csvData <- BL.readFile filePath
  -- Estimate size; can be tuned. The CSV has ~12000 rows.
  ht <- HT.initialize 12000

  case Csv.decode Csv.HasHeader csvData of
    Left err -> do
      putStrLn $ "Error parsing CSV (" ++ filePath ++ "): " ++ err
      pure ht -- Return empty or partially filled table on error
    Right (rows :: V.Vector LocationRow) -> do
      V.forM_ rows $ \row -> do
        let key = lrKey row
            val = (lrType1 row, lrType2 row)
        HT.insert ht key val
      pure ht

-- Cached Location data for performance
locationDataCache :: LocationHashTable
locationDataCache = unsafePerformIO $ do
  resolvedPath <- resolveFilePath "datasets/zeri/resources/zeri_lcdn_prcd.csv"
  loadLocationData resolvedPath
{-# NOINLINE locationDataCache #-}

-- Lookup function for location data
-- Returns Maybe (Type1, Type2)
lookupLocationData :: T.Text -> Maybe LocationCacheValue
lookupLocationData key = unsafePerformIO $ HT.lookup locationDataCache (T.strip key)
{-# NOINLINE lookupLocationData #-}
