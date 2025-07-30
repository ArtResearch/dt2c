module Pharos.Datasets.Frick.Mappings.Repositories  where

import qualified Data.Text as T
import CommonImports
import qualified Vocabularies.PHAROS as P
import Pharos.Datasets.Frick.Mappings.Vocabulary
import Pharos.CidocCrm.Patterns (appellation_0_1)
import Pharos.Datasets.Frick.Mappings.NamedUris (work)
import Pharos.Utils.CsvHashTable (LocationReconciliationHash, lookupLocation, loadLocationReconciliationHash)
import GHC.IO (unsafePerformIO)

-- | Repository links
repositoryLinks :: [PathTree E22_]
repositoryLinks = [place]

-- Defines the hierarchical structure for repository locations and links the repository
-- to the deepest available geographical entity.
-- The MARC subfield codes used are:
--   'd' for Country
--   'c' for Region
--   'b' for City
--   'a' for Repository Name
--
-- The function handles the following cases for linking the repository:
--
-- Case 1: No Country, No Region, No City
--   └── Repository (a) (standalone)
--
-- Case 2: No Country, No Region, BUT City exists
--   └── City (b)
--       └── Repository (a)
--
-- Case 3: No Country, BUT Region exists
--   └── Region (c)
--       ├── City (b) (if present as sibling to Region)
--       │   └── Repository (a)
--       └── Repository (a) (if City is not present as sibling to Region)
--
-- Case 4: Country exists
--   └── Country (d)
--       ├── Region (c) (if present as sibling to Country)
--       │   ├── City (b) (if present as sibling to Region)
--       │   │   └── Repository (a)
--       │   └── Repository (a) (if City is not present as sibling to Region)
--       ├── City (b) (if present as sibling to Country, and Region is not)
--       │   └── Repository (a)
--       └── Repository (a) (if neither Region nor City are present as siblings to Country)
--
-- The repository (linked via P50 or P74i to an E39 Actor) is associated with the E22 Work (via P50i).
place :: PathTree E22_
place = 
  [x|datafield[@ind1='1' and @ind2=' ' and @tag='590']|] @> [
    -- Case 1: No Country, No Region, No City.
    when (and_ [
      not_ (exists countryXpath),
      not_ (exists regionXpath),
      not_ (exists cityXpath)
    ]) (
      -- Repository stands alone.
      repositoryXpath @> standaloneRepository
    ),

    -- Case 2: No Country, No Region, BUT City exists.
    when (and_ [
      not_ (exists countryXpath),
      not_ (exists regionXpath)
    ]) (
      -- Repository is in City.
      cityXpath @> (
        P55 ---> (E53, absolutePlaceUri P.city) ==> [
          P2 --> (E55, P.city),
          placeName,
          relativeRepository -- Link repository to City
        ]
      )
    ),

    -- Case 3: No Country, BUT Region exists.
    when (not_ (exists countryXpath)) (
      regionXpath @> ( -- Context is the Region subfield
        P55 ---> (E53, absolutePlaceUri P.region) ==> [
          P2 --> (E55, P.region),
          placeName,

          -- Case 3.1: Region exists, AND City exists (as sibling to Region).
          -- The 'parentOf cityXpath' refers to a city subfield that is a sibling of the current region subfield.
          parentOf cityXpath @> ( -- Context shifts to the City subfield
            P89i ---> (E53, relativePlaceUri P.city) ==> [
              P2 --> (E55, P.city),
              placeName,
              relativeRepository -- Link repository to City (within Region)
            ]
          ),

          -- Case 3.2: Region exists, BUT City does NOT exist (as sibling to Region).
          when (not_ (exists (parentOf cityXpath))) (
            -- The 'parentOf repositoryXpath' refers to a repository subfield that is a sibling of the current region subfield.
            parentOf repositoryXpath @> relativeRepository -- Link repository to Region
          )
        ]
      )
    ),

    -- Case 4: Country exists.
    countryXpath @> ( -- Context is the Country subfield
      P55 ---> (E53, absolutePlaceUri P.country) ==> [
        P2 --> (E55, P.country),
        placeName,

        -- country reconciliation
        SameAs --> (E53, uriFn [x|marc-value()|] reconciledCountryUri),

        -- Case 4.1: Country AND Region exist.
        -- The 'parentOf regionXpath' refers to a region subfield that is a sibling of the current country subfield.
        parentOf regionXpath @> ( -- Context shifts to the Region subfield
          P89i ---> (E53, relativePlaceUri P.region) ==> [
            P2 --> (E55, P.region),
            placeName,

            -- region reconciliation
            SameAs --> (E53, uriMultiExprFn [[x|../subfield[@code='d']/marc-value()|], [x|marc-value()|]] reconciledRegionUri),

            -- Case 4.1.1: Country, Region, AND City exist.
            -- The 'parentOf cityXpath' here refers to a city subfield that is a sibling of the current region subfield.
            parentOf cityXpath @> city, -- Context shifts to the City subfield

            -- Case 4.1.2: Country AND Region exist, BUT City does NOT exist (as sibling to the current region).
            when (not_ (exists (parentOf cityXpath))) relativeRepository -- Link repository to Region
          ]
        ),

        -- Case 4.2: Country exists, Region does NOT exist (as sibling to country), BUT City exists (as sibling to country).
        -- The 'parentOf cityXpath' here refers to a city subfield that is a sibling of the current country subfield.
        when (not_ (exists (parentOf regionXpath))) (
          parentOf cityXpath @> city  -- Context is the City subfield (sibling of country)
        ),

        -- Case 4.3: Country exists, BUT Region does NOT exist AND City does NOT exist (as siblings to country).
        when (and_ [
          not_ (exists (parentOf regionXpath)),
          not_ (exists (parentOf cityXpath))
        ]) relativeRepository -- Link repository to Country
      ]
    )
  ]
  where
    countryXpath = [x|subfield[@code='d']|]
    regionXpath = [x|subfield[@code='c']|]
    cityXpath = [x|subfield[@code='b']|]
    repositoryXpath = [x|subfield[@code='a']|]

    relativePlaceUri typ = relativeUriT ("/" <> localName typ <> "/{location_id}") [("location_id", [x|marc-value()|])]
    absolutePlaceUri typ = templateUri ("place/" <> localName typ <> "/{location_id}") [("location_id", [x|marc-value()|])]

    placeName = appellation_0_1 P.preferred_name [x|marc-value()|]

    -- repository without any associated place
    standaloneRepository = repositoryActor P50 (templateUri "actor/repository_actor/{x}" [("x", [x|marc-value()|])])

    -- repository relative to some place
    relativeRepository =
      parentOf repositoryXpath @>
        repositoryActor P74i (relativeUriT "/actor/repository_actor/{x}" [("x", [x|marc-value()|])])

    city =
      P89i ---> (E53, relativePlaceUri P.city) ==> [ -- City is part of Country
        P2 --> (E55, P.city),
        placeName,

        -- city reconciliation
        SameAs --> (E53, uriMultiExprFn [[x|../subfield[@code='d']/marc-value()|], [x|../subfield[@code='c']/marc-value()|], [x|marc-value()|]] reconciledCityUri),

        relativeRepository -- Link repository to City
      ]

repositoryActor p generator =
  p ---> (E39,  generator) ==> [
    P2 --> (E55, repository_actor),
    P50i --> (E22, typedNamedUri work),

    -- repository reconciliation
    SameAs --> (E39, uriMultiExprFn [
        [x|../subfield[@code='d']/marc-value()|], 
        [x|../subfield[@code='c']/marc-value()|], 
        [x|../subfield[@code='b']/marc-value()|], 
        [x|marc-value()|]
      ] reconciledRepositoryUri),
    appellation_0_1 P.preferred_name [x|marc-value()|]
  ]


-- City Reconciliation Cache
reconciledCountryUri key = unsafePerformIO $ lookupLocation countryTable [key]
{-# NOINLINE reconciledCountryUri #-}

countryTable :: LocationReconciliationHash
countryTable = unsafePerformIO $ loadLocationReconciliationHash
    "datasets/frick/resources/places/countries.csv"
    [T.pack "country"]
    [T.pack "tgn_uri", T.pack "wikidata_uri"]
{-# NOINLINE countryTable #-}

-- Region Reconciliation Cache
reconciledRegionUri = unsafePerformIO . lookupLocation regionTable
{-# NOINLINE reconciledRegionUri #-}

regionTable :: LocationReconciliationHash
regionTable = unsafePerformIO $ loadLocationReconciliationHash
    "datasets/frick/resources/places/regions.csv"
    [T.pack "country", T.pack "region"]
    [T.pack "tgn_uri", T.pack "wikidata_uri"]
{-# NOINLINE regionTable #-}

-- City/Town Reconciliation Cache
reconciledCityUri = unsafePerformIO . lookupLocation cityTable
{-# NOINLINE reconciledCityUri #-}

cityTable :: LocationReconciliationHash
cityTable = unsafePerformIO $ loadLocationReconciliationHash
    "datasets/frick/resources/places/cities.csv"
    [T.pack "country", T.pack "region", T.pack "city"]
    [T.pack "tgn_uri", T.pack "wikidata_uri"]
{-# NOINLINE cityTable #-}


-- Repository Reconciliation Cache
reconciledRepositoryUri = unsafePerformIO . lookupLocation repositoryTable
{-# NOINLINE reconciledRepositoryUri #-}

repositoryTable :: LocationReconciliationHash
repositoryTable = unsafePerformIO $ loadLocationReconciliationHash
    "datasets/frick/resources/actors/repositories.csv"
    [T.pack "country", T.pack "region", T.pack "city", T.pack "name"]
    [T.pack "wikidata_uri"]
{-# NOINLINE repositoryTable #-}