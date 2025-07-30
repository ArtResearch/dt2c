module Pmc.Mappings.Artist (artistLinks) where

import GHC.IO (unsafePerformIO)
import CommonImports
import qualified Data.Text as T
import qualified Vocabularies.PHAROS as P
import Pharos.CidocCrm.Patterns.Appellations
import Pharos.Utils.CsvHashTable (LocationReconciliationHash, lookupValues, lookupLocation, lookupValue, loadLocationReconciliationHash)

-- for now we don't map artist qualifier like, circle of, etc.
--Unique role terms for 'production' events found:
-- After
-- Artist
-- Attributed To
-- Circle of
-- Copyist of
-- Finished by
-- Follower of
-- Formerly attributed to
-- Possibly by
-- Printmaker
-- Probably by
-- Style of
-- Workshop of
artistLinks :: [PathTree E22_]
artistLinks = [
  [x|lido:descriptiveMetadata/lido:eventWrap/lido:eventSet/lido:event|] @>
    when (equals [x|lido:eventType/lido:conceptID/text()|] "http://terminology.lido-schema.org/eventType/production")
    (
      P108i ---> (E12, relativeUri "/production") ==> [
        [x|lido:eventActor/lido:actorInRole/lido:actor[@lido:type='Individual']|] @> (
          -- we always use local ID for Actor URI, and then add reconciliations
          P14 ---> (E21, templateUri "actor/{id}" [("id", [x|lido:actorID[@lido:source='local']/text()|])]) ==> [

            -- preferred artist name
            -- some records have multiple preferred names, but it looks like that they are just duplicates
            [x|lido:nameActorSet/lido:appellationValue[@lido:pref='preferred']|] @> appellation_0_1 P.preferred_name [x|text()|],

            -- alternative artist name
            [x|lido:nameActorSet/lido:appellationValue[@lido:pref='alternate']|] @> appellation_0_N P.alternative_name [x|text()|],

            -- gender
            [x|lido:genderActor|] @> [
              when (equals [x|text()|] "male") (
                P2 --> (E55, P.male)
              ),
              when (equals [x|text()|] "female") (
                P2 --> (E55, P.female)
              )
            ],

            -- nationality
            -- TODO reconciliation 
            [x|lido:nationalityActor/lido:term|] @> (
              P107i ---> (E74, templateUri "nationality/{name}" [("name", [x|text()|])]) ==> [
                appellation_0_1 P.nationality [x|text()|]
              ]
            ),

            -- reconciliation to ULAN/Wikidata/VIAF/GND
            SameAs --> (E39, uriFn [x|lido:actorID[@lido:source='local']/text()|] reconciledArtistUri)
          ]
        ),

        -- production date
        [x|lido:eventDate/lido:date|] @> (
          when (or_ [
              not_ $ equals [x|lido:earliestDate/text()|] "0", 
              not_ $ equals [x|lido:latestDate/text()|] "0",
              exists [x|../lido:displayDate|]
            ]
          ) (
            P4 ---> (E52, relativeUri "/date") ==> [

              -- sometimes the value can exist is "0", we treat it as no date
              [x|lido:earliestDate|] @> when (not_ $ equals [x|text()|] "0") (
                P82a --> dateTime [x|text()|] (Year, Lower)
              ),

              -- sometimes the value can exist is "0", we treat it as no date
              [x|lido:latestDate|] @> when (not_ $ equals [x|text()|] "0") (
                P82b --> dateTime [x|text()|] (Year, Upper)
              ),

              -- display date in a human readabale format, like
              -- ca. 1805–15, 1644, 1725–27, 17th–18th century
              --
              -- TODO, there are cases where displayDate is "undated" 
              -- but there is earliest and latest. Need to think what to do
              [x|../lido:displayDate|] @> appellation_0_1 P.preferred_name [x|text()|]
            ]
          )
        )
      ]
    )
  ]

-- Artist Reconciliation Cache
reconciledArtistUri artistId = unsafePerformIO $ lookupLocation artistTable [artistId]
{-# NOINLINE reconciledArtistUri #-}

artistTable :: LocationReconciliationHash
artistTable = unsafePerformIO $ loadLocationReconciliationHash
    "datasets/pmc/resources/actors/artists.csv"
    [T.pack "local_id"]
    [T.pack "ulan", T.pack "wikidata", T.pack "viaf"]
{-# NOINLINE artistTable #-}
