module Midas.Mappings.Artist (artistLinks) where

import qualified Data.Text as T
import GHC.IO (unsafePerformIO)
import CommonImports
import Data.Maybe
import Pharos.CidocCrm.Patterns.Appellations
import qualified Debug.Trace as Debug
import qualified Vocabularies.PHAROS as P
import Midas.Mappings.Utils.DateParser (parseSingleUnitToRange, DateRangeResult(..))
import MidasGeo.Authority.Geo (placeId)
import Pharos.Utils.CsvHashTable (LocationReconciliationHash, lookupLocation, lookupValue, loadLocationReconciliationHash)

-- TODO, map event dates from aob30/a3496, they first need to be normalized with LLM
artistLinks :: [PathTree E22_]
artistLinks = [
    P108i ---> (E12, relativeUri "/production") ==> [
        [x|aob30|] @> (
          P9 ---> (E7, relativeUriT "/aob30/{i}" [("i", i)]) ==> [
            [x|tokenize(@modifier, '&')|] @> (
              P2 ---> (E55, templateUri "vocab/aob30_event_type/{modifier}" [("modifier", [x|normalize-space(text())|])]) ==> [
                appellation_0_1 P.preferred_name [x|text()|]
              ]
            ),

            when (or_ [exists [x|a3100|], exists [x|a3000|]]) (
              P14 ---> (E39, uriMultiExprFn [[x|a3100/text()|], [x|a3000/text()|]] midasArtistUri) ==> [
                appellation_0_1 P.preferred_name [x|a3100/text()|],

                -- alternative artist names a separated by '&'
                -- e.g Adamo Tedesco & Ehlsheimer, Adam & Elzheimer, Adam
                [x|tokenize(a3105/text(), '&')|] @> appellation_0_N P.alternative_name [x|normalize-space(text())|],

                -- reconliliation to ULAN/Wikidata/VIAF/GND
                SameAs --> (E39, uriMultiExprFn [[x|a3100/text()|], [x|a3000/text()|]] reconciledArtistUri)
              ]
            ),

            -- dates
            [x|a3496|] @> (
              [x|tokenize(text(), '( & )|( / )')|] @> mapDate
            )
          ]
        ),

        -- Various Events
        [x|a5060|] @> (
          P9 ---> (E7, relativeUriT "/a5060/{i}" [("i", i)]) ==> [
            [x|tokenize(@modifier, '&')|] @> (
              P2 ---> (E55, templateUri "vocab/a5060_event_type/{modifier}" [("modifier", [x|normalize-space(text())|])]) ==> [
                appellation_0_1 P.preferred_name [x|normalize-space(text())|]
              ]
            ),

            -- dates
            [x|a5064|] @> (
              [x|tokenize(text(), '( & )|( / )')|] @> mapDate
            )
            ]
        ),

        -- production place
        [x|a5140|] @> (
          P9 ---> (E7, relativeUriT "/a5140/{i}" [("i", i)]) ==> [
            productionPlace
          ]
        )
      ]
  ]

productionPlace :: XMLNode -> PathTree E7_
productionPlace node =
  let (maybeGeoId, geoUri) = placeId [x|a54na/text()|] [x|a5145/text()|] node
  in
    P7 ---> (E53, geoUri) ==> [
      -- if there is no geo id then we need to generate appellation for the place here
      if isNothing maybeGeoId then
        appellation_0_1 P.preferred_name [x|a5145/text()|]
      else NullTree
    ]

mapDate :: XMLNode -> [PathTree E7_] -- Changed signature
mapDate node =
  let dateString = nodeText node
  in
    case parseSingleUnitToRange dateString of
      Left err -> unsafePerformIO $ do
        -- Updated error message to reflect parsing or validation errors
        Debug.traceIO $ "Can't parse or validate date: " <> T.unpack dateString <> " Error: " <> err
        return [NullTree] -- Return list with NullTree
      Right dateRange ->
        -- Construct PathTree and wrap in a list
        [ P4 ---> (E52, relativeUriT "/date/{i}" [("i", i)]) ==> [
            P82a --> dateTime (rangeStart dateRange) DateTime,
            P82b --> dateTime (rangeEnd dateRange) DateTime,
            appellation_0_1 P.preferred_name dateString
          ]
        ]

midasArtistUri :: [T.Text] -> T.Text
midasArtistUri [_, kue] | not $ T.null kue = "artist/" <> kue
midasArtistUri [name, _] | not $ T.null name =
  case lookupKueForName name of
    Just kue -> "artist/" <> kue
    Nothing -> "artist/" <> makeUrlFriendly name
midasArtistUri _ = error "should not happen"

-- Artist KUE ID Cache
lookupKueForName name = lookupValue kueTable [name]
{-# NOINLINE lookupKueForName #-}

kueTable :: LocationReconciliationHash
kueTable = unsafePerformIO $ loadLocationReconciliationHash
    "datasets/midas/resources/actors/artists.csv"
    [T.pack "name"]
    [T.pack "kue_id"]
{-# NOINLINE kueTable #-}


-- Artist Reconciliation Cache
reconciledArtistUri = unsafePerformIO . lookupLocation artistTable
{-# NOINLINE reconciledArtistUri #-}

artistTable :: LocationReconciliationHash
artistTable = unsafePerformIO $ loadLocationReconciliationHash
    "datasets/midas/resources/actors/artists.csv"
    [T.pack "name", T.pack "kue_id"]
    [T.pack "ulan", T.pack "wikidata", T.pack "viaf", T.pack "gnd"]
{-# NOINLINE artistTable #-}
