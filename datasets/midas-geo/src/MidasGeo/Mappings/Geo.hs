module MidasGeo.Mappings.Geo (geoMapping) where

import qualified Data.Text as T
import CommonImports
import Pharos.CidocCrm.Patterns.Appellations (appellation_0_1)
import Pharos.CidocCrm.Patterns.Identifiers (identifier_0_1)
import MidasGeo.Authority.Geo (lookupGeoId, lookupPlace, lookupRegion, lookupCountry)
import qualified Vocabularies.PHAROS as P
import MidasGeo.Mappings.Vocabulary (geo_id)

baseGeoMapping :: D E53_
baseGeoMapping = D E53 [x|/geo|]
    ( templateUri "place/{id}" [("id", [x|a2000|])])


geoMapping =
  Mapping baseGeoMapping geoLinks

geoLinks :: [PathTree E53_]
geoLinks = [
    [x|a2050|] @> appellation_0_1 P.preferred_name [x|text()|],
    [x|a2000|] @> identifier_0_1 geo_id,

    -- geo_id based reconciliation
    SameAs --> (E53, uriMultiExprFn [[x|a2050/text()|], [x|a2000/text()|]] lookupPlaceReconciliation),

    stateOrLand,
    when (and_ [
      not_ $ exists stateOrLandXpath
    ]) country
  ]
  where
    stateOrLandXpath = [x|a2190|]
    countryXpath = [x|a2194|]
    stateOrLand = 
      stateOrLandXpath @> (
        P89 ---> (E53, uriFn [x|text()|] geoId) ==> [
          P2 --> (E55, P.geographical_entity),
          appellation_0_1 P.preferred_name [x|text()|],
          SameAs --> (E53, uriMultiExprFn [parentOf countryXpath, [x|text()|]] (lookupRegion Nothing))
        ]
      )
    country = 
      countryXpath @> (
        P89 ---> (E53, uriFn [x|text()|] geoId) ==> [
          P2 --> (E55, P.geographical_entity),
          appellation_0_1 P.preferred_name [x|text()|],
          SameAs --> (E53, uriMultiExprFn [[x|text()|]] (lookupCountry Nothing))
        ]
      )

lookupPlaceReconciliation [name, geoid] =
  if T.null geoid then lookupPlace Nothing ["", "", name] else lookupPlace (Just geoid) []
lookupPlaceReconciliation _ = ""

geoId placeName =
  case lookupGeoId placeName of
    Just gid -> "place/" <> gid
    Nothing -> "place/" <> makeUrlFriendly placeName