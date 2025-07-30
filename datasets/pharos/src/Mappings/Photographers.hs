module Mappings.Photographers (actorMapping, pharosBaseUri) where

import CommonImports
import qualified Data.Text as T
import qualified Vocabularies.PHAROS as P
import Pharos.CidocCrm.Patterns (appellation_0_1)

pharosBaseUri :: Maybe T.Text
pharosBaseUri = Just "https://artresearch.net/resource/pharos/"

actorMapping :: Mapping 'E39_
actorMapping =
    D E39 [x|/RECORDS/RECORD|] (uriMultiExprFn [[x|normalize-space(PHOTOGRAPHER/text())|], [x|normalize-space(INSTITUTE/text())|], [x|normalize-space(WIKIDATA/text())|], [x|normalize-space(SEPARATE_ENTITY/text())|]] genPhotographerUri)
    +> [ -- Appellation
        [x|SEPARATE_ENTITY|] @> appellation_0_1 P.preferred_name [x|normalize-space(text())|],

        -- Entity Type
        [x|ENTITY_TYPE|] @> (
          P2 --> (E55, templateUri "pharos-meta:{type}" [("type", [x|normalize-space(text())|])])
        ),

        P2 --> (E55, P.photographer),

        -- Group Membership
        [x|POSSIBLE_UMBRELLA_TERM|] @> (
          (P107i, (E74, templateUri "group/photographer/umbrella/{name}" [("name", [x|normalize-space(text())|])])) ==> [
            appellation_0_1 P.preferred_name [x|text()|],
            P2 --> (E55, P.photograph_umbrella_term)
          ]
        )
    ]

genPhotographerUri :: [T.Text] -> T.Text
genPhotographerUri [_, _, wikidata, _] | not (T.isInfixOf ";" wikidata) && not (T.isInfixOf "none" wikidata) && not (T.null wikidata)  = wikidata
genPhotographerUri [_, _, _, name] | not $ T.null name = "actor/photographer/" <> makeUrlFriendly name
genPhotographerUri [photographer, institute, _, _] = "https://artresearch.net/resource/" <> institute <> "/actor/photographer/" <> makeUrlFriendly photographer
genPhotographerUri _ = ""
