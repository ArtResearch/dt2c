module Midas.Mappings.Location where

import qualified Data.Text as T
import Data.Char (isDigit)
import Data.Maybe
import CommonImports
import qualified Vocabularies.PHAROS as P
import Pharos.CidocCrm.Patterns.Appellations
import Pharos.CidocCrm.Patterns.Types

import Midas.Mappings.NamedUris
import MidasGeo.Authority.Geo (placeId, lookupPlaceReconciliation)

import Pharos.Utils.CsvHashTable (LocationReconciliationHash, lookupLocation, loadLocationReconciliationHash)
import GHC.IO (unsafePerformIO)

--
-- currently we run this mapping only for top level works.
--
-- TODO check if we have works where a2740, a2750, a2780 are defined in nested works
-- and refer to the position in parent work 
locationLinks :: [PathTree E22_]
locationLinks = [
    -- object location
    [x|aob26|] @> place,

    -- architectural object location
    [x|a5108|] @> architecturePlace,

    -- part of relationship
    [x|a5007|] @> when (equals [x|@modifier|] "Teil von") (
      [x|a5008|] @> (P46i ---> (E22, templateUri "work/{work_id}" [("work_id", [x|text()|])]) ==> [
        -- when we have related object ID we need to make sure that if it is a reconciled work
        -- then is defined in other dataset that we properly rewrite URIs 
        -- (that is triggered by ObjectSameAs property)
        ObjectSameAs --> (E22, uriFn [x|text()|] reconciledWorkUri)
      ])
    )
  ]

architecturePlace :: XMLNode -> [PathTree E22_]
architecturePlace node = 
  let (maybeGeoId, geoUri) = placeId [x|a51na/text()|] [x|text()|] node
  in
    [
      P55 ---> (E53, geoUri) ==> [
        P2 --> (E55, P.geographical_entity),
        -- geo_id based reconciliation
        SameAs --> (E53, uriFn [x|text()|] (lookupPlaceReconciliation maybeGeoId)),
        -- if there is no geo id then we need to generate appellation for the place here
        if isNothing maybeGeoId then
          appellation_0_1 P.preferred_name [x|text()|]
        else NullTree
      ]
    ]

place :: XMLNode -> [PathTree E22_]
place node = 
  let (maybeGeoId, geoUri) = placeId [x|a26na/text()|] [x|a2664/text()|] node
  in
    [
      when (equals [x|@modifier|] "Standort") (
        when (exists [x|a2664|]) (
          P55 ---> (E53, geoUri) ==> [
            P2 --> (E55, P.geographical_entity),
             -- geo_id based reconciliation
            SameAs --> (E53, uriFn [x|a2664/text()|] (lookupPlaceReconciliation maybeGeoId)),
            -- if there is no geo id then we need to generate appellation for the place here
            if isNothing maybeGeoId then
              appellation_0_1 P.preferred_name [x|normalize-space(a2664/text())|]
            else NullTree
          ]
        )
      ),

      when (equals [x|@modifier|] "Standort") (
        -- a2606 - Bez-Objekt-Nr. (Related Object Number)
        when (or_ [exists [x|a2606|], exists [x|a2664|], exists [x|a2700|], exists [x|a2690|]]) (
          P0 ---> (E22, uriMultiExprFn [[x|a2606/text()|], [x|a2664/text()|], [x|a2700/text()|], [x|a2690/text()|]] builtworkUri) ==> [
            P2 --> (E55, P.built_work),

            -- when we have related object ID we need to make sure that if it is a reconciled work
            -- then is defined in other dataset that we properly rewrite URIs 
            -- (that is triggered by ObjectSameAs property)
            [x|a2606|] @> (ObjectSameAs --> (E22, uriFn [x|text()|] reconciledWorkUri)),

            -- a2690 - Sachbegriff (Object Type for Builtwork)
            [x|a2690|] @> e55_type "builtwork_type", 
            
            -- Bauwerk-Name (Building Name)
            [x|a2700|] @> appellation_0_1 P.preferred_name [x|normalize-space(text())|],

            when (exists [x|a2664|]) (
              -- location (city) of the builtwork
              P55 --> (E53, geoUri)
            ),

            -- builtwork occupies some place (we can use it later to store builtwork spatial coordinates)
            P156 ---> (E53, relativeUri "/place") ==> [
              -- this place falls within a city
              when (exists [x|a2664|]) (
                P89 --> (E53, geoUri)
              ),

              -- builtwork place contains place occupied by builtwork part (see it's mapping bellow)
              when (or_ [exists [x|a2740|], exists [x|a2750|]]) (
                P89i --> (E53, relativeUriT "/part/{name}_{type}/place" [("name", [x|a2750/text()|]), ("type", [x|a2740/text()|])])
              )
            ],

            -- a2740 - Teilbau-Sachbegr. (Object Type for the Part of the Building)
            -- Categorizes the specific type of the part of the building where the object is located (e.g., "Raum," "Kapelle").
            -- 
            -- a2750 - Teilbauwerk-Name (Name of the Part of the Building)
            -- Names the specific part of the building (e.g., "Sala del Mascherone").
            when (or_ [exists [x|a2740|], exists [x|a2750|]]) (
              P46 ---> (E22, relativeUriT "/place/part/{name}_{type}" [("name", [x|a2750/text()|]), ("type", [x|a2740/text()|])]) ==> [
                P2 --> (E55, P.built_work_part),
                [x|a2750|] @> appellation_0_1 P.preferred_name [x|normalize-space(text())|],
                [x|a2740|] @> e55_type "builtwork_part_type",

                -- main work is part of this specific builtwork part
                P46 --> (E22, typedNamedUri work),

                P156 ---> (E53, relativeUri "/place") ==> [

                  -- a2780 - T-B-Stelle (Placement of the Building Part)
                  -- Pinpoints the exact location within the named part (defined by a2740/a2750) 
                  -- where the object is found (e.g., "Ostwand," "Kuppelpendentifs").
                  [x|a2780|] @> (
                    P89i ---> (E53, relativeUriT "/sub-place/{name}" [("name", [x|normalize-space(text())|])]) ==> [
                      appellation_0_1 P.preferred_name [x|normalize-space(text())|],

                      -- location of the work
                      P55i --> (E22, typedNamedUri work)
                    ]
                  )
                ]
              ]
            ),

            -- main work is part of the builtwork
            when (not_ $ or_ [exists [x|a2740|], exists [x|a2750|]]) (
              P46 --> (E22, typedNamedUri work)
            )
          ]
        )
      )
    ]  

builtworkUri :: [T.Text] -> T.Text
-- if we have a2606 (Related Object Number) then we can directly build builtwork URI
-- TODO, we have cases with multiple obejct id's in a2606, what to do with them
-- e.g. in KHI oai_obj_07990003.khi.xml
--
-- Hertziana also has invalid a2606 values like "Palazzina di Pio IV"
builtworkUri (a2606 : _) | (not $ T.null a2606) && (T.all isDigit a2606) = "work/" <> a2606
-- otherwise we construct it based on combination of a2664, a2700, a2690
builtworkUri (_ : rest) | any (not . T.null) rest = 
  "work/" <> (makeUrlFriendly $ T.intercalate "_" (map T.toLower $ filter (not . T.null) rest))
-- TODO think on how we can produce relative URI with uriMultiExprFn
builtworkUri _ = "work/undefined"


-- Artist Reconciliation Cache
reconciledWorkUri objectId = unsafePerformIO $ lookupLocation workTable [objectId]

workTable :: LocationReconciliationHash
workTable = unsafePerformIO $ loadLocationReconciliationHash
    "datasets/midas/resources/objects/reconciliations.csv"
    [T.pack "id"]
    [T.pack "uri"]
{-# NOINLINE workTable #-}
