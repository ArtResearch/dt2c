module Pmc.Mappings.Photo (photoLinks) where

import qualified Data.Text as T
import CommonImports
import qualified Vocabularies.PHAROS as P
import Pharos.CidocCrm.Patterns.Appellations (appellation_0_1)
import qualified Pmc.Mappings.Vocabulary as PMC
import Authority.Photographers (photographerUri)

photoLinks :: [PathTree E22_]
photoLinks = [photo]

photo :: PathTree E22_
photo =
  [x|lido:administrativeMetadata/lido:resourceWrap/lido:resourceSet|] @>
    when isFirst (
      [x|lido:resourceRepresentation|] @>
        when (equals [x|@lido:type|] "thumb") (
          P138i ---> (E36, relativeUri "/visual_item") ==> [
            P65i ---> (E22, relativeUri "/photo") ==> [
              P2 --> (E55, P.photographic_print),
              P70i --> (E31, constUri ("https://artresearch.net/resource/e31/pmc")),

              -- photographer
              [x|../../../../lido:descriptiveMetadata/lido:eventWrap/lido:eventSet/lido:event|] @> (
                when (and_ [
                  equals [x|lido:eventType/lido:term/text()|] "Acquisition",
                  equals [x|lido:eventActor/lido:actorInRole/lido:roleActor/lido:term/text()|] "Photograph Source"
                ]) (
                  [x|lido:eventActor/lido:actorInRole/lido:actor|] @> (
                    P108i ---> (E12, relativeUri "/production") ==> [
                      P14 ---> (E39, templateUri "actor/photographer/{name}" [("name", [x|lido:nameActorSet/lido:appellationValue[@lido:pref='preferred']/text()|])]) ==> [
                        P2 --> (E55, P.photographer),
                        appellation_0_1 P.preferred_name [x|lido:nameActorSet/lido:appellationValue[@lido:pref='preferred']/text()|],
                        SameAs --> (E39, uriFn [x|lido:nameActorSet/lido:appellationValue[@lido:pref='preferred']/text()|] (photographerUri "pmc"))
                      ]
                    ]
                  )
                )
              ),

              -- current keeper for all PMC photographs is PMC, but just in case we don't hard-code it here
              -- but use actual values from the data
              [x|../../../lido:rightsWorkWrap/lido:rightsWorkSet|] @> [
                [x|lido:rightsHolder|] @> (
                  P50 ---> (E39, uriFn [x|lido:legalBodyID/text()|] ("http://vocab.getty.edu/ulan/" <>)) ==> [
                    P1 ---> (E41, relativeUri "/name") ==> [
                      P2 --> (E55, P.preferred_name),
                      P190 --> literal [x|lido:legalBodyName/lido:appellationValue/text()|]
                    ]
                  ]
                ),

                [x|lido:rightsType/lido:term|] @> [
                  -- if rights term starts with http then we assume it is URI
                  when (starts [x|text()|] "http") (
                    P104 ---> (E30, relativeUri "/rights") ==> [
                      P2 ---> (E55, uriFn [x|text()|] (T.replace "https://" "http://")) ==> [
                        P2 --> (E55, P.license_type)
                      ]
                    ]
                  ),

                  -- PMC had two cases of non URI copyright: "Crown Copyright"
                  when (not_ $ starts [x|text()|] "http") (
                    P104 ---> (E30, relativeUri "/rights") ==> [
                      P2 ---> (E55, templateUri "copyright/{v}" [("v", [x|text()|])]) ==> [
                        appellation_0_1 P.preferred_name [x|text()|],
                        P2 --> (E55, P.license_type)
                      ]
                    ]
                  )
                ]
              ],

              P138i ---> (E36, relativeUri "/visual_item") ==> [
                P165i ---> (D1, relativeUri "/image") ==> [
                  P2 --> (E55, P.digital_image),
                  P1 ---> (E42, uriFn [x|normalize-space(lido:linkResource/text())|] createFullImageUrl) ==> [
                    P2 --> (E55, P.photo_file_url),
                    ImageStorageId --> literal ("pmc" :: T.Text)
                  ]
                ]
              ]
            ]
          ]
        ) 
    )

createFullImageUrl = T.replace "300%2C" "full"