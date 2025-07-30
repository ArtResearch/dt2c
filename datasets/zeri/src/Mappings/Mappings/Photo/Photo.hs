module Mappings.Mappings.Photo.Photo (photoLinks, photoMapping) where

import CommonImports
import Mappings.Vocabulary
import qualified Vocabularies.PHAROS as P
import qualified Data.Text as T
import Pharos.CidocCrm.Patterns (appellation_0_1, identifier_0_1)
import Authority.Photographers (photographerUri)

baseMapping :: D E22_
baseMapping = D E22 [x|/RISULTATI/SCHEDA|]
    (templateUri "work/{work_id}/visual_item/photo/{photo_id}" [("work_id", [x|@sercdoa|]), ("photo_id", [x|@sercdf|])])

photoMapping :: Mapping 'E22_
photoMapping = 
  baseMapping +> photoLinks

photoLinks :: [PathTree E22_]
photoLinks = [
  P2 --> (E55, P.photographic_print),
  P70i --> (E31, constUri "https://artresearch.net/resource/e31/zeri"),
  P50 --> (E39, constUri "https://artresearch.net/resource/provider/zeri"),

  -- photo inventory number
  [x|PARAGRAFO[@etichetta="CLASSIFICATION"]/INVN|] @> identifier_0_1 inventory_number,

  -- photo identifier
  -- [x|@sercdf|] @> identifier_0_1 photo_identifier,

  -- photo titles
  [x|PARAGRAFO[@etichetta="SOURCE OF TITLE"]/RIPETIZIONE|] @> [
    [x|SGLT|] @> (
      appellation_0_1 proper_title [x|text()|]
        ++> [
          -- Use SGLT (Proper title) as preferred title in pharos
          (P2 --> (E55, P.pharos_preferred_name)) :: PathTree E41_
        ]
    ),
    [x|SGLA|] @> appellation_0_1 attributed_title [x|text()|],
    [x|SGLL|] @> appellation_0_1 parallel_title [x|text()|]
  ],

  [x|ELENCO_FOTO/FOTO|] @> (
    P138i ---> (E36, relativeUri "/visual_item") ==> [
      P165i ---> (D1, relativeUri "/image") ==> [
        P2 --> (E55, P.digital_image),
        P1 ---> (E42, uriFn  [x|text()|] (\v -> "https://iiif.artresearch.net/iiif/3/zeri" <> v <> "/full/full/0/default.jpg")) ==> [
          P2 --> (E55, P.photo_file_url),
          ImageStorageId --> literal ("zeri" :: T.Text)
        ]
      ]
    ]
  ),

  -- photographer
  [x|PARAGRAFO[@etichetta='PHOTOGRAPHERS']/RIPETIZIONE/AUFN|] @> (
    P108i ---> (E12, relativeUri "/production") ==> [
      P14 ---> (E39, templateUri "actor/photographer/{name}" [("name", [x|text()|])]) ==> [
        P2 --> (E55, P.photographer),
        appellation_0_1 P.preferred_name [x|text()|],
        SameAs --> (E39, uriFn [x|text()|] (photographerUri "zeri"))
      ]
    ]
  ),

  P104 ---> (E30, relativeUri "/rights") ==> [
    -- all Zeri images have the same license
    P2 --> (E55, constUri "http://creativecommons.org/licenses/by-nc-nd/4.0/")
  ]

  ]