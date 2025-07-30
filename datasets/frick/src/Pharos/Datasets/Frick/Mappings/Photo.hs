module Pharos.Datasets.Frick.Mappings.Photo  where

import CommonImports
import qualified Data.Text as T
import qualified Vocabularies.PHAROS as P
import Pharos.CidocCrm.Patterns.Identifiers
import Pharos.CidocCrm.Patterns.Appellations
import Pharos.Datasets.Frick.Mappings.Vocabulary
import Authority.Photographers (photographerUri)

test = error "not implemented yet"

photoLinks = [
    farlNegative,
    nonFarlPhotographs,
    firstPhotoImage
  ]

-- |
-- Artwork -> Photo -> Image File
--
-- E22 -> P138i -> E38 (work visual item) -> P65i 
--   -> E22 (photo/negative) -> P138i -> E36 (photo/negative visual item) 
--   -> P165i -> D1 (image file)
farlNegative :: PathTree E22_
farlNegative = 
  [x|datafield[@tag='590' and @ind1='9']|] @>
    when (equals [x|subfield[@code="k"]/marc-value()|] farlNegativeStr) (
      -- work has E36_Visual_Item
      P138i ---> (E36, relativeUri "/visual_item") ==> [

        -- Visual Item is P65i_is_shown_by some E22 (photo negative)
        P65i ---> (E22, relativeUriT "/photo/farl_negative/{id}" [("id", [x|subfield[@code="d"]/marc-value()|])]) ==> [
          P2 --> (E55, P.photograph_negative),

          -- we assume that the order in which images are listed in the data
          -- is the preferred order of images for a given work
          ImageOrder --> (typedLiteral i (PrefixedURI "xsd:integer")),

          frickKeeper,
          frickPhotoArchive,
          [x|subfield[@code="d"]|] @> [
            identifier_0_1_marc farl_negative_number,

            -- we assume that all FARL negatives have a corresponding image file              
            -- E22 which is photo negative has its own E36_Visual_Item
            P138i ---> (E36, relativeUri "/visual_item") ==> [
              -- which is crm:P165i_is_incorporated_in some D1_Digital_Object image file
              P165i ---> (D1, relativeUri "/image") ==> [
                P2 --> (E55, P.digital_image),
                P1 ---> (E42, templateUri "https://iiif.artresearch.net/iiif/3/frick/{fileId}_POST.tif/full/full/0/default.jpg" [("fileId", [x|marc-value()|])]) ==> [
                  P2 --> (E55, P.photo_file_url),
                  ImageStorageId --> literal ("frick" :: T.Text)
                ]
              ]
            ]
          ],
          frickRights,

          -- Negative production event
          [x|subfield[@code="b"]|] @> photoProduction
        ]
      ]
    )

-- Transform all non-FARL photographs, even if they don't have corresponding image file.
nonFarlPhotographs :: PathTree E22_
nonFarlPhotographs =  
  [x|datafield[@tag='590' and @ind1='9']|] @>
    when (not_ $ equals [x|subfield[@code="k"]/marc-value()|] farlNegativeStr) (
      P138i ---> (E36, relativeUri "/visual_item") ==> [
        P65i ---> (E22, relativeUriT "/photo/print/{i}" [("i", i)]) ==> [
          P2 --> (E55, P.photographic_print),

          -- we assume that the order in which images are listed in the data
          -- is the preferred order of images for a given work
          ImageOrder --> (typedLiteral i (PrefixedURI "xsd:integer")),

          frickRights,
          frickKeeper,
          frickPhotoArchive,
          [x|subfield[@code="b"]|] @> photoProduction
        ]
      ]
   )

-- |
-- For non FARL negatives. 
-- At the moment there is no way to associate photo with image.
-- But it looks like that first photo object always corresponds to 001 image
-- Image file has 01. in the 590 0 subfield g. 
-- So for now we only map FARL negatives and first photo image.
firstPhotoImage :: PathTree E22_
firstPhotoImage =
  when (and_ [
    not_ $ equals [x|datafield[@tag='590' and @ind1='9']/subfield[@code="k"]/marc-value()|] farlNegativeStr,
    contains [x|datafield[@tag='590' and @ind1='0']/subfield[@code="g"]/marc-value()|] "01."
  ]) (
      P138i ---> (E36, relativeUri "/visual_item") ==> [
        [x|datafield[@tag='590' and @ind1='0']/subfield[@code="g"]|] @>
          when (contains [x|marc-value()|] "01.") (
            P65i ---> (E22, relativeUriT "/photo/print/{i}" [("i", i)]) ==> [
              P138i ---> (E36, relativeUri "/visual_item") ==> [
                P165i ---> (D1, relativeUri "/image") ==> [
                  P2 --> (E55, P.digital_image),
                  P1 ---> (E42, uriFn  [x|marc-value()|] (\v -> "https://iiif.artresearch.net/iiif/3/frick/" <> v <> "/full/full/0/default.jpg")) ==> [
                    P2 --> (E55, P.photo_file_url),
                    ImageStorageId --> literal ("frick" :: T.Text)
                  ]
                ]
              ]
            ]
          )
      ]
  )

frickRights = 
  P104 ---> (E30, relativeUri "/rights") ==> [
    -- for now we assume that all FRICK images are under UND rights statement
    P2 --> (E55, constUri "http://rightsstatements.org/vocab/UND/1.0/")
  ]

frickKeeper =
  P50 --> (E39, constUri "https://artresearch.net/resource/provider/frick")

frickPhotoArchive =
  P70i --> (E31, constUri ("https://artresearch.net/resource/e31/frick"))


photoProduction =
  P108i ---> (E12, relativeUri "/production") ==> [
    P14 ---> (E39, templateUri "actor/photographer/{name}" [("name", [x|marc-value()|])]) ==> [
      P2 --> (E55, P.photographer),
      appellation_0_1 P.preferred_name [x|marc-value()|],
      SameAs --> (E39, uriFn [x|marc-value()|] (photographerUri "frick"))
    ]
  ]

farlNegativeStr = "Frick Art Reference Library, New York (FARL) negative"