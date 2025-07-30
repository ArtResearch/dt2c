module Midas.Mappings.Photo (photoLinks) where

import Debug.Trace (trace)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import CommonImports
import qualified Vocabularies.PHAROS as P
import Pharos.CidocCrm.Patterns (appellation_0_1, identifier_0_1)
import Authority.Photographers (photographerUri)
import Midas.Mappings.Vocabulary (iiif_not_available, photo_identifier, photo_inventory_number)

photoLinks :: T.Text -> PathTree E22_
photoLinks datasetName =
  [x|a8450|] @> (
    case datasetName of
      "hertziana" ->
          -- for Hertziana we don't exclude gesperrt images
          when (
            and_ [
              or_ [exists [x|a8540|], exists [x|a8555|]]
            ]
          ) (
            photoObject datasetName
          )
      "khi"       ->
          -- for KHI we don't include images where a8541 matches “gesperrt”
          when (
            and_ [
              or_ [exists [x|a8540|], exists [x|a8555|]],
              not_ $ equals [x|a8541/text()|] "gesperrt",

              -- verso in KHI data contains vscan or just v in a8541
              not_ $ contains [x|a84fl/text()|] "v"
            ]
          ) (
            photoObject datasetName
          )

      "marburg"   ->
          -- per instruction from Christoph Glorius:
          --  DO NOT SHOW image files from obj/a8450:
          --    where a8543c is starting with “http://rightsstatements.org/(http://rightsstatements.org/)”
          --    where a8541 matches “gesperrt” or a8541a matches “kein Download”
          -- Please SHOW ONLY obj/a8450 image files:
          --    where a8540 image file name is starting with “fm” or a8555 is starting with “mi”
          --    where a8543c is empty or starts-with “http://creativecommons.org”
          when (
            and_ [
              or_ [exists [x|a8540|], exists [x|a8555|]],
              not_ $ equals [x|a8541/text()|] "gesperrt",
              not_ $ equals [x|a8541a/text()|] "kein Download",
              not_ $ starts [x|a8543c/text()|] "http://rightsstatements.org/",
              or_ [
                starts [x|a8540/text()|] "fm",
                starts [x|a8555/text()|] "mi"
              ],
              or_ [
                starts [x|a8543c/text()|] "http://creativecommons.org/",
                not_ $ exists [x|a8543c|],
                equals [x|normalize-space(a8543c/text())|] ""
              ]
            ]
          ) (
            photoObject datasetName
          )
      _           -> NullTree
  )

photoObject datasetName =
  P138i ---> (E36, relativeUri "/visual_item") ==> [
        case datasetName of
          "marburg" -> [x|.|] @> [
                -- we generate image object only
                -- where a8540 image file name is starting with “fm” or a8555 is starting with “mi”
                [x|a8540|] @> when (starts [x|text()|] "fm") (
                  P65i ---> (E22, templateUri (TE.encodeUtf8 datasetName <> "/photo/{id}") [("id", [x|text()|])]) ==> photoObjectLinks datasetName
                ),

                -- a8555 is used in Marburg and represent microfiche
                -- sometimes it has multiple values separated by '&'
                [x|a8555|] @> [x|tokenize(text(), '&')|] @> when (starts [x|text()|] "mi") (
                  P65i ---> (E22, templateUri (TE.encodeUtf8 datasetName <> "/photo/{id}") [("id", [x|text()|])]) ==> photoObjectLinks datasetName
                )
              ]
          _           -> [x|.|] @> (
              [x|a8540|] @> (
                P65i ---> (E22, templateUri (TE.encodeUtf8 datasetName <> "/photo/{id}") [("id", [x|text()|])]) ==> photoObjectLinks datasetName
              )
            )
      ]

photoObjectLinks datasetName =
  [
    digitalPhoto datasetName,
    identifier_0_1 photo_identifier,

    [x|..|] @> [
      -- we assume that the order in which images are listed in the data
      -- is the preferred order of images for a given work
      ImageOrder --> (typedLiteral i (PrefixedURI "xsd:integer")),

      P2 --> (E55, P.photograph),
      -- photo type, like Negativ, Foto (digital), etc.
      [x|@modifier|] @> (
        P2 ---> (E55, templateUri "vocab/photo-type/{type}" [("type", [x|text()|])]) ==> [
          P127 --> (E55, P.photograph),
          appellation_0_1 P.preferred_name [x|text()|]
        ]
      ),

      -- image source (e.g khi, marburg or hertziana collection)
      P70i --> (E31, constUri ("https://artresearch.net/resource/e31/" <> datasetName)),

      -- photo repository (photo archive, photo credit line)
      photoRepository datasetName,

      -- photographer
      [x|a8490|] @> (
        P108i ---> (E12, relativeUri "/production") ==> [
          P14 ---> (E39, templateUri "actor/photographer/{name}" [("name", [x|text()|])]) ==> [
            P2 --> (E55, P.photographer),
            appellation_0_1 P.preferred_name [x|text()|],
            SameAs --> (E39, uriFn [x|text()|] (photographerUri datasetName))
          ]
        ]
      ),

      -- photo rights statement/license
      -- we assume that all digital photos inherit license from the main physical photo
      -- so we don't need to repeat it in digital image
      P104 ---> (E30, relativeUri "/rights") ==> [
        [x|a8543c|] @> (
          P2 ---> (E55, uriFn [x|text()|] (rightsStatement datasetName)) ==> [
            P2 --> (E55, P.license_type)
          ]
        ),

        when (not_ $ exists [x|a8543c|]) (
          -- if there is no explicit rights statement in the data, 
          -- we use default rights statement for the dataset
          P2 ---> (E55, constUri (defaultRightsStatement datasetName)) ==> [
            P2 --> (E55, P.license_type)
          ]
        )
      ],

      case datasetName of
        "khi" -> 
          [x|a84fl|] @> identifier_0_1 photo_inventory_number
        _           -> NullTree
    ]
  ]

photoRepository datasetName =
  case datasetName of
    -- in KHI the owner should be mapped always mapped from a8577
    "khi"       -> [x|a8577|] @> photoRepositoryActor
    _           -> [x|a8460|] @> photoRepositoryActor

photoRepositoryActor =
  P50 ---> (E39, templateUri "actor/{name}" [("name", [x|text()|])]) ==> [
    appellation_0_1 P.preferred_name [x|text()|]
  ]

rightsStatement :: T.Text -> T.Text -> T.Text
rightsStatement datasetName rightsText =
  case datasetName of
    -- in KHI license is specified not with URI as in Marburg and Hertziana
    "khi"       -> case rightsText of
      "CC BY-SA" -> "http://creativecommons.org/licenses/by-sa/4.0/"
      "CC BY-NC-ND" -> "http://creativecommons.org/licenses/by-nc-nd/4.0/"
      "CC0" -> "http://creativecommons.org/publicdomain/zero/1.0/"
      "not accessible" -> "http://rightsstatements.org/vocab/InC/1.0/"
      unknow           -> error $ T.unpack ("unkwnown rights statement for KHI dataset, in a8543c: '" <> unknow <> "'")
    "hertziana" -> case rightsText of
      "https://rightsstatements.org/page/CNE/1.0/" -> "http://rightsstatements.org/vocab/CNE/1.0/"
      _ -> case T.isPrefixOf "http" rightsText of
        -- TODO issue with the data, need to think on how to handle such cases better
        -- there are values like "Fassade zur Via Ludovisi", etc.
        False -> trace (T.unpack ("Possibly wrong value in a8543c license field: " <> rightsText)) "http://rightsstatements.org/vocab/CNE/1.0/"
        True -> T.replace "https://" "http://" rightsText

    -- normalize license URI to use http instead of https
    -- we do this for all datasets to make sure that URIs are consistent
    _           -> T.replace "https://" "http://" rightsText


defaultRightsStatement datasetName =
  case datasetName of
    "hertziana" -> "http://rightsstatements.org/vocab/CNE/1.0/"
    "khi"       -> "http://creativecommons.org/licenses/by-sa/4.0/"
    "marburg"   -> "http://rightsstatements.org/vocab/CNE/1.0/"
    _           -> error "unknown midas dataset, only hertziana, khi and marburg are supported"

digitalPhoto datasetName =
  P138i ---> (E36, relativeUri "/visual_item") ==> [
    P165i ---> (D1, relativeUri "/image") ==> [
      P2 --> (E55, P.digital_image),
      P1 ---> (E42, uriFn [x|text()|] (fullImageUrl datasetName)) ==> [
        P2 --> (E55, P.photo_file_url),
        ImageStorageId --> literal datasetName
      ],

      -- Hertziana specific, we use this one when image is gessperrt
      -- but we still add it to the dataset with the hope to get at least thumbnail
      case datasetName of
        "hertziana" ->
          when (
            or_ [
              equals [x|../a8541/text()|] "gesperrt",
              equals [x|../a8541a/text()|] "kein Download"
            ]
          ) (
            P2 --> (E55, iiif_not_available)
          )
        _           -> NullTree
    ]
  ]

fullImageUrl :: T.Text -> T.Text -> T.Text
fullImageUrl datasetName photoId =
  case datasetName of
    "hertziana" -> "https://hertz-foto-os1.biblhertz.it/iiif/3/" <> photoId <> "/full/max/0/default.jpg"
    "khi" -> "https://iiif.khi.fi.it/iiif/2/photothek%2F" <> photoId <> ".tif/full/max/0/default.jpg"
    "marburg" -> "https://iiif.artresearch.net/iiif/3/marburg/" <> photoId <> ".jpg/full/max/0/default.jpg"
    _       -> error "unknown midas dataset, only hertziana, khi and marburg are supported"
