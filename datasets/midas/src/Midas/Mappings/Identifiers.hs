module Midas.Mappings.Identifiers where

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Types.URI (urlEncode)
import CommonImports
import Pharos.CidocCrm.Patterns.Identifiers (identifier_0_1, identifier_0_1_fn)
import qualified Vocabularies.PHAROS as P

identifiers :: T.Text -> XPathExpr -> [PathTree E22_]
identifiers dataset workIdExpr = [
    -- Main catalog record identifier
    workIdExpr @> identifier_0_1 P.preferred_identifier,

    case dataset of
      "hertziana" -> [x|.|] @> [
          -- 1. if there is no parent object element then we just use ID for catalog URL
          -- 2. or if there is no comma in the id then we use it as is 
          -- (sometimes nested objects have their own top level IDs, see 08041820 in the IdentifiersSpec)
          when (
            or_ [
              not_ $ exists [x|../../obj|],
              not_ $ contains workIdExpr ","
            ]
          ) (
            workIdExpr @> identifier_0_1_fn P.catalog_url hertzianaUrl
          ),

          -- 3. otherwise we need to count preceding obj elements to generate ?part= query parameter
          when (
            and_ [
              exists [x|../../obj|],
              contains workIdExpr ","
            ]
          ) (
            P1 ---> (E42, relativeUri ("/id/" <> localName P.catalog_url)) ==> [
              P2 --> (E55, P.catalog_url),
              P190 --> literalMultiExprFn [[x|count(preceding::obj)|], workIdExpr] hertzianaNestedObjUrl
            ]
          )
        ]

      "khi" -> [x|.|] @> [
          -- 1. if there is no parent object element then we just use ID for catalog URL
          when (not_ $ exists [x|../../obj|]) (
            workIdExpr @> identifier_0_1_fn P.catalog_url khiUrl
          ),

          -- 2. otherwise for KHI we should always use the top-level object ID from a5000,
          -- because at the moment KHI system doesn't support links to nested records,
          -- even if they have their own top level IDs.
          when (exists [x|../../obj|]) (
            [x|preceding::obj/a5000|] @> identifier_0_1_fn P.catalog_url khiUrl
          )
        ]

      -- Marburg has catalog URL in id element
      "marburg" -> [x|id|] @> identifier_0_1 P.catalog_url
      _ -> NullTree
  ]

hertzianaUrl :: T.Text -> T.Text
hertzianaUrl identifier =
  T.pack "https://foto.biblhertz.it/document/obj/" <> TE.decodeUtf8 (urlEncode True $ TE.encodeUtf8 identifier)

-- for nested objects in Hertziana we take the top-level object ID
-- + we count preceding obj elements to generate ?part= query parameter
hertzianaNestedObjUrl :: [T.Text] -> T.Text
hertzianaNestedObjUrl [objCount, identifier] =
  let
    recordIdentifier = T.takeWhile (/= ',') identifier
  in
    T.pack "https://foto.biblhertz.it/document/obj/" <> TE.decodeUtf8 (urlEncode True $ TE.encodeUtf8 recordIdentifier) <> "?part=" <> objCount
hertzianaNestedObjUrl _ = T.empty

khiUrl :: T.Text -> T.Text
khiUrl identifier = T.pack "http://photothek.khi.fi.it/documents/obj/" <> TE.decodeUtf8 (urlEncode True $ TE.encodeUtf8 identifier)
