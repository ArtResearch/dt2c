{-# LANGUAGE FlexibleContexts #-}
module Midas.Mappings.Work where

import Prelude
import CommonImports
import qualified Vocabularies.AAT as AAT
import Midas.Mappings.Titles (titles)
import Midas.Mappings.NamedUris (work)
import Data.Text (Text)
import Midas.Mappings.Location (locationLinks, reconciledWorkUri)
import Midas.Mappings.Repository (repositoryLinks)
import Midas.Mappings.Photo (photoLinks)
import Midas.Mappings.Artist (artistLinks)
import Midas.Mappings.Subjects (subjectLinks)
import Midas.Mappings.Identifiers (identifiers)
import Midas.Mappings.Types (types)
import Midas.Mappings.Material (materials)
import Midas.Mappings.Technique (technique)

import qualified Vocabularies.PHAROS as P

import qualified Data.Text as T

midasBaseUri :: Maybe Text
midasBaseUri = Just "https://artresearch.net/resource/midas/"

workMapping :: Text -> Mapping 'E22_
workMapping datasetName = baseMapping +> topLevelWorkLinks datasetName ++ (nestedWorks (workLinks datasetName))

-- We don't map location related data for nested works
-- because it is already done in the top level work.
-- Some datasets like Marburg one duplicate location data
-- for nested works, so if we do map it we get very confusing results.
topLevelWorkLinks :: Text -> [PathTree E22_]
topLevelWorkLinks datasetName =
  workLinks datasetName [x|a5000|] ++ locationLinks

workLinks :: Text -> XPathExpr -> [PathTree E22_]
workLinks datasetName workIdExpr =
  baseTypeLinks datasetName workIdExpr ++
  repositoryLinks ++
  titles ++
  artistLinks ++
  identifiers datasetName workIdExpr ++
  subjectLinks ++
  types ++
  materials ++
  technique ++
  [photoLinks datasetName]

-- | Works hierarchy in MIDAS
nestedWorks :: (XPathExpr -> [PathTree E22_]) -> [PathTree E22_]
nestedWorks linksFn =
  [ mkNestedWorkNode linksFn [x|a5001|] [
      mkNestedWorkNode linksFn [x|a5002|] [
        mkNestedWorkNode linksFn [x|a5003|] [
          mkNestedWorkNode linksFn [x|a5004|] [
            mkNestedWorkNode linksFn [x|a5005|] [
              mkNestedWorkLeaf linksFn [x|a5006|]
            ]
          ]
        ]
      ]
    ]
  ]

baseMapping :: D E22_
baseMapping = D E22 [x|/obj|] (workUri [x|a5000|])

baseTypeLinks datasetName workIdExpr = [
    P2 --> (E55, AAT.workOfArt),

    -- object is a built work if it has a5202 or a5204
    -- which are the name of the building and alternative building name
    when (or_ [exists [x|a5202|], exists [x|a5204|]]) (
      P2 --> (E55, P.built_work)
    ),

    P70i --> (E31, constUri ("https://artresearch.net/resource/e31/" <> datasetName)),

    ObjectSameAs --> (E22, uriFn workIdExpr reconciledWorkUri)
  ]

-- Helper functions for creating nested work structures
mkNestedWorkNode linksFn workIdExpr children =
  [x|obj|] @> (P46 ---> (E22, workUri workIdExpr) ==> linksFn workIdExpr ++ children)

mkNestedWorkLeaf linksFn workIdExpr =
  [x|obj|] @> (P46 ---> (E22, workUri workIdExpr) ==> linksFn workIdExpr)

workUri workIdExpr = typedNamed work (templateUri "work/{work_id}" [("work_id", workIdExpr)])