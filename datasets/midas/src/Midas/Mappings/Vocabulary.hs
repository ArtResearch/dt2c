{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Midas.Mappings.Vocabulary where

import Vocabularies.Base
import Vocabularies (extractValue)
import RDF (Value)

base = "vocab/meta/"

-- | a5200
object_title = base </> "object_title"

-- | a5201
alternative_object_title = base </> "alternative_object_title"

-- | a5202
name_of_the_building = base </> "name_of_the_building"

-- | a5204
alternative_building_name = base </> "alternative_building_name"

-- Identifiers
photo_inventory_number = base </> "photo_inventory_number"
photo_identifier = base </> "photo_identifier"

-- Object types
classification = base </> "classification"
art = base </> "art"
object_type = base </> "object_type"
formtyp = base </> "formtyp"

-- image flags
iiif_not_available = base </> "iiif_not_available"

midasVocabularyItems :: IO [Value]
midasVocabularyItems =
  mapM
    extractValue
    [ 
      object_title
    , alternative_object_title
    , name_of_the_building
    , alternative_building_name
    ]
