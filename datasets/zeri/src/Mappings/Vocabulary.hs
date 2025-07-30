{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
module Mappings.Vocabulary where

import Vocabularies (extractValue)
import Vocabularies.Base

base = "vocab/meta/"

-- Identifier types
nrscheda = base </> "nrscheda"

isshownby = base </> "isshownby"
inventory_number = base </> "inventory_number"
photo_identifier = base </> "photo_identifier"

-- Title types
traditional_title = base </> "traditional_title"

subjects_as_title = base </> "subjects_as_title"

proper_title = base </> "proper_title"
attributed_title = base </> "attributed_title"
parallel_title = base </> "parallel_title"

-- Date types
alternative_date = base </> "alternative_date"

-- Material/Technique types
mtc = base </> "mtc"

auts = base </> "auts"


-- Place Types
region = base </> "region"
town = base </> "town"
district = base </> "district"
village = base </> "village"
precise_location = base </> "precise_location"

-- Repository Types
repository_institution = base </> "repository_institution"
repository_organization = base </> "repository_organization"
repository_actor = base </> "repository_actor"
repository_collection = base </> "repository_collection"

zeriVocabularyItems = []
