{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Pmc.Mappings.Vocabulary where

import Vocabularies.Base
import Vocabularies (extractValue)
import RDF (Value)

base = "vocab/meta/"

record_id = base </> "record"
preferred_title = base </> "preferred_title"
alternative_title = base </> "alternative_title"

pmcVocabularyItems :: IO [Value]
pmcVocabularyItems =
  mapM
    extractValue
    [ record_id,
      preferred_title,
      alternative_title
    ]
