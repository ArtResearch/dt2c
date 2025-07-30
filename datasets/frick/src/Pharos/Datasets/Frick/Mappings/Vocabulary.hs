{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Pharos.Datasets.Frick.Mappings.Vocabulary where

import Vocabularies.Base
import Vocabularies (extractValue)
import RDF (Value)

base = "vocab/meta/"

-- Identifier types
alma_id = base </> "alma_id"

-- Title types
title = base </> "title"
variant_title = base </> "variant_title"

-- Production date types
approximate_production_date = base </> "approximate-production-date"

-- Repository
repository_actor = base </> "repository_actor"

farl_negative_number = base </> "farl-negative-number"