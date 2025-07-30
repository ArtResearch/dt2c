{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module MidasGeo.Mappings.Vocabulary where

import Vocabularies.Base
import Vocabularies (extractValue)
import RDF (Value)

base = "vocab/meta/"

geo_id = base </> "geo_id"
