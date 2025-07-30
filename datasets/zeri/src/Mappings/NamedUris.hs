{-# LANGUAGE TypeFamilies #-}

module Mappings.NamedUris where

import NamedUris (NamedUriNamespace(..))

-- | Zeri namespace
data ZeriNS

-- | Zeri named URIs
instance NamedUriNamespace ZeriNS where
  data NamedUri ZeriNS
    = Production
    | Work
    | Building
    | Place
  
  uriToText Production = "production"
  uriToText Work = "work"
  uriToText Building = "building"
  uriToText Place = "place"

-- | Export named URIs for convenience
production = Production
work = Work
building = Building
place = Place
