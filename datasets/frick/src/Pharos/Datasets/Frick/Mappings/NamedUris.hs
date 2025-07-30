{-# LANGUAGE TypeFamilies #-}

module Pharos.Datasets.Frick.Mappings.NamedUris where

import NamedUris (NamedUriNamespace(..))

-- | Frick namespace
data FrickNS

-- | Frick named URIs
instance NamedUriNamespace FrickNS where
  data NamedUri FrickNS
    = Production |
      Work |
      WorkVisualItem
  
  uriToText Production = "production"
  uriToText Work = "work"
  uriToText WorkVisualItem = "work_visual_item"

-- | Export named URIs for convenience
production = Production
work = Work
workVisualItem = WorkVisualItem
