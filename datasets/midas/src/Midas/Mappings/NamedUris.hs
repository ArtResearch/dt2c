{-# LANGUAGE TypeFamilies #-}

module Midas.Mappings.NamedUris where

import NamedUris (NamedUriNamespace(..))

-- | Midas namespace
data MidasNS

-- | Midas named URIs
instance NamedUriNamespace MidasNS where
  data NamedUri MidasNS
    = Work
  
  uriToText Work = "work"

-- | Export named URIs for convenience
work :: NamedUri MidasNS
work = Work
