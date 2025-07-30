{-# LANGUAGE TypeFamilies #-}

module Pmc.Mappings.NamedUris where

import NamedUris (NamedUriNamespace(..))

data PmcNS

instance NamedUriNamespace PmcNS where
  data NamedUri PmcNS
    = Work
  
  uriToText Work = "work"

work :: NamedUri PmcNS
work = Work
