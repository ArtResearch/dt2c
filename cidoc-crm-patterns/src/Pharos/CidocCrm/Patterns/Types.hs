{-# LANGUAGE TypeOperators #-}
module Pharos.CidocCrm.Patterns.Types where

import CommonImports
import qualified Vocabularies.PHAROS as P
import Pharos.CidocCrm.Patterns.Appellations

e55_type typeName = 
  P2 ---> (E55, templateUri ("vocab/" <> typeName <> "/{type}") [("type", [x|normalize-space(text())|])]) ==> [
    appellation_0_1 P.preferred_name [x|text()|]
  ]
