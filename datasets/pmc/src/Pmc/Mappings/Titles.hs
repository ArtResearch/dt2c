{-# LANGUAGE FlexibleContexts #-}
module Pmc.Mappings.Titles where

import CommonImports
import Pmc.Mappings.Vocabulary
import Pharos.CidocCrm.Patterns

titles :: PathTree E22_
titles =
  [x|lido:descriptiveMetadata/lido:objectIdentificationWrap/lido:titleWrap/lido:titleSet|] @>
    [
      title "preferred" preferred_title,
      title "alternative" alternative_title
    ]
  where
    title typ vocabType = 
      when (equals [x|lido:appellationValue/@lido:pref|] typ) (
        appellation_0_N vocabType [x|normalize-space(lido:appellationValue/text())|]
          ++> [
            [x|@lido:type|] @> (e55_type "title" :: PathTree E41_) 
          ]
      )