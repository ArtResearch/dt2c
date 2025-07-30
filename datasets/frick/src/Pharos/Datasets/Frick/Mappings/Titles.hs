module Pharos.Datasets.Frick.Mappings.Titles  where

import CommonImports
import Pharos.Datasets.Frick.Mappings.Vocabulary
import Pharos.CidocCrm.Patterns.Appellations

titles :: [PathTree 'E22_]
titles = 
  [
    -- Main title
    [x|datafield[@tag='245']/subfield[@code='a']|] @> appellation_0_1 title [x|text()|],

    -- Variant title
    [x|datafield[@tag='246']|] @> appellation_0_N variant_title [x|subfield[@code='a']/text()|]
  ]
