module Midas.Mappings.Titles where

import CommonImports
import Pharos.CidocCrm.Patterns.Appellations
import Midas.Mappings.Vocabulary
import qualified Vocabularies.PHAROS as P

titles :: [PathTree E22_]
titles = 
  [
    -- Object Title (present only in artworks)
    [x|a5200|] @> appellation_0_N object_title [x|text()|],

    -- Alternative Object Title (present only in artworks)
    [x|a5201|] @> appellation_0_N alternative_object_title [x|text()|],

    -- Name of the Building (present only in buildworks)
    [x|a5202|] @> [
      appellation_0_N name_of_the_building [x|text()|],

      -- we create a label for the building that contains place
      when (exists [x|../a5108|]) (
        appellation_0_1 P.pharos_preferred_name [x|concat(text(), ', ', ../a5108/text())|]
      )
    ],

    -- Alternative Building Name (present only in buildworks)
    [x|a5204|] @> appellation_0_N alternative_building_name [x|text()|],

    -- If we have an object that doesn't have a name/title, then we use object type as a label
    when (
      and_ [
        not_ $ exists [x|a5200|], -- there is no object title
        not_ $ exists [x|a5201|], -- there is no alternative object title
        not_ $ exists [x|a5202|], -- there is no name of the building
        not_ $ exists [x|a5204|], -- there is no alternative building
        exists [x|a5230|] -- there is an object type
      ]
    ) (
      appellation_0_N P.pharos_preferred_name [x|a5230/text()|]
    )
  ]
