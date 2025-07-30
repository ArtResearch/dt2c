module Mappings.Mappings.Work.Titles where

import qualified Data.Text as T

import CommonImports
import Mappings.Vocabulary
import Mappings.Mappings.Work.Subjects (splitTextOnConditionalComma)
import Pharos.CidocCrm.Patterns (appellation_0_1, appellation_0_1_fn)
import qualified Vocabularies.PHAROS as P

titles :: PathTree E22_
titles = 
  [x|PARAGRAFO[@etichetta="OBJECT"]|] @> [
    -- Traditional title
    -- Cardinality 0..1
    [x|SGTT|] @> appellation_0_1 traditional_title [x|text()|],

    -- We use subject as title when traditional title is not present
    -- this is how it works in Zeri catalog UI
    -- SGTI is a comma separated list of subjects
    -- Cardinality 0..1
    -- see splitting logic in Subjects.hs, we need to rejoin them because by default they don't have whitespaces
    not_ (exists [x|SGTT|]) ?> [x|SGTI|] @> appellation_0_1_fn P.pharos_preferred_name  [x|text()|] subjectAsTitle
  ]

-- subject as title
subjectAsTitle = T.intercalate ", " . splitTextOnConditionalComma