module Mappings.Mappings.Work.Identifiers where

import CommonImports

import qualified Vocabularies.PHAROS as P
import Pharos.CidocCrm.Patterns.Identifiers (identifier_0_1)

-- Legacy URI, used in the old PHAROS mappings
-- legacyUri :: PathTree E22_
-- legacyUri =
  -- we still need to keep it to maintain compatibility with vision and vocab
--  P1 --> (E42, templateUri "work/{id}" [("id", [x|text()|])])

identifiers :: [PathTree E22_]
identifiers = [
    -- Main catalog record URL
    [x|ISSHOWNBY|] @> identifier_0_1 P.catalog_url,

    -- Main catalog record identifier
    [x|PARAGRAFO[@etichetta="CODES"]/NRSCHEDA|] @> identifier_0_1 P.preferred_identifier
  ]
