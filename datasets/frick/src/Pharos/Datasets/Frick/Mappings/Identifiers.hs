module Pharos.Datasets.Frick.Mappings.Identifiers  where

import CommonImports
import qualified Vocabularies.PHAROS as P
import Pharos.Datasets.Frick.Mappings.Vocabulary
import Pharos.CidocCrm.Patterns.Identifiers

frickCatalogUrl = "https://library.frick.org/discovery/fulldisplay?context=L&vid=01NYA_INST:Frick&search_scope=Frick&tab=SearchScopes&docid=alma"

identifiers :: [PathTree E22_]
identifiers = [
    -- ALMA ID
    -- e.g. 991007202809707141
    [x| controlfield[@tag='001'] |] @> identifier_0_1 alma_id,
    
    -- Catalog record URL
    -- e.g. https://library.frick.org/discovery/fulldisplay?context=L&vid=01NYA_INST:Frick&search_scope=Frick&tab=SearchScopes&docid=alma991007202809707141
    [x| controlfield[@tag='001'] |] @> identifier_0_1_fn P.catalog_url (frickCatalogUrl <>)
  ]
