module Pmc.Mappings.Work where

import Prelude
import CommonImports
import qualified Vocabularies.AAT as AAT
import Pmc.Mappings.Identifiers
import Pmc.Mappings.Cataloging
import Pmc.Mappings.Titles (titles)
import Data.Text (Text)
import Pmc.Mappings.NamedUris (work)
import Pmc.Mappings.Repository (repositoryLinks)
import Pmc.Mappings.Photo (photoLinks)
import Pmc.Mappings.Artist (artistLinks)
import Pmc.Mappings.Subjects (subjectLinks)

pmcBaseUri :: Maybe Text
pmcBaseUri = Just "https://artresearch.net/resource/pmc/"

baseMapping :: D E22_
baseMapping = D E22 [x|/OAI-PMH/GetRecord/record/metadata/lido:lido|]
    (typedNamed work ( templateUri "work/{work_id}" [("work_id", [x|lido:administrativeMetadata/lido:recordWrap/lido:recordID|])]))

-- Base type links that are common to all mappings
baseTypeLinks = [
    P2 --> (E55, AAT.workOfArt),
    P70i --> (E31, constUri "https://artresearch.net/resource/e31/pmc")
  ]

-- Full mapping combining all components
workMapping :: Mapping 'E22_
workMapping =
    Mapping baseMapping $
      baseTypeLinks ++
      identifiers ++
      repositoryLinks ++
      photoLinks ++
      artistLinks ++
      subjectLinks ++
      [
        catalogRecord,
        titles
        
      ]
