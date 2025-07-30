module Pmc.Mappings.Cataloging
  ( catalogRecord,
  )
where

import CommonImports
import qualified Vocabularies.PHAROS as P
import Pharos.CidocCrm.Patterns
import Pmc.Mappings.Vocabulary

pmcOaiPmhBaseUrl = "https://photoarchive.paul-mellon-centre.ac.uk/apis/oai/pmh/v2?verb=GetRecord&metadataPrefix=lido&identifier="

catalogRecord :: PathTree E22_
catalogRecord =
  P70i ---> (E31, relativeUri "/catalog_record") ==> [
    P2 --> (E55, P.catalog_record),

    -- PMC catolog record ID
    -- E.g. PA-F00423-0005
    [x|lido:lidoRecID|] @> identifier_0_1 record_id,

    -- Record's catalog entry URL
    -- E.g. https://photoarchive.paul-mellon-centre.ac.uk/objects/402798/romney-marsh
    [x|lido:administrativeMetadata/lido:recordWrap/lido:recordInfoSet/lido:recordInfoLink|] @> identifier_0_1 P.catalog_url,

    [x|../../header|] @> [
      -- OAI-PMH record URL
      -- E.g https://photoarchive.paul-mellon-centre.ac.uk/apis/oai/pmh/v2?verb=GetRecord&metadataPrefix=lido&identifier=objects-402798
      [x|identifier|] @> identifier_0_1_fn P.oai_pmh_url (pmcOaiPmhBaseUrl <>),

      P94i ---> (E65, relativeUri "/event/update") ==> [
          P2 --> (E55, P.catalog_record_update_event),

          -- date when the record was updated last time, according to OAI-PMH data
          P4 ---> (E52, relativeUri "/date/update") ==> [
              P2 -->  (E55, P.catalog_record_update_time),
              P82 --> dateTime [x|datestamp/text()|] DateTime
          ],

          -- date when the record was fetched by Pharos, according to OAI-PMH data
          P4 ---> (E52, relativeUri "/date/fetch") ==> [
              P2 -->  (E55, P.catalog_record_fetch_time),
              P82 --> dateTime [x|../../responseDate/text()|] DateTime
          ]
      ]
    ]
  ]
