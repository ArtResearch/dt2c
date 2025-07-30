module Pharos.Datasets.Pmc.CatalogingSpec (spec) where

import Test.Hspec
import CommonImports
import Pmc.Mappings.Cataloging (catalogRecord)
import Pmc.Mappings.Work (baseMapping, pmcBaseUri)

spec :: Spec
spec = describe "PMC Catalog record mapping" $ do
  it "catalog record entity with all the metadata" $ do
    let mapping = baseMapping +> [catalogRecord]
    let pmcIdentifiersXml = [w|
      <OAI-PMH xmlns:lido="http://www.lido-schema.org">
        <GetRecord>
          <responseDate>2025-01-15T17:29:56Z</responseDate>
          <record>
            <header>
                <identifier>objects-402798</identifier>
                <datestamp>2024-09-27T11:46:53Z</datestamp>
            </header>
            <metadata>
              <lido:lido>
                <lido:lidoRecID lido:source="Paul Mellon Centre" lido:type="local">PA-F00423-0005</lido:lidoRecID>
                <lido:administrativeMetadata>
                  <lido:recordWrap>
                    <lido:recordID>402798</lido:recordID>
                    <lido:recordInfoSet>
                      <lido:recordInfoLink>https://photoarchive.paul-mellon-centre.ac.uk/objects/402798/romney-marsh</lido:recordInfoLink>
                    </lido:recordInfoSet>
                  </lido:recordWrap>
                </lido:administrativeMetadata>
              </lido:lido>
            </metadata>
          </record>
        </GetRecord>
      </OAI-PMH>
    |]
    
    result <- processXMLStringAsSet mapping pmcBaseUri pmcIdentifiersXml
    
    result
      `shouldBe` [t|
        <work/402798> a crm:E22_Human-Made_Object .
        <work/402798> crm:P70i_is_documented_in <work/402798/catalog_record> .
        <work/402798/catalog_record> a crm:E31_Document .
        <work/402798/catalog_record> crm:P1_is_identified_by <work/402798/catalog_record/id/catalog_url> .
        <work/402798/catalog_record> crm:P1_is_identified_by <work/402798/catalog_record/id/oai_pmh_url> .
        <work/402798/catalog_record> crm:P1_is_identified_by <work/402798/catalog_record/id/record> .
        <work/402798/catalog_record> crm:P2_has_type pharos-meta:catalog_record .
        <work/402798/catalog_record> crm:P94i_was_created_by <work/402798/catalog_record/event/update> .
        <work/402798/catalog_record/event/update> a crm:E65_Creation .
        <work/402798/catalog_record/event/update> crm:P2_has_type pharos-meta:catalog_record_update_event .
        <work/402798/catalog_record/event/update> crm:P4_has_time-span <work/402798/catalog_record/event/update/date/fetch> .
        <work/402798/catalog_record/event/update> crm:P4_has_time-span <work/402798/catalog_record/event/update/date/update> .
        <work/402798/catalog_record/event/update/date/fetch> a crm:E52_Time-Span .
        <work/402798/catalog_record/event/update/date/fetch> crm:P2_has_type pharos-meta:catalog_record_fetch_time .
        <work/402798/catalog_record/event/update/date/fetch> crm:P82_at_some_time_within "2025-01-15T17:29:56Z"^^xsd:dateTime .
        <work/402798/catalog_record/event/update/date/update> a crm:E52_Time-Span .
        <work/402798/catalog_record/event/update/date/update> crm:P2_has_type pharos-meta:catalog_record_update_time .
        <work/402798/catalog_record/event/update/date/update> crm:P82_at_some_time_within "2024-09-27T11:46:53Z"^^xsd:dateTime .
        <work/402798/catalog_record/id/catalog_url> a crm:E42_Identifier .
        <work/402798/catalog_record/id/catalog_url> crm:P190_has_symbolic_content "https://photoarchive.paul-mellon-centre.ac.uk/objects/402798/romney-marsh" .
        <work/402798/catalog_record/id/catalog_url> crm:P2_has_type pharos-meta:catalog_url .
        <work/402798/catalog_record/id/oai_pmh_url> a crm:E42_Identifier .
        <work/402798/catalog_record/id/oai_pmh_url> crm:P190_has_symbolic_content "https://photoarchive.paul-mellon-centre.ac.uk/apis/oai/pmh/v2?verb=GetRecord&metadataPrefix=lido&identifier=objects-402798" .
        <work/402798/catalog_record/id/oai_pmh_url> crm:P2_has_type pharos-meta:oai_pmh_url .
        <work/402798/catalog_record/id/record> a crm:E42_Identifier .
        <work/402798/catalog_record/id/record> crm:P190_has_symbolic_content "PA-F00423-0005" .
        <work/402798/catalog_record/id/record> crm:P2_has_type <vocab/meta/record> .
      |]
