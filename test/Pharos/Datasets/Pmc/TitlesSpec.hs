module Pharos.Datasets.Pmc.TitlesSpec (spec) where

import Test.Hspec
import CommonImports
import Pmc.Mappings.Titles (titles)
import Pmc.Mappings.Work (baseMapping, pmcBaseUri)


spec :: Spec
spec = describe "PMC Titles mapping" $ do
  it "preferred title" $ do
    let mapping = baseMapping +> [titles]
    let pmcIdentifiersXml = [w|
      <OAI-PMH xmlns:lido="http://www.lido-schema.org">
        <GetRecord>
          <record>
            <metadata>
              <lido:lido>
                <lido:administrativeMetadata>
                  <lido:recordWrap>
                    <lido:recordID>402798</lido:recordID>
                  </lido:recordWrap>
                </lido:administrativeMetadata>
                <lido:descriptiveMetadata xml:lang="en">
                  <lido:objectIdentificationWrap>
                    <lido:titleWrap>
                        <lido:titleSet lido:type="Primary Title">
                            <lido:appellationValue lido:pref="preferred" xml:lang="en">Romney
                                Marsh</lido:appellationValue>
                        </lido:titleSet>
                    </lido:titleWrap>
                  </lido:objectIdentificationWrap>
                </lido:descriptiveMetadata>
              </lido:lido>
            </metadata>
          </record>
        </GetRecord>
      </OAI-PMH>
    |]
    
    result <- processXMLStringAsSet mapping pmcBaseUri pmcIdentifiersXml
    
    result
      `shouldBe`
      [t|
        <work/402798> a crm:E22_Human-Made_Object .
        <work/402798> crm:P1_is_identified_by <work/402798/appellation/1/preferred_title> .
        <work/402798/appellation/1/preferred_title> a crm:E41_Appellation .
        <work/402798/appellation/1/preferred_title> crm:P2_has_type <vocab/meta/preferred_title> .
        <work/402798/appellation/1/preferred_title> crm:P190_has_symbolic_content "Romney Marsh" .
        <work/402798/appellation/1/preferred_title> crm:P2_has_type <vocab/title/primary_title> .
        <vocab/title/primary_title> a crm:E55_Type .
        <vocab/title/primary_title> crm:P1_is_identified_by <vocab/title/primary_title/appellation/preferred_name> .
        <vocab/title/primary_title/appellation/preferred_name> a crm:E41_Appellation .
        <vocab/title/primary_title/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
        <vocab/title/primary_title/appellation/preferred_name> crm:P190_has_symbolic_content "Primary Title" .                 
      |]

  it "preferred and alternative titles" $ do
    let mapping = baseMapping +> [titles]
    let pmcIdentifiersXml = [w|
      <OAI-PMH xmlns:lido="http://www.lido-schema.org">
        <GetRecord>
          <record>
            <metadata>
              <lido:lido>
                <lido:descriptiveMetadata xml:lang="en">
                  <lido:objectIdentificationWrap>
                    <lido:titleWrap>
                      <lido:titleSet lido:type="Foreign Language">
                        <lido:appellationValue lido:pref="preferred" xml:lang="fr">L'Enfant du Régiment</lido:appellationValue>
                      </lido:titleSet>
                      <lido:titleSet lido:type="Historic Title">
                        <lido:appellationValue lido:pref="alternative" xml:lang="en">L'Enfant du Regiment (The Random Shot)</lido:appellationValue>
                      </lido:titleSet>
                    </lido:titleWrap>
                  </lido:objectIdentificationWrap>
                </lido:descriptiveMetadata>
                <lido:administrativeMetadata>
                  <lido:recordWrap>
                    <lido:recordID lido:type="local">491091</lido:recordID>
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
        <vocab/title/foreign_language> a crm:E55_Type .
        <vocab/title/foreign_language> crm:P1_is_identified_by <vocab/title/foreign_language/appellation/preferred_name> .
        <vocab/title/foreign_language/appellation/preferred_name> a crm:E41_Appellation .
        <vocab/title/foreign_language/appellation/preferred_name> crm:P190_has_symbolic_content "Foreign Language" .
        <vocab/title/foreign_language/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
        <vocab/title/historic_title> a crm:E55_Type .
        <vocab/title/historic_title> crm:P1_is_identified_by <vocab/title/historic_title/appellation/preferred_name> .
        <vocab/title/historic_title/appellation/preferred_name> a crm:E41_Appellation .
        <vocab/title/historic_title/appellation/preferred_name> crm:P190_has_symbolic_content "Historic Title" .
        <vocab/title/historic_title/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
        <work/491091> a crm:E22_Human-Made_Object .
        <work/491091> crm:P1_is_identified_by <work/491091/appellation/2/alternative_title> .
        <work/491091> crm:P1_is_identified_by <work/491091/appellation/1/preferred_title> .
        <work/491091/appellation/2/alternative_title> a crm:E41_Appellation .
        <work/491091/appellation/2/alternative_title> crm:P190_has_symbolic_content "L'Enfant du Regiment (The Random Shot)" .
        <work/491091/appellation/2/alternative_title> crm:P2_has_type <vocab/meta/alternative_title> .
        <work/491091/appellation/2/alternative_title> crm:P2_has_type <vocab/title/historic_title> .
        <work/491091/appellation/1/preferred_title> a crm:E41_Appellation .
        <work/491091/appellation/1/preferred_title> crm:P190_has_symbolic_content "L'Enfant du Régiment" .
        <work/491091/appellation/1/preferred_title> crm:P2_has_type <vocab/meta/preferred_title> .
        <work/491091/appellation/1/preferred_title> crm:P2_has_type <vocab/title/foreign_language> .
      |]