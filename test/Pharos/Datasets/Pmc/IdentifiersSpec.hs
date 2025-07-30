module Pharos.Datasets.Pmc.IdentifiersSpec (spec) where

import Test.Hspec
import DSL
import Util.QQ (w, t)
import Engine (processXMLStringAsSet)
import Pmc.Mappings.Identifiers (identifiers)
import Pmc.Mappings.Work (baseMapping, pmcBaseUri)

spec :: Spec
spec = describe "PMC Identifiers mapping" $ do
  it "correctly maps record ID and catalog URL" $ do
    let mapping = baseMapping +> identifiers
    let pmcIdentifiersXml = [w|
      <OAI-PMH xmlns:lido="http://www.lido-schema.org">
        <GetRecord>
          <record>
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
      `shouldBe`
      [t|
        <work/402798> a crm:E22_Human-Made_Object .
      |]
