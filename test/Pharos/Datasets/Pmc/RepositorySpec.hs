module Pharos.Datasets.Pmc.RepositorySpec (spec) where

import CommonImports
import Test.Hspec
import Pmc.Mappings.Work (baseMapping, pmcBaseUri)
import Pmc.Mappings.Repository (repositoryLinks)

spec :: Spec
spec = describe "Repository mapping" $ do
  it "repository without a place" $ do
    let mapping = baseMapping +> repositoryLinks
    let pmcIdentifiersXml = [w|
      <OAI-PMH>
        <GetRecord>
          <record>
            <metadata>
              <lido:lido>
                <lido:administrativeMetadata xml:lang="en">
                  <lido:recordWrap>
                    <lido:lidoRecID lido:source="Paul Mellon Centre" lido:type="local">PA-F00111-0007</lido:lidoRecID>
                  </lido:recordWrap>
                </lido:administrativeMetadata>
                <lido:descriptiveMetadata xml:lang="en">
                  <lido:objectIdentificationWrap>
                    <lido:repositoryWrap>
                      <lido:repositorySet>
                        <lido:repositoryName>
                          <lido:legalBodyName>
                            <lido:appellationValue>Ham House and Garden</lido:appellationValue>
                          </lido:legalBodyName>
                        </lido:repositoryName>
                      </lido:repositorySet>
                    </lido:repositoryWrap>
                  </lido:objectIdentificationWrap>
                </lido:descriptiveMetadata>
              </lido:lido>
            </metadata>
          </record>
        </GetRecord>
      </OAI-PMH>
    |]
    
    result <- processXMLStringAsSet mapping pmcBaseUri pmcIdentifiersXml
    
    result `shouldBe` [t|
<repository/ham_house_and_garden> a crm:E39_Actor .
<repository/ham_house_and_garden> crm:P1_is_identified_by <repository/ham_house_and_garden/appellation/preferred_name> .
<repository/ham_house_and_garden> crm:P2_has_type pharos-meta:repository .
<repository/ham_house_and_garden> crm:P50i_is_current_keeper_of <work/> .
<repository/ham_house_and_garden/appellation/preferred_name> a crm:E41_Appellation .
<repository/ham_house_and_garden/appellation/preferred_name> crm:P190_has_symbolic_content "Ham House and Garden" .
<repository/ham_house_and_garden/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<work/> a crm:E22_Human-Made_Object .
<work/> crm:P50_has_current_keeper <repository/ham_house_and_garden> .
    |]

  it "repository with a place" $ do
    let mapping = baseMapping +> repositoryLinks
    let pmcIdentifiersXml = [w|
      <OAI-PMH>
        <GetRecord>
          <record>
            <metadata>
              <lido:lido>
                <lido:administrativeMetadata xml:lang="en">
                  <lido:recordWrap>
                    <lido:lidoRecID lido:source="Paul Mellon Centre" lido:type="local">PA-F00075-0005</lido:lidoRecID>
                  </lido:recordWrap>
                </lido:administrativeMetadata>
                <lido:descriptiveMetadata xml:lang="en">
                  <lido:objectIdentificationWrap>
                    <lido:repositoryWrap>
                      <lido:repositorySet>
                        <lido:repositoryName>
                          <lido:legalBodyID lido:source="ULAN" lido:type="id">500311109</lido:legalBodyID>
                          <lido:legalBodyName>
                            <lido:appellationValue>Petworth House</lido:appellationValue>
                          </lido:legalBodyName>
                        </lido:repositoryName>
                        <lido:repositoryLocation>
                          <lido:namePlaceSet>
                            <lido:appellationValue xml:label="Site">West Sussex</lido:appellationValue>
                          </lido:namePlaceSet>
                        </lido:repositoryLocation>
                      </lido:repositorySet>
                    </lido:repositoryWrap>
                  </lido:objectIdentificationWrap>
                </lido:descriptiveMetadata>
              </lido:lido>
            </metadata>
          </record>
        </GetRecord>
      </OAI-PMH>
    |]
    
    result <- processXMLStringAsSet mapping pmcBaseUri pmcIdentifiersXml
    
    result `shouldBe` [t|
<http://vocab.getty.edu/tgn/7008134> a crm:E53_Place .
<http://www.wikidata.org/entity/Q2081335> a crm:E39_Actor .
<place/west_sussex> a crm:E53_Place .
<place/west_sussex> crm:P1_is_identified_by <place/west_sussex/appellation/preferred_name> .
<place/west_sussex> crm:P2_has_type pharos-meta:geographical_entity .
<place/west_sussex> crm:P74i_is_current_or_former_residence_of <place/west_sussex/repository/petworth_house> .
<place/west_sussex> custom:sameAs <http://vocab.getty.edu/tgn/7008134> .
<place/west_sussex/appellation/preferred_name> a crm:E41_Appellation .
<place/west_sussex/appellation/preferred_name> crm:P190_has_symbolic_content "West Sussex" .
<place/west_sussex/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<place/west_sussex/repository/petworth_house> a crm:E39_Actor .
<place/west_sussex/repository/petworth_house> crm:P1_is_identified_by <place/west_sussex/repository/petworth_house/appellation/preferred_name> .
<place/west_sussex/repository/petworth_house> crm:P2_has_type pharos-meta:repository .
<place/west_sussex/repository/petworth_house> crm:P50i_is_current_keeper_of <work/> .
<place/west_sussex/repository/petworth_house> custom:sameAs <http://www.wikidata.org/entity/Q2081335> .
<place/west_sussex/repository/petworth_house/appellation/preferred_name> a crm:E41_Appellation .
<place/west_sussex/repository/petworth_house/appellation/preferred_name> crm:P190_has_symbolic_content "Petworth House" .
<place/west_sussex/repository/petworth_house/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<work/> a crm:E22_Human-Made_Object .
<work/> crm:P55_has_current_location <place/west_sussex> .
    |]
