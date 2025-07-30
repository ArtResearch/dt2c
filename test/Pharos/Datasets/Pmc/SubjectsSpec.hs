module Pharos.Datasets.Pmc.SubjectsSpec (spec) where

import Test.Hspec
import CommonImports
import Pmc.Mappings.Subjects (subjectLinks)
import Pmc.Mappings.Work (baseMapping, pmcBaseUri)

spec :: Spec
spec = describe "subjects" $ do
  it "subject from title" $ do
    let mapping = baseMapping +> subjectLinks
    let xml = [w|
<?xml version='1.0' encoding='UTF-8'?>
<OAI-PMH>
  <GetRecord>
    <record>
      <metadata>
        <lido:lido>
          <lido:administrativeMetadata xml:lang="en">
            <lido:recordWrap>
              <lido:recordID lido:type="local">412175</lido:recordID>
            </lido:recordWrap>
          </lido:administrativeMetadata>
          <lido:descriptiveMetadata xml:lang="en">
            <lido:objectIdentificationWrap>
              <lido:titleWrap>
                <lido:titleSet lido:type="Primary Title">
                  <lido:appellationValue lido:pref="preferred" xml:lang="en">Self-Portrait</lido:appellationValue>
                </lido:titleSet>
              </lido:titleWrap>
              <lido:objectMeasurementsWrap/>
            </lido:objectIdentificationWrap>
          </lido:descriptiveMetadata>
        </lido:lido>
      </metadata>
    </record>
  </GetRecord>
</OAI-PMH>
    |]
    
    result <- processXMLStringAsSet mapping pmcBaseUri xml
    
    result `shouldBe` [t|
<http://iconclass.org/48B3> a crm:E55_Type .
<http://iconclass.org/48B3> crm:P1_is_identified_by <http://iconclass.org/48B3/id/preferred_identifier> .
<http://iconclass.org/48B3> crm:P2_has_type pharos-meta:iconclass .
<http://iconclass.org/48B3/id/preferred_identifier> a crm:E42_Identifier .
<http://iconclass.org/48B3/id/preferred_identifier> crm:P190_has_symbolic_content "48B3" .
<http://iconclass.org/48B3/id/preferred_identifier> crm:P2_has_type pharos-meta:preferred_identifier .
<subject/from_title/self_portrait> a crm:E55_Type .
<subject/from_title/self_portrait> crm:P1_is_identified_by <subject/from_title/self_portrait/appellation/preferred_name> .
<subject/from_title/self_portrait> crm:P2_has_type pharos-meta:subject .
<subject/from_title/self_portrait> custom:sameAs <http://iconclass.org/48B3> .
<subject/from_title/self_portrait/appellation/preferred_name> a crm:E41_Appellation .
<subject/from_title/self_portrait/appellation/preferred_name> crm:P190_has_symbolic_content "Self-Portrait" .
<subject/from_title/self_portrait/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<work/412175> a crm:E22_Human-Made_Object .
<work/412175> crm:P65_shows_visual_item <work/412175/visual_item/subject/from_title> .
<work/412175/visual_item/subject/from_title> a crm:E36_Visual_Item .
<work/412175/visual_item/subject/from_title> crm:P2_has_type <subject/from_title/self_portrait> .
    |]

  it "subject concept" $ do
    let mapping = baseMapping +> subjectLinks
    let xml = [w|
<?xml version='1.0' encoding='UTF-8'?>
<OAI-PMH>
  <GetRecord>
    <record>
      <metadata>
        <lido:lido>
          <lido:administrativeMetadata xml:lang="en">
            <lido:recordWrap>
              <lido:recordID lido:type="local">414127</lido:recordID>
            </lido:recordWrap>
          </lido:administrativeMetadata>
          <lido:descriptiveMetadata xml:lang="en">
            <lido:objectIdentificationWrap>
              <lido:titleWrap>
                <lido:titleSet lido:type="Primary Title">
                  <lido:appellationValue lido:pref="preferred" xml:lang="en">Battle Of Blenheim</lido:appellationValue>
                </lido:titleSet>
              </lido:titleWrap>
              <lido:objectMeasurementsWrap/>
            </lido:objectIdentificationWrap>
            <lido:objectRelationWrap>
              <lido:subjectWrap>
                <lido:subjectSet>
                  <lido:subject lido:type="description">
                    <lido:subjectConcept>
                      <lido:conceptID lido:source="AAT" lido:type="local">7666</lido:conceptID>
                      <lido:term>military history</lido:term>
                    </lido:subjectConcept>
                    <lido:subjectConcept>
                      <lido:conceptID lido:source="AAT" lido:type="local">8412</lido:conceptID>
                      <lido:term>battles</lido:term>
                    </lido:subjectConcept>
                  </lido:subject>
                </lido:subjectSet>
              </lido:subjectWrap>
            </lido:objectRelationWrap>
          </lido:descriptiveMetadata>
        </lido:lido>
      </metadata>
    </record>
  </GetRecord>
</OAI-PMH>
    |]
    
    result <- processXMLStringAsSet mapping pmcBaseUri xml
    
    result `shouldBe` [t|
<http://iconclass.org/45> a crm:E55_Type .
<http://iconclass.org/45> crm:P1_is_identified_by <http://iconclass.org/45/id/preferred_identifier> .
<http://iconclass.org/45> crm:P2_has_type pharos-meta:iconclass .
<http://iconclass.org/45/id/preferred_identifier> a crm:E42_Identifier .
<http://iconclass.org/45/id/preferred_identifier> crm:P190_has_symbolic_content "45" .
<http://iconclass.org/45/id/preferred_identifier> crm:P2_has_type pharos-meta:preferred_identifier .
<http://iconclass.org/45H3> a crm:E55_Type .
<http://iconclass.org/45H3> crm:P1_is_identified_by <http://iconclass.org/45H3/id/preferred_identifier> .
<http://iconclass.org/45H3> crm:P2_has_type pharos-meta:iconclass .
<http://iconclass.org/45H3/id/preferred_identifier> a crm:E42_Identifier .
<http://iconclass.org/45H3/id/preferred_identifier> crm:P190_has_symbolic_content "45H3" .
<http://iconclass.org/45H3/id/preferred_identifier> crm:P2_has_type pharos-meta:preferred_identifier .
<http://iconclass.org/49K1> a crm:E55_Type .
<http://iconclass.org/49K1> crm:P1_is_identified_by <http://iconclass.org/49K1/id/preferred_identifier> .
<http://iconclass.org/49K1> crm:P2_has_type pharos-meta:iconclass .
<http://iconclass.org/49K1%3A45> a crm:E55_Type .
<http://iconclass.org/49K1%3A45> crm:P127_has_broader_term <http://iconclass.org/49K1> .
<http://iconclass.org/49K1%3A45> crm:P130_shows_features_of <http://iconclass.org/45> .
<http://iconclass.org/49K1%3A45> crm:P1_is_identified_by <http://iconclass.org/49K1%3A45/id/preferred_identifier> .
<http://iconclass.org/49K1%3A45> crm:P2_has_type pharos-meta:iconclass .
<http://iconclass.org/49K1%3A45/id/preferred_identifier> a crm:E42_Identifier .
<http://iconclass.org/49K1%3A45/id/preferred_identifier> crm:P190_has_symbolic_content "49K1 : 45" .
<http://iconclass.org/49K1%3A45/id/preferred_identifier> crm:P2_has_type pharos-meta:preferred_identifier .
<http://iconclass.org/49K1/id/preferred_identifier> a crm:E42_Identifier .
<http://iconclass.org/49K1/id/preferred_identifier> crm:P190_has_symbolic_content "49K1" .
<http://iconclass.org/49K1/id/preferred_identifier> crm:P2_has_type pharos-meta:preferred_identifier .
<subject/battles> a crm:E55_Type .
<subject/battles> crm:P1_is_identified_by <subject/battles/appellation/preferred_name> .
<subject/battles> crm:P2_has_type pharos-meta:subject .
<subject/battles> custom:sameAs <http://iconclass.org/45H3> .
<subject/battles/appellation/preferred_name> a crm:E41_Appellation .
<subject/battles/appellation/preferred_name> crm:P190_has_symbolic_content "battles" .
<subject/battles/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<subject/military_history> a crm:E55_Type .
<subject/military_history> crm:P1_is_identified_by <subject/military_history/appellation/preferred_name> .
<subject/military_history> crm:P2_has_type pharos-meta:subject .
<subject/military_history> custom:sameAs <http://iconclass.org/49K1%3A45> .
<subject/military_history/appellation/preferred_name> a crm:E41_Appellation .
<subject/military_history/appellation/preferred_name> crm:P190_has_symbolic_content "military history" .
<subject/military_history/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<work/414127> a crm:E22_Human-Made_Object .
<work/414127> crm:P65_shows_visual_item <work/414127/visual_item/subject/1> .
<work/414127> crm:P65_shows_visual_item <work/414127/visual_item/subject/2> .
<work/414127/visual_item/subject/1> a crm:E36_Visual_Item .
<work/414127/visual_item/subject/1> crm:P2_has_type <subject/military_history> .
<work/414127/visual_item/subject/2> a crm:E36_Visual_Item .
<work/414127/visual_item/subject/2> crm:P2_has_type <subject/battles> .
    |]
