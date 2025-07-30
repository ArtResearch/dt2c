module Pharos.Datasets.Pmc.ArtistSpec (spec) where


import Test.Hspec
import DSL
import Util.QQ (w, t)
import Engine (processXMLStringAsSet)
import Pmc.Mappings.Artist (artistLinks)
import Pmc.Mappings.Work (baseMapping, pmcBaseUri)

spec :: Spec
spec = describe "Artists" $ do
  it "artist 1" $ do
    let mapping = baseMapping +> artistLinks
    let xml = [w|
<OAI-PMH xmlns:lido="http://www.lido-schema.org">
  <GetRecord>
    <record>
      <metadata>
        <lido:lido xmlns:lido="http://www.lido-schema.org">
          <lido:administrativeMetadata>
            <lido:recordWrap>
              <lido:recordID>402798</lido:recordID>
            </lido:recordWrap>
          </lido:administrativeMetadata>
          <lido:descriptiveMetadata>
            <lido:eventWrap>
              <lido:eventSet>
                <lido:event>
                  <lido:eventID lido:source="AAT" lido:type="id">300404387</lido:eventID>
                  <lido:eventType>
                    <lido:conceptID lido:source="LIDO Terminology" lido:type="uri">http://terminology.lido-schema.org/eventType/production</lido:conceptID>
                    <lido:term>Production</lido:term>
                  </lido:eventType>
                  <lido:eventActor>
                    <lido:displayActorInRole>Harold Gilman</lido:displayActorInRole>
                    <lido:actorInRole>
                      <lido:actor lido:type="Individual">
                        <lido:actorID lido:source="local" lido:type="id">2408</lido:actorID>
                        <lido:actorID lido:source="ULAN" lido:type="id">500027829</lido:actorID>
                        <lido:actorID lido:source="Heritage E-Number" lido:type="id">E6348</lido:actorID>
                        <lido:actorID lido:source="Heritage F-Number" lido:type="id">F1685</lido:actorID>
                        <lido:actorID lido:source="Heritage F-Number" lido:type="id">F2976</lido:actorID>
                        <lido:actorID lido:source="Heritage F-Number" lido:type="id">F853</lido:actorID>
                        <lido:nameActorSet>
                          <lido:appellationValue lido:pref="alternate">Gilman</lido:appellationValue>
                          <lido:appellationValue lido:pref="alternate">Gilman, Harold J. W.</lido:appellationValue>
                          <lido:appellationValue lido:pref="preferred">Harold Gilman</lido:appellationValue>
                          <lido:appellationValue lido:pref="preferred">Harold Gilman</lido:appellationValue>
                          <lido:appellationValue lido:pref="alternate">Harold John Wilde Gilman</lido:appellationValue>
                          <lido:appellationValue lido:pref="alternate">Gilman, Harold John Wilde</lido:appellationValue>
                        </lido:nameActorSet>
                        <lido:genderActor>male</lido:genderActor>
                      </lido:actor>
                      <lido:roleActor>
                        <lido:conceptID lido:source="AAT" lido:type="id">300025103</lido:conceptID>
                        <lido:term>Artist</lido:term>
                      </lido:roleActor>
                    </lido:actorInRole>
                  </lido:eventActor>
                  <lido:eventDate>
                    <lido:displayDate>undated</lido:displayDate>
                    <lido:date>
                      <lido:earliestDate>1876</lido:earliestDate>
                      <lido:latestDate>1919</lido:latestDate>
                    </lido:date>
                  </lido:eventDate>
                </lido:event>
              </lido:eventSet>
            </lido:eventWrap>
          </lido:descriptiveMetadata>
        </lido:lido>
      </metadata>
    </record>
  </GetRecord>
</OAI-PMH>
    |]

    result <- processXMLStringAsSet mapping pmcBaseUri xml
    
    result `shouldBe` [t|
<actor/2408> a crm:E21_Person .
<actor/2408> crm:P1_is_identified_by <actor/2408/appellation/1/alternative_name> .
<actor/2408> crm:P1_is_identified_by <actor/2408/appellation/2/alternative_name> .
<actor/2408> crm:P1_is_identified_by <actor/2408/appellation/3/alternative_name> .
<actor/2408> crm:P1_is_identified_by <actor/2408/appellation/4/alternative_name> .
<actor/2408> crm:P1_is_identified_by <actor/2408/appellation/preferred_name> .
<actor/2408> crm:P2_has_type pharos-meta:male .
<actor/2408> custom:sameAs <http://vocab.getty.edu/ulan/500027829> .
<actor/2408/appellation/1/alternative_name> a crm:E41_Appellation .
<actor/2408/appellation/1/alternative_name> crm:P190_has_symbolic_content "Gilman" .
<actor/2408/appellation/1/alternative_name> crm:P2_has_type pharos-meta:alternative_name .
<actor/2408/appellation/2/alternative_name> a crm:E41_Appellation .
<actor/2408/appellation/2/alternative_name> crm:P190_has_symbolic_content "Gilman, Harold J. W." .
<actor/2408/appellation/2/alternative_name> crm:P2_has_type pharos-meta:alternative_name .
<actor/2408/appellation/3/alternative_name> a crm:E41_Appellation .
<actor/2408/appellation/3/alternative_name> crm:P190_has_symbolic_content "Harold John Wilde Gilman" .
<actor/2408/appellation/3/alternative_name> crm:P2_has_type pharos-meta:alternative_name .
<actor/2408/appellation/4/alternative_name> a crm:E41_Appellation .
<actor/2408/appellation/4/alternative_name> crm:P190_has_symbolic_content "Gilman, Harold John Wilde" .
<actor/2408/appellation/4/alternative_name> crm:P2_has_type pharos-meta:alternative_name .
<actor/2408/appellation/preferred_name> a crm:E41_Appellation .
<actor/2408/appellation/preferred_name> crm:P190_has_symbolic_content "Harold Gilman" .
<actor/2408/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<http://vocab.getty.edu/ulan/500027829> a crm:E39_Actor .
<work/402798> a crm:E22_Human-Made_Object .
<work/402798> crm:P108i_was_produced_by <work/402798/production> .
<work/402798/production> a crm:E12_Production .
<work/402798/production> crm:P14_carried_out_by <actor/2408> .
<work/402798/production> crm:P4_has_time-span <work/402798/production/date> .
<work/402798/production/date> a crm:E52_Time-Span .
<work/402798/production/date> crm:P1_is_identified_by <work/402798/production/date/appellation/preferred_name> .
<work/402798/production/date> crm:P82a_begin_of_the_begin "1876-01-01T00:00:00Z"^^xsd:dateTime .
<work/402798/production/date> crm:P82b_end_of_the_end "1919-12-31T23:59:59Z"^^xsd:dateTime .
<work/402798/production/date/appellation/preferred_name> a crm:E41_Appellation .
<work/402798/production/date/appellation/preferred_name> crm:P190_has_symbolic_content "undated" .
<work/402798/production/date/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
    |]
