module Pharos.Datasets.Pmc.PhotoSpec (spec) where

import CommonImports
import Test.Hspec

import Pmc.Mappings.Work (baseMapping, pmcBaseUri)
import Pmc.Mappings.Photo (photoLinks)

spec :: Spec
spec = describe "PMC photo mappings" $ do
  let mapping = baseMapping +> photoLinks

  it "correctly maps photo with recto image" $ do
    let pmcIdentifiersXml = [w|
      <OAI-PMH xmlns:lido="http://www.lido-schema.org">
        <GetRecord>
          <record>
            <metadata>
              <lido:lido>
                <lido:descriptiveMetadata xml:lang="en">
                  <lido:eventWrap>
                    <lido:eventSet>
                      <lido:event>
                        <lido:eventID lido:source="AAT" lido:type="id">300157782</lido:eventID>
                        <lido:eventType>
                          <lido:conceptID lido:source="LIDO Terminology" lido:type="uri">http://terminology.lido-schema.org/eventType/acquisition</lido:conceptID>
                          <lido:term>Acquisition</lido:term>
                        </lido:eventType>
                        <lido:eventActor>
                          <lido:displayActorInRole>Paul Mellon Centre for Studies in British Art</lido:displayActorInRole>
                          <lido:actorInRole>
                            <lido:actor lido:type="Institution">
                              <lido:actorID lido:source="local" lido:type="id">7</lido:actorID>
                              <lido:actorID lido:source="WikiData" lido:type="id">Q7152454</lido:actorID>
                              <lido:actorID lido:source="Archon" lido:type="id">GB3010</lido:actorID>
                              <lido:actorID lido:source="VIAF" lido:type="id">131545782</lido:actorID>
                              <lido:actorID lido:source="Library of Congress" lido:type="id">n79109777</lido:actorID>
                              <lido:actorID lido:source="ULAN" lido:type="id">500301928</lido:actorID>
                              <lido:nameActorSet>
                                <lido:appellationValue lido:pref="preferred">Paul Mellon Centre for Studies in British Art</lido:appellationValue>
                                <lido:appellationValue lido:pref="alternate">Mellon Centre</lido:appellationValue>
                                <lido:appellationValue lido:pref="alternate">Mellon Foundation</lido:appellationValue>
                                <lido:appellationValue lido:pref="alternate">PMC</lido:appellationValue>
                              </lido:nameActorSet>
                            </lido:actor>
                            <lido:roleActor>
                              <lido:conceptID lido:source="AAT" lido:type="id">300312296</lido:conceptID>
                              <lido:term>Photograph Source</lido:term>
                            </lido:roleActor>
                          </lido:actorInRole>
                        </lido:eventActor>
                        <lido:eventDate/>
                        <lido:eventMaterialsTech>
                          <lido:displayMaterialsTech>Notes on condition: "Carbon ink drawing in fair condition. No foxing apparent. Some surface dirt. Slight staining round all four edges. Top and bottom uneven serrated edges. Signed. Drawing dry cleanedTreated cold and hot water. Sized.Pressed.Notes on restoration done in London. Original note card in envelope. </lido:displayMaterialsTech>
                          <lido:materialsTech>
                            <lido:termMaterialsTech>
                              <lido:conceptID lido:source="AAT" lido:type="uri">ink</lido:conceptID>
                              <lido:term xml:lang="en">ink</lido:term>
                            </lido:termMaterialsTech>
                          </lido:materialsTech>
                        </lido:eventMaterialsTech>
                      </lido:event>
                    </lido:eventSet>
                  </lido:eventWrap>
                </lido:descriptiveMetadata>
                  <lido:administrativeMetadata xml:lang="en">
                    <lido:recordWrap>
                      <lido:recordID lido:type="local">402798</lido:recordID>
                    </lido:recordWrap>
                    <lido:rightsWorkWrap>
                      <lido:rightsWorkSet>
                        <lido:rightsType>
                          <lido:conceptID lido:source="AAT" lido:type="id">300055598</lido:conceptID>
                          <lido:term>https://creativecommons.org/publicdomain/zero/1.0/</lido:term>
                        </lido:rightsType>
                        <lido:rightsHolder>
                          <lido:legalBodyID lido:source="ULAN" lido:type="id">500301928</lido:legalBodyID>
                          <lido:legalBodyName>
                            <lido:appellationValue>Paul Mellon Centre for Studies in British Art</lido:appellationValue>
                          </lido:legalBodyName>
                          <lido:legalBodyWeblink>https://www.paul-mellon-centre.ac.uk</lido:legalBodyWeblink>
                        </lido:rightsHolder>
                      </lido:rightsWorkSet>
                    </lido:rightsWorkWrap>
                    <lido:resourceWrap>
                        <lido:resourceSet>
                            <lido:resourceRepresentation lido:type="thumb">
                                <lido:linkResource lido:formatResource="image/jpg">
                                    https://photoarchive.paul-mellon-centre.ac.uk/apis/iiif/image/v2/153505/full/300%2C/0/default.jpg</lido:linkResource>
                            </lido:resourceRepresentation>
                            <lido:resourceRepresentation
                                lido:type="http://iiif.io/api/presentation/2/context.json">
                                <lido:linkResource lido:formatResource="application/ld+json">
                                    https://photoarchive.paul-mellon-centre.ac.uk/apis/iiif/presentation/v2/1-objects-402798/manifest</lido:linkResource>
                            </lido:resourceRepresentation>
                            <lido:resourceType>
                                <lido:term>IIIF Image Info</lido:term>
                            </lido:resourceType>
                            <lido:resourceSource>
                                <lido:legalBodyName>
                                    <lido:appellationValue>Paul Mellon Centre</lido:appellationValue>
                                </lido:legalBodyName>
                            </lido:resourceSource>
                        </lido:resourceSet>
                        <lido:resourceSet>
                            <lido:resourceRepresentation lido:type="thumb">
                                <lido:linkResource lido:formatResource="image/jpg">
                                    https://photoarchive.paul-mellon-centre.ac.uk/apis/iiif/image/v2/153506/full/300%2C/0/default.jpg</lido:linkResource>
                            </lido:resourceRepresentation>
                            <lido:resourceRepresentation
                                lido:type="http://iiif.io/api/presentation/2/context.json">
                                <lido:linkResource lido:formatResource="application/ld+json">
                                    https://photoarchive.paul-mellon-centre.ac.uk/apis/iiif/presentation/v2/1-objects-402798/manifest</lido:linkResource>
                            </lido:resourceRepresentation>
                            <lido:resourceType>
                                <lido:term>IIIF Image Info</lido:term>
                            </lido:resourceType>
                            <lido:resourceSource>
                                <lido:legalBodyName>
                                    <lido:appellationValue>Paul Mellon Centre</lido:appellationValue>
                                </lido:legalBodyName>
                            </lido:resourceSource>
                        </lido:resourceSet>
                    </lido:resourceWrap>
                  </lido:administrativeMetadata>
              </lido:lido>
            </metadata>
          </record>
        </GetRecord>
      </OAI-PMH>
    |]
    
    result <- processXMLStringAsSet mapping pmcBaseUri pmcIdentifiersXml
    
    result `shouldBe` [t|
<actor/photographer/paul_mellon_centre_for_studies_in_british_art> a crm:E39_Actor .
<actor/photographer/paul_mellon_centre_for_studies_in_british_art> crm:P1_is_identified_by <actor/photographer/paul_mellon_centre_for_studies_in_british_art/appellation/preferred_name> .
<actor/photographer/paul_mellon_centre_for_studies_in_british_art> crm:P2_has_type pharos-meta:photographer .
<actor/photographer/paul_mellon_centre_for_studies_in_british_art> custom:sameAs <http://www.wikidata.org/entity/Q7152454> .
<actor/photographer/paul_mellon_centre_for_studies_in_british_art/appellation/preferred_name> a crm:E41_Appellation .
<actor/photographer/paul_mellon_centre_for_studies_in_british_art/appellation/preferred_name> crm:P190_has_symbolic_content "Paul Mellon Centre for Studies in British Art" .
<actor/photographer/paul_mellon_centre_for_studies_in_british_art/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<http://creativecommons.org/publicdomain/zero/1.0/> a crm:E55_Type .
<http://creativecommons.org/publicdomain/zero/1.0/> crm:P2_has_type pharos-meta:license_type .
<http://vocab.getty.edu/ulan/500301928> a crm:E39_Actor .
<http://vocab.getty.edu/ulan/500301928> crm:P1_is_identified_by <http://vocab.getty.edu/ulan/500301928/name> .
<http://vocab.getty.edu/ulan/500301928/name> a crm:E41_Appellation .
<http://vocab.getty.edu/ulan/500301928/name> crm:P190_has_symbolic_content "Paul Mellon Centre for Studies in British Art" .
<http://vocab.getty.edu/ulan/500301928/name> crm:P2_has_type pharos-meta:preferred_name .
<http://www.wikidata.org/entity/Q7152454> a crm:E39_Actor .
<https://artresearch.net/resource/e31/pmc> a crm:E31_Document .
<https://photoarchive.paul-mellon-centre.ac.uk/apis/iiif/image/v2/153505/full/full/0/default.jpg> a crm:E42_Identifier .
<https://photoarchive.paul-mellon-centre.ac.uk/apis/iiif/image/v2/153505/full/full/0/default.jpg> crm:P2_has_type pharos-meta:photo_file_url .
<https://photoarchive.paul-mellon-centre.ac.uk/apis/iiif/image/v2/153505/full/full/0/default.jpg> image-api:storage-id "pmc" .
<work/402798> a crm:E22_Human-Made_Object .
<work/402798> crm:P138i_has_representation <work/402798/visual_item> .
<work/402798/visual_item> a crm:E36_Visual_Item .
<work/402798/visual_item> crm:P65i_is_shown_by <work/402798/visual_item/photo> .
<work/402798/visual_item/photo> a crm:E22_Human-Made_Object .
<work/402798/visual_item/photo> crm:P104_is_subject_to <work/402798/visual_item/photo/rights> .
<work/402798/visual_item/photo> crm:P108i_was_produced_by <work/402798/visual_item/photo/production> .
<work/402798/visual_item/photo> crm:P138i_has_representation <work/402798/visual_item/photo/visual_item> .
<work/402798/visual_item/photo> crm:P2_has_type pharos-meta:photographic_print .
<work/402798/visual_item/photo> crm:P50_has_current_keeper <http://vocab.getty.edu/ulan/500301928> .
<work/402798/visual_item/photo> crm:P70i_is_documented_in <https://artresearch.net/resource/e31/pmc> .
<work/402798/visual_item/photo/rights> a crm:E30_Right .
<work/402798/visual_item/photo/production> a crm:E12_Production .
<work/402798/visual_item/photo/production> crm:P14_carried_out_by <actor/photographer/paul_mellon_centre_for_studies_in_british_art> .
<work/402798/visual_item/photo/rights> a crm:E30_Right .
<work/402798/visual_item/photo/rights> crm:P2_has_type <http://creativecommons.org/publicdomain/zero/1.0/> .
<work/402798/visual_item/photo/visual_item> a crm:E36_Visual_Item .
<work/402798/visual_item/photo/visual_item> crm:P165i_is_incorporated_in <work/402798/visual_item/photo/visual_item/image> .
<work/402798/visual_item/photo/visual_item/image> a crm:D1_Digital_Object .
<work/402798/visual_item/photo/visual_item/image> crm:P1_is_identified_by <https://photoarchive.paul-mellon-centre.ac.uk/apis/iiif/image/v2/153505/full/full/0/default.jpg> .
<work/402798/visual_item/photo/visual_item/image> crm:P2_has_type pharos-meta:digital_image .
    |]

  it "correctly maps photo with non-URI rights type" $ do
    let pmcIdentifiersXml = [w|
      <OAI-PMH xmlns:lido="http://www.lido-schema.org">
        <GetRecord>
          <record>
            <metadata>
              <lido:lido>
                  <lido:administrativeMetadata xml:lang="en">
                    <lido:recordWrap>
                      <lido:recordID lido:type="local">420383</lido:recordID>
                    </lido:recordWrap>
                    <lido:rightsWorkWrap>
                      <lido:rightsWorkSet>
                        <lido:rightsType>
                          <lido:conceptID lido:source="AAT" lido:type="id">300055598</lido:conceptID>
                          <lido:term>Crown Copyright</lido:term>
                        </lido:rightsType>
                        <lido:rightsHolder>
                          <lido:legalBodyID lido:source="ULAN" lido:type="id">500301928</lido:legalBodyID>
                          <lido:legalBodyName>
                            <lido:appellationValue>Paul Mellon Centre for Studies in British Art</lido:appellationValue>
                          </lido:legalBodyName>
                          <lido:legalBodyWeblink>https://www.paul-mellon-centre.ac.uk</lido:legalBodyWeblink>
                        </lido:rightsHolder>
                      </lido:rightsWorkSet>
                    </lido:rightsWorkWrap>
                    <lido:resourceWrap>
                        <lido:resourceSet>
                            <lido:resourceRepresentation lido:type="thumb">
                                <lido:linkResource lido:formatResource="image/jpg">
                                    https://photoarchive.paul-mellon-centre.ac.uk/apis/iiif/image/v2/188675/full/300%2C/0/default.jpg</lido:linkResource>
                            </lido:resourceRepresentation>
                            <lido:resourceRepresentation
                                lido:type="http://iiif.io/api/presentation/2/context.json">
                                <lido:linkResource lido:formatResource="application/ld+json">
                                    https://photoarchive.paul-mellon-centre.ac.uk/apis/iiif/presentation/v2/1-objects-420383/manifest</lido:linkResource>
                            </lido:resourceRepresentation>
                            <lido:resourceType>
                                <lido:term>IIIF Image Info</lido:term>
                            </lido:resourceType>
                            <lido:resourceSource>
                                <lido:legalBodyName>
                                    <lido:appellationValue>Paul Mellon Centre</lido:appellationValue>
                                </lido:legalBodyName>
                            </lido:resourceSource>
                        </lido:resourceSet>
                    </lido:resourceWrap>
                  </lido:administrativeMetadata>
              </lido:lido>
            </metadata>
          </record>
        </GetRecord>
      </OAI-PMH>
    |]
    
    result <- processXMLStringAsSet mapping pmcBaseUri pmcIdentifiersXml
    
    result `shouldBe` [t|
<copyright/crown_copyright> a crm:E55_Type .
<copyright/crown_copyright> crm:P1_is_identified_by <copyright/crown_copyright/appellation/preferred_name> .
<copyright/crown_copyright> crm:P2_has_type pharos-meta:license_type .
<copyright/crown_copyright/appellation/preferred_name> a crm:E41_Appellation .
<copyright/crown_copyright/appellation/preferred_name> crm:P190_has_symbolic_content "Crown Copyright" .
<copyright/crown_copyright/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<http://vocab.getty.edu/ulan/500301928> a crm:E39_Actor .
<http://vocab.getty.edu/ulan/500301928> crm:P1_is_identified_by <http://vocab.getty.edu/ulan/500301928/name> .
<http://vocab.getty.edu/ulan/500301928/name> a crm:E41_Appellation .
<http://vocab.getty.edu/ulan/500301928/name> crm:P190_has_symbolic_content "Paul Mellon Centre for Studies in British Art" .
<http://vocab.getty.edu/ulan/500301928/name> crm:P2_has_type pharos-meta:preferred_name .
<https://artresearch.net/resource/e31/pmc> a crm:E31_Document .
<https://photoarchive.paul-mellon-centre.ac.uk/apis/iiif/image/v2/188675/full/full/0/default.jpg> a crm:E42_Identifier .
<https://photoarchive.paul-mellon-centre.ac.uk/apis/iiif/image/v2/188675/full/full/0/default.jpg> crm:P2_has_type pharos-meta:photo_file_url .
<https://photoarchive.paul-mellon-centre.ac.uk/apis/iiif/image/v2/188675/full/full/0/default.jpg> image-api:storage-id "pmc" .
<work/420383> a crm:E22_Human-Made_Object .
<work/420383> crm:P138i_has_representation <work/420383/visual_item> .
<work/420383/visual_item> a crm:E36_Visual_Item .
<work/420383/visual_item> crm:P65i_is_shown_by <work/420383/visual_item/photo> .
<work/420383/visual_item/photo> a crm:E22_Human-Made_Object .
<work/420383/visual_item/photo> crm:P104_is_subject_to <work/420383/visual_item/photo/rights> .
<work/420383/visual_item/photo> crm:P138i_has_representation <work/420383/visual_item/photo/visual_item> .
<work/420383/visual_item/photo> crm:P2_has_type pharos-meta:photographic_print .
<work/420383/visual_item/photo> crm:P50_has_current_keeper <http://vocab.getty.edu/ulan/500301928> .
<work/420383/visual_item/photo> crm:P70i_is_documented_in <https://artresearch.net/resource/e31/pmc> .
<work/420383/visual_item/photo/rights> a crm:E30_Right .
<work/420383/visual_item/photo/rights> crm:P2_has_type <copyright/crown_copyright> .
<work/420383/visual_item/photo/visual_item> a crm:E36_Visual_Item .
<work/420383/visual_item/photo/visual_item> crm:P165i_is_incorporated_in <work/420383/visual_item/photo/visual_item/image> .
<work/420383/visual_item/photo/visual_item/image> a crm:D1_Digital_Object .
<work/420383/visual_item/photo/visual_item/image> crm:P1_is_identified_by <https://photoarchive.paul-mellon-centre.ac.uk/apis/iiif/image/v2/188675/full/full/0/default.jpg> .
<work/420383/visual_item/photo/visual_item/image> crm:P2_has_type pharos-meta:digital_image .
    |]
