module Pharos.Datasets.Zeri.SubjectsSpec (spec) where

import CommonImports
import Test.Hspec
import Mappings.Mappings.Work.WorkMappings
import Mappings.Mappings.Work.Subjects

spec :: Spec
spec = describe "subject mapping" $ do
  it "multiple subjects, with one linked to iconclass" $ do
    let mapping = baseMapping +> [subjectLink]
    let xml = [w|
      <RISULTATI>
        <SCHEDA sercdoa="28129">
          <PARAGRAFO etichetta="OBJECT">
            <SGTI etichetta="Subject">Cristo Redentore benedicente e angeli,Evangelisti, Dottori della Chiesa e profeti,Santi,Martirii di santi</SGTI>
          </PARAGRAFO>
        </SCHEDA>
      </RISULTATI>
    |]
    
    result <- processXMLStringAsSet mapping zeriBaseUri xml
    
    result `shouldBe` [t|
<http://iconclass.org/11H> a crm:E55_Type .
<http://iconclass.org/11H> crm:P1_is_identified_by <http://iconclass.org/11H/id/preferred_identifier> .
<http://iconclass.org/11H> crm:P2_has_type pharos-meta:iconclass .
<http://iconclass.org/11H/id/preferred_identifier> a crm:E42_Identifier .
<http://iconclass.org/11H/id/preferred_identifier> crm:P190_has_symbolic_content "11 H" .
<http://iconclass.org/11H/id/preferred_identifier> crm:P2_has_type pharos-meta:preferred_identifier .
<subject/cristo_redentore_benedicente_e_angeli> a crm:E55_Type .
<subject/cristo_redentore_benedicente_e_angeli> crm:P1_is_identified_by <subject/cristo_redentore_benedicente_e_angeli/appellation/preferred_name> .
<subject/cristo_redentore_benedicente_e_angeli> crm:P2_has_type pharos-meta:subject .
<subject/cristo_redentore_benedicente_e_angeli/appellation/preferred_name> a crm:E41_Appellation .
<subject/cristo_redentore_benedicente_e_angeli/appellation/preferred_name> crm:P190_has_symbolic_content "Cristo Redentore benedicente e angeli" .
<subject/cristo_redentore_benedicente_e_angeli/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<subject/evangelisti_dottori_della_chiesa_e_profeti> a crm:E55_Type .
<subject/evangelisti_dottori_della_chiesa_e_profeti> crm:P1_is_identified_by <subject/evangelisti_dottori_della_chiesa_e_profeti/appellation/preferred_name> .
<subject/evangelisti_dottori_della_chiesa_e_profeti> crm:P2_has_type pharos-meta:subject .
<subject/evangelisti_dottori_della_chiesa_e_profeti/appellation/preferred_name> a crm:E41_Appellation .
<subject/evangelisti_dottori_della_chiesa_e_profeti/appellation/preferred_name> crm:P190_has_symbolic_content "Evangelisti, Dottori della Chiesa e profeti" .
<subject/evangelisti_dottori_della_chiesa_e_profeti/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<subject/martirii_di_santi> a crm:E55_Type .
<subject/martirii_di_santi> crm:P1_is_identified_by <subject/martirii_di_santi/appellation/preferred_name> .
<subject/martirii_di_santi> crm:P2_has_type pharos-meta:subject .
<subject/martirii_di_santi/appellation/preferred_name> a crm:E41_Appellation .
<subject/martirii_di_santi/appellation/preferred_name> crm:P190_has_symbolic_content "Martirii di santi" .
<subject/martirii_di_santi/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<subject/santi> a crm:E55_Type .
<subject/santi> crm:P1_is_identified_by <subject/santi/appellation/preferred_name> .
<subject/santi> crm:P2_has_type pharos-meta:subject .
<subject/santi> custom:sameAs <http://iconclass.org/11H> .
<subject/santi/appellation/preferred_name> a crm:E41_Appellation .
<subject/santi/appellation/preferred_name> crm:P190_has_symbolic_content "Santi" .
<subject/santi/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<work/28129> a crm:E22_Human-Made_Object .
<work/28129> crm:P65_shows_visual_item <work/28129/visual_item/subject/1> .
<work/28129/visual_item/subject/1> a crm:E36_Visual_Item .
<work/28129/visual_item/subject/1> crm:P2_has_type <subject/cristo_redentore_benedicente_e_angeli> .
<work/28129/visual_item/subject/1> crm:P2_has_type <subject/evangelisti_dottori_della_chiesa_e_profeti> .
<work/28129/visual_item/subject/1> crm:P2_has_type <subject/martirii_di_santi> .
<work/28129/visual_item/subject/1> crm:P2_has_type <subject/santi> .
    |]

  it "complex iconclass subject" $ do
    let mapping = baseMapping +> [subjectLink]
    let xml = [w|
      <RISULTATI>
        <SCHEDA sercdoa="78710">
          <PARAGRAFO etichetta="OBJECT">
            <SGTI etichetta="Subject">Orfeo incanta gli animali con la musica</SGTI>
          </PARAGRAFO>
        </SCHEDA>
      </RISULTATI>
    |]
    
    result <- processXMLStringAsSet mapping zeriBaseUri xml
    
    result `shouldBe` [t|
<http://iconclass.org/25F1> a crm:E55_Type .
<http://iconclass.org/25F1> crm:P1_is_identified_by <http://iconclass.org/25F1/id/preferred_identifier> .
<http://iconclass.org/25F1> crm:P2_has_type pharos-meta:iconclass .
<http://iconclass.org/25F1/id/preferred_identifier> a crm:E42_Identifier .
<http://iconclass.org/25F1/id/preferred_identifier> crm:P190_has_symbolic_content "25 F 1" .
<http://iconclass.org/25F1/id/preferred_identifier> crm:P2_has_type pharos-meta:preferred_identifier .
<http://iconclass.org/48C7313> a crm:E55_Type .
<http://iconclass.org/48C7313> crm:P1_is_identified_by <http://iconclass.org/48C7313/id/preferred_identifier> .
<http://iconclass.org/48C7313> crm:P2_has_type pharos-meta:iconclass .
<http://iconclass.org/48C7313/id/preferred_identifier> a crm:E42_Identifier .
<http://iconclass.org/48C7313/id/preferred_identifier> crm:P190_has_symbolic_content "48 C 73 13" .
<http://iconclass.org/48C7313/id/preferred_identifier> crm:P2_has_type pharos-meta:preferred_identifier .
<http://iconclass.org/94O511> a crm:E55_Type .
<http://iconclass.org/94O511> crm:P1_is_identified_by <http://iconclass.org/94O511/id/preferred_identifier> .
<http://iconclass.org/94O511> crm:P2_has_type pharos-meta:iconclass .
<http://iconclass.org/94O511%3A48C7313%3A25F1> a crm:E55_Type .
<http://iconclass.org/94O511%3A48C7313%3A25F1> crm:P127_has_broader_term <http://iconclass.org/94O511> .
<http://iconclass.org/94O511%3A48C7313%3A25F1> crm:P130_shows_features_of <http://iconclass.org/25F1> .
<http://iconclass.org/94O511%3A48C7313%3A25F1> crm:P130_shows_features_of <http://iconclass.org/48C7313> .
<http://iconclass.org/94O511%3A48C7313%3A25F1> crm:P1_is_identified_by <http://iconclass.org/94O511%3A48C7313%3A25F1/id/preferred_identifier> .
<http://iconclass.org/94O511%3A48C7313%3A25F1> crm:P2_has_type pharos-meta:iconclass .
<http://iconclass.org/94O511%3A48C7313%3A25F1/id/preferred_identifier> a crm:E42_Identifier .
<http://iconclass.org/94O511%3A48C7313%3A25F1/id/preferred_identifier> crm:P190_has_symbolic_content "94 O 51 1 : 48 C 73 13 : 25 F 1" .
<http://iconclass.org/94O511%3A48C7313%3A25F1/id/preferred_identifier> crm:P2_has_type pharos-meta:preferred_identifier .
<http://iconclass.org/94O511/id/preferred_identifier> a crm:E42_Identifier .
<http://iconclass.org/94O511/id/preferred_identifier> crm:P190_has_symbolic_content "94 O 51 1" .
<http://iconclass.org/94O511/id/preferred_identifier> crm:P2_has_type pharos-meta:preferred_identifier .
<subject/orfeo_incanta_gli_animali_con_la_musica> a crm:E55_Type .
<subject/orfeo_incanta_gli_animali_con_la_musica> crm:P1_is_identified_by <subject/orfeo_incanta_gli_animali_con_la_musica/appellation/preferred_name> .
<subject/orfeo_incanta_gli_animali_con_la_musica> crm:P2_has_type pharos-meta:subject .
<subject/orfeo_incanta_gli_animali_con_la_musica> custom:sameAs <http://iconclass.org/94O511%3A48C7313%3A25F1> .
<subject/orfeo_incanta_gli_animali_con_la_musica/appellation/preferred_name> a crm:E41_Appellation .
<subject/orfeo_incanta_gli_animali_con_la_musica/appellation/preferred_name> crm:P190_has_symbolic_content "Orfeo incanta gli animali con la musica" .
<subject/orfeo_incanta_gli_animali_con_la_musica/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<work/78710> a crm:E22_Human-Made_Object .
<work/78710> crm:P65_shows_visual_item <work/78710/visual_item/subject/1> .
<work/78710/visual_item/subject/1> a crm:E36_Visual_Item .
<work/78710/visual_item/subject/1> crm:P2_has_type <subject/orfeo_incanta_gli_animali_con_la_musica> .
    |]
