module Pharos.Datasets.Frick.SubjectsSpec (spec) where

import CommonImports
import Test.Hspec
import Pharos.Datasets.Frick.Mappings.Work (baseMapping, frickBaseUri)
import Pharos.Datasets.Frick.Mappings.Subjects (subjects)

spec :: Spec
spec = describe "subject mapping" $ do
  let mapping = baseMapping +> subjects

  it "frick hierarchical subjects" $ do
    let xml = [w|
<record>
  <controlfield tag="001">991013623928607141</controlfield>
  <datafield tag="650" ind1=" " ind2="7">
    <subfield code="a">Sculpture: New Testament: Life of Christ: Crucifixion.</subfield>
  <subfield code="2">local</subfield></datafield>
</record>
    |]
    
    result <- processXMLStringAsSet mapping frickBaseUri xml
    
    result `shouldBe` [t|
<subject/sculpture> a crm:E55_Type .
<subject/sculpture> crm:P1_is_identified_by <subject/sculpture/appellation/preferred_name> .
<subject/sculpture> crm:P2_has_type pharos-meta:subject .
<subject/sculpture/appellation/preferred_name> a crm:E41_Appellation .
<subject/sculpture/appellation/preferred_name> crm:P190_has_symbolic_content "Sculpture" .
<subject/sculpture/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<subject/sculpture_new_testament> a crm:E55_Type .
<subject/sculpture_new_testament> crm:P127_has_broader_term <subject/sculpture> .
<subject/sculpture_new_testament> crm:P1_is_identified_by <subject/sculpture_new_testament/appellation/preferred_name> .
<subject/sculpture_new_testament> crm:P2_has_type pharos-meta:subject .
<subject/sculpture_new_testament/appellation/preferred_name> a crm:E41_Appellation .
<subject/sculpture_new_testament/appellation/preferred_name> crm:P190_has_symbolic_content "Sculpture : New Testament" .
<subject/sculpture_new_testament/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<subject/sculpture_new_testament_life_of_christ> a crm:E55_Type .
<subject/sculpture_new_testament_life_of_christ> crm:P127_has_broader_term <subject/sculpture_new_testament> .
<subject/sculpture_new_testament_life_of_christ> crm:P1_is_identified_by <subject/sculpture_new_testament_life_of_christ/appellation/preferred_name> .
<subject/sculpture_new_testament_life_of_christ> crm:P2_has_type pharos-meta:subject .
<subject/sculpture_new_testament_life_of_christ/appellation/preferred_name> a crm:E41_Appellation .
<subject/sculpture_new_testament_life_of_christ/appellation/preferred_name> crm:P190_has_symbolic_content "Sculpture : New Testament : Life of Christ" .
<subject/sculpture_new_testament_life_of_christ/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<subject/sculpture_new_testament_life_of_christ_crucifixion> a crm:E55_Type .
<subject/sculpture_new_testament_life_of_christ_crucifixion> crm:P127_has_broader_term <subject/sculpture_new_testament_life_of_christ> .
<subject/sculpture_new_testament_life_of_christ_crucifixion> crm:P1_is_identified_by <subject/sculpture_new_testament_life_of_christ_crucifixion/appellation/preferred_name> .
<subject/sculpture_new_testament_life_of_christ_crucifixion> crm:P2_has_type pharos-meta:subject .
<subject/sculpture_new_testament_life_of_christ_crucifixion/appellation/preferred_name> a crm:E41_Appellation .
<subject/sculpture_new_testament_life_of_christ_crucifixion/appellation/preferred_name> crm:P190_has_symbolic_content "Sculpture: New Testament: Life of Christ: Crucifixion" .
<subject/sculpture_new_testament_life_of_christ_crucifixion/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<work/991013623928607141> a crm:E22_Human-Made_Object .
<work/991013623928607141> crm:P65_shows_visual_item <work/991013623928607141/visual_item/subject/1> .
<work/991013623928607141/visual_item/subject/1> a crm:E36_Visual_Item .
<work/991013623928607141/visual_item/subject/1> crm:P2_has_type <subject/sculpture_new_testament_life_of_christ_crucifixion> .
|]



  it "subjects from title" $ do
    let xml = [w|
<record>
  <controlfield tag="001">991013498709707141</controlfield>
  <datafield tag="245" ind1="1" ind2="0">
    <subfield code="a">Self-Portrait.</subfield>
  </datafield>
</record>
    |]
    
    result <- processXMLStringAsSet mapping frickBaseUri xml
    
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
<work/991013498709707141> a crm:E22_Human-Made_Object .
<work/991013498709707141> crm:P65_shows_visual_item <work/991013498709707141/visual_item/subject/from_title> .
<work/991013498709707141/visual_item/subject/from_title> a crm:E36_Visual_Item .
<work/991013498709707141/visual_item/subject/from_title> crm:P2_has_type <subject/from_title/self_portrait> .
|]


