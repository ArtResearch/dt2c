module Pharos.Datasets.Midas.SubjectsSpec (spec) where

import CommonImports
import Test.Hspec
import Midas.Mappings.Work (baseMapping, midasBaseUri)
import Midas.Mappings.Subjects (subjectLinks)

spec :: Spec
spec = describe "subject mapping" $ do
  let mapping = baseMapping +> subjectLinks

  it "hertziana, multiple subjects, with one linked to iconclass" $ do
    let xml = [w|
<obj>
  <a5000>08032609</a5000>
  <a5500>49 L 7</a5500>
  <a5500>48 C 11 1</a5500>
  <a5500>49 D 52 : 11 Q 71 3 : 61 F (SAN PIETRO) : 61 E (ROMA)</a5500>
</obj>
    |]
    
    result <- processXMLStringAsSet mapping midasBaseUri xml
    
    result `shouldBe` [t|
<http://iconclass.org/11Q713> crm:P1_is_identified_by <http://iconclass.org/11Q713/id/preferred_identifier> .
<http://iconclass.org/11Q713> crm:P2_has_type pharos-meta:iconclass .
<http://iconclass.org/11Q713> a crm:E55_Type .
<http://iconclass.org/11Q713/id/preferred_identifier> crm:P190_has_symbolic_content "11 Q 71 3" .
<http://iconclass.org/11Q713/id/preferred_identifier> crm:P2_has_type pharos-meta:preferred_identifier .
<http://iconclass.org/11Q713/id/preferred_identifier> a crm:E42_Identifier .
<http://iconclass.org/48C111> crm:P1_is_identified_by <http://iconclass.org/48C111/id/preferred_identifier> .
<http://iconclass.org/48C111> crm:P2_has_type pharos-meta:iconclass .
<http://iconclass.org/48C111> a crm:E55_Type .
<http://iconclass.org/48C111/id/preferred_identifier> crm:P190_has_symbolic_content "48 C 11 1" .
<http://iconclass.org/48C111/id/preferred_identifier> crm:P2_has_type pharos-meta:preferred_identifier .
<http://iconclass.org/48C111/id/preferred_identifier> a crm:E42_Identifier .
<http://iconclass.org/49D52> crm:P1_is_identified_by <http://iconclass.org/49D52/id/preferred_identifier> .
<http://iconclass.org/49D52> crm:P2_has_type pharos-meta:iconclass .
<http://iconclass.org/49D52> a crm:E55_Type .
<http://iconclass.org/49D52%3A11Q713%3A61F%28...%29%3A61E%28ROMA%29> crm:P1_is_identified_by <http://iconclass.org/49D52%3A11Q713%3A61F%28...%29%3A61E%28ROMA%29/id/preferred_identifier> .
<http://iconclass.org/49D52%3A11Q713%3A61F%28...%29%3A61E%28ROMA%29> crm:P2_has_type pharos-meta:iconclass .
<http://iconclass.org/49D52%3A11Q713%3A61F%28...%29%3A61E%28ROMA%29> a crm:E55_Type .
<http://iconclass.org/49D52%3A11Q713%3A61F%28...%29%3A61E%28ROMA%29/id/preferred_identifier> crm:P190_has_symbolic_content "49 D 52 : 11 Q 71 3 : 61 F (...) : 61 E (ROMA)" .
<http://iconclass.org/49D52%3A11Q713%3A61F%28...%29%3A61E%28ROMA%29/id/preferred_identifier> crm:P2_has_type pharos-meta:preferred_identifier .
<http://iconclass.org/49D52%3A11Q713%3A61F%28...%29%3A61E%28ROMA%29/id/preferred_identifier> a crm:E42_Identifier .
<http://iconclass.org/49D52%3A11Q713%3A61F%28SANPIETRO%29%3A61E%28ROMA%29> crm:P127_has_broader_term <http://iconclass.org/49D52> .
<http://iconclass.org/49D52%3A11Q713%3A61F%28SANPIETRO%29%3A61E%28ROMA%29> crm:P127_has_broader_term <http://iconclass.org/49D52%3A11Q713%3A61F%28...%29%3A61E%28ROMA%29> .
<http://iconclass.org/49D52%3A11Q713%3A61F%28SANPIETRO%29%3A61E%28ROMA%29> crm:P130_shows_features_of <http://iconclass.org/11Q713> .
<http://iconclass.org/49D52%3A11Q713%3A61F%28SANPIETRO%29%3A61E%28ROMA%29> crm:P130_shows_features_of <http://iconclass.org/61E%28ROMA%29> .
<http://iconclass.org/49D52%3A11Q713%3A61F%28SANPIETRO%29%3A61E%28ROMA%29> crm:P130_shows_features_of <http://iconclass.org/61F%28SANPIETRO%29> .
<http://iconclass.org/49D52%3A11Q713%3A61F%28SANPIETRO%29%3A61E%28ROMA%29> crm:P1_is_identified_by <http://iconclass.org/49D52%3A11Q713%3A61F%28SANPIETRO%29%3A61E%28ROMA%29/appellation/preferred_name> .
<http://iconclass.org/49D52%3A11Q713%3A61F%28SANPIETRO%29%3A61E%28ROMA%29> crm:P1_is_identified_by <http://iconclass.org/49D52%3A11Q713%3A61F%28SANPIETRO%29%3A61E%28ROMA%29/id/preferred_identifier> .
<http://iconclass.org/49D52%3A11Q713%3A61F%28SANPIETRO%29%3A61E%28ROMA%29> crm:P2_has_type pharos-meta:iconclass .
<http://iconclass.org/49D52%3A11Q713%3A61F%28SANPIETRO%29%3A61E%28ROMA%29> a crm:E55_Type .
<http://iconclass.org/49D52%3A11Q713%3A61F%28SANPIETRO%29%3A61E%28ROMA%29/appellation/preferred_name> crm:P190_has_symbolic_content "SAN PIETRO" .
<http://iconclass.org/49D52%3A11Q713%3A61F%28SANPIETRO%29%3A61E%28ROMA%29/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<http://iconclass.org/49D52%3A11Q713%3A61F%28SANPIETRO%29%3A61E%28ROMA%29/appellation/preferred_name> a crm:E41_Appellation .
<http://iconclass.org/49D52%3A11Q713%3A61F%28SANPIETRO%29%3A61E%28ROMA%29/id/preferred_identifier> crm:P190_has_symbolic_content "49 D 52 : 11 Q 71 3 : 61 F (SAN PIETRO) : 61 E (ROMA)" .
<http://iconclass.org/49D52%3A11Q713%3A61F%28SANPIETRO%29%3A61E%28ROMA%29/id/preferred_identifier> crm:P2_has_type pharos-meta:preferred_identifier .
<http://iconclass.org/49D52%3A11Q713%3A61F%28SANPIETRO%29%3A61E%28ROMA%29/id/preferred_identifier> a crm:E42_Identifier .
<http://iconclass.org/49D52/id/preferred_identifier> crm:P190_has_symbolic_content "49 D 52" .
<http://iconclass.org/49D52/id/preferred_identifier> crm:P2_has_type pharos-meta:preferred_identifier .
<http://iconclass.org/49D52/id/preferred_identifier> a crm:E42_Identifier .
<http://iconclass.org/49L7> crm:P1_is_identified_by <http://iconclass.org/49L7/id/preferred_identifier> .
<http://iconclass.org/49L7> crm:P2_has_type pharos-meta:iconclass .
<http://iconclass.org/49L7> a crm:E55_Type .
<http://iconclass.org/49L7/id/preferred_identifier> crm:P190_has_symbolic_content "49 L 7" .
<http://iconclass.org/49L7/id/preferred_identifier> crm:P2_has_type pharos-meta:preferred_identifier .
<http://iconclass.org/49L7/id/preferred_identifier> a crm:E42_Identifier .
<http://iconclass.org/61E%28...%29> crm:P1_is_identified_by <http://iconclass.org/61E%28...%29/id/preferred_identifier> .
<http://iconclass.org/61E%28...%29> crm:P2_has_type pharos-meta:iconclass .
<http://iconclass.org/61E%28...%29> a crm:E55_Type .
<http://iconclass.org/61E%28...%29/id/preferred_identifier> crm:P190_has_symbolic_content "61 E (...)" .
<http://iconclass.org/61E%28...%29/id/preferred_identifier> crm:P2_has_type pharos-meta:preferred_identifier .
<http://iconclass.org/61E%28...%29/id/preferred_identifier> a crm:E42_Identifier .
<http://iconclass.org/61E%28ROMA%29> crm:P127_has_broader_term <http://iconclass.org/61E%28...%29> .
<http://iconclass.org/61E%28ROMA%29> crm:P1_is_identified_by <http://iconclass.org/61E%28ROMA%29/appellation/preferred_name> .
<http://iconclass.org/61E%28ROMA%29> crm:P1_is_identified_by <http://iconclass.org/61E%28ROMA%29/id/preferred_identifier> .
<http://iconclass.org/61E%28ROMA%29> crm:P2_has_type pharos-meta:iconclass .
<http://iconclass.org/61E%28ROMA%29> a crm:E55_Type .
<http://iconclass.org/61E%28ROMA%29/appellation/preferred_name> crm:P190_has_symbolic_content "ROMA" .
<http://iconclass.org/61E%28ROMA%29/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<http://iconclass.org/61E%28ROMA%29/appellation/preferred_name> a crm:E41_Appellation .
<http://iconclass.org/61E%28ROMA%29/id/preferred_identifier> crm:P190_has_symbolic_content "61 E (ROMA)" .
<http://iconclass.org/61E%28ROMA%29/id/preferred_identifier> crm:P2_has_type pharos-meta:preferred_identifier .
<http://iconclass.org/61E%28ROMA%29/id/preferred_identifier> a crm:E42_Identifier .
<http://iconclass.org/61F%28...%29> crm:P1_is_identified_by <http://iconclass.org/61F%28...%29/id/preferred_identifier> .
<http://iconclass.org/61F%28...%29> crm:P2_has_type pharos-meta:iconclass .
<http://iconclass.org/61F%28...%29> a crm:E55_Type .
<http://iconclass.org/61F%28...%29/id/preferred_identifier> crm:P190_has_symbolic_content "61 F (...)" .
<http://iconclass.org/61F%28...%29/id/preferred_identifier> crm:P2_has_type pharos-meta:preferred_identifier .
<http://iconclass.org/61F%28...%29/id/preferred_identifier> a crm:E42_Identifier .
<http://iconclass.org/61F%28SANPIETRO%29> crm:P127_has_broader_term <http://iconclass.org/61F%28...%29> .
<http://iconclass.org/61F%28SANPIETRO%29> crm:P1_is_identified_by <http://iconclass.org/61F%28SANPIETRO%29/appellation/preferred_name> .
<http://iconclass.org/61F%28SANPIETRO%29> crm:P1_is_identified_by <http://iconclass.org/61F%28SANPIETRO%29/id/preferred_identifier> .
<http://iconclass.org/61F%28SANPIETRO%29> crm:P2_has_type pharos-meta:iconclass .
<http://iconclass.org/61F%28SANPIETRO%29> a crm:E55_Type .
<http://iconclass.org/61F%28SANPIETRO%29/appellation/preferred_name> crm:P190_has_symbolic_content "SAN PIETRO" .
<http://iconclass.org/61F%28SANPIETRO%29/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<http://iconclass.org/61F%28SANPIETRO%29/appellation/preferred_name> a crm:E41_Appellation .
<http://iconclass.org/61F%28SANPIETRO%29/id/preferred_identifier> crm:P190_has_symbolic_content "61 F (SAN PIETRO)" .
<http://iconclass.org/61F%28SANPIETRO%29/id/preferred_identifier> crm:P2_has_type pharos-meta:preferred_identifier .
<http://iconclass.org/61F%28SANPIETRO%29/id/preferred_identifier> a crm:E42_Identifier .
<work/08032609> a crm:E22_Human-Made_Object .
<work/08032609> crm:P65_shows_visual_item <work/08032609/visual_item/subject/primary/1> .
<work/08032609> crm:P65_shows_visual_item <work/08032609/visual_item/subject/primary/2> .
<work/08032609> crm:P65_shows_visual_item <work/08032609/visual_item/subject/primary/3> .
<work/08032609/visual_item/subject/primary/1> a crm:E36_Visual_Item .
<work/08032609/visual_item/subject/primary/1> crm:P2_has_type <http://iconclass.org/49L7> .
<work/08032609/visual_item/subject/primary/2> a crm:E36_Visual_Item .
<work/08032609/visual_item/subject/primary/2> crm:P2_has_type <http://iconclass.org/48C111> .
<work/08032609/visual_item/subject/primary/3> a crm:E36_Visual_Item .
<work/08032609/visual_item/subject/primary/3> crm:P2_has_type <http://iconclass.org/49D52%3A11Q713%3A61F%28SANPIETRO%29%3A61E%28ROMA%29> .
|]

  it "marburg, secondary iconography without ICO ref" $ do
    let xml = [w|
<obj>
  <a5000>21028279</a5000>
  <a5510>11H &amp; 48A9831(+1)</a5510>
</obj>
    |]
    
    result <- processXMLStringAsSet mapping midasBaseUri xml
    
    result `shouldBe` [t|
<http://iconclass.org/11H> crm:P1_is_identified_by <http://iconclass.org/11H/id/preferred_identifier> .
<http://iconclass.org/11H> crm:P2_has_type pharos-meta:iconclass .
<http://iconclass.org/11H> a crm:E55_Type .
<http://iconclass.org/11H/id/preferred_identifier> crm:P190_has_symbolic_content "11H" .
<http://iconclass.org/11H/id/preferred_identifier> crm:P2_has_type pharos-meta:preferred_identifier .
<http://iconclass.org/11H/id/preferred_identifier> a crm:E42_Identifier .
<http://iconclass.org/48A9831%28%2B1%29> crm:P1_is_identified_by <http://iconclass.org/48A9831%28%2B1%29/id/preferred_identifier> .
<http://iconclass.org/48A9831%28%2B1%29> crm:P2_has_type pharos-meta:iconclass .
<http://iconclass.org/48A9831%28%2B1%29> a crm:E55_Type .
<http://iconclass.org/48A9831%28%2B1%29/id/preferred_identifier> crm:P190_has_symbolic_content "48A9831(+1)" .
<http://iconclass.org/48A9831%28%2B1%29/id/preferred_identifier> crm:P2_has_type pharos-meta:preferred_identifier .
<http://iconclass.org/48A9831%28%2B1%29/id/preferred_identifier> a crm:E42_Identifier .
<work/21028279> crm:P65_shows_visual_item <work/21028279/visual_item/subject/secondary/1> .
<work/21028279> crm:P65_shows_visual_item <work/21028279/visual_item/subject/secondary/2> .
<work/21028279> a crm:E22_Human-Made_Object .
<work/21028279/visual_item/subject/secondary/1> crm:P2_has_type <http://iconclass.org/11H> .
<work/21028279/visual_item/subject/secondary/1> a crm:E36_Visual_Item .
<work/21028279/visual_item/subject/secondary/2> crm:P2_has_type <http://iconclass.org/48A9831%28%2B1%29> .
<work/21028279/visual_item/subject/secondary/2> a crm:E36_Visual_Item .
|]


  it "khi, complex iconclass" $ do
    let xml = [w|
<obj>
  <a5000>07810480</a5000>
  <a5500 modifier="98C(Cleopatra)52"></a5500>
</obj>
    |]
    
    result <- processXMLStringAsSet mapping midasBaseUri xml
    
    result `shouldBe` [t|
<http://iconclass.org/98C%28...%2952> crm:P1_is_identified_by <http://iconclass.org/98C%28...%2952/id/preferred_identifier> .
<http://iconclass.org/98C%28...%2952> crm:P2_has_type pharos-meta:iconclass .
<http://iconclass.org/98C%28...%2952> a crm:E55_Type .
<http://iconclass.org/98C%28...%2952/id/preferred_identifier> crm:P190_has_symbolic_content "98C(...)52" .
<http://iconclass.org/98C%28...%2952/id/preferred_identifier> crm:P2_has_type pharos-meta:preferred_identifier .
<http://iconclass.org/98C%28...%2952/id/preferred_identifier> a crm:E42_Identifier .
<http://iconclass.org/98C%28CLEOPATRA%2952> crm:P127_has_broader_term <http://iconclass.org/98C%28...%2952> .
<http://iconclass.org/98C%28CLEOPATRA%2952> crm:P1_is_identified_by <http://iconclass.org/98C%28CLEOPATRA%2952/appellation/preferred_name> .
<http://iconclass.org/98C%28CLEOPATRA%2952> crm:P1_is_identified_by <http://iconclass.org/98C%28CLEOPATRA%2952/id/preferred_identifier> .
<http://iconclass.org/98C%28CLEOPATRA%2952> crm:P2_has_type pharos-meta:iconclass .
<http://iconclass.org/98C%28CLEOPATRA%2952> a crm:E55_Type .
<http://iconclass.org/98C%28CLEOPATRA%2952/appellation/preferred_name> crm:P190_has_symbolic_content "CLEOPATRA" .
<http://iconclass.org/98C%28CLEOPATRA%2952/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<http://iconclass.org/98C%28CLEOPATRA%2952/appellation/preferred_name> a crm:E41_Appellation .
<http://iconclass.org/98C%28CLEOPATRA%2952/id/preferred_identifier> crm:P190_has_symbolic_content "98C(CLEOPATRA)52" .
<http://iconclass.org/98C%28CLEOPATRA%2952/id/preferred_identifier> crm:P2_has_type pharos-meta:preferred_identifier .
<http://iconclass.org/98C%28CLEOPATRA%2952/id/preferred_identifier> a crm:E42_Identifier .
<work/07810480> crm:P65_shows_visual_item <work/07810480/visual_item/subject/primary/1> .
<work/07810480> a crm:E22_Human-Made_Object .
<work/07810480/visual_item/subject/primary/1> crm:P2_has_type <http://iconclass.org/98C%28CLEOPATRA%2952> .
<work/07810480/visual_item/subject/primary/1> a crm:E36_Visual_Item .
|]
