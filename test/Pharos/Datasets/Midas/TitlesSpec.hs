module Pharos.Datasets.Midas.TitlesSpec (spec) where

import Test.Hspec
import CommonImports
import Midas.Mappings.Titles (titles)
import Midas.Mappings.Work (baseMapping, workMapping, midasBaseUri, nestedWorks)


spec :: Spec
spec = describe "Midas Titles mapping" $ do
  it "main title" $ do
    let mapping = baseMapping +> titles
    let xml = [w|
      <obj lvl="1">
        <a5000>08122409</a5000>
        <a5200>Kapellenausstattung</a5200>
      </obj>
    |]
    
    result <- processXMLStringAsSet mapping midasBaseUri xml
    
    result
      `shouldBe`
        [t|
          <work/08122409> a crm:E22_Human-Made_Object .
          <work/08122409> crm:P1_is_identified_by <work/08122409/appellation/1/object_title> .
          <work/08122409/appellation/1/object_title> a crm:E41_Appellation .
          <work/08122409/appellation/1/object_title> crm:P190_has_symbolic_content "Kapellenausstattung" .
          <work/08122409/appellation/1/object_title> crm:P2_has_type <vocab/meta/object_title> .
        |]

  it "alternative titles ()" $ do
    let mapping = baseMapping +> titles
    let xml = [w|
      <obj lvl="1">
        <a5000>20980972</a5000>
        <a5200>Puerta de las Platerias</a5200>
        <a5201>Portal der Goldschmiede &amp; Südportal</a5201>
      </obj>
    |]
    result <- processXMLStringAsSet mapping midasBaseUri xml
    result `shouldBe` [t|
      <work/20980972> a crm:E22_Human-Made_Object .
      <work/20980972> crm:P1_is_identified_by <work/20980972/appellation/1/alternative_object_title> .
      <work/20980972> crm:P1_is_identified_by <work/20980972/appellation/1/object_title> .
      <work/20980972/appellation/1/alternative_object_title> a crm:E41_Appellation .
      <work/20980972/appellation/1/alternative_object_title> crm:P190_has_symbolic_content "Portal der Goldschmiede & Südportal" .
      <work/20980972/appellation/1/alternative_object_title> crm:P2_has_type <vocab/meta/alternative_object_title> .
      <work/20980972/appellation/1/object_title> a crm:E41_Appellation .
      <work/20980972/appellation/1/object_title> crm:P190_has_symbolic_content "Puerta de las Platerias" .
      <work/20980972/appellation/1/object_title> crm:P2_has_type <vocab/meta/object_title> .
    |]

  it "buildwork titles" $ do
    let mapping = baseMapping +> titles
    let xml = [w|
      <obj lvl="1">
        <a5000>08138886</a5000>
        <a5202>Palazzo Biscari</a5202>
        <a5204>Palazzo Paternò Castello</a5204>
        <a5204>Palazzo Biscari alla Marina</a5204>
      </obj>
    |]

    result <- processXMLStringAsSet mapping midasBaseUri xml

    result `shouldBe` [t|
      <work/08138886> a crm:E22_Human-Made_Object .
      <work/08138886> crm:P1_is_identified_by <work/08138886/appellation/1/alternative_building_name> .
      <work/08138886> crm:P1_is_identified_by <work/08138886/appellation/1/name_of_the_building> .
      <work/08138886> crm:P1_is_identified_by <work/08138886/appellation/2/alternative_building_name> .
      <work/08138886/appellation/1/alternative_building_name> a crm:E41_Appellation .
      <work/08138886/appellation/1/alternative_building_name> crm:P190_has_symbolic_content "Palazzo Paternò Castello" .
      <work/08138886/appellation/1/alternative_building_name> crm:P2_has_type <vocab/meta/alternative_building_name> .
      <work/08138886/appellation/1/name_of_the_building> a crm:E41_Appellation .
      <work/08138886/appellation/1/name_of_the_building> crm:P190_has_symbolic_content "Palazzo Biscari" .
      <work/08138886/appellation/1/name_of_the_building> crm:P2_has_type <vocab/meta/name_of_the_building> .
      <work/08138886/appellation/2/alternative_building_name> a crm:E41_Appellation .
      <work/08138886/appellation/2/alternative_building_name> crm:P190_has_symbolic_content "Palazzo Biscari alla Marina" .
      <work/08138886/appellation/2/alternative_building_name> crm:P2_has_type <vocab/meta/alternative_building_name> .    
    |]

  it "nested object titles" $ do
    let mapping = workMapping "hertziana"
    let xml = [w|
      <obj lvl="1">
        <a5000>08122409</a5000>
        <a5200>Kapellenausstattung</a5200>
        <obj lvl="2">
          <a5001>08122409,T,001,T</a5001>
          <a5200>Gewölbe</a5200>
          <obj lvl="3">
            <a5002>08122409,T,001,T,001</a5002>
            <a5200>Stuck des Gewölbes</a5200>
          </obj>
          <obj lvl="3">
            <a5002>08122409,T,001,T,002,T</a5002>
            <a5200>Szenen aus dem Leben des heiligen Paulus und dessen Tugenden</a5200>
            <obj lvl="4">
              <a5003>08122409,T,001,T,002,T,001</a5003>
              <a5200>Heiliger Paulus</a5200>
            </obj>
          </obj>
        </obj>
      </obj>
    |]

    result <- processXMLStringAsSet mapping midasBaseUri xml

    result `shouldBe` [t|
<https://artresearch.net/resource/e31/hertziana> a crm:E31_Document .
<work/08122409> crm:P108i_was_produced_by <work/08122409/production> .
<work/08122409> crm:P1_is_identified_by <work/08122409/appellation/1/object_title> .
<work/08122409> crm:P1_is_identified_by <work/08122409/id/catalog_url> .
<work/08122409> crm:P1_is_identified_by <work/08122409/id/preferred_identifier> .
<work/08122409> crm:P2_has_type aat:300133025 .
<work/08122409> crm:P46_is_composed_of <work/08122409_t_001_t> .
<work/08122409> crm:P70i_is_documented_in <https://artresearch.net/resource/e31/hertziana> .
<work/08122409> a crm:E22_Human-Made_Object .
<work/08122409/appellation/1/object_title> crm:P190_has_symbolic_content "Kapellenausstattung" .
<work/08122409/appellation/1/object_title> crm:P2_has_type <vocab/meta/object_title> .
<work/08122409/appellation/1/object_title> a crm:E41_Appellation .
<work/08122409/id/catalog_url> crm:P190_has_symbolic_content "https://foto.biblhertz.it/document/obj/08122409" .
<work/08122409/id/catalog_url> crm:P2_has_type pharos-meta:catalog_url .
<work/08122409/id/catalog_url> a crm:E42_Identifier .
<work/08122409/id/preferred_identifier> crm:P190_has_symbolic_content "08122409" .
<work/08122409/id/preferred_identifier> crm:P2_has_type pharos-meta:preferred_identifier .
<work/08122409/id/preferred_identifier> a crm:E42_Identifier .
<work/08122409/production> a crm:E12_Production .
<work/08122409_t_001_t> crm:P108i_was_produced_by <work/08122409_t_001_t/production> .
<work/08122409_t_001_t> crm:P1_is_identified_by <work/08122409_t_001_t/appellation/1/object_title> .
<work/08122409_t_001_t> crm:P1_is_identified_by <work/08122409_t_001_t/id/catalog_url> .
<work/08122409_t_001_t> crm:P1_is_identified_by <work/08122409_t_001_t/id/preferred_identifier> .
<work/08122409_t_001_t> crm:P2_has_type aat:300133025 .
<work/08122409_t_001_t> crm:P46_is_composed_of <work/08122409_t_001_t_001> .
<work/08122409_t_001_t> crm:P46_is_composed_of <work/08122409_t_001_t_002_t> .
<work/08122409_t_001_t> crm:P70i_is_documented_in <https://artresearch.net/resource/e31/hertziana> .
<work/08122409_t_001_t> a crm:E22_Human-Made_Object .
<work/08122409_t_001_t/appellation/1/object_title> crm:P190_has_symbolic_content "Gewölbe" .
<work/08122409_t_001_t/appellation/1/object_title> crm:P2_has_type <vocab/meta/object_title> .
<work/08122409_t_001_t/appellation/1/object_title> a crm:E41_Appellation .
<work/08122409_t_001_t/id/catalog_url> crm:P190_has_symbolic_content "https://foto.biblhertz.it/document/obj/08122409?part=1" .
<work/08122409_t_001_t/id/catalog_url> crm:P2_has_type pharos-meta:catalog_url .
<work/08122409_t_001_t/id/catalog_url> a crm:E42_Identifier .
<work/08122409_t_001_t/id/preferred_identifier> crm:P190_has_symbolic_content "08122409,T,001,T" .
<work/08122409_t_001_t/id/preferred_identifier> crm:P2_has_type pharos-meta:preferred_identifier .
<work/08122409_t_001_t/id/preferred_identifier> a crm:E42_Identifier .
<work/08122409_t_001_t/production> a crm:E12_Production .
<work/08122409_t_001_t_001> crm:P108i_was_produced_by <work/08122409_t_001_t_001/production> .
<work/08122409_t_001_t_001> crm:P1_is_identified_by <work/08122409_t_001_t_001/appellation/1/object_title> .
<work/08122409_t_001_t_001> crm:P1_is_identified_by <work/08122409_t_001_t_001/id/catalog_url> .
<work/08122409_t_001_t_001> crm:P1_is_identified_by <work/08122409_t_001_t_001/id/preferred_identifier> .
<work/08122409_t_001_t_001> crm:P2_has_type aat:300133025 .
<work/08122409_t_001_t_001> crm:P70i_is_documented_in <https://artresearch.net/resource/e31/hertziana> .
<work/08122409_t_001_t_001> a crm:E22_Human-Made_Object .
<work/08122409_t_001_t_001/appellation/1/object_title> crm:P190_has_symbolic_content "Stuck des Gewölbes" .
<work/08122409_t_001_t_001/appellation/1/object_title> crm:P2_has_type <vocab/meta/object_title> .
<work/08122409_t_001_t_001/appellation/1/object_title> a crm:E41_Appellation .
<work/08122409_t_001_t_001/id/catalog_url> crm:P190_has_symbolic_content "https://foto.biblhertz.it/document/obj/08122409?part=2" .
<work/08122409_t_001_t_001/id/catalog_url> crm:P2_has_type pharos-meta:catalog_url .
<work/08122409_t_001_t_001/id/catalog_url> a crm:E42_Identifier .
<work/08122409_t_001_t_001/id/preferred_identifier> crm:P190_has_symbolic_content "08122409,T,001,T,001" .
<work/08122409_t_001_t_001/id/preferred_identifier> crm:P2_has_type pharos-meta:preferred_identifier .
<work/08122409_t_001_t_001/id/preferred_identifier> a crm:E42_Identifier .
<work/08122409_t_001_t_001/production> a crm:E12_Production .
<work/08122409_t_001_t_002_t> crm:P108i_was_produced_by <work/08122409_t_001_t_002_t/production> .
<work/08122409_t_001_t_002_t> crm:P1_is_identified_by <work/08122409_t_001_t_002_t/appellation/1/object_title> .
<work/08122409_t_001_t_002_t> crm:P1_is_identified_by <work/08122409_t_001_t_002_t/id/catalog_url> .
<work/08122409_t_001_t_002_t> crm:P1_is_identified_by <work/08122409_t_001_t_002_t/id/preferred_identifier> .
<work/08122409_t_001_t_002_t> crm:P2_has_type aat:300133025 .
<work/08122409_t_001_t_002_t> crm:P46_is_composed_of <work/08122409_t_001_t_002_t_001> .
<work/08122409_t_001_t_002_t> crm:P70i_is_documented_in <https://artresearch.net/resource/e31/hertziana> .
<work/08122409_t_001_t_002_t> a crm:E22_Human-Made_Object .
<work/08122409_t_001_t_002_t/appellation/1/object_title> crm:P190_has_symbolic_content "Szenen aus dem Leben des heiligen Paulus und dessen Tugenden" .
<work/08122409_t_001_t_002_t/appellation/1/object_title> crm:P2_has_type <vocab/meta/object_title> .
<work/08122409_t_001_t_002_t/appellation/1/object_title> a crm:E41_Appellation .
<work/08122409_t_001_t_002_t/id/catalog_url> crm:P190_has_symbolic_content "https://foto.biblhertz.it/document/obj/08122409?part=3" .
<work/08122409_t_001_t_002_t/id/catalog_url> crm:P2_has_type pharos-meta:catalog_url .
<work/08122409_t_001_t_002_t/id/catalog_url> a crm:E42_Identifier .
<work/08122409_t_001_t_002_t/id/preferred_identifier> crm:P190_has_symbolic_content "08122409,T,001,T,002,T" .
<work/08122409_t_001_t_002_t/id/preferred_identifier> crm:P2_has_type pharos-meta:preferred_identifier .
<work/08122409_t_001_t_002_t/id/preferred_identifier> a crm:E42_Identifier .
<work/08122409_t_001_t_002_t/production> a crm:E12_Production .
<work/08122409_t_001_t_002_t_001> crm:P108i_was_produced_by <work/08122409_t_001_t_002_t_001/production> .
<work/08122409_t_001_t_002_t_001> crm:P1_is_identified_by <work/08122409_t_001_t_002_t_001/appellation/1/object_title> .
<work/08122409_t_001_t_002_t_001> crm:P1_is_identified_by <work/08122409_t_001_t_002_t_001/id/catalog_url> .
<work/08122409_t_001_t_002_t_001> crm:P1_is_identified_by <work/08122409_t_001_t_002_t_001/id/preferred_identifier> .
<work/08122409_t_001_t_002_t_001> crm:P2_has_type aat:300133025 .
<work/08122409_t_001_t_002_t_001> crm:P70i_is_documented_in <https://artresearch.net/resource/e31/hertziana> .
<work/08122409_t_001_t_002_t_001> a crm:E22_Human-Made_Object .
<work/08122409_t_001_t_002_t_001/appellation/1/object_title> crm:P190_has_symbolic_content "Heiliger Paulus" .
<work/08122409_t_001_t_002_t_001/appellation/1/object_title> crm:P2_has_type <vocab/meta/object_title> .
<work/08122409_t_001_t_002_t_001/appellation/1/object_title> a crm:E41_Appellation .
<work/08122409_t_001_t_002_t_001/id/catalog_url> crm:P190_has_symbolic_content "https://foto.biblhertz.it/document/obj/08122409?part=4" .
<work/08122409_t_001_t_002_t_001/id/catalog_url> crm:P2_has_type pharos-meta:catalog_url .
<work/08122409_t_001_t_002_t_001/id/catalog_url> a crm:E42_Identifier .
<work/08122409_t_001_t_002_t_001/id/preferred_identifier> crm:P190_has_symbolic_content "08122409,T,001,T,002,T,001" .
<work/08122409_t_001_t_002_t_001/id/preferred_identifier> crm:P2_has_type pharos-meta:preferred_identifier .
<work/08122409_t_001_t_002_t_001/id/preferred_identifier> a crm:E42_Identifier .
<work/08122409_t_001_t_002_t_001/production> a crm:E12_Production .
    |]

  it "architectural object title, with place" $ do
    let mapping = baseMapping +> titles
    let xml = [w|
      <obj lvl="1">
        <a5000>08041820</a5000>
        <a5202>San Giovanni a Carbonara</a5202>
        <a5108>Neapel</a5108>
      </obj>
    |]
    
    result <- processXMLStringAsSet mapping midasBaseUri xml
    
    result `shouldBe` [t|
<work/08041820> crm:P1_is_identified_by <work/08041820/appellation/1/name_of_the_building> .
<work/08041820> crm:P1_is_identified_by <work/08041820/appellation/pharos_preferred_name> .
<work/08041820> a crm:E22_Human-Made_Object .
<work/08041820/appellation/1/name_of_the_building> crm:P190_has_symbolic_content "San Giovanni a Carbonara" .
<work/08041820/appellation/1/name_of_the_building> crm:P2_has_type <vocab/meta/name_of_the_building> .
<work/08041820/appellation/1/name_of_the_building> a crm:E41_Appellation .
<work/08041820/appellation/pharos_preferred_name> crm:P190_has_symbolic_content "San Giovanni a Carbonara, Neapel" .
<work/08041820/appellation/pharos_preferred_name> crm:P2_has_type pharos-meta:pharos_preferred_name .
<work/08041820/appellation/pharos_preferred_name> a crm:E41_Appellation .
    |]

  it "architectural sub-object title, when there is no main title" $ do
    let mapping = baseMapping +> titles ++ (nestedWorks (\_ -> titles))
    let xml = [w|
      <obj lvl="1">
        <a5000>70011006</a5000>
        <a5202>Giardino Buonaccorsi</a5202>
        <obj lvl="2">
          <a5001>70012177</a5001>
          <a5230>Statuengruppe</a5230>
        </obj>
      </obj>
    |]
    
    result <- processXMLStringAsSet mapping midasBaseUri xml
    
    result `shouldBe` [t|
<work/70011006> crm:P1_is_identified_by <work/70011006/appellation/1/name_of_the_building> .
<work/70011006> crm:P46_is_composed_of <work/70012177> .
<work/70011006> a crm:E22_Human-Made_Object .
<work/70011006/appellation/1/name_of_the_building> crm:P190_has_symbolic_content "Giardino Buonaccorsi" .
<work/70011006/appellation/1/name_of_the_building> crm:P2_has_type <vocab/meta/name_of_the_building> .
<work/70011006/appellation/1/name_of_the_building> a crm:E41_Appellation .
<work/70012177> crm:P1_is_identified_by <work/70012177/appellation/1/pharos_preferred_name> .
<work/70012177> a crm:E22_Human-Made_Object .
<work/70012177/appellation/1/pharos_preferred_name> crm:P190_has_symbolic_content "Statuengruppe" .
<work/70012177/appellation/1/pharos_preferred_name> crm:P2_has_type pharos-meta:pharos_preferred_name .
<work/70012177/appellation/1/pharos_preferred_name> a crm:E41_Appellation .
    |]
