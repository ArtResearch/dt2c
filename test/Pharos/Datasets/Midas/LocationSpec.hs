module Pharos.Datasets.Midas.LocationSpec (spec) where

import Test.Hspec
import CommonImports
import Midas.Mappings.Work (baseMapping, midasBaseUri)
import Midas.Mappings.Location (locationLinks)
import Engine (processXMLStringAsSetWithConfig)

spec :: Spec
spec = describe "location" $ do
  it "place with repository" $ do
    let mapping = baseMapping +> locationLinks
    let xml = [w|
      <obj lvl="1">
        <a5000>20864436</a5000>
        <aob26 modifier="Standort">
          <a2664>Bury (Oise)</a2664>
          <a26na edp:augmented="geo::20950669">20950669</a26na>
        </aob26>
      </obj>
    |]
    
    result <- processXMLStringAsSet mapping midasBaseUri xml
    
    result `shouldBe` [t|
<http://vocab.getty.edu/tgn/5001506> a crm:E53_Place .
<place/20950669> a crm:E53_Place .
<place/20950669> crm:P2_has_type pharos-meta:geographical_entity .
<place/20950669> custom:sameAs <http://vocab.getty.edu/tgn/5001506> .
<work/20864436> a crm:E22_Human-Made_Object .
<work/20864436> crm:P55_has_current_location <place/20950669> .
<work/bury_oise> a crm:E22_Human-Made_Object .
<work/bury_oise> crm:P156_occupies <work/bury_oise/place> .
<work/bury_oise> crm:P2_has_type pharos-meta:built_work .
<work/bury_oise> crm:P46_is_composed_of <work/20864436> .
<work/bury_oise> crm:P55_has_current_location <place/20950669> .
<work/bury_oise/place> a crm:E53_Place .
<work/bury_oise/place> crm:P89_falls_within <place/20950669> .
    |]

  it "hertziana, architectural object location" $ do
    let mapping = baseMapping +> locationLinks
    let xml = [w|
      <obj lvl="1">
        <a5000>08041820</a5000>
        <a5108>Neapel</a5108>
      </obj>
    |]
    
    result <- processXMLStringAsSet mapping midasBaseUri xml
    
    result `shouldBe` [t|
<http://vocab.getty.edu/tgn/7004474> a crm:E53_Place .
<place/08120254> crm:P2_has_type pharos-meta:geographical_entity .
<place/08120254> custom:sameAs <http://vocab.getty.edu/tgn/7004474> .
<place/08120254> a crm:E53_Place .
<work/08041820> crm:P55_has_current_location <place/08120254> .
<work/08041820> a crm:E22_Human-Made_Object .
    |]

  it "part of other object" $ do
    let mapping = baseMapping +> locationLinks
    let xml = [w|
      <obj lvl="1">
        <a5000>70009416</a5000>
        <a5007 modifier="Teil von">Teil von<a5008>70009280</a5008></a5007>
      </obj>
    |]
    
    result <- processXMLStringAsSet mapping midasBaseUri xml
    
    result `shouldBe` [t|
<work/70009280> a crm:E22_Human-Made_Object .
<work/70009416> crm:P46i_forms_part_of <work/70009280> .
<work/70009416> a crm:E22_Human-Made_Object .
    |]

  it "part of other object, with reconciled URI rewrite" $ do
    let mapping = baseMapping +> locationLinks
    let xml = [w|
      <obj lvl="1">
        <a5000>70013956</a5000>
        <aob26 modifier="Standort">Standort<a2606>07602751</a2606>
            <a2664>Neapel</a2664>
            <a2690>Kirche</a2690>
            <a2700>San Giovanni a Carbonara</a2700>
        </aob26>
      </obj>
    |]
    
    result <- processXMLStringAsSetWithConfig EngineConfig
                { ecGenerateReverseProps = False
                , ecMaterializeAlignments = True
                , ecOutputFormat = TTL
                , ecGenerateE13 = False
                , ecDatasetName = Nothing
                } mapping midasBaseUri xml
    
    result `shouldBe` [t|
<http://vocab.getty.edu/tgn/7004474> a crm:E53_Place .
<https://artresearch.net/resource/pharos/artwork/da8ca1b682187cedc9a1384a44965fa365774201c6e78c9e3eed85c3bcabc474> crm:P156_occupies <work/07602751/place> .
<https://artresearch.net/resource/pharos/artwork/da8ca1b682187cedc9a1384a44965fa365774201c6e78c9e3eed85c3bcabc474> crm:P1_is_identified_by <work/07602751/appellation/preferred_name> .
<https://artresearch.net/resource/pharos/artwork/da8ca1b682187cedc9a1384a44965fa365774201c6e78c9e3eed85c3bcabc474> crm:P2_has_type pharos-meta:built_work .
<https://artresearch.net/resource/pharos/artwork/da8ca1b682187cedc9a1384a44965fa365774201c6e78c9e3eed85c3bcabc474> crm:P2_has_type <vocab/builtwork_type/kirche> .
<https://artresearch.net/resource/pharos/artwork/da8ca1b682187cedc9a1384a44965fa365774201c6e78c9e3eed85c3bcabc474> crm:P46_is_composed_of <work/70013956> .
<https://artresearch.net/resource/pharos/artwork/da8ca1b682187cedc9a1384a44965fa365774201c6e78c9e3eed85c3bcabc474> crm:P55_has_current_location <http://vocab.getty.edu/tgn/7004474> .
<https://artresearch.net/resource/pharos/artwork/da8ca1b682187cedc9a1384a44965fa365774201c6e78c9e3eed85c3bcabc474> a crm:E22_Human-Made_Object .
<place/08120254> crm:P2_has_type pharos-meta:geographical_entity .
<place/08120254> custom:sameAs <http://vocab.getty.edu/tgn/7004474> .
<place/08120254> a crm:E53_Place .
<vocab/builtwork_type/kirche> crm:P1_is_identified_by <vocab/builtwork_type/kirche/appellation/preferred_name> .
<vocab/builtwork_type/kirche> a crm:E55_Type .
<vocab/builtwork_type/kirche/appellation/preferred_name> crm:P190_has_symbolic_content "Kirche" .
<vocab/builtwork_type/kirche/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<vocab/builtwork_type/kirche/appellation/preferred_name> a crm:E41_Appellation .
<work/07602751> custom:objectSameAs <https://artresearch.net/resource/pharos/artwork/da8ca1b682187cedc9a1384a44965fa365774201c6e78c9e3eed85c3bcabc474> .
<work/07602751/appellation/preferred_name> crm:P190_has_symbolic_content "San Giovanni a Carbonara" .
<work/07602751/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<work/07602751/appellation/preferred_name> a crm:E41_Appellation .
<work/07602751/place> crm:P89_falls_within <http://vocab.getty.edu/tgn/7004474> .
<work/07602751/place> a crm:E53_Place .
<work/70013956> crm:P55_has_current_location <http://vocab.getty.edu/tgn/7004474> .
<work/70013956> a crm:E22_Human-Made_Object .
    |]