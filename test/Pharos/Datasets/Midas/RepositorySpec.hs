module Pharos.Datasets.Midas.RepositorySpec (spec) where

import Test.Hspec
import CommonImports
import Midas.Mappings.Work (baseMapping, midasBaseUri)
import Midas.Mappings.Repository (repositoryLinks)


spec :: Spec
spec = describe "Repository mappings" $ do
  it "place with repository" $ do
    let mapping = baseMapping +> repositoryLinks
    let xml = [w|
      <obj lvl="1">
        <a5000>08093321</a5000>
        <aob28 modifier="Verwalter">
          <a2864>Los Angeles</a2864>
          <a2900>Los Angeles County Museum of Art</a2900>
        </aob28>
      </obj>
    |]
    
    result <- processXMLStringAsSet mapping midasBaseUri xml
    
    result `shouldBe` [t|
<http://vocab.getty.edu/tgn/7023900> a crm:E53_Place .
<http://www.wikidata.org/entity/Q1641836> a crm:E39_Actor .
<place/00260332> crm:P2_has_type pharos-meta:geographical_entity .
<place/00260332> crm:P74i_is_current_or_former_residence_of <place/00260332/repository/los_angeles_county_museum_of_art> .
<place/00260332> custom:sameAs <http://vocab.getty.edu/tgn/7023900> .
<place/00260332> a crm:E53_Place .
<place/00260332/repository/los_angeles_county_museum_of_art> crm:P1_is_identified_by <place/00260332/repository/los_angeles_county_museum_of_art/appellation/preferred_name> .
<place/00260332/repository/los_angeles_county_museum_of_art> crm:P2_has_type pharos-meta:repository .
<place/00260332/repository/los_angeles_county_museum_of_art> crm:P50i_is_current_keeper_of <work/08093321> .
<place/00260332/repository/los_angeles_county_museum_of_art> custom:sameAs <http://www.wikidata.org/entity/Q1641836> .
<place/00260332/repository/los_angeles_county_museum_of_art> a crm:E39_Actor .
<place/00260332/repository/los_angeles_county_museum_of_art/appellation/preferred_name> crm:P190_has_symbolic_content "Los Angeles County Museum of Art" .
<place/00260332/repository/los_angeles_county_museum_of_art/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<place/00260332/repository/los_angeles_county_museum_of_art/appellation/preferred_name> a crm:E41_Appellation .
<work/08093321> crm:P55_has_current_location <place/00260332> .
<work/08093321> a crm:E22_Human-Made_Object .    
    |]

  it "place without repository" $ do
    let mapping = baseMapping +> repositoryLinks
    let xml = [w|
      <obj lvl="1">
        <a5000>07501130</a5000>
        <aob28 modifier="Verwalter">
          <a2864>Italien</a2864>
        </aob28>
      </obj>
    |]
    
    result <- processXMLStringAsSet mapping midasBaseUri xml
    
    result `shouldBe` [t|
<http://vocab.getty.edu/tgn/1000080> a crm:E53_Place .
<place/00001351> crm:P2_has_type pharos-meta:geographical_entity .
<place/00001351> custom:sameAs <http://vocab.getty.edu/tgn/1000080> .
<place/00001351> a crm:E53_Place .
<work/07501130> crm:P55_has_current_location <place/00001351> .
<work/07501130> a crm:E22_Human-Made_Object .
    |]

  it "place with explicit geo id" $ do
    let mapping = baseMapping +> repositoryLinks
    let xml = [w|
      <obj lvl="1">
        <a5000>00001010</a5000>
        <aob28 modifier="Verwalter">
          <a2864>Berlin</a2864>
          <a2900>Nationalgalerie, Staatliche Museen zu Berlin - Preußischer Kulturbesitz</a2900>
          <a28na edp:augmented="geo::00000301">00000301</a28na>
        </aob28>
      </obj>
    |]
    
    result <- processXMLStringAsSet mapping midasBaseUri xml
    
    result `shouldBe` [t|
<http://vocab.getty.edu/tgn/7003712> a crm:E53_Place .
<place/00000301> crm:P2_has_type pharos-meta:geographical_entity .
<place/00000301> crm:P74i_is_current_or_former_residence_of <place/00000301/repository/nationalgalerie_staatliche_museen_zu_berlin_preussischer_kulturbesitz> .
<place/00000301> custom:sameAs <http://vocab.getty.edu/tgn/7003712> .
<place/00000301> a crm:E53_Place .
<place/00000301/repository/nationalgalerie_staatliche_museen_zu_berlin_preussischer_kulturbesitz> crm:P1_is_identified_by <place/00000301/repository/nationalgalerie_staatliche_museen_zu_berlin_preussischer_kulturbesitz/appellation/preferred_name> .
<place/00000301/repository/nationalgalerie_staatliche_museen_zu_berlin_preussischer_kulturbesitz> crm:P2_has_type pharos-meta:repository .
<place/00000301/repository/nationalgalerie_staatliche_museen_zu_berlin_preussischer_kulturbesitz> crm:P50i_is_current_keeper_of <work/00001010> .
<place/00000301/repository/nationalgalerie_staatliche_museen_zu_berlin_preussischer_kulturbesitz> a crm:E39_Actor .
<place/00000301/repository/nationalgalerie_staatliche_museen_zu_berlin_preussischer_kulturbesitz/appellation/preferred_name> crm:P190_has_symbolic_content "Nationalgalerie, Staatliche Museen zu Berlin - Preußischer Kulturbesitz" .
<place/00000301/repository/nationalgalerie_staatliche_museen_zu_berlin_preussischer_kulturbesitz/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<place/00000301/repository/nationalgalerie_staatliche_museen_zu_berlin_preussischer_kulturbesitz/appellation/preferred_name> a crm:E41_Appellation .
<work/00001010> crm:P55_has_current_location <place/00000301> .
<work/00001010> a crm:E22_Human-Made_Object .
    |]


  it "place without matching geo id" $ do
    let mapping = baseMapping +> repositoryLinks
    let xml = [w|
      <obj lvl="1">
        <a5000>xxxxx31</a5000>
        <aob28 modifier="Verwalter">
          <a2864>Sambuci</a2864>
          <a2900>Test</a2900>
        </aob28>     
      </obj>
    |]
    
    result <- processXMLStringAsSet mapping midasBaseUri xml
    
    result `shouldBe` [t|
<http://vocab.getty.edu/tgn/4009877> a crm:E53_Place .
<place/sambuci> custom:sameAs <http://vocab.getty.edu/tgn/4009877> .
<place/sambuci> a crm:E53_Place .
<place/sambuci> crm:P1_is_identified_by <place/sambuci/appellation/preferred_name> .
<place/sambuci> crm:P2_has_type pharos-meta:geographical_entity .
<place/sambuci> crm:P74i_is_current_or_former_residence_of <place/sambuci/repository/test> .
<place/sambuci/appellation/preferred_name> a crm:E41_Appellation .
<place/sambuci/appellation/preferred_name> crm:P190_has_symbolic_content "Sambuci" .
<place/sambuci/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<place/sambuci/repository/test> a crm:E39_Actor .
<place/sambuci/repository/test> crm:P1_is_identified_by <place/sambuci/repository/test/appellation/preferred_name> .
<place/sambuci/repository/test> crm:P2_has_type pharos-meta:repository .
<place/sambuci/repository/test> crm:P50i_is_current_keeper_of <work/xxxxx31> .
<place/sambuci/repository/test/appellation/preferred_name> a crm:E41_Appellation .
<place/sambuci/repository/test/appellation/preferred_name> crm:P190_has_symbolic_content "Test" .
<place/sambuci/repository/test/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<work/xxxxx31> a crm:E22_Human-Made_Object .
<work/xxxxx31> crm:P55_has_current_location <place/sambuci> .
    |]

  it "place with repository containing question marks" $ do
    let mapping = baseMapping +> repositoryLinks
    let xml = [w|
      <obj lvl="1">
        <a5000>08014982</a5000>
        <aob28 modifier="Verwalter">
          <a2864>London?</a2864>
          <a2900>British Museum?</a2900>
        </aob28>
      </obj>
    |]
    
    result <- processXMLStringAsSet mapping midasBaseUri xml
    
    result `shouldBe` [t|
<http://www.wikidata.org/entity/Q6373> a crm:E39_Actor .
<place/london_q> a crm:E53_Place .
<place/london_q> crm:P1_is_identified_by <place/london_q/appellation/preferred_name> .
<place/london_q> crm:P2_has_type pharos-meta:geographical_entity .
<place/london_q> crm:P74i_is_current_or_former_residence_of <place/london_q/repository/british_museum_q> .
<place/london_q/appellation/preferred_name> a crm:E41_Appellation .
<place/london_q/appellation/preferred_name> crm:P190_has_symbolic_content "London?" .
<place/london_q/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<place/london_q/repository/british_museum_q> a crm:E39_Actor .
<place/london_q/repository/british_museum_q> crm:P1_is_identified_by <place/london_q/repository/british_museum_q/appellation/preferred_name> .
<place/london_q/repository/british_museum_q> crm:P2_has_type pharos-meta:repository .
<place/london_q/repository/british_museum_q> crm:P50i_is_current_keeper_of <work/08014982> .
<place/london_q/repository/british_museum_q> custom:sameAs <http://www.wikidata.org/entity/Q6373> .
<place/london_q/repository/british_museum_q/appellation/preferred_name> a crm:E41_Appellation .
<place/london_q/repository/british_museum_q/appellation/preferred_name> crm:P190_has_symbolic_content "British Museum?" .
<place/london_q/repository/british_museum_q/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<work/08014982> a crm:E22_Human-Made_Object .
<work/08014982> crm:P55_has_current_location <place/london_q> .
    |]
