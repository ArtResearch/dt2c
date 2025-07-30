module Pharos.Datasets.Pharos.PhotographerSpec (spec) where

import CommonImports
import Test.Hspec
import Mappings.Photographers

spec :: Spec
spec = describe "Photographer mapping" $ do
    it "correctly maps photographer data" $ do
      let xml = [w|
<?xml version="1.0" encoding="UTF-8"?>
<RECORDS>
  <RECORD>
    <PHOTOGRAPHER>Agnew and Son</PHOTOGRAPHER>
    <INSTITUTE>frick</INSTITUTE>
    <WIKIDATA>http://www.wikidata.org/wiki/Q17021787</WIKIDATA>
    <POSSIBLE_UMBRELLA_TERM>Agnew</POSSIBLE_UMBRELLA_TERM>
    <SEPARATE_ENTITY>Agnew and Sons</SEPARATE_ENTITY>
  </RECORD>
  <RECORD>
    <PHOTOGRAPHER>Pennsylvania Academy of the Fine Arts (Chappel Studio)</PHOTOGRAPHER>
    <INSTITUTE>frick</INSTITUTE>
    <WIKIDATA>none</WIKIDATA>
    <POSSIBLE_UMBRELLA_TERM>Chappel</POSSIBLE_UMBRELLA_TERM>
    <SEPARATE_ENTITY>Chappel Studio</SEPARATE_ENTITY>
    <ENTITY_TYPE>atelier</ENTITY_TYPE>
  </RECORD>
  <RECORD>
    <PHOTOGRAPHER>unindentified photographic source</PHOTOGRAPHER>
    <INSTITUTE>frick</INSTITUTE>
    <POSSIBLE_UMBRELLA_TERM>anonymous</POSSIBLE_UMBRELLA_TERM>
  </RECORD>
</RECORDS>
      |]
      result <- processXMLStringAsSet actorMapping (Just "https://artresearch.net/resource/pharos/") xml
      result `shouldBe` [t|
pharos-meta:atelier a crm:E55_Type .
<actor/photographer/chappel_studio> a crm:E39_Actor .
<actor/photographer/chappel_studio> crm:P107i_is_current_or_former_member_of <group/photographer/umbrella/chappel> .
<actor/photographer/chappel_studio> crm:P1_is_identified_by <actor/photographer/chappel_studio/appellation/preferred_name> .
<actor/photographer/chappel_studio> crm:P2_has_type pharos-meta:atelier .
<actor/photographer/chappel_studio> crm:P2_has_type pharos-meta:photographer .
<actor/photographer/chappel_studio/appellation/preferred_name> a crm:E41_Appellation .
<actor/photographer/chappel_studio/appellation/preferred_name> crm:P190_has_symbolic_content "Chappel Studio" .
<actor/photographer/chappel_studio/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<group/photographer/umbrella/agnew> a crm:E74_Group .
<group/photographer/umbrella/agnew> crm:P1_is_identified_by <group/photographer/umbrella/agnew/appellation/preferred_name> .
<group/photographer/umbrella/agnew> crm:P2_has_type pharos-meta:photograph_umbrella_term .
<group/photographer/umbrella/agnew/appellation/preferred_name> a crm:E41_Appellation .
<group/photographer/umbrella/agnew/appellation/preferred_name> crm:P190_has_symbolic_content "Agnew" .
<group/photographer/umbrella/agnew/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<group/photographer/umbrella/anonymous> a crm:E74_Group .
<group/photographer/umbrella/anonymous> crm:P1_is_identified_by <group/photographer/umbrella/anonymous/appellation/preferred_name> .
<group/photographer/umbrella/anonymous> crm:P2_has_type pharos-meta:photograph_umbrella_term .
<group/photographer/umbrella/anonymous/appellation/preferred_name> a crm:E41_Appellation .
<group/photographer/umbrella/anonymous/appellation/preferred_name> crm:P190_has_symbolic_content "anonymous" .
<group/photographer/umbrella/anonymous/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<group/photographer/umbrella/chappel> a crm:E74_Group .
<group/photographer/umbrella/chappel> crm:P1_is_identified_by <group/photographer/umbrella/chappel/appellation/preferred_name> .
<group/photographer/umbrella/chappel> crm:P2_has_type pharos-meta:photograph_umbrella_term .
<group/photographer/umbrella/chappel/appellation/preferred_name> a crm:E41_Appellation .
<group/photographer/umbrella/chappel/appellation/preferred_name> crm:P190_has_symbolic_content "Chappel" .
<group/photographer/umbrella/chappel/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<http://www.wikidata.org/wiki/Q17021787> a crm:E39_Actor .
<http://www.wikidata.org/wiki/Q17021787> crm:P107i_is_current_or_former_member_of <group/photographer/umbrella/agnew> .
<http://www.wikidata.org/wiki/Q17021787> crm:P1_is_identified_by <http://www.wikidata.org/wiki/Q17021787/appellation/preferred_name> .
<http://www.wikidata.org/wiki/Q17021787> crm:P2_has_type pharos-meta:photographer .
<http://www.wikidata.org/wiki/Q17021787/appellation/preferred_name> a crm:E41_Appellation .
<http://www.wikidata.org/wiki/Q17021787/appellation/preferred_name> crm:P190_has_symbolic_content "Agnew and Sons" .
<http://www.wikidata.org/wiki/Q17021787/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<https://artresearch.net/resource/frick/actor/photographer/unindentified_photographic_source> a crm:E39_Actor .
<https://artresearch.net/resource/frick/actor/photographer/unindentified_photographic_source> crm:P107i_is_current_or_former_member_of <group/photographer/umbrella/anonymous> .
<https://artresearch.net/resource/frick/actor/photographer/unindentified_photographic_source> crm:P2_has_type pharos-meta:photographer .
      |]
