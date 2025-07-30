module Pharos.Datasets.Frick.ArtistSpec (spec) where


import Test.Hspec
import DSL
import Util.QQ (w, t)
import Engine (processXMLStringAsSet)
import Pharos.Datasets.Frick.Mappings.Artist (productionLinks)
import Pharos.Datasets.Frick.Mappings.Work (baseMapping, frickBaseUri)

spec :: Spec
spec = describe "Artists" $ do
  it "artist 1" $ do
    let mapping = baseMapping +> productionLinks
    let xml = [w|
<record>
  <controlfield tag="001">991013149679707141</controlfield>
  <controlfield tag="008">110428k16531653xx nnn | | cneng|d</controlfield>
  <datafield tag="100" ind1="1" ind2=" ">
    <subfield code="a">Goyen, Jan van,</subfield>
    <subfield code="d">1596-1656.</subfield>
    <subfield code="0">http://id.loc.gov/authorities/names/n82134567</subfield>
    <subfield code="1">http://viaf.org/viaf/14817785</subfield>
  </datafield>
</record>
    |]

    result <- processXMLStringAsSet mapping frickBaseUri xml
    
    result `shouldBe` [t|
<actor/goyen_jan_van1596_1656> a crm:E39_Actor .
<actor/goyen_jan_van1596_1656> crm:P1_is_identified_by <actor/goyen_jan_van1596_1656/appellation/preferred_name> .
<actor/goyen_jan_van1596_1656> custom:sameAs <http://vocab.getty.edu/ulan/500115612> .
<actor/goyen_jan_van1596_1656/appellation/preferred_name> a crm:E41_Appellation .
<actor/goyen_jan_van1596_1656/appellation/preferred_name> crm:P190_has_symbolic_content "Goyen, Jan van" .
<actor/goyen_jan_van1596_1656/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<http://vocab.getty.edu/ulan/500115612> a crm:E39_Actor .
<work/991013149679707141> a crm:E22_Human-Made_Object .
<work/991013149679707141> crm:P108i_was_produced_by <work/991013149679707141/production> .
<work/991013149679707141/production> a crm:E12_Production .
<work/991013149679707141/production> crm:P14_carried_out_by <actor/goyen_jan_van1596_1656> .
<work/991013149679707141/production> crm:P4_has_time-span <work/991013149679707141/production/timespan> .
<work/991013149679707141/production/timespan> a crm:E52_Time-Span .
<work/991013149679707141/production/timespan> crm:P1_is_identified_by <work/991013149679707141/production/timespan/appellation/preferred_name> .
<work/991013149679707141/production/timespan> crm:P82a_begin_of_the_begin "1653-01-01T00:00:00Z"^^xsd:dateTime .
<work/991013149679707141/production/timespan> crm:P82b_end_of_the_end "1653-12-31T23:59:59Z"^^xsd:dateTime .
<work/991013149679707141/production/timespan/appellation/preferred_name> a crm:E41_Appellation .
<work/991013149679707141/production/timespan/appellation/preferred_name> crm:P190_has_symbolic_content "1653" .
<work/991013149679707141/production/timespan/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
    |]
