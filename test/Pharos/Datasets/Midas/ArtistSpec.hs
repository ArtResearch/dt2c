module Pharos.Datasets.Midas.ArtistSpec (spec) where

import Test.Hspec
import DSL
import Util.QQ (w, t)
import Engine (processXMLStringAsSet)
import Midas.Mappings.Artist (artistLinks)
import Midas.Mappings.Work (baseMapping, midasBaseUri)

spec :: Spec
spec = describe "Artists" $ do
  it "hertziana, multiple evenets, all artist with KUE, one artist reconciled" $ do
    let mapping = baseMapping +> artistLinks
    let xml = [w|
<obj>
  <a5000>08197155</a5000>
  <aob30 modifier="Entwurf">
    <a3100>Rocco di Tommaso da Vicenza</a3100>
    <a3475>Architekt</a3475>
    <a3000 edp:augmented="kue::07500458">07500458</a3000>
    <a31nn edp:augmented="kue::07500458">Rocco di Tommaso da Vicenza</a31nn>
  </aob30>
  <aob30 modifier="Herstellung">
    <a3100>Giuliano da Verona (1525)</a3100>
    <a3470>zugeschrieben</a3470>
    <a3475>Steinmetz</a3475>
    <a3498>https://www.degruyter.com/database/AKL/entry/_00071710/html</a3498>
    <aob30rl>oder</aob30rl>
    <a3000 edp:augmented="kue::08000234">08000234</a3000>
    <a31nn edp:augmented="kue::08000234">Giuliano da Verona</a31nn>
  </aob30>
  <aob30 modifier="Herstellung">
    <a3100>Andrea di Lorenzo da Carrara (1528)</a3100>
    <a3470>zugeschrieben</a3470>
    <a3475>Steinmetz</a3475>
    <a3498>https://www.degruyter.com/database/AKL/entry/_10042527/html</a3498>
    <aob30rl>oder</aob30rl>
    <a3000 edp:augmented="kue::08000235">08000235</a3000>
    <a3105 edp:augmented="kue::08000235">Andrea Lorenzo Carrara (1538)</a3105>
    <a31nn edp:augmented="kue::08000235">Andrea di Lorenzo da Carrara</a31nn>
  </aob30>
  <aob30 modifier="Herstellung">
    <a3100>Bernardino da Siena (1532)</a3100>
    <a3470>zugeschrieben</a3470>
    <a3475>Steinmetz</a3475>
    <a3000 edp:augmented="kue::08000236">08000236</a3000>
    <a31nn edp:augmented="kue::08000236">Bernardino da Siena</a31nn>
  </aob30>
  <aob30 modifier="Herstellung">
    <a3100>Carota, Orsino di Antonio</a3100>
    <a3475>Maler</a3475>
    <a3000 edp:augmented="kue::08000237">08000237</a3000>
    <a3105 edp:augmented="kue::08000237">Carota, Orsino d'Antonio</a3105>
    <a31nn edp:augmented="kue::08000237">Orsino di Antonio Carota</a31nn>
  </aob30>
  <a5060 modifier="Datierung"><a5064>1524/1534</a5064></a5060>
  <a5060 modifier="Dekoration"><a5064>1578/1582</a5064><a5061>Teza S. 51</a5061></a5060>
</obj>
    |]

    result <- processXMLStringAsSet mapping midasBaseUri xml
    
    result `shouldBe` [t|
<artist/07500458> a crm:E39_Actor .
<artist/07500458> crm:P1_is_identified_by <artist/07500458/appellation/preferred_name> .
<artist/07500458> custom:sameAs <http://vocab.getty.edu/ulan/500058122> .
<artist/07500458/appellation/preferred_name> a crm:E41_Appellation .
<artist/07500458/appellation/preferred_name> crm:P190_has_symbolic_content "Rocco di Tommaso da Vicenza" .
<artist/07500458/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<artist/08000234> a crm:E39_Actor .
<artist/08000234> crm:P1_is_identified_by <artist/08000234/appellation/preferred_name> .
<artist/08000234/appellation/preferred_name> a crm:E41_Appellation .
<artist/08000234/appellation/preferred_name> crm:P190_has_symbolic_content "Giuliano da Verona (1525)" .
<artist/08000234/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<artist/08000235> a crm:E39_Actor .
<artist/08000235> crm:P1_is_identified_by <artist/08000235/appellation/1/alternative_name> .
<artist/08000235> crm:P1_is_identified_by <artist/08000235/appellation/preferred_name> .
<artist/08000235/appellation/1/alternative_name> a crm:E41_Appellation .
<artist/08000235/appellation/1/alternative_name> crm:P190_has_symbolic_content "Andrea Lorenzo Carrara (1538)" .
<artist/08000235/appellation/1/alternative_name> crm:P2_has_type pharos-meta:alternative_name .
<artist/08000235/appellation/preferred_name> a crm:E41_Appellation .
<artist/08000235/appellation/preferred_name> crm:P190_has_symbolic_content "Andrea di Lorenzo da Carrara (1528)" .
<artist/08000235/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<artist/08000236> a crm:E39_Actor .
<artist/08000236> crm:P1_is_identified_by <artist/08000236/appellation/preferred_name> .
<artist/08000236/appellation/preferred_name> a crm:E41_Appellation .
<artist/08000236/appellation/preferred_name> crm:P190_has_symbolic_content "Bernardino da Siena (1532)" .
<artist/08000236/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<artist/08000237> a crm:E39_Actor .
<artist/08000237> crm:P1_is_identified_by <artist/08000237/appellation/1/alternative_name> .
<artist/08000237> crm:P1_is_identified_by <artist/08000237/appellation/preferred_name> .
<artist/08000237/appellation/1/alternative_name> a crm:E41_Appellation .
<artist/08000237/appellation/1/alternative_name> crm:P190_has_symbolic_content "Carota, Orsino d'Antonio" .
<artist/08000237/appellation/1/alternative_name> crm:P2_has_type pharos-meta:alternative_name .
<artist/08000237/appellation/preferred_name> a crm:E41_Appellation .
<artist/08000237/appellation/preferred_name> crm:P190_has_symbolic_content "Carota, Orsino di Antonio" .
<artist/08000237/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<http://vocab.getty.edu/ulan/500058122> a crm:E39_Actor .
<vocab/a5060_event_type/datierung> a crm:E55_Type .
<vocab/a5060_event_type/datierung> crm:P1_is_identified_by <vocab/a5060_event_type/datierung/appellation/preferred_name> .
<vocab/a5060_event_type/datierung/appellation/preferred_name> a crm:E41_Appellation .
<vocab/a5060_event_type/datierung/appellation/preferred_name> crm:P190_has_symbolic_content "Datierung" .
<vocab/a5060_event_type/datierung/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<vocab/a5060_event_type/dekoration> a crm:E55_Type .
<vocab/a5060_event_type/dekoration> crm:P1_is_identified_by <vocab/a5060_event_type/dekoration/appellation/preferred_name> .
<vocab/a5060_event_type/dekoration/appellation/preferred_name> a crm:E41_Appellation .
<vocab/a5060_event_type/dekoration/appellation/preferred_name> crm:P190_has_symbolic_content "Dekoration" .
<vocab/a5060_event_type/dekoration/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<vocab/aob30_event_type/entwurf> a crm:E55_Type .
<vocab/aob30_event_type/entwurf> crm:P1_is_identified_by <vocab/aob30_event_type/entwurf/appellation/preferred_name> .
<vocab/aob30_event_type/entwurf/appellation/preferred_name> a crm:E41_Appellation .
<vocab/aob30_event_type/entwurf/appellation/preferred_name> crm:P190_has_symbolic_content "Entwurf" .
<vocab/aob30_event_type/entwurf/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<vocab/aob30_event_type/herstellung> a crm:E55_Type .
<vocab/aob30_event_type/herstellung> crm:P1_is_identified_by <vocab/aob30_event_type/herstellung/appellation/preferred_name> .
<vocab/aob30_event_type/herstellung/appellation/preferred_name> a crm:E41_Appellation .
<vocab/aob30_event_type/herstellung/appellation/preferred_name> crm:P190_has_symbolic_content "Herstellung" .
<vocab/aob30_event_type/herstellung/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<work/08197155> a crm:E22_Human-Made_Object .
<work/08197155> crm:P108i_was_produced_by <work/08197155/production> .
<work/08197155/production> a crm:E12_Production .
<work/08197155/production> crm:P9_consists_of <work/08197155/production/a5060/1> .
<work/08197155/production> crm:P9_consists_of <work/08197155/production/a5060/2> .
<work/08197155/production> crm:P9_consists_of <work/08197155/production/aob30/1> .
<work/08197155/production> crm:P9_consists_of <work/08197155/production/aob30/2> .
<work/08197155/production> crm:P9_consists_of <work/08197155/production/aob30/3> .
<work/08197155/production> crm:P9_consists_of <work/08197155/production/aob30/4> .
<work/08197155/production> crm:P9_consists_of <work/08197155/production/aob30/5> .
<work/08197155/production/a5060/1> a crm:E7_Activity .
<work/08197155/production/a5060/1> crm:P2_has_type <vocab/a5060_event_type/datierung> .
<work/08197155/production/a5060/1> crm:P4_has_time-span <work/08197155/production/a5060/1/date/1> .
<work/08197155/production/a5060/1/date/1> a crm:E52_Time-Span .
<work/08197155/production/a5060/1/date/1> crm:P1_is_identified_by <work/08197155/production/a5060/1/date/1/appellation/preferred_name> .
<work/08197155/production/a5060/1/date/1> crm:P82a_begin_of_the_begin "1524-01-01T00:00:00Z"^^xsd:dateTime .
<work/08197155/production/a5060/1/date/1> crm:P82b_end_of_the_end "1534-12-31T23:59:59Z"^^xsd:dateTime .
<work/08197155/production/a5060/1/date/1/appellation/preferred_name> a crm:E41_Appellation .
<work/08197155/production/a5060/1/date/1/appellation/preferred_name> crm:P190_has_symbolic_content "1524/1534" .
<work/08197155/production/a5060/1/date/1/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<work/08197155/production/a5060/2> a crm:E7_Activity .
<work/08197155/production/a5060/2> crm:P2_has_type <vocab/a5060_event_type/dekoration> .
<work/08197155/production/a5060/2> crm:P4_has_time-span <work/08197155/production/a5060/2/date/1> .
<work/08197155/production/a5060/2/date/1> a crm:E52_Time-Span .
<work/08197155/production/a5060/2/date/1> crm:P1_is_identified_by <work/08197155/production/a5060/2/date/1/appellation/preferred_name> .
<work/08197155/production/a5060/2/date/1> crm:P82a_begin_of_the_begin "1578-01-01T00:00:00Z"^^xsd:dateTime .
<work/08197155/production/a5060/2/date/1> crm:P82b_end_of_the_end "1582-12-31T23:59:59Z"^^xsd:dateTime .
<work/08197155/production/a5060/2/date/1/appellation/preferred_name> a crm:E41_Appellation .
<work/08197155/production/a5060/2/date/1/appellation/preferred_name> crm:P190_has_symbolic_content "1578/1582" .
<work/08197155/production/a5060/2/date/1/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<work/08197155/production/aob30/1> a crm:E7_Activity .
<work/08197155/production/aob30/1> crm:P14_carried_out_by <artist/07500458> .
<work/08197155/production/aob30/1> crm:P2_has_type <vocab/aob30_event_type/entwurf> .
<work/08197155/production/aob30/2> a crm:E7_Activity .
<work/08197155/production/aob30/2> crm:P14_carried_out_by <artist/08000234> .
<work/08197155/production/aob30/2> crm:P2_has_type <vocab/aob30_event_type/herstellung> .
<work/08197155/production/aob30/3> a crm:E7_Activity .
<work/08197155/production/aob30/3> crm:P14_carried_out_by <artist/08000235> .
<work/08197155/production/aob30/3> crm:P2_has_type <vocab/aob30_event_type/herstellung> .
<work/08197155/production/aob30/4> a crm:E7_Activity .
<work/08197155/production/aob30/4> crm:P14_carried_out_by <artist/08000236> .
<work/08197155/production/aob30/4> crm:P2_has_type <vocab/aob30_event_type/herstellung> .
<work/08197155/production/aob30/5> a crm:E7_Activity .
<work/08197155/production/aob30/5> crm:P14_carried_out_by <artist/08000237> .
<work/08197155/production/aob30/5> crm:P2_has_type <vocab/aob30_event_type/herstellung> .
    |]


  it "marburg, no aob30, only dates" $ do
    let mapping = baseMapping +> artistLinks
    let xml = [w|
<obj>
   <a5000>20806825</a5000>
   <a5060 modifier="Datierung"><a5064>1101/1200 &amp; 1501/1600</a5064></a5060>
</obj>
    |]

    result <- processXMLStringAsSet mapping midasBaseUri xml
    
    result `shouldBe` [t|
<vocab/a5060_event_type/datierung> a crm:E55_Type .
<vocab/a5060_event_type/datierung> crm:P1_is_identified_by <vocab/a5060_event_type/datierung/appellation/preferred_name> .
<vocab/a5060_event_type/datierung/appellation/preferred_name> a crm:E41_Appellation .
<vocab/a5060_event_type/datierung/appellation/preferred_name> crm:P190_has_symbolic_content "Datierung" .
<vocab/a5060_event_type/datierung/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<work/20806825> a crm:E22_Human-Made_Object .
<work/20806825> crm:P108i_was_produced_by <work/20806825/production> .
<work/20806825/production> a crm:E12_Production .
<work/20806825/production> crm:P9_consists_of <work/20806825/production/a5060/1> .
<work/20806825/production/a5060/1> a crm:E7_Activity .
<work/20806825/production/a5060/1> crm:P2_has_type <vocab/a5060_event_type/datierung> .
<work/20806825/production/a5060/1> crm:P4_has_time-span <work/20806825/production/a5060/1/date/1> .
<work/20806825/production/a5060/1> crm:P4_has_time-span <work/20806825/production/a5060/1/date/2> .
<work/20806825/production/a5060/1/date/1> a crm:E52_Time-Span .
<work/20806825/production/a5060/1/date/1> crm:P1_is_identified_by <work/20806825/production/a5060/1/date/1/appellation/preferred_name> .
<work/20806825/production/a5060/1/date/1> crm:P82a_begin_of_the_begin "1101-01-01T00:00:00Z"^^xsd:dateTime .
<work/20806825/production/a5060/1/date/1> crm:P82b_end_of_the_end "1200-12-31T23:59:59Z"^^xsd:dateTime .
<work/20806825/production/a5060/1/date/1/appellation/preferred_name> a crm:E41_Appellation .
<work/20806825/production/a5060/1/date/1/appellation/preferred_name> crm:P190_has_symbolic_content "1101/1200" .
<work/20806825/production/a5060/1/date/1/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<work/20806825/production/a5060/1/date/2> a crm:E52_Time-Span .
<work/20806825/production/a5060/1/date/2> crm:P1_is_identified_by <work/20806825/production/a5060/1/date/2/appellation/preferred_name> .
<work/20806825/production/a5060/1/date/2> crm:P82a_begin_of_the_begin "1501-01-01T00:00:00Z"^^xsd:dateTime .
<work/20806825/production/a5060/1/date/2> crm:P82b_end_of_the_end "1600-12-31T23:59:59Z"^^xsd:dateTime .
<work/20806825/production/a5060/1/date/2/appellation/preferred_name> a crm:E41_Appellation .
<work/20806825/production/a5060/1/date/2/appellation/preferred_name> crm:P190_has_symbolic_content "1501/1600" .
<work/20806825/production/a5060/1/date/2/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
    |]
