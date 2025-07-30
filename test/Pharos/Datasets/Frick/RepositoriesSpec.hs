module Pharos.Datasets.Frick.RepositoriesSpec (spec) where

import CommonImports
import Test.Hspec
import Pharos.Datasets.Frick.Mappings.Repositories (repositoryLinks)
import Pharos.Datasets.Frick.Mappings.Work (baseMapping, frickBaseUri) -- for baseMapping and frickBaseUri

spec :: Spec
spec = describe "Frick Repositories mapping" $ do

  let mapping = baseMapping +> repositoryLinks

  it "should process Case 1: Repository only (a)" $ do
    let frickXml = [w|
      <record>
        <controlfield tag="001">991000007089707141</controlfield>
        <datafield tag="590" ind1="1" ind2=" ">
          <subfield code="a">Mrs. Beverley Upshaw Estate.</subfield>
        </datafield>
      </record>
    |]
    result <- processXMLStringAsSet mapping frickBaseUri frickXml
    result `shouldBe` [t|
<actor/repository_actor/mrs_beverley_upshaw_estate> a crm:E39_Actor .
<actor/repository_actor/mrs_beverley_upshaw_estate> crm:P1_is_identified_by <actor/repository_actor/mrs_beverley_upshaw_estate/appellation/preferred_name> .
<actor/repository_actor/mrs_beverley_upshaw_estate> crm:P2_has_type <vocab/meta/repository_actor> .
<actor/repository_actor/mrs_beverley_upshaw_estate> crm:P50i_is_current_keeper_of <work/991000007089707141> .
<actor/repository_actor/mrs_beverley_upshaw_estate/appellation/preferred_name> a crm:E41_Appellation .
<actor/repository_actor/mrs_beverley_upshaw_estate/appellation/preferred_name> crm:P190_has_symbolic_content "Mrs. Beverley Upshaw Estate" .
<actor/repository_actor/mrs_beverley_upshaw_estate/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<work/991000007089707141> a crm:E22_Human-Made_Object .
<work/991000007089707141> crm:P50_has_current_keeper <actor/repository_actor/mrs_beverley_upshaw_estate> .
|]

  it "should process Case 2: City and Repository (a, b)" $ do
    let frickXml = [w|
      <record>
        <controlfield tag="001">991000038679707141</controlfield>
        <datafield tag="590" ind1="1" ind2=" ">
          <subfield code="a">Mesdag Museum,</subfield>
          <subfield code="b">Hague,</subfield>
        </datafield>
      </record>
    |]
    result <- processXMLStringAsSet mapping frickBaseUri frickXml
    result `shouldBe` [t|
<place/city/hague> a crm:E53_Place .
<place/city/hague> crm:P1_is_identified_by <place/city/hague/appellation/preferred_name> .
<place/city/hague> crm:P2_has_type pharos-meta:city .
<place/city/hague> crm:P74i_is_current_or_former_residence_of <place/city/hague/actor/repository_actor/mesdag_museum> .
<place/city/hague/actor/repository_actor/mesdag_museum> a crm:E39_Actor .
<place/city/hague/actor/repository_actor/mesdag_museum> crm:P1_is_identified_by <place/city/hague/actor/repository_actor/mesdag_museum/appellation/preferred_name> .
<place/city/hague/actor/repository_actor/mesdag_museum> crm:P2_has_type <vocab/meta/repository_actor> .
<place/city/hague/actor/repository_actor/mesdag_museum> crm:P50i_is_current_keeper_of <work/991000038679707141> .
<place/city/hague/actor/repository_actor/mesdag_museum/appellation/preferred_name> a crm:E41_Appellation .
<place/city/hague/actor/repository_actor/mesdag_museum/appellation/preferred_name> crm:P190_has_symbolic_content "Mesdag Museum" .
<place/city/hague/actor/repository_actor/mesdag_museum/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<place/city/hague/appellation/preferred_name> a crm:E41_Appellation .
<place/city/hague/appellation/preferred_name> crm:P190_has_symbolic_content "Hague" .
<place/city/hague/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<work/991000038679707141> a crm:E22_Human-Made_Object .
<work/991000038679707141> crm:P55_has_current_location <place/city/hague> .
|]

  it "should process Case 3.1: Region, City, and Repository (a, b, c)" $ do
    let frickXml = [w|
      <record>
        <controlfield tag="001">991002706219707141</controlfield>
        <datafield tag="590" ind1="1" ind2=" ">
          <subfield code="a">Mrs. Mary Fowler,</subfield>
          <subfield code="b">Tarpon Springs,</subfield>
          <subfield code="c">Florida.</subfield>
        </datafield>
      </record>
    |]
    result <- processXMLStringAsSet mapping frickBaseUri frickXml
    result `shouldBe` [t|
<place/region/florida> a crm:E53_Place .
<place/region/florida> crm:P1_is_identified_by <place/region/florida/appellation/preferred_name> .
<place/region/florida> crm:P2_has_type pharos-meta:region .
<place/region/florida> crm:P89i_contains <place/region/florida/city/tarpon_springs> .
<place/region/florida/appellation/preferred_name> a crm:E41_Appellation .
<place/region/florida/appellation/preferred_name> crm:P190_has_symbolic_content "Florida" .
<place/region/florida/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<place/region/florida/city/tarpon_springs> a crm:E53_Place .
<place/region/florida/city/tarpon_springs> crm:P1_is_identified_by <place/region/florida/city/tarpon_springs/appellation/preferred_name> .
<place/region/florida/city/tarpon_springs> crm:P2_has_type pharos-meta:city .
<place/region/florida/city/tarpon_springs> crm:P74i_is_current_or_former_residence_of <place/region/florida/city/tarpon_springs/actor/repository_actor/mrs_mary_fowler> .
<place/region/florida/city/tarpon_springs/actor/repository_actor/mrs_mary_fowler> a crm:E39_Actor .
<place/region/florida/city/tarpon_springs/actor/repository_actor/mrs_mary_fowler> crm:P1_is_identified_by <place/region/florida/city/tarpon_springs/actor/repository_actor/mrs_mary_fowler/appellation/preferred_name> .
<place/region/florida/city/tarpon_springs/actor/repository_actor/mrs_mary_fowler> crm:P2_has_type <vocab/meta/repository_actor> .
<place/region/florida/city/tarpon_springs/actor/repository_actor/mrs_mary_fowler> crm:P50i_is_current_keeper_of <work/991002706219707141> .
<place/region/florida/city/tarpon_springs/actor/repository_actor/mrs_mary_fowler/appellation/preferred_name> a crm:E41_Appellation .
<place/region/florida/city/tarpon_springs/actor/repository_actor/mrs_mary_fowler/appellation/preferred_name> crm:P190_has_symbolic_content "Mrs. Mary Fowler" .
<place/region/florida/city/tarpon_springs/actor/repository_actor/mrs_mary_fowler/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<place/region/florida/city/tarpon_springs/appellation/preferred_name> a crm:E41_Appellation .
<place/region/florida/city/tarpon_springs/appellation/preferred_name> crm:P190_has_symbolic_content "Tarpon Springs" .
<place/region/florida/city/tarpon_springs/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<work/991002706219707141> a crm:E22_Human-Made_Object .
<work/991002706219707141> crm:P55_has_current_location <place/region/florida> .
|]

  it "should process Case 3.2: Region and Repository (a, c; no b sibling)" $ do
    let frickXml = [w|
      <record>
        <controlfield tag="001">991001057589707141</controlfield>
        <datafield tag="590" ind1="1" ind2=" ">
          <subfield code="a">Private Collection,</subfield>
          <subfield code="c">United States,</subfield>
        </datafield>
      </record>
    |]
    result <- processXMLStringAsSet mapping frickBaseUri frickXml
    result `shouldBe` [t|
<place/region/united_states> a crm:E53_Place .
<place/region/united_states> crm:P1_is_identified_by <place/region/united_states/appellation/preferred_name> .
<place/region/united_states> crm:P2_has_type pharos-meta:region .
<place/region/united_states> crm:P74i_is_current_or_former_residence_of <place/region/united_states/actor/repository_actor/private_collection> .
<place/region/united_states/actor/repository_actor/private_collection> a crm:E39_Actor .
<place/region/united_states/actor/repository_actor/private_collection> crm:P1_is_identified_by <place/region/united_states/actor/repository_actor/private_collection/appellation/preferred_name> .
<place/region/united_states/actor/repository_actor/private_collection> crm:P2_has_type <vocab/meta/repository_actor> .
<place/region/united_states/actor/repository_actor/private_collection> crm:P50i_is_current_keeper_of <work/991001057589707141> .
<place/region/united_states/actor/repository_actor/private_collection/appellation/preferred_name> a crm:E41_Appellation .
<place/region/united_states/actor/repository_actor/private_collection/appellation/preferred_name> crm:P190_has_symbolic_content "Private Collection" .
<place/region/united_states/actor/repository_actor/private_collection/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<place/region/united_states/appellation/preferred_name> a crm:E41_Appellation .
<place/region/united_states/appellation/preferred_name> crm:P190_has_symbolic_content "United States" .
<place/region/united_states/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<work/991001057589707141> a crm:E22_Human-Made_Object .
<work/991001057589707141> crm:P55_has_current_location <place/region/united_states> .
|]

  it "should process Case 4.1.1: Country, Region, City, and Repository (a, b, c, d)" $ do
    let frickXml = [w|
      <record>
        <controlfield tag="001">991000026369707141</controlfield>
        <datafield tag="590" ind1="1" ind2=" ">
          <subfield code="a">Glenn Tilley Morse Estate,</subfield>
          <subfield code="b">West Newbury,</subfield>
          <subfield code="c">Massachusetts,</subfield>
          <subfield code="d">United States.</subfield>
        </datafield>
      </record>
    |]
    result <- processXMLStringAsSet mapping frickBaseUri frickXml
    result `shouldBe` [t|
<http://vocab.getty.edu/tgn/2050913> a crm:E53_Place .
<http://vocab.getty.edu/tgn/7007517> a crm:E53_Place .
<http://vocab.getty.edu/tgn/7012149> a crm:E53_Place .
<place/country/united_states> a crm:E53_Place .
<place/country/united_states> crm:P1_is_identified_by <place/country/united_states/appellation/preferred_name> .
<place/country/united_states> crm:P2_has_type pharos-meta:country .
<place/country/united_states> crm:P89i_contains <place/country/united_states/region/massachusetts> .
<place/country/united_states> custom:sameAs <http://vocab.getty.edu/tgn/7012149> .
<place/country/united_states/appellation/preferred_name> a crm:E41_Appellation .
<place/country/united_states/appellation/preferred_name> crm:P190_has_symbolic_content "United States" .
<place/country/united_states/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<place/country/united_states/region/massachusetts> a crm:E53_Place .
<place/country/united_states/region/massachusetts> crm:P1_is_identified_by <place/country/united_states/region/massachusetts/appellation/preferred_name> .
<place/country/united_states/region/massachusetts> crm:P2_has_type pharos-meta:region .
<place/country/united_states/region/massachusetts> crm:P89i_contains <place/country/united_states/region/massachusetts/city/west_newbury> .
<place/country/united_states/region/massachusetts> custom:sameAs <http://vocab.getty.edu/tgn/7007517> .
<place/country/united_states/region/massachusetts/appellation/preferred_name> a crm:E41_Appellation .
<place/country/united_states/region/massachusetts/appellation/preferred_name> crm:P190_has_symbolic_content "Massachusetts" .
<place/country/united_states/region/massachusetts/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<place/country/united_states/region/massachusetts/city/west_newbury> a crm:E53_Place .
<place/country/united_states/region/massachusetts/city/west_newbury> crm:P1_is_identified_by <place/country/united_states/region/massachusetts/city/west_newbury/appellation/preferred_name> .
<place/country/united_states/region/massachusetts/city/west_newbury> crm:P2_has_type pharos-meta:city .
<place/country/united_states/region/massachusetts/city/west_newbury> crm:P74i_is_current_or_former_residence_of <place/country/united_states/region/massachusetts/city/west_newbury/actor/repository_actor/glenn_tilley_morse_estate> .
<place/country/united_states/region/massachusetts/city/west_newbury> custom:sameAs <http://vocab.getty.edu/tgn/2050913> .
<place/country/united_states/region/massachusetts/city/west_newbury/actor/repository_actor/glenn_tilley_morse_estate> a crm:E39_Actor .
<place/country/united_states/region/massachusetts/city/west_newbury/actor/repository_actor/glenn_tilley_morse_estate> crm:P1_is_identified_by <place/country/united_states/region/massachusetts/city/west_newbury/actor/repository_actor/glenn_tilley_morse_estate/appellation/preferred_name> .
<place/country/united_states/region/massachusetts/city/west_newbury/actor/repository_actor/glenn_tilley_morse_estate> crm:P2_has_type <vocab/meta/repository_actor> .
<place/country/united_states/region/massachusetts/city/west_newbury/actor/repository_actor/glenn_tilley_morse_estate> crm:P50i_is_current_keeper_of <work/991000026369707141> .
<place/country/united_states/region/massachusetts/city/west_newbury/actor/repository_actor/glenn_tilley_morse_estate/appellation/preferred_name> a crm:E41_Appellation .
<place/country/united_states/region/massachusetts/city/west_newbury/actor/repository_actor/glenn_tilley_morse_estate/appellation/preferred_name> crm:P190_has_symbolic_content "Glenn Tilley Morse Estate" .
<place/country/united_states/region/massachusetts/city/west_newbury/actor/repository_actor/glenn_tilley_morse_estate/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<place/country/united_states/region/massachusetts/city/west_newbury/appellation/preferred_name> a crm:E41_Appellation .
<place/country/united_states/region/massachusetts/city/west_newbury/appellation/preferred_name> crm:P190_has_symbolic_content "West Newbury" .
<place/country/united_states/region/massachusetts/city/west_newbury/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<work/991000026369707141> a crm:E22_Human-Made_Object .
<work/991000026369707141> crm:P55_has_current_location <place/country/united_states> .
|]

  it "should process Case 4.1.2: Country, Region, and Repository (a, c, d; no b sibling to c)" $ do
    let frickXml = [w|
      <record>
        <controlfield tag="001">991000019829707141</controlfield>
        <datafield tag="590" ind1="1" ind2=" ">
          <subfield code="a">Private collection, New York,</subfield>
          <subfield code="c">New York,</subfield>
          <subfield code="d">United States,</subfield>
        </datafield>
      </record>
    |]
    result <- processXMLStringAsSet mapping frickBaseUri frickXml
    result `shouldBe` [t|
<http://vocab.getty.edu/tgn/7007568> a crm:E53_Place .
<http://vocab.getty.edu/tgn/7012149> a crm:E53_Place .
<place/country/united_states> a crm:E53_Place .
<place/country/united_states> crm:P1_is_identified_by <place/country/united_states/appellation/preferred_name> .
<place/country/united_states> crm:P2_has_type pharos-meta:country .
<place/country/united_states> crm:P89i_contains <place/country/united_states/region/new_york> .
<place/country/united_states> custom:sameAs <http://vocab.getty.edu/tgn/7012149> .
<place/country/united_states/appellation/preferred_name> a crm:E41_Appellation .
<place/country/united_states/appellation/preferred_name> crm:P190_has_symbolic_content "United States" .
<place/country/united_states/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<place/country/united_states/region/new_york> a crm:E53_Place .
<place/country/united_states/region/new_york> crm:P1_is_identified_by <place/country/united_states/region/new_york/appellation/preferred_name> .
<place/country/united_states/region/new_york> crm:P2_has_type pharos-meta:region .
<place/country/united_states/region/new_york> crm:P74i_is_current_or_former_residence_of <place/country/united_states/region/new_york/actor/repository_actor/private_collection_new_york> .
<place/country/united_states/region/new_york> custom:sameAs <http://vocab.getty.edu/tgn/7007568> .
<place/country/united_states/region/new_york/actor/repository_actor/private_collection_new_york> a crm:E39_Actor .
<place/country/united_states/region/new_york/actor/repository_actor/private_collection_new_york> crm:P1_is_identified_by <place/country/united_states/region/new_york/actor/repository_actor/private_collection_new_york/appellation/preferred_name> .
<place/country/united_states/region/new_york/actor/repository_actor/private_collection_new_york> crm:P2_has_type <vocab/meta/repository_actor> .
<place/country/united_states/region/new_york/actor/repository_actor/private_collection_new_york> crm:P50i_is_current_keeper_of <work/991000019829707141> .
<place/country/united_states/region/new_york/actor/repository_actor/private_collection_new_york/appellation/preferred_name> a crm:E41_Appellation .
<place/country/united_states/region/new_york/actor/repository_actor/private_collection_new_york/appellation/preferred_name> crm:P190_has_symbolic_content "Private collection, New York" .
<place/country/united_states/region/new_york/actor/repository_actor/private_collection_new_york/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<place/country/united_states/region/new_york/appellation/preferred_name> a crm:E41_Appellation .
<place/country/united_states/region/new_york/appellation/preferred_name> crm:P190_has_symbolic_content "New York" .
<place/country/united_states/region/new_york/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<work/991000019829707141> a crm:E22_Human-Made_Object .
<work/991000019829707141> crm:P55_has_current_location <place/country/united_states> .
|]

  it "should process Case 4.2: Country, City, and Repository (a, b, d; no c sibling to d)" $ do
    let frickXml = [w|
      <record>
        <controlfield tag="001">991000756469707141</controlfield>
        <datafield tag="590" ind1="1" ind2=" ">
          <subfield code="a">British Museum,</subfield>
          <subfield code="b">London,</subfield>
          <subfield code="d">England,</subfield>
        </datafield>
      </record>
    |]
    result <- processXMLStringAsSet mapping frickBaseUri frickXml
    result `shouldBe` [t|
<http://vocab.getty.edu/tgn/7002445> a crm:E53_Place .
<http://vocab.getty.edu/tgn/7011781> a crm:E53_Place .
<http://www.wikidata.org/entity/Q6373> a crm:E39_Actor .
<place/country/england> a crm:E53_Place .
<place/country/england> crm:P1_is_identified_by <place/country/england/appellation/preferred_name> .
<place/country/england> crm:P2_has_type pharos-meta:country .
<place/country/england> crm:P89i_contains <place/country/england/city/london> .
<place/country/england> custom:sameAs <http://vocab.getty.edu/tgn/7002445> .
<place/country/england/appellation/preferred_name> a crm:E41_Appellation .
<place/country/england/appellation/preferred_name> crm:P190_has_symbolic_content "England" .
<place/country/england/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<place/country/england/city/london> a crm:E53_Place .
<place/country/england/city/london> crm:P1_is_identified_by <place/country/england/city/london/appellation/preferred_name> .
<place/country/england/city/london> crm:P2_has_type pharos-meta:city .
<place/country/england/city/london> crm:P74i_is_current_or_former_residence_of <place/country/england/city/london/actor/repository_actor/british_museum> .
<place/country/england/city/london> custom:sameAs <http://vocab.getty.edu/tgn/7011781> .
<place/country/england/city/london/actor/repository_actor/british_museum> a crm:E39_Actor .
<place/country/england/city/london/actor/repository_actor/british_museum> crm:P1_is_identified_by <place/country/england/city/london/actor/repository_actor/british_museum/appellation/preferred_name> .
<place/country/england/city/london/actor/repository_actor/british_museum> crm:P2_has_type <vocab/meta/repository_actor> .
<place/country/england/city/london/actor/repository_actor/british_museum> crm:P50i_is_current_keeper_of <work/991000756469707141> .
<place/country/england/city/london/actor/repository_actor/british_museum> custom:sameAs <http://www.wikidata.org/entity/Q6373> .
<place/country/england/city/london/actor/repository_actor/british_museum/appellation/preferred_name> a crm:E41_Appellation .
<place/country/england/city/london/actor/repository_actor/british_museum/appellation/preferred_name> crm:P190_has_symbolic_content "British Museum" .
<place/country/england/city/london/actor/repository_actor/british_museum/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<place/country/england/city/london/appellation/preferred_name> a crm:E41_Appellation .
<place/country/england/city/london/appellation/preferred_name> crm:P190_has_symbolic_content "London" .
<place/country/england/city/london/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<work/991000756469707141> a crm:E22_Human-Made_Object .
<work/991000756469707141> crm:P55_has_current_location <place/country/england> .
|]

  it "should process Case 4.3: Country and Repository (a, d; no b or c siblings to d)" $ do
    let frickXml = [w|
      <record>
        <controlfield tag="001">991007177599707141</controlfield>
        <datafield tag="590" ind1="1" ind2=" ">
          <subfield code="a">Mrs. and Mrs.Edgar W. Garbisch,</subfield>
          <subfield code="d">United States,</subfield>
        </datafield>
      </record>
    |]
    result <- processXMLStringAsSet mapping frickBaseUri frickXml
    result `shouldBe` [t|
<http://vocab.getty.edu/tgn/7012149> a crm:E53_Place .
<place/country/united_states> a crm:E53_Place .
<place/country/united_states> crm:P1_is_identified_by <place/country/united_states/appellation/preferred_name> .
<place/country/united_states> crm:P2_has_type pharos-meta:country .
<place/country/united_states> crm:P74i_is_current_or_former_residence_of <place/country/united_states/actor/repository_actor/mrs_and_mrs_edgar_w_garbisch> .
<place/country/united_states> custom:sameAs <http://vocab.getty.edu/tgn/7012149> .
<place/country/united_states/actor/repository_actor/mrs_and_mrs_edgar_w_garbisch> a crm:E39_Actor .
<place/country/united_states/actor/repository_actor/mrs_and_mrs_edgar_w_garbisch> crm:P1_is_identified_by <place/country/united_states/actor/repository_actor/mrs_and_mrs_edgar_w_garbisch/appellation/preferred_name> .
<place/country/united_states/actor/repository_actor/mrs_and_mrs_edgar_w_garbisch> crm:P2_has_type <vocab/meta/repository_actor> .
<place/country/united_states/actor/repository_actor/mrs_and_mrs_edgar_w_garbisch> crm:P50i_is_current_keeper_of <work/991007177599707141> .
<place/country/united_states/actor/repository_actor/mrs_and_mrs_edgar_w_garbisch/appellation/preferred_name> a crm:E41_Appellation .
<place/country/united_states/actor/repository_actor/mrs_and_mrs_edgar_w_garbisch/appellation/preferred_name> crm:P190_has_symbolic_content "Mrs. and Mrs.Edgar W. Garbisch" .
<place/country/united_states/actor/repository_actor/mrs_and_mrs_edgar_w_garbisch/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<place/country/united_states/appellation/preferred_name> a crm:E41_Appellation .
<place/country/united_states/appellation/preferred_name> crm:P190_has_symbolic_content "United States" .
<place/country/united_states/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<work/991007177599707141> a crm:E22_Human-Made_Object .
<work/991007177599707141> crm:P55_has_current_location <place/country/united_states> .
|]
