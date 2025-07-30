module Pharos.Datasets.Frick.TitlesSpec (spec) where

import CommonImports
import Pharos.Datasets.Frick.Mappings.Work (baseMapping, frickBaseUri)
import Pharos.Datasets.Frick.Mappings.Titles
import Test.Hspec

spec :: Spec
spec = describe "Frick Titles mapping" $ do
  it "correctly maps main title only" $ do
    let mapping = baseMapping +> titles
    let xml = [w|
      <?xml version="1.0" encoding="UTF-8"?>
      <record>
          <controlfield tag="001">991013625112907141</controlfield>
          <datafield ind1="1" ind2="0" tag="245">
              <subfield code="a">Queen of Sheba</subfield>
          </datafield>
      </record>
    |]

    result <- processXMLStringAsSet mapping frickBaseUri xml
    result
      `shouldBe`
        [t|
          <work/991013625112907141> a crm:E22_Human-Made_Object .
          <work/991013625112907141> crm:P1_is_identified_by <work/991013625112907141/appellation/title> .
          <work/991013625112907141/appellation/title> a crm:E41_Appellation .
          <work/991013625112907141/appellation/title> crm:P2_has_type <vocab/meta/title> .
          <work/991013625112907141/appellation/title> crm:P190_has_symbolic_content "Queen of Sheba" .
        |]

  it "correctly maps main title with variant title" $ do
    let mapping = baseMapping +> titles
    let xml = [w|
        <?xml version="1.0" encoding="UTF-8"?>
        <record>
            <controlfield tag="001">991007202809707141</controlfield>
            <datafield ind1="1" ind2="0" tag="245">
                <subfield code="a">Portrait of a Gentleman</subfield>
            </datafield>
            <datafield ind1="1" ind2="3" tag="246">
                <subfield code="a">Unidentified Man</subfield>
            </datafield>
        </record>
      |]

    result <- processXMLStringAsSet mapping frickBaseUri xml
    result
      `shouldBe`
        [t|
          <work/991007202809707141> a crm:E22_Human-Made_Object .
          <work/991007202809707141> crm:P1_is_identified_by <work/991007202809707141/appellation/title> .
          <work/991007202809707141> crm:P1_is_identified_by <work/991007202809707141/appellation/1/variant_title> .
          <work/991007202809707141/appellation/title> a crm:E41_Appellation .
          <work/991007202809707141/appellation/title> crm:P190_has_symbolic_content "Portrait of a Gentleman" .
          <work/991007202809707141/appellation/title> crm:P2_has_type <vocab/meta/title> .
          <work/991007202809707141/appellation/1/variant_title> a crm:E41_Appellation .
          <work/991007202809707141/appellation/1/variant_title> crm:P190_has_symbolic_content "Unidentified Man" .
          <work/991007202809707141/appellation/1/variant_title> crm:P2_has_type <vocab/meta/variant_title> .                 
        |]

  it "correctly maps main title with French variant title" $ do
    let mapping = baseMapping +> titles
    let xml = [w|
      <?xml version="1.0" encoding="UTF-8"?>
      <record>
          <controlfield tag="001">991013625048207141</controlfield>
          <datafield ind1="1" ind2="0" tag="245">
              <subfield code="a">Father Gioacchino Ventura di Raulica</subfield>
          </datafield>
          <datafield ind1="3" ind2="3" tag="246">
              <subfield code="a">R. Père Ventura de Raulica</subfield>
          </datafield>
      </record>
    |]

    result <- processXMLStringAsSet mapping frickBaseUri xml

    result
      `shouldBe`
        [t|
          <work/991013625048207141> a crm:E22_Human-Made_Object .
          <work/991013625048207141> crm:P1_is_identified_by <work/991013625048207141/appellation/title> .
          <work/991013625048207141> crm:P1_is_identified_by <work/991013625048207141/appellation/1/variant_title> .
          <work/991013625048207141/appellation/title> a crm:E41_Appellation .
          <work/991013625048207141/appellation/title> crm:P190_has_symbolic_content "Father Gioacchino Ventura di Raulica" .
          <work/991013625048207141/appellation/title> crm:P2_has_type <vocab/meta/title> .
          <work/991013625048207141/appellation/1/variant_title> a crm:E41_Appellation .
          <work/991013625048207141/appellation/1/variant_title> crm:P190_has_symbolic_content "R. Père Ventura de Raulica" .
          <work/991013625048207141/appellation/1/variant_title> crm:P2_has_type <vocab/meta/variant_title> .                 
        |]
