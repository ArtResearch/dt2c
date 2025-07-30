module Pharos.Datasets.Frick.TypesSpec (spec) where

import CommonImports
import qualified Pharos.Datasets.Frick.Mappings.Types as M
import Pharos.Datasets.Frick.Mappings.Work (baseMapping, frickBaseUri)
import Test.Hspec

spec :: Spec
spec = describe "Frick Types mapping" $ do
  it "type with exact match" $ do
    let mapping = baseMapping +> M.types
    let xml = [w|
      <?xml version="1.0" encoding="UTF-8"?>
      <record>
          <controlfield tag="001">991000126199707141</controlfield>
          <datafield tag="300" ind1="1" ind2=" ">
            <subfield code="e">Graphic reproduction(s) with documentation of a</subfield>
            <subfield code="a">fresco.</subfield>
          </datafield>
      </record>
    |]

    result <- processXMLStringAsSet mapping frickBaseUri xml
    result `shouldBe` [t|
<http://vocab.getty.edu/aat/> a crm:E32_Authority_Document .
<http://vocab.getty.edu/aat/300177433> crm:P71i_is_listed_in <http://vocab.getty.edu/aat/> .
<http://vocab.getty.edu/aat/300177433> a crm:E55_Type .
<vocab/physical_description/fresco> crm:P1_is_identified_by <vocab/physical_description/fresco/appellation/preferred_name> .
<vocab/physical_description/fresco> crm:P2_has_type pharos-meta:object_type .
<vocab/physical_description/fresco> custom:sameAs <http://vocab.getty.edu/aat/300177433> .
<vocab/physical_description/fresco> a crm:E55_Type .
<vocab/physical_description/fresco/appellation/preferred_name> crm:P190_has_symbolic_content "fresco" .
<vocab/physical_description/fresco/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<vocab/physical_description/fresco/appellation/preferred_name> a crm:E41_Appellation .
<work/991000126199707141> crm:P2_has_type <vocab/physical_description/fresco> .
<work/991000126199707141> a crm:E22_Human-Made_Object .
    |]

  it "type with broad match" $ do
    let mapping = baseMapping +> M.types
    let xml = [w|
      <?xml version="1.0" encoding="UTF-8"?>
      <record>
          <controlfield tag="001">991012870909707141</controlfield>
          <datafield tag="300" ind1="1" ind2=" ">
            <subfield code="e">Graphic reproduction(s) with documentation of a</subfield>
            <subfield code="a">detached fresco.</subfield>
          </datafield>
      </record>
    |]

    result <- processXMLStringAsSet mapping frickBaseUri xml
    result `shouldBe` [t|
<http://vocab.getty.edu/aat/> a crm:E32_Authority_Document .
<http://vocab.getty.edu/aat/300177433> crm:P71i_is_listed_in <http://vocab.getty.edu/aat/> .
<http://vocab.getty.edu/aat/300177433> a crm:E55_Type .
<vocab/physical_description/detached_fresco> crm:P127_has_broader_term <http://vocab.getty.edu/aat/300177433> .
<vocab/physical_description/detached_fresco> crm:P1_is_identified_by <vocab/physical_description/detached_fresco/appellation/preferred_name> .
<vocab/physical_description/detached_fresco> crm:P2_has_type pharos-meta:object_type .
<vocab/physical_description/detached_fresco> crm:P71i_is_listed_in <http://vocab.getty.edu/aat/> .
<vocab/physical_description/detached_fresco> a crm:E55_Type .
<vocab/physical_description/detached_fresco/appellation/preferred_name> crm:P190_has_symbolic_content "detached fresco" .
<vocab/physical_description/detached_fresco/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<vocab/physical_description/detached_fresco/appellation/preferred_name> a crm:E41_Appellation .
<work/991012870909707141> crm:P2_has_type <vocab/physical_description/detached_fresco> .
<work/991012870909707141> a crm:E22_Human-Made_Object .
    |]
