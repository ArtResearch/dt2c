module Pharos.Datasets.Zeri.TypesSpec (spec) where

import CommonImports
import qualified Mappings.Mappings.Work.Types as M
import Mappings.Mappings.Work.WorkMappings (baseMapping, zeriBaseUri)
import Test.Hspec

spec :: Spec
spec = describe "Zeri Types mapping" $ do
  it "ogtd" $ do
    let mapping = baseMapping +> M.types
    let xml = [w|
<RISULTATI>
  <SCHEDA sercdoa="9920">
    <PARAGRAFO etichetta="OBJECT">
      <OGTD etichetta="Object">dipinto</OGTD>
    </PARAGRAFO>
  </SCHEDA>
</RISULTATI>
    |]

    result <- processXMLStringAsSet mapping zeriBaseUri xml
    result `shouldBe` [t|
<http://vocab.getty.edu/aat/> a crm:E32_Authority_Document .
<http://vocab.getty.edu/aat/300033618> crm:P71i_is_listed_in <http://vocab.getty.edu/aat/> .
<http://vocab.getty.edu/aat/300033618> a crm:E55_Type .
<vocab/object_type/dipinto> crm:P1_is_identified_by <vocab/object_type/dipinto/appellation/preferred_name> .
<vocab/object_type/dipinto> crm:P2_has_type pharos-meta:object_type .
<vocab/object_type/dipinto> custom:sameAs <http://vocab.getty.edu/aat/300033618> .
<vocab/object_type/dipinto> a crm:E55_Type .
<vocab/object_type/dipinto/appellation/preferred_name> crm:P190_has_symbolic_content "dipinto" .
<vocab/object_type/dipinto/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<vocab/object_type/dipinto/appellation/preferred_name> a crm:E41_Appellation .
<work/9920> crm:P2_has_type <vocab/object_type/dipinto> .
<work/9920> a crm:E22_Human-Made_Object .
    |]

  it "ogtt and ogtd" $ do
    let mapping = baseMapping +> M.types
    let xml = [w|
<RISULTATI>
  <SCHEDA sercdoa="531">
    <PARAGRAFO etichetta="OBJECT">
      <OGTD etichetta="Object">disegno</OGTD>
      <OGTT etichetta="Object type">grafica</OGTT>    
    </PARAGRAFO>
  </SCHEDA>
</RISULTATI>
    |]

    result <- processXMLStringAsSet mapping zeriBaseUri xml
    result `shouldBe` [t|
<http://vocab.getty.edu/aat/> a crm:E32_Authority_Document .
<http://vocab.getty.edu/aat/300002330> crm:P71i_is_listed_in <http://vocab.getty.edu/aat/> .
<http://vocab.getty.edu/aat/300002330> a crm:E55_Type .
<http://vocab.getty.edu/aat/300264849> crm:P71i_is_listed_in <http://vocab.getty.edu/aat/> .
<http://vocab.getty.edu/aat/300264849> a crm:E55_Type .
<vocab/object_type/disegno> crm:P1_is_identified_by <vocab/object_type/disegno/appellation/preferred_name> .
<vocab/object_type/disegno> crm:P2_has_type pharos-meta:object_type .
<vocab/object_type/disegno> custom:sameAs <http://vocab.getty.edu/aat/300002330> .
<vocab/object_type/disegno> a crm:E55_Type .
<vocab/object_type/disegno/appellation/preferred_name> crm:P190_has_symbolic_content "disegno" .
<vocab/object_type/disegno/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<vocab/object_type/disegno/appellation/preferred_name> a crm:E41_Appellation .
<vocab/typology/grafica> crm:P1_is_identified_by <vocab/typology/grafica/appellation/preferred_name> .
<vocab/typology/grafica> crm:P2_has_type pharos-meta:object_type .
<vocab/typology/grafica> custom:sameAs <http://vocab.getty.edu/aat/300264849> .
<vocab/typology/grafica> a crm:E55_Type .
<vocab/typology/grafica/appellation/preferred_name> crm:P190_has_symbolic_content "grafica" .
<vocab/typology/grafica/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<vocab/typology/grafica/appellation/preferred_name> a crm:E41_Appellation .
<work/531> crm:P2_has_type <vocab/object_type/disegno> .
<work/531> crm:P2_has_type <vocab/typology/grafica> .
<work/531> a crm:E22_Human-Made_Object .
    |]
