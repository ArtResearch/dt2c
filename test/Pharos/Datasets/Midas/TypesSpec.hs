module Pharos.Datasets.Midas.TypesSpec (spec) where

import CommonImports
import qualified Midas.Mappings.Types as M
import Midas.Mappings.Work (baseMapping, nestedWorks, midasBaseUri)
import Test.Hspec

spec :: Spec
spec = describe "Midas Types mapping" $ do
  it "test a5220, a5226, a5230" $ do
    let mapping = baseMapping +> M.types
    let xml = [w|
<obj>
  <a5000>08015408</a5000>

  <a5220>Skulptur</a5220>
  <a5220>Bauskulptur</a5220>
  <a5226>Reliefplastik</a5226>
  <a5230>Schlussstein</a5230>
</obj>
    |]

    result <- processXMLStringAsSet mapping midasBaseUri xml
    result `shouldBe` [t|
<http://vocab.getty.edu/aat/> a crm:E32_Authority_Document .
<http://vocab.getty.edu/aat/300001183> crm:P71i_is_listed_in <http://vocab.getty.edu/aat/> .
<http://vocab.getty.edu/aat/300001183> a crm:E55_Type .
<http://vocab.getty.edu/aat/300047090> crm:P71i_is_listed_in <http://vocab.getty.edu/aat/> .
<http://vocab.getty.edu/aat/300047090> a crm:E55_Type .
<http://vocab.getty.edu/aat/300379328> crm:P71i_is_listed_in <http://vocab.getty.edu/aat/> .
<http://vocab.getty.edu/aat/300379328> a crm:E55_Type .
<https://artresearch.net/resource/pharos/vocab/types> a crm:E32_Authority_Document .
<vocab/art/reliefplastik> crm:P1_is_identified_by <vocab/art/reliefplastik/appellation/preferred_name> .
<vocab/art/reliefplastik> crm:P2_has_type <vocab/meta/art> .
<vocab/art/reliefplastik> crm:P71i_is_listed_in <https://artresearch.net/resource/pharos/vocab/types> .
<vocab/art/reliefplastik> a crm:E55_Type .
<vocab/art/reliefplastik/appellation/preferred_name> crm:P190_has_symbolic_content "Reliefplastik" .
<vocab/art/reliefplastik/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<vocab/art/reliefplastik/appellation/preferred_name> a crm:E41_Appellation .
<vocab/classification/bauskulptur> crm:P1_is_identified_by <vocab/classification/bauskulptur/appellation/preferred_name> .
<vocab/classification/bauskulptur> crm:P2_has_type <vocab/meta/classification> .
<vocab/classification/bauskulptur> custom:sameAs <http://vocab.getty.edu/aat/300379328> .
<vocab/classification/bauskulptur> a crm:E55_Type .
<vocab/classification/bauskulptur/appellation/preferred_name> crm:P190_has_symbolic_content "Bauskulptur" .
<vocab/classification/bauskulptur/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<vocab/classification/bauskulptur/appellation/preferred_name> a crm:E41_Appellation .
<vocab/classification/skulptur> crm:P127_has_broader_term <http://vocab.getty.edu/aat/300047090> .
<vocab/classification/skulptur> crm:P1_is_identified_by <vocab/classification/skulptur/appellation/preferred_name> .
<vocab/classification/skulptur> crm:P2_has_type <vocab/meta/classification> .
<vocab/classification/skulptur> crm:P71i_is_listed_in <https://artresearch.net/resource/pharos/vocab/types> .
<vocab/classification/skulptur> a crm:E55_Type .
<vocab/classification/skulptur/appellation/preferred_name> crm:P190_has_symbolic_content "Skulptur" .
<vocab/classification/skulptur/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<vocab/classification/skulptur/appellation/preferred_name> a crm:E41_Appellation .
<vocab/object_type/schlussstein> crm:P1_is_identified_by <vocab/object_type/schlussstein/appellation/preferred_name> .
<vocab/object_type/schlussstein> crm:P2_has_type <vocab/meta/object_type> .
<vocab/object_type/schlussstein> custom:sameAs <http://vocab.getty.edu/aat/300001183> .
<vocab/object_type/schlussstein> a crm:E55_Type .
<vocab/object_type/schlussstein/appellation/preferred_name> crm:P190_has_symbolic_content "Schlussstein" .
<vocab/object_type/schlussstein/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<vocab/object_type/schlussstein/appellation/preferred_name> a crm:E41_Appellation .
<work/08015408> crm:P2_has_type <vocab/art/reliefplastik> .
<work/08015408> crm:P2_has_type <vocab/classification/bauskulptur> .
<work/08015408> crm:P2_has_type <vocab/classification/skulptur> .
<work/08015408> crm:P2_has_type <vocab/object_type/schlussstein> .
<work/08015408> a crm:E22_Human-Made_Object .
|]

  it "test a5220 inheritance" $ do
    let mapping = baseMapping +> M.types ++ (nestedWorks (\_ -> M.types))
    let xml = [w|
<obj lvl="1">
  <a5000>08139334</a5000>
  <a5220>Angewandte Kunst</a5220>
  <a5220>Wandmalerei</a5220>
  <a5220>Portalskulptur</a5220>
  <obj lvl="2">
    <a5001>08196640</a5001>
  </obj>
  <obj lvl="2">
    <a5001>08196641</a5001>
  </obj>
  <obj lvl="2">
    <a5001>08196642</a5001>
    <a5220>Angewandte Kunst</a5220>
  </obj>
</obj>
    |]

    result <- processXMLStringAsSet mapping midasBaseUri xml
    result `shouldBe` [t|
<http://vocab.getty.edu/aat/> a crm:E32_Authority_Document .
<http://vocab.getty.edu/aat/300033644> crm:P71i_is_listed_in <http://vocab.getty.edu/aat/> .
<http://vocab.getty.edu/aat/300033644> a crm:E55_Type .
<http://vocab.getty.edu/aat/300054168> crm:P71i_is_listed_in <http://vocab.getty.edu/aat/> .
<http://vocab.getty.edu/aat/300054168> a crm:E55_Type .
<http://vocab.getty.edu/aat/300054704> crm:P71i_is_listed_in <http://vocab.getty.edu/aat/> .
<http://vocab.getty.edu/aat/300054704> a crm:E55_Type .
<http://vocab.getty.edu/aat/300379328> crm:P71i_is_listed_in <http://vocab.getty.edu/aat/> .
<http://vocab.getty.edu/aat/300379328> a crm:E55_Type .
<https://artresearch.net/resource/pharos/vocab/types> a crm:E32_Authority_Document .
<vocab/classification/angewandte_kunst> crm:P127_has_broader_term <http://vocab.getty.edu/aat/300054168> .
<vocab/classification/angewandte_kunst> crm:P127_has_broader_term <http://vocab.getty.edu/aat/300054704> .
<vocab/classification/angewandte_kunst> crm:P1_is_identified_by <vocab/classification/angewandte_kunst/appellation/preferred_name> .
<vocab/classification/angewandte_kunst> crm:P2_has_type <vocab/meta/classification> .
<vocab/classification/angewandte_kunst> crm:P71i_is_listed_in <https://artresearch.net/resource/pharos/vocab/types> .
<vocab/classification/angewandte_kunst> a crm:E55_Type .
<vocab/classification/angewandte_kunst/appellation/preferred_name> crm:P190_has_symbolic_content "Angewandte Kunst" .
<vocab/classification/angewandte_kunst/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<vocab/classification/angewandte_kunst/appellation/preferred_name> a crm:E41_Appellation .
<vocab/classification/portalskulptur> crm:P127_has_broader_term <http://vocab.getty.edu/aat/300379328> .
<vocab/classification/portalskulptur> crm:P1_is_identified_by <vocab/classification/portalskulptur/appellation/preferred_name> .
<vocab/classification/portalskulptur> crm:P2_has_type <vocab/meta/classification> .
<vocab/classification/portalskulptur> crm:P71i_is_listed_in <https://artresearch.net/resource/pharos/vocab/types> .
<vocab/classification/portalskulptur> a crm:E55_Type .
<vocab/classification/portalskulptur/appellation/preferred_name> crm:P190_has_symbolic_content "Portalskulptur" .
<vocab/classification/portalskulptur/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<vocab/classification/portalskulptur/appellation/preferred_name> a crm:E41_Appellation .
<vocab/classification/wandmalerei> crm:P127_has_broader_term <http://vocab.getty.edu/aat/300033644> .
<vocab/classification/wandmalerei> crm:P1_is_identified_by <vocab/classification/wandmalerei/appellation/preferred_name> .
<vocab/classification/wandmalerei> crm:P2_has_type <vocab/meta/classification> .
<vocab/classification/wandmalerei> crm:P71i_is_listed_in <https://artresearch.net/resource/pharos/vocab/types> .
<vocab/classification/wandmalerei> a crm:E55_Type .
<vocab/classification/wandmalerei/appellation/preferred_name> crm:P190_has_symbolic_content "Wandmalerei" .
<vocab/classification/wandmalerei/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<vocab/classification/wandmalerei/appellation/preferred_name> a crm:E41_Appellation .
<work/08139334> crm:P2_has_type <vocab/classification/angewandte_kunst> .
<work/08139334> crm:P2_has_type <vocab/classification/portalskulptur> .
<work/08139334> crm:P2_has_type <vocab/classification/wandmalerei> .
<work/08139334> crm:P46_is_composed_of <work/08196640> .
<work/08139334> crm:P46_is_composed_of <work/08196641> .
<work/08139334> crm:P46_is_composed_of <work/08196642> .
<work/08139334> a crm:E22_Human-Made_Object .
<work/08196640> crm:P2_has_type <vocab/classification/angewandte_kunst> .
<work/08196640> crm:P2_has_type <vocab/classification/portalskulptur> .
<work/08196640> crm:P2_has_type <vocab/classification/wandmalerei> .
<work/08196640> a crm:E22_Human-Made_Object .
<work/08196641> crm:P2_has_type <vocab/classification/angewandte_kunst> .
<work/08196641> crm:P2_has_type <vocab/classification/portalskulptur> .
<work/08196641> crm:P2_has_type <vocab/classification/wandmalerei> .
<work/08196641> a crm:E22_Human-Made_Object .
<work/08196642> crm:P2_has_type <vocab/classification/angewandte_kunst> .
<work/08196642> a crm:E22_Human-Made_Object .
|]
