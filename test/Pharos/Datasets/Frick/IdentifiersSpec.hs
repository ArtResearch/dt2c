module Pharos.Datasets.Frick.IdentifiersSpec (spec) where

import CommonImports
import Pharos.Datasets.Frick.Mappings.Work
import Pharos.Datasets.Frick.Mappings.Identifiers
import Test.Hspec

spec :: Spec
spec = describe "Frick mappings" $ do
  describe "identifiers" $ do
    it "correctly maps ALMA ID and catalog URL" $ do
      let mapping = baseMapping +> identifiers
      let xml = [w|
        <?xml version="1.0" encoding="UTF-8"?>
        <record>
            <controlfield tag="001">991007202809707141</controlfield>
        </record>
      |]

      result <- processXMLStringAsSet mapping frickBaseUri xml
      result
        `shouldBe` [t|
          <work/991007202809707141> a crm:E22_Human-Made_Object .
          <work/991007202809707141> crm:P1_is_identified_by <work/991007202809707141/id/alma_id> .
          <work/991007202809707141/id/alma_id> a crm:E42_Identifier .
          <work/991007202809707141/id/alma_id> crm:P2_has_type <vocab/meta/alma_id> .
          <work/991007202809707141/id/alma_id> crm:P190_has_symbolic_content "991007202809707141" .
          <work/991007202809707141> crm:P1_is_identified_by <work/991007202809707141/id/catalog_url> .
          <work/991007202809707141/id/catalog_url> a crm:E42_Identifier .
          <work/991007202809707141/id/catalog_url> crm:P2_has_type pharos-meta:catalog_url .
          <work/991007202809707141/id/catalog_url> crm:P190_has_symbolic_content "https://library.frick.org/discovery/fulldisplay?context=L&vid=01NYA_INST:Frick&search_scope=Frick&tab=SearchScopes&docid=alma991007202809707141" .
        |]
