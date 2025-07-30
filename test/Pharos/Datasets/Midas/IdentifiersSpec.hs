module Pharos.Datasets.Midas.IdentifiersSpec (spec) where

import CommonImports
import Midas.Mappings.Work
import Midas.Mappings.Identifiers
import Test.Hspec

spec :: Spec
spec = describe "identifiers" $ do
  let
    mapping datasetName = baseMapping +> identifiers datasetName [x|a5000|] ++ nestedWorks (identifiers datasetName)
    hertzianaMapping = mapping "hertziana"
    khiMapping = mapping "khi"
    marburgMapping = mapping "marburg"

  it "hertziana, top-level and nested objects with it's own top level ID" $ do
    let xml = [w|
      <obj>
        <a5000>08041820</a5000>
        <obj lvl="2">
          <a5001>08104140</a5001>
          <obj lvl="3">
            <a5002>08104141</a5002>
          </obj>
        </obj>
      </obj>
    |]

    result <- processXMLStringAsSet hertzianaMapping midasBaseUri xml
    result `shouldBe` [t|
<work/08041820> crm:P1_is_identified_by <work/08041820/id/catalog_url> .
<work/08041820> crm:P1_is_identified_by <work/08041820/id/preferred_identifier> .
<work/08041820> crm:P46_is_composed_of <work/08104140> .
<work/08041820> a crm:E22_Human-Made_Object .
<work/08041820/id/catalog_url> crm:P190_has_symbolic_content "https://foto.biblhertz.it/document/obj/08041820" .
<work/08041820/id/catalog_url> crm:P2_has_type pharos-meta:catalog_url .
<work/08041820/id/catalog_url> a crm:E42_Identifier .
<work/08041820/id/preferred_identifier> crm:P190_has_symbolic_content "08041820" .
<work/08041820/id/preferred_identifier> crm:P2_has_type pharos-meta:preferred_identifier .
<work/08041820/id/preferred_identifier> a crm:E42_Identifier .
<work/08104140> crm:P1_is_identified_by <work/08104140/id/catalog_url> .
<work/08104140> crm:P1_is_identified_by <work/08104140/id/preferred_identifier> .
<work/08104140> crm:P46_is_composed_of <work/08104141> .
<work/08104140> a crm:E22_Human-Made_Object .
<work/08104140/id/catalog_url> crm:P190_has_symbolic_content "https://foto.biblhertz.it/document/obj/08104140" .
<work/08104140/id/catalog_url> crm:P2_has_type pharos-meta:catalog_url .
<work/08104140/id/catalog_url> a crm:E42_Identifier .
<work/08104140/id/preferred_identifier> crm:P190_has_symbolic_content "08104140" .
<work/08104140/id/preferred_identifier> crm:P2_has_type pharos-meta:preferred_identifier .
<work/08104140/id/preferred_identifier> a crm:E42_Identifier .
<work/08104141> crm:P1_is_identified_by <work/08104141/id/catalog_url> .
<work/08104141> crm:P1_is_identified_by <work/08104141/id/preferred_identifier> .
<work/08104141> a crm:E22_Human-Made_Object .
<work/08104141/id/catalog_url> crm:P190_has_symbolic_content "https://foto.biblhertz.it/document/obj/08104141" .
<work/08104141/id/catalog_url> crm:P2_has_type pharos-meta:catalog_url .
<work/08104141/id/catalog_url> a crm:E42_Identifier .
<work/08104141/id/preferred_identifier> crm:P190_has_symbolic_content "08104141" .
<work/08104141/id/preferred_identifier> crm:P2_has_type pharos-meta:preferred_identifier .
<work/08104141/id/preferred_identifier> a crm:E42_Identifier .
    |]

  it "khi, top-level and nested objects with IDs without comma" $ do
    -- KHI system doesn't support links to nested records,
    -- so all nested objects should have the same catalog URL
    let xml = [w|
      <?xml version="1.0" encoding="UTF-8"?>
      <obj>
        <a5000>70011006</a5000>
        <obj lvl="2">
          <a5001>70011007</a5001>
        </obj>
        <obj lvl="2">
          <a5001>70012177</a5001>
        </obj>      
      </obj>
    |]

    result <- processXMLStringAsSet khiMapping midasBaseUri xml
    result `shouldBe` [t|
<work/70011006> crm:P1_is_identified_by <work/70011006/id/catalog_url> .
<work/70011006> crm:P1_is_identified_by <work/70011006/id/preferred_identifier> .
<work/70011006> crm:P46_is_composed_of <work/70011007> .
<work/70011006> crm:P46_is_composed_of <work/70012177> .
<work/70011006> a crm:E22_Human-Made_Object .
<work/70011006/id/catalog_url> crm:P190_has_symbolic_content "http://photothek.khi.fi.it/documents/obj/70011006" .
<work/70011006/id/catalog_url> crm:P2_has_type pharos-meta:catalog_url .
<work/70011006/id/catalog_url> a crm:E42_Identifier .
<work/70011006/id/preferred_identifier> crm:P190_has_symbolic_content "70011006" .
<work/70011006/id/preferred_identifier> crm:P2_has_type pharos-meta:preferred_identifier .
<work/70011006/id/preferred_identifier> a crm:E42_Identifier .
<work/70011007> crm:P1_is_identified_by <work/70011007/id/catalog_url> .
<work/70011007> crm:P1_is_identified_by <work/70011007/id/preferred_identifier> .
<work/70011007> a crm:E22_Human-Made_Object .
<work/70011007/id/catalog_url> crm:P190_has_symbolic_content "http://photothek.khi.fi.it/documents/obj/70011006" .
<work/70011007/id/catalog_url> crm:P2_has_type pharos-meta:catalog_url .
<work/70011007/id/catalog_url> a crm:E42_Identifier .
<work/70011007/id/preferred_identifier> crm:P190_has_symbolic_content "70011007" .
<work/70011007/id/preferred_identifier> crm:P2_has_type pharos-meta:preferred_identifier .
<work/70011007/id/preferred_identifier> a crm:E42_Identifier .
<work/70012177> crm:P1_is_identified_by <work/70012177/id/catalog_url> .
<work/70012177> crm:P1_is_identified_by <work/70012177/id/preferred_identifier> .
<work/70012177> a crm:E22_Human-Made_Object .
<work/70012177/id/catalog_url> crm:P190_has_symbolic_content "http://photothek.khi.fi.it/documents/obj/70011006" .
<work/70012177/id/catalog_url> crm:P2_has_type pharos-meta:catalog_url .
<work/70012177/id/catalog_url> a crm:E42_Identifier .
<work/70012177/id/preferred_identifier> crm:P190_has_symbolic_content "70012177" .
<work/70012177/id/preferred_identifier> crm:P2_has_type pharos-meta:preferred_identifier .
<work/70012177/id/preferred_identifier> a crm:E42_Identifier .
    |]

  it "marburg, catalog record url" $ do
    let xml = [w|
      <?xml version="1.0" encoding="UTF-8"?>
      <obj>
        <a5000>20133589</a5000>
        <id>http://id.bildindex.de/thing/0001270852</id>
      </obj>
    |]

    result <- processXMLStringAsSet marburgMapping midasBaseUri xml
    result `shouldBe` [t|
<work/20133589> crm:P1_is_identified_by <work/20133589/id/catalog_url> .
<work/20133589> crm:P1_is_identified_by <work/20133589/id/preferred_identifier> .
<work/20133589> a crm:E22_Human-Made_Object .
<work/20133589/id/catalog_url> crm:P190_has_symbolic_content "http://id.bildindex.de/thing/0001270852" .
<work/20133589/id/catalog_url> crm:P2_has_type pharos-meta:catalog_url .
<work/20133589/id/catalog_url> a crm:E42_Identifier .
<work/20133589/id/preferred_identifier> crm:P190_has_symbolic_content "20133589" .
<work/20133589/id/preferred_identifier> crm:P2_has_type pharos-meta:preferred_identifier .
<work/20133589/id/preferred_identifier> a crm:E42_Identifier .
    |]
