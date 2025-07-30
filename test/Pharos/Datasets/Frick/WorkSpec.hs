module Pharos.Datasets.Frick.WorkSpec (spec) where

import CommonImports

import Pharos.Datasets.Frick.Mappings.Work
import Test.Hspec

-- Test data (removed title-specific XML)

spec :: Spec
spec = describe "Frick mappings" $ do
  describe "base type links" $ do
    it "correctly adds base type links" $ do
      let mapping = baseMapping +> baseTypeLinks
      let xml = [w|<?xml version="1.0" encoding="UTF-8"?>
<record>
    <controlfield tag="001">991007202809707141</controlfield>
</record>|]

      result <- processXMLStringAsSet mapping frickBaseUri xml
      result
        `shouldBe`
          [t|
<work/991007202809707141> a crm:E22_Human-Made_Object .
<work/991007202809707141> crm:P2_has_type aat:300133025 .
<work/991007202809707141> crm:P2_has_type <https://artresearch.net/resource/e31/frick> .
<https://artresearch.net/resource/e31/frick> a crm:E55_Type .
|]

  -- Removed "titles" describe block
