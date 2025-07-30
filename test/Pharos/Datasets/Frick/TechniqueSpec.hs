module Pharos.Datasets.Frick.TechniqueSpec (spec) where

import CommonImports
import qualified Pharos.Datasets.Frick.Mappings.Technique as M
import Pharos.Datasets.Frick.Mappings.Work (baseMapping, frickBaseUri)
import Test.Hspec

spec :: Spec
spec = describe "Frick Technique mapping" $ do
  it "should be empty" $ do
    let mapping = baseMapping +> M.technique
    let xml = [w|
      <record>
        <controlfield tag="001">1</controlfield>
      </record>
    |]

    result <- processXMLStringAsSet mapping frickBaseUri xml
    result
      `shouldBe`
        [t|
          <work/1> a crm:E22_Human-Made_Object .
        |]
