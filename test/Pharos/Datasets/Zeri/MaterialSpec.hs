module Pharos.Datasets.Zeri.MaterialSpec (spec) where

import CommonImports
import qualified Mappings.Mappings.Work.Material as M
import Mappings.Mappings.Work.WorkMappings (baseMapping, zeriBaseUri)
import Test.Hspec

spec :: Spec
spec = describe "Zeri Material mapping" $ do
  it "should be empty" $ do
    let mapping = baseMapping +> M.materials
    let xml = [w|
      <record>
        <resource>1</resource>
      </record>
    |]

    result <- processXMLStringAsSet mapping zeriBaseUri xml
    result
      `shouldBe`
        [t|
        |]
