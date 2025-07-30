module Pharos.Datasets.Midas.MaterialSpec (spec) where

import CommonImports
import qualified Midas.Mappings.Material as M
import Midas.Mappings.Work (baseMapping, midasBaseUri)
import Test.Hspec

spec :: Spec
spec = describe "Midas Material mapping" $ do
  it "should be empty" $ do
    let mapping = baseMapping +> M.materials
    let xml = [w|
      <record>
        <resource>1</resource>
      </record>
    |]

    result <- processXMLStringAsSet mapping midasBaseUri xml
    result
      `shouldBe`
        [t|
        |]
