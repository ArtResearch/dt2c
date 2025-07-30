module Pharos.Datasets.Pmc.MaterialSpec (spec) where

import CommonImports
import qualified Pmc.Mappings.Material as M
import Pmc.Mappings.Work (baseMapping, pmcBaseUri)
import Test.Hspec

spec :: Spec
spec = describe "Pmc Material mapping" $ do
  it "should be empty" $ do
    let mapping = baseMapping +> M.materials
    let xml = [w|
      <record>
        <resource>1</resource>
      </record>
    |]

    result <- processXMLStringAsSet mapping pmcBaseUri xml
    result
      `shouldBe`
        [t|
        |]
