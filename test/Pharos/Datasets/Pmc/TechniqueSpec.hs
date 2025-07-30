module Pharos.Datasets.Pmc.TechniqueSpec (spec) where

import CommonImports
import qualified Pmc.Mappings.Technique as M
import Pmc.Mappings.Work (baseMapping, pmcBaseUri)
import Test.Hspec

spec :: Spec
spec = describe "Pmc Technique mapping" $ do
  it "should be empty" $ do
    let mapping = baseMapping +> M.technique
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
