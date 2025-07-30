module Pharos.Datasets.Pmc.TypesSpec (spec) where

import CommonImports
import qualified Pmc.Mappings.Types as M
import Pmc.Mappings.Work (baseMapping, pmcBaseUri)
import Test.Hspec

spec :: Spec
spec = describe "Pmc Types mapping" $ do
  it "should be empty" $ do
    let mapping = baseMapping +> M.types
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
