module Pharos.Datasets.Midas.TechniqueSpec (spec) where

import CommonImports
import qualified Midas.Mappings.Technique as M
import Midas.Mappings.Work (baseMapping, midasBaseUri)
import Test.Hspec

spec :: Spec
spec = describe "Midas Technique mapping" $ do
  it "should be empty" $ do
    let mapping = baseMapping +> M.technique
    let xml = [w|
      <obj>
        <a5000>1</a5000>
      </obj>
    |]

    result <- processXMLStringAsSet mapping midasBaseUri xml
    result
      `shouldBe`
        [t|
          <work/1> crm:P108i_was_produced_by <work/1/production> .
          <work/1> a crm:E22_Human-Made_Object .
          <work/1/production> a crm:E12_Production .
        |]
