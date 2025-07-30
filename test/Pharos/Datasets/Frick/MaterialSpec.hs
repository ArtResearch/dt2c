module Pharos.Datasets.Frick.MaterialSpec (spec) where

import CommonImports
import qualified Pharos.Datasets.Frick.Mappings.Material as M
import Pharos.Datasets.Frick.Mappings.Work (baseMapping, frickBaseUri)
import Test.Hspec

spec :: Spec
spec = describe "Frick Material mapping" $ do
  it "should be empty" $ do
    let mapping = baseMapping +> M.materials
    let xml = [w|
      <record>
        <resource>1</resource>
      </record>
    |]

    result <- processXMLStringAsSet mapping frickBaseUri xml
    result `shouldBe` [t|
 <work/> a crm:E22_Human-Made_Object .
    |]
