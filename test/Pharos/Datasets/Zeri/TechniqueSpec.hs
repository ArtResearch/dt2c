module Pharos.Datasets.Zeri.TechniqueSpec (spec) where

import CommonImports
import qualified Mappings.Mappings.Work.Technique as M
import Mappings.Mappings.Work.WorkMappings (baseMapping, zeriBaseUri)
import Test.Hspec

spec :: Spec
spec = describe "Zeri Technique mapping" $ do
  it "should be empty" $ do
    let mapping = baseMapping +> M.technique
    let xml = [w|
<RISULTATI>
  <SCHEDA sercdoa="1">
  </SCHEDA>
</RISULTATI>
    |]

    result <- processXMLStringAsSet mapping zeriBaseUri xml
    result
      `shouldBe`
        [t|
          <work/1> a crm:E22_Human-Made_Object .
        |]
