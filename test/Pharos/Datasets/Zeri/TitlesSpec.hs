module Pharos.Datasets.Zeri.TitlesSpec (spec) where

import Test.Hspec
import CommonImports
import Mappings.Mappings.Work.Titles (titles)
import Mappings.Mappings.Work.WorkMappings (baseMapping, zeriBaseUri)

spec :: Spec
spec = describe "Zeri Titles mapping" $ do
  it "traditional title" $ do
    let mapping = baseMapping +> [titles]
    let zeriXml = [w|
      <RISULTATI>
        <SCHEDA sercdoa="28993">
          <PARAGRAFO etichetta="OBJECT">
               <SGTT etichetta="Traditional title">Madonna con Bambino tra san Pietro e santa Margherita</SGTT>
               <SGTI etichetta="Subject">Madonna con Bambino tra san Pietro e santa Margherita (?)</SGTI>
          </PARAGRAFO>
        </SCHEDA>
      </RISULTATI>
    |]

    result <- processXMLStringAsSet mapping zeriBaseUri zeriXml

    result
      `shouldBe`
      [t|
        <work/28993> a crm:E22_Human-Made_Object .
        <work/28993> crm:P1_is_identified_by <work/28993/appellation/traditional_title> .
        <work/28993/appellation/traditional_title> a crm:E41_Appellation .
        <work/28993/appellation/traditional_title> crm:P2_has_type <vocab/meta/traditional_title> .
        <work/28993/appellation/traditional_title> crm:P190_has_symbolic_content "Madonna con Bambino tra san Pietro e santa Margherita" .
      |]

  it "extracts subject title from SGTI when SGTT is absent" $ do
    let mapping = baseMapping +> [titles]
    let zeriXml = [w|
      <RISULTATI>
        <SCHEDA sercdoa="28993">
          <PARAGRAFO etichetta="OBJECT">
               <!-- SGTT is absent -->
               <SGTI etichetta="Subject">Madonna con Bambino tra san Pietro e santa Margherita (?)</SGTI>
          </PARAGRAFO>
        </SCHEDA>
      </RISULTATI>
    |]

    result <- processXMLStringAsSet mapping zeriBaseUri zeriXml

    result
      `shouldBe`
      [t|
        <work/28993> crm:P1_is_identified_by <work/28993/appellation/pharos_preferred_name> .
        <work/28993> a crm:E22_Human-Made_Object .
        <work/28993/appellation/pharos_preferred_name> crm:P190_has_symbolic_content "Madonna con Bambino tra san Pietro e santa Margherita (?)" .
        <work/28993/appellation/pharos_preferred_name> crm:P2_has_type pharos-meta:pharos_preferred_name .
        <work/28993/appellation/pharos_preferred_name> a crm:E41_Appellation .
      |]

  it "SGTI as title when there is more than one comma separated value" $ do
    let mapping = baseMapping +> [titles]
    let zeriXml = [w|
      <RISULTATI>
        <SCHEDA  sercdoa="87641">
          <PARAGRAFO etichetta="OBJECT">
            <!-- SGTT is absent -->
            <SGTI etichetta="Subject">Madonna con Bambino,San Cosma,San Damiano</SGTI>
          </PARAGRAFO>
        </SCHEDA>
      </RISULTATI>
    |]

    result <- processXMLStringAsSet mapping zeriBaseUri zeriXml
    result `shouldBe` [t|
<work/87641> crm:P1_is_identified_by <work/87641/appellation/pharos_preferred_name> .
<work/87641> a crm:E22_Human-Made_Object .
<work/87641/appellation/pharos_preferred_name> crm:P190_has_symbolic_content "Madonna con Bambino, San Cosma, San Damiano" .
<work/87641/appellation/pharos_preferred_name> crm:P2_has_type pharos-meta:pharos_preferred_name .
<work/87641/appellation/pharos_preferred_name> a crm:E41_Appellation .
    |]
