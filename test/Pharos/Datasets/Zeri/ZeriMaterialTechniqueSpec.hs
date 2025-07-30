module Pharos.Datasets.Zeri.ZeriMaterialTechniqueSpec (spec) where

import DSL
import Engine (processXMLStringAsSet)
import Mappings.Mappings.Work.WorkMappings
import Mappings.Mappings.Work.MaterialTechnique
import Test.Hspec
import Util.QQ (w, t)

-- Test with MTC element
zeriMTCXml =
  [w|<?xml version="1.0" encoding="UTF-8"?>
<RISULTATI>
  <SCHEDA sercdoa="82365">
      <PARAGRAFO etichetta="OBJECT">
          <MTC etichetta="Medium or materials">marmo bianco, marmo giallo, marmo bardiglio, bronzo dorato</MTC>
      </PARAGRAFO>
  </SCHEDA>
</RISULTATI>|]


spec :: Spec
spec = describe "Zeri material and technique mappings" $ do
      
  describe "material mappings" $ do
    it "correctly maps materials from MTC element" $ do
      let mapping = baseMapping +> [materialTechniqueLinks]
      result <- processXMLStringAsSet mapping zeriBaseUri zeriMTCXml
      result
        `shouldBe`
          [t|
            <work/82365> a crm:E22_Human-Made_Object .
            <work/82365> crm:P45_consists_of <vocab/marmo_bianco> .
            <vocab/marmo_bianco> a crm:E57_Material .
            <vocab/marmo_bianco> crm:P2_has_type <vocab/meta/mtc> .
            <vocab/marmo_bianco> crm:P1_is_identified_by <vocab/marmo_bianco/name> .
            <vocab/marmo_bianco/name> a crm:E41_Appellation .
            <vocab/marmo_bianco/name> crm:P190_has_symbolic_content "marmo bianco" .
            <work/82365> crm:P45_consists_of <vocab/marmo_giallo> .
            <vocab/marmo_giallo> a crm:E57_Material .
            <vocab/marmo_giallo> crm:P2_has_type <vocab/meta/mtc> .
            <vocab/marmo_giallo> crm:P1_is_identified_by <vocab/marmo_giallo/name> .
            <vocab/marmo_giallo/name> a crm:E41_Appellation .
            <vocab/marmo_giallo/name> crm:P190_has_symbolic_content "marmo giallo" .
            <work/82365> crm:P45_consists_of <vocab/marmo_bardiglio> .
            <vocab/marmo_bardiglio> a crm:E57_Material .
            <vocab/marmo_bardiglio> crm:P2_has_type <vocab/meta/mtc> .
            <vocab/marmo_bardiglio> crm:P1_is_identified_by <vocab/marmo_bardiglio/name> .
            <vocab/marmo_bardiglio/name> a crm:E41_Appellation .
            <vocab/marmo_bardiglio/name> crm:P190_has_symbolic_content "marmo bardiglio" .
            <work/82365> crm:P45_consists_of <vocab/bronzo_dorato> .
            <vocab/bronzo_dorato> a crm:E57_Material .
            <vocab/bronzo_dorato> crm:P2_has_type <vocab/meta/mtc> .
            <vocab/bronzo_dorato> crm:P1_is_identified_by <vocab/bronzo_dorato/name> .
            <vocab/bronzo_dorato/name> a crm:E41_Appellation .
            <vocab/bronzo_dorato/name> crm:P190_has_symbolic_content "bronzo dorato" .                 
          |]
