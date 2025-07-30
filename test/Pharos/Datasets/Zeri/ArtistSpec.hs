module Pharos.Datasets.Zeri.ArtistSpec (spec) where

import DSL
import Util.QQ
import Engine (processXMLStringAsSet)
import Mappings.Mappings.Work.WorkMappings
import Mappings.Mappings.Work.Production
import Test.Hspec

spec :: Spec
spec = describe "Zeri artist mappings" $ do
      
  describe "regular author mappings" $ do
    it "correctly maps named author" $ do
      let mapping = baseMapping +> [productionLinks]
      let zeriAuthorXml = [w|
        <RISULTATI>
          <SCHEDA sercdoa="82365">
              <PARAGRAFO etichetta="AUTHOR">
                  <RIPETIZIONE>
                      <AUTN>Leonardo da Vinci</AUTN>
                  </RIPETIZIONE>
              </PARAGRAFO>
              <PARAGRAFO etichetta="DATING">
                <DTZG etichetta="Century">sec. XVIII</DTZG>
                <DTZS etichetta="Part of century">prima metà</DTZS>
                <DTSI etichetta="From">1722</DTSI>
                <DTSF etichetta="To">1725</DTSF>
              </PARAGRAFO>
          </SCHEDA>
        </RISULTATI>
      |]
      result <- processXMLStringAsSet mapping zeriBaseUri zeriAuthorXml      
      result `shouldBe` 
        [t|
<actor/leonardo_da_vinci> a crm:E39_Actor .
<actor/leonardo_da_vinci> crm:P1_is_identified_by <actor/leonardo_da_vinci/name> .
<actor/leonardo_da_vinci> custom:sameAs <http://vocab.getty.edu/ulan/500010879> .
<actor/leonardo_da_vinci/name> a crm:E41_Appellation .
<actor/leonardo_da_vinci/name> crm:P190_has_symbolic_content "Leonardo da Vinci" .
<actor/leonardo_da_vinci/name> crm:P2_has_type pharos-meta:preferred_name .
<http://vocab.getty.edu/ulan/500010879> a crm:E39_Actor .
<work/82365> a crm:E22_Human-Made_Object .
<work/82365> crm:P108i_was_produced_by <work/82365/production> .
<work/82365/production> a crm:E12_Production .
<work/82365/production> crm:P14_carried_out_by <actor/leonardo_da_vinci> .
<work/82365/production> crm:P4_has_time-span <work/82365/production/date> .
<work/82365/production/date> a crm:E52_Time-Span .
<work/82365/production/date> crm:P1_is_identified_by <work/82365/production/date/appellation/preferred_name> .
<work/82365/production/date> crm:P82a_begin_of_the_begin "1722-01-01T00:00:00Z"^^xsd:dateTime .
<work/82365/production/date> crm:P82b_end_of_the_end "1725-12-31T23:59:59Z"^^xsd:dateTime .
<work/82365/production/date/appellation/preferred_name> a crm:E41_Appellation .
<work/82365/production/date/appellation/preferred_name> crm:P190_has_symbolic_content "1722 - 1725" .
<work/82365/production/date/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
        |]

  describe "anonymous author mappings" $ do
    it "correctly maps anonymous author with regional/local school and century" $ do
      let mapping = baseMapping +> [productionLinks]
      let zeriAnonymousRegionalXml = [w|
        <RISULTATI>
          <SCHEDA sercdoa="82366">
              <PARAGRAFO etichetta="AUTHOR">
                  <RIPETIZIONE>
                      <AUTN>Anonimo toscano sec. XIV</AUTN>
                  </RIPETIZIONE>
              </PARAGRAFO>
          </SCHEDA>
        </RISULTATI>
      |]

      result <- processXMLStringAsSet mapping zeriBaseUri zeriAnonymousRegionalXml      
      result
        `shouldBe` 
        [t|
          <work/82366> a crm:E22_Human-Made_Object .
          <work/82366> crm:P108i_was_produced_by <work/82366/production> .
          <work/82366/production> a crm:E12_Production .
          <work/82366/production> crm:P14_carried_out_by <work/82366/production/actor/anonymous/1> .
          <work/82366/production/actor/anonymous/1> a crm:E39_Actor .
          <work/82366/production/actor/anonymous/1> crm:P2_has_type pharos-meta:anonymous_author .
          <work/82366/production/actor/anonymous/1> crm:P107i_is_current_or_former_member_of <group/anonymous/anonimo_toscano_sec_xiv> .
          <group/anonymous/anonimo_toscano_sec_xiv> a crm:E74_Group .
          <group/anonymous/anonimo_toscano_sec_xiv> crm:P1_is_identified_by <group/anonymous/anonimo_toscano_sec_xiv/name> .
          <group/anonymous/anonimo_toscano_sec_xiv/name> a crm:E41_Appellation .
          <group/anonymous/anonimo_toscano_sec_xiv/name> crm:P2_has_type pharos-meta:preferred_name .
          <group/anonymous/anonimo_toscano_sec_xiv/name> crm:P190_has_symbolic_content "Anonimo toscano sec. XIV" .
          <group/anonymous/anonimo_toscano_sec_xiv> crm:P2_has_type pharos-meta:anonymous_author_group .
          <work/82366/production/actor/anonymous/1> crm:P1_is_identified_by <work/82366/production/actor/anonymous/1/name> .
          <work/82366/production/actor/anonymous/1/name> a crm:E41_Appellation .
          <work/82366/production/actor/anonymous/1/name> crm:P2_has_type pharos-meta:preferred_name .
          <work/82366/production/actor/anonymous/1/name> crm:P190_has_symbolic_content "Anonimo toscano sec. XIV" .
        |]

    it "correctly maps simple anonymous author (no references)" $ do
      let mapping = baseMapping +> [productionLinks]
      let zeriAnonymousSimpleXml = [w|
        <RISULTATI>
          <SCHEDA sercdoa="82367">
              <PARAGRAFO etichetta="AUTHOR">
                  <RIPETIZIONE>
                      <AUTN>Anonimo</AUTN>
                  </RIPETIZIONE>
              </PARAGRAFO>
          </SCHEDA>
        </RISULTATI>
      |]
      result <- processXMLStringAsSet mapping zeriBaseUri zeriAnonymousSimpleXml      
      result
        `shouldBe` 
        [t|
          <work/82367> a crm:E22_Human-Made_Object .
          <work/82367> crm:P108i_was_produced_by <work/82367/production> .
          <work/82367/production> a crm:E12_Production .
          <work/82367/production> crm:P14_carried_out_by <work/82367/production/actor/anonymous/1> .
          <work/82367/production/actor/anonymous/1> a crm:E39_Actor .
          <work/82367/production/actor/anonymous/1> crm:P2_has_type pharos-meta:anonymous_author .
          <work/82367/production/actor/anonymous/1> crm:P107i_is_current_or_former_member_of <group/anonymous/anonimo> .
          <group/anonymous/anonimo> a crm:E74_Group .
          <group/anonymous/anonimo> crm:P1_is_identified_by <group/anonymous/anonimo/name> .
          <group/anonymous/anonimo/name> a crm:E41_Appellation .
          <group/anonymous/anonimo/name> crm:P2_has_type pharos-meta:preferred_name .
          <group/anonymous/anonimo/name> crm:P190_has_symbolic_content "Anonimo" .
          <group/anonymous/anonimo> crm:P2_has_type pharos-meta:anonymous_author_group .
          <work/82367/production/actor/anonymous/1> crm:P1_is_identified_by <work/82367/production/actor/anonymous/1/name> .
          <work/82367/production/actor/anonymous/1/name> a crm:E41_Appellation .
          <work/82367/production/actor/anonymous/1/name> crm:P2_has_type pharos-meta:preferred_name .
          <work/82367/production/actor/anonymous/1/name> crm:P190_has_symbolic_content "Anonimo" .
        |]

    it "correctly maps anonymous author with century only" $ do
      let mapping = baseMapping +> [productionLinks]
      let zeriAnonymousCenturyXml = [w|
        <RISULTATI>
          <SCHEDA sercdoa="82368">
              <PARAGRAFO etichetta="AUTHOR">
                  <RIPETIZIONE>
                      <AUTN>Anonimo sec. XIX</AUTN>
                  </RIPETIZIONE>
              </PARAGRAFO>
          </SCHEDA>
        </RISULTATI>
      |]
      result <- processXMLStringAsSet mapping zeriBaseUri zeriAnonymousCenturyXml      
      result
        `shouldBe` 
        [t|
          <work/82368> a crm:E22_Human-Made_Object .
          <work/82368> crm:P108i_was_produced_by <work/82368/production> .
          <work/82368/production> a crm:E12_Production .
          <work/82368/production> crm:P14_carried_out_by <work/82368/production/actor/anonymous/1> .
          <work/82368/production/actor/anonymous/1> a crm:E39_Actor .
          <work/82368/production/actor/anonymous/1> crm:P2_has_type pharos-meta:anonymous_author .
          <work/82368/production/actor/anonymous/1> crm:P107i_is_current_or_former_member_of <group/anonymous/anonimo_sec_xix> .
          <group/anonymous/anonimo_sec_xix> a crm:E74_Group .
          <group/anonymous/anonimo_sec_xix> crm:P1_is_identified_by <group/anonymous/anonimo_sec_xix/name> .
          <group/anonymous/anonimo_sec_xix/name> a crm:E41_Appellation .
          <group/anonymous/anonimo_sec_xix/name> crm:P2_has_type pharos-meta:preferred_name .
          <group/anonymous/anonimo_sec_xix/name> crm:P190_has_symbolic_content "Anonimo sec. XIX" .
          <group/anonymous/anonimo_sec_xix> crm:P2_has_type pharos-meta:anonymous_author_group .
          <work/82368/production/actor/anonymous/1> crm:P1_is_identified_by <work/82368/production/actor/anonymous/1/name> .
          <work/82368/production/actor/anonymous/1/name> a crm:E41_Appellation .
          <work/82368/production/actor/anonymous/1/name> crm:P2_has_type pharos-meta:preferred_name .
          <work/82368/production/actor/anonymous/1/name> crm:P190_has_symbolic_content "Anonimo sec. XIX" .
        |]

    it "correctly maps anonymous author with style reference" $ do
      let mapping = baseMapping +> [productionLinks]
      let zeriAnonymousStyleXml = [w|
        <RISULTATI>
          <SCHEDA sercdoa="82369">
              <PARAGRAFO etichetta="AUTHOR">
                  <RIPETIZIONE>
                      <AUTN>Anonimo bambocciante sec. XVII</AUTN>
                  </RIPETIZIONE>
              </PARAGRAFO>
          </SCHEDA>
        </RISULTATI>
      |]
      result <- processXMLStringAsSet mapping zeriBaseUri zeriAnonymousStyleXml      
      result
        `shouldBe` 
        [t|
          <work/82369> a crm:E22_Human-Made_Object .
          <work/82369> crm:P108i_was_produced_by <work/82369/production> .
          <work/82369/production> a crm:E12_Production .
          <work/82369/production> crm:P14_carried_out_by <work/82369/production/actor/anonymous/1> .
          <work/82369/production/actor/anonymous/1> a crm:E39_Actor .
          <work/82369/production/actor/anonymous/1> crm:P2_has_type pharos-meta:anonymous_author .
          <work/82369/production/actor/anonymous/1> crm:P107i_is_current_or_former_member_of <group/anonymous/anonimo_bambocciante_sec_xvii> .
          <group/anonymous/anonimo_bambocciante_sec_xvii> a crm:E74_Group .
          <group/anonymous/anonimo_bambocciante_sec_xvii> crm:P1_is_identified_by <group/anonymous/anonimo_bambocciante_sec_xvii/name> .
          <group/anonymous/anonimo_bambocciante_sec_xvii/name> a crm:E41_Appellation .
          <group/anonymous/anonimo_bambocciante_sec_xvii/name> crm:P2_has_type pharos-meta:preferred_name .
          <group/anonymous/anonimo_bambocciante_sec_xvii/name> crm:P190_has_symbolic_content "Anonimo bambocciante sec. XVII" .
          <group/anonymous/anonimo_bambocciante_sec_xvii> crm:P2_has_type pharos-meta:anonymous_author_group .
          <work/82369/production/actor/anonymous/1> crm:P1_is_identified_by <work/82369/production/actor/anonymous/1/name> .
          <work/82369/production/actor/anonymous/1/name> a crm:E41_Appellation .
          <work/82369/production/actor/anonymous/1/name> crm:P2_has_type pharos-meta:preferred_name .
          <work/82369/production/actor/anonymous/1/name> crm:P190_has_symbolic_content "Anonimo bambocciante sec. XVII" .
        |]

    it "correctly maps anonymous author with place reference" $ do
      let mapping = baseMapping +> [productionLinks]
      let zeriAnonymousPlaceXml = [w|
        <RISULTATI>
          <SCHEDA sercdoa="82370">
              <PARAGRAFO etichetta="AUTHOR">
                  <RIPETIZIONE>
                      <AUTN>Anonimo attivo in Umbria sec. XIII</AUTN>
                  </RIPETIZIONE>
              </PARAGRAFO>
          </SCHEDA>
        </RISULTATI>
      |]
      result <- processXMLStringAsSet mapping zeriBaseUri zeriAnonymousPlaceXml      
      result
        `shouldBe` 
        [t|
          <work/82370> a crm:E22_Human-Made_Object .
          <work/82370> crm:P108i_was_produced_by <work/82370/production> .
          <work/82370/production> a crm:E12_Production .
          <work/82370/production> crm:P14_carried_out_by <work/82370/production/actor/anonymous/1> .
          <work/82370/production/actor/anonymous/1> a crm:E39_Actor .
          <work/82370/production/actor/anonymous/1> crm:P2_has_type pharos-meta:anonymous_author .
          <work/82370/production/actor/anonymous/1> crm:P107i_is_current_or_former_member_of <group/anonymous/anonimo_attivo_in_umbria_sec_xiii> .
          <group/anonymous/anonimo_attivo_in_umbria_sec_xiii> a crm:E74_Group .
          <group/anonymous/anonimo_attivo_in_umbria_sec_xiii> crm:P1_is_identified_by <group/anonymous/anonimo_attivo_in_umbria_sec_xiii/name> .
          <group/anonymous/anonimo_attivo_in_umbria_sec_xiii/name> a crm:E41_Appellation .
          <group/anonymous/anonimo_attivo_in_umbria_sec_xiii/name> crm:P2_has_type pharos-meta:preferred_name .
          <group/anonymous/anonimo_attivo_in_umbria_sec_xiii/name> crm:P190_has_symbolic_content "Anonimo attivo in Umbria sec. XIII" .
          <group/anonymous/anonimo_attivo_in_umbria_sec_xiii> crm:P2_has_type pharos-meta:anonymous_author_group .
          <work/82370/production/actor/anonymous/1> crm:P1_is_identified_by <work/82370/production/actor/anonymous/1/name> .
          <work/82370/production/actor/anonymous/1/name> a crm:E41_Appellation .
          <work/82370/production/actor/anonymous/1/name> crm:P2_has_type pharos-meta:preferred_name .
          <work/82370/production/actor/anonymous/1/name> crm:P190_has_symbolic_content "Anonimo attivo in Umbria sec. XIII" .
        |]

    it "correctly maps anonymous author with specific year" $ do
      let mapping = baseMapping +> [productionLinks]
      let zeriAnonymousYearXml = [w|
        <RISULTATI>
          <SCHEDA sercdoa="82371">
              <PARAGRAFO etichetta="AUTHOR">
                  <RIPETIZIONE>
                      <AUTN>Anonimo del 1312</AUTN>
                  </RIPETIZIONE>
              </PARAGRAFO>
          </SCHEDA>
        </RISULTATI>
      |]
      result <- processXMLStringAsSet mapping zeriBaseUri zeriAnonymousYearXml      
      result
        `shouldBe` 
        [t|
          <work/82371> a crm:E22_Human-Made_Object .
          <work/82371> crm:P108i_was_produced_by <work/82371/production> .
          <work/82371/production> a crm:E12_Production .
          <work/82371/production> crm:P14_carried_out_by <work/82371/production/actor/anonymous/1> .
          <work/82371/production/actor/anonymous/1> a crm:E39_Actor .
          <work/82371/production/actor/anonymous/1> crm:P2_has_type pharos-meta:anonymous_author .
          <work/82371/production/actor/anonymous/1> crm:P107i_is_current_or_former_member_of <group/anonymous/anonimo_del_1312> .
          <group/anonymous/anonimo_del_1312> a crm:E74_Group .
          <group/anonymous/anonimo_del_1312> crm:P1_is_identified_by <group/anonymous/anonimo_del_1312/name> .
          <group/anonymous/anonimo_del_1312/name> a crm:E41_Appellation .
          <group/anonymous/anonimo_del_1312/name> crm:P2_has_type pharos-meta:preferred_name .
          <group/anonymous/anonimo_del_1312/name> crm:P190_has_symbolic_content "Anonimo del 1312" .
          <group/anonymous/anonimo_del_1312> crm:P2_has_type pharos-meta:anonymous_author_group .
          <work/82371/production/actor/anonymous/1> crm:P1_is_identified_by <work/82371/production/actor/anonymous/1/name> .
          <work/82371/production/actor/anonymous/1/name> a crm:E41_Appellation .
          <work/82371/production/actor/anonymous/1/name> crm:P2_has_type pharos-meta:preferred_name .
          <work/82371/production/actor/anonymous/1/name> crm:P190_has_symbolic_content "Anonimo del 1312" .
        |]

    it "correctly maps anonymous author influenced by master" $ do
      let mapping = baseMapping +> [productionLinks]
      let zeriAnonymousInfluencedXml = [w|
        <RISULTATI>
          <SCHEDA sercdoa="82372">
              <PARAGRAFO etichetta="AUTHOR">
                  <RIPETIZIONE>
                      <AUTN>Anonimo bolognese influenzato da Giunta Pisano sec. XIII</AUTN>
                  </RIPETIZIONE>
              </PARAGRAFO>
          </SCHEDA>
        </RISULTATI>
      |]
      result <- processXMLStringAsSet mapping zeriBaseUri zeriAnonymousInfluencedXml      
      result
        `shouldBe` 
        [t|
          <work/82372> a crm:E22_Human-Made_Object .
          <work/82372> crm:P108i_was_produced_by <work/82372/production> .
          <work/82372/production> a crm:E12_Production .
          <work/82372/production> crm:P14_carried_out_by <work/82372/production/actor/anonymous/1> .
          <work/82372/production/actor/anonymous/1> a crm:E39_Actor .
          <work/82372/production/actor/anonymous/1> crm:P2_has_type pharos-meta:anonymous_author .
          <work/82372/production/actor/anonymous/1> crm:P107i_is_current_or_former_member_of <group/anonymous/anonimo_bolognese_influenzato_da_giunta_pisano_sec_xiii> .
          <group/anonymous/anonimo_bolognese_influenzato_da_giunta_pisano_sec_xiii> a crm:E74_Group .
          <group/anonymous/anonimo_bolognese_influenzato_da_giunta_pisano_sec_xiii> crm:P1_is_identified_by <group/anonymous/anonimo_bolognese_influenzato_da_giunta_pisano_sec_xiii/name> .
          <group/anonymous/anonimo_bolognese_influenzato_da_giunta_pisano_sec_xiii/name> a crm:E41_Appellation .
          <group/anonymous/anonimo_bolognese_influenzato_da_giunta_pisano_sec_xiii/name> crm:P2_has_type pharos-meta:preferred_name .
          <group/anonymous/anonimo_bolognese_influenzato_da_giunta_pisano_sec_xiii/name> crm:P190_has_symbolic_content "Anonimo bolognese influenzato da Giunta Pisano sec. XIII" .
          <group/anonymous/anonimo_bolognese_influenzato_da_giunta_pisano_sec_xiii> crm:P2_has_type pharos-meta:anonymous_author_group .
          <work/82372/production/actor/anonymous/1> crm:P1_is_identified_by <work/82372/production/actor/anonymous/1/name> .
          <work/82372/production/actor/anonymous/1/name> a crm:E41_Appellation .
          <work/82372/production/actor/anonymous/1/name> crm:P2_has_type pharos-meta:preferred_name .
          <work/82372/production/actor/anonymous/1/name> crm:P190_has_symbolic_content "Anonimo bolognese influenzato da Giunta Pisano sec. XIII" .
        |]

    it "correctly maps corporate body (maestranze)" $ do
      let mapping = baseMapping +> [productionLinks]
      let zeriMaestranzeXml = [w|
        <RISULTATI>
          <SCHEDA sercdoa="82373">
              <PARAGRAFO etichetta="AUTHOR">
                  <RIPETIZIONE>
                      <AUTN>Maestranze venete sec. XIII</AUTN>
                  </RIPETIZIONE>
              </PARAGRAFO>
          </SCHEDA>
        </RISULTATI>
      |]
      result <- processXMLStringAsSet mapping zeriBaseUri zeriMaestranzeXml      
      result
        `shouldBe` 
        [t|
          <work/82373> a crm:E22_Human-Made_Object .
          <work/82373> crm:P108i_was_produced_by <work/82373/production> .
          <work/82373/production> a crm:E12_Production .
          <work/82373/production> crm:P14_carried_out_by <work/82373/production/actor/anonymous/maestranze_venete_sec_xiii> .
          <work/82373/production/actor/anonymous/maestranze_venete_sec_xiii> a crm:E74_Group .
          <work/82373/production/actor/anonymous/maestranze_venete_sec_xiii> crm:P2_has_type pharos-meta:anonymous_author .
          <work/82373/production/actor/anonymous/maestranze_venete_sec_xiii> crm:P107i_is_current_or_former_member_of <group/anonymous/maestranze_venete_sec_xiii> .
          <group/anonymous/maestranze_venete_sec_xiii> a crm:E74_Group .
          <group/anonymous/maestranze_venete_sec_xiii> crm:P1_is_identified_by <group/anonymous/maestranze_venete_sec_xiii/name> .
          <group/anonymous/maestranze_venete_sec_xiii/name> a crm:E41_Appellation .
          <group/anonymous/maestranze_venete_sec_xiii/name> crm:P2_has_type pharos-meta:preferred_name .
          <group/anonymous/maestranze_venete_sec_xiii/name> crm:P190_has_symbolic_content "Maestranze venete sec. XIII" .
          <group/anonymous/maestranze_venete_sec_xiii> crm:P2_has_type pharos-meta:anonymous_author_group .
          <work/82373/production/actor/anonymous/maestranze_venete_sec_xiii> crm:P1_is_identified_by <work/82373/production/actor/anonymous/maestranze_venete_sec_xiii/name> .
          <work/82373/production/actor/anonymous/maestranze_venete_sec_xiii/name> a crm:E41_Appellation .
          <work/82373/production/actor/anonymous/maestranze_venete_sec_xiii/name> crm:P2_has_type pharos-meta:preferred_name .
          <work/82373/production/actor/anonymous/maestranze_venete_sec_xiii/name> crm:P190_has_symbolic_content "Maestranze venete sec. XIII" .
        |]

    it "correctly maps corporate body (manifattura)" $ do
      let mapping = baseMapping +> [productionLinks]
      let zeriManifatturaXml = [w|
        <RISULTATI>
          <SCHEDA sercdoa="82374">
              <PARAGRAFO etichetta="AUTHOR">
                  <RIPETIZIONE>
                      <AUTN>Manifattura fiorentina sec. XVIII</AUTN>
                  </RIPETIZIONE>
              </PARAGRAFO>
          </SCHEDA>
        </RISULTATI>
      |]
      result <- processXMLStringAsSet mapping zeriBaseUri zeriManifatturaXml      
      result
        `shouldBe` 
        [t|
          <work/82374> a crm:E22_Human-Made_Object .
          <work/82374> crm:P108i_was_produced_by <work/82374/production> .
          <work/82374/production> a crm:E12_Production .
          <work/82374/production> crm:P14_carried_out_by <work/82374/production/actor/anonymous/manifattura_fiorentina_sec_xviii> .
          <work/82374/production/actor/anonymous/manifattura_fiorentina_sec_xviii> a crm:E74_Group .
          <work/82374/production/actor/anonymous/manifattura_fiorentina_sec_xviii> crm:P2_has_type pharos-meta:anonymous_author .
          <work/82374/production/actor/anonymous/manifattura_fiorentina_sec_xviii> crm:P107i_is_current_or_former_member_of <group/anonymous/manifattura_fiorentina_sec_xviii> .
          <group/anonymous/manifattura_fiorentina_sec_xviii> a crm:E74_Group .
          <group/anonymous/manifattura_fiorentina_sec_xviii> crm:P1_is_identified_by <group/anonymous/manifattura_fiorentina_sec_xviii/name> .
          <group/anonymous/manifattura_fiorentina_sec_xviii/name> a crm:E41_Appellation .
          <group/anonymous/manifattura_fiorentina_sec_xviii/name> crm:P2_has_type pharos-meta:preferred_name .
          <group/anonymous/manifattura_fiorentina_sec_xviii/name> crm:P190_has_symbolic_content "Manifattura fiorentina sec. XVIII" .
          <group/anonymous/manifattura_fiorentina_sec_xviii> crm:P2_has_type pharos-meta:anonymous_author_group .
          <work/82374/production/actor/anonymous/manifattura_fiorentina_sec_xviii> crm:P1_is_identified_by <work/82374/production/actor/anonymous/manifattura_fiorentina_sec_xviii/name> .
          <work/82374/production/actor/anonymous/manifattura_fiorentina_sec_xviii/name> a crm:E41_Appellation .
          <work/82374/production/actor/anonymous/manifattura_fiorentina_sec_xviii/name> crm:P2_has_type pharos-meta:preferred_name .
          <work/82374/production/actor/anonymous/manifattura_fiorentina_sec_xviii/name> crm:P190_has_symbolic_content "Manifattura fiorentina sec. XVIII" .
        |]
        
    it "correctly maps anonymous author with attribution" $ do
      let mapping = baseMapping +> [productionLinks]
      let zeriAnonymousAttributionXml = [w|
        <RISULTATI>
          <SCHEDA sercdoa="6530">
              <PARAGRAFO etichetta="AUTHOR">
                  <RIPETIZIONE>
                      <AUTN etichetta="Artist's name">Anonimo italo-bizantino sec. XIII</AUTN>
                      <AUTS etichetta="Connection with artist">attr.</AUTS>
                  </RIPETIZIONE>
              </PARAGRAFO>
          </SCHEDA>
        </RISULTATI>
      |]
      result <- processXMLStringAsSet mapping zeriBaseUri zeriAnonymousAttributionXml      
      result
        `shouldBe` 
        [t|
          <work/6530> a crm:E22_Human-Made_Object .
          <work/6530> crm:P108i_was_produced_by <work/6530/production> .
          <work/6530/production> a crm:E12_Production .
          <work/6530/production> crm:P14_carried_out_by <work/6530/production/actor/anonymous/1> .
          <work/6530/production/actor/anonymous/1> a crm:E39_Actor .
          <work/6530/production/actor/anonymous/1> crm:P141i_was_assigned_by <work/6530/production/actor/anonymous/1/attribution_assignment> .
          <work/6530/production/actor/anonymous/1/attribution_assignment> a crm:E13_Attribute_Assignment .
          <work/6530/production/actor/anonymous/1/attribution_assignment> crm:P140_assigned_attribute_to <work/6530/production> .
          <work/6530/production> a crm:E12_Production .
          <work/6530/production/actor/anonymous/1/attribution_assignment> crm:P177_assigned_property_of_type crm:P14_carried_out_by .
          crm:P14_carried_out_by a crm:E55_Type .
          <work/6530/production/actor/anonymous/1/attribution_assignment> crm:P2_has_type pharos-meta:attribution .
          <work/6530/production/actor/anonymous/1> crm:P2_has_type pharos-meta:anonymous_author .
          <work/6530/production/actor/anonymous/1> crm:P107i_is_current_or_former_member_of <group/anonymous/anonimo_italo_bizantino_sec_xiii> .
          <group/anonymous/anonimo_italo_bizantino_sec_xiii> a crm:E74_Group .
          <group/anonymous/anonimo_italo_bizantino_sec_xiii> crm:P1_is_identified_by <group/anonymous/anonimo_italo_bizantino_sec_xiii/name> .
          <group/anonymous/anonimo_italo_bizantino_sec_xiii/name> a crm:E41_Appellation .
          <group/anonymous/anonimo_italo_bizantino_sec_xiii/name> crm:P2_has_type pharos-meta:preferred_name .
          <group/anonymous/anonimo_italo_bizantino_sec_xiii/name> crm:P190_has_symbolic_content "Anonimo italo-bizantino sec. XIII" .
          <group/anonymous/anonimo_italo_bizantino_sec_xiii> crm:P2_has_type pharos-meta:anonymous_author_group .
          <work/6530/production/actor/anonymous/1> crm:P1_is_identified_by <work/6530/production/actor/anonymous/1/name> .
          <work/6530/production/actor/anonymous/1/name> a crm:E41_Appellation .
          <work/6530/production/actor/anonymous/1/name> crm:P2_has_type pharos-meta:preferred_name .
          <work/6530/production/actor/anonymous/1/name> crm:P190_has_symbolic_content "Anonimo italo-bizantino sec. XIII" .
        |]

    it "correctly maps multiple anonymous authors" $ do
      let mapping = baseMapping +> [productionLinks]
      let zeriMultipleAnonymousXml = [w|
        <RISULTATI>
          <SCHEDA sercdoa="817">
              <PARAGRAFO etichetta="AUTHOR">
                  <RIPETIZIONE>
                      <AUTN etichetta="Artist's name">Anonimo romano sec. VI</AUTN>
                      <AUTM etichetta="Reason for attribution">Bibliografia</AUTM>
                  </RIPETIZIONE>
                  <RIPETIZIONE>
                      <AUTN etichetta="Artist's name">Anonimo romano sec. VII</AUTN>
                      <AUTM etichetta="Reason for attribution">Bibliografia</AUTM>
                  </RIPETIZIONE>
                  <RIPETIZIONE>
                      <AUTN etichetta="Artist's name">Anonimo bizantino sec. VIII</AUTN>
                      <AUTS etichetta="Connection with artist">(?)</AUTS>
                      <AUTM etichetta="Reason for attribution">Bibliografia</AUTM>
                      <AUTHORITIES></AUTHORITIES>
                  </RIPETIZIONE>
              </PARAGRAFO>
          </SCHEDA>
        </RISULTATI>
      |]
      result <- processXMLStringAsSet mapping zeriBaseUri zeriMultipleAnonymousXml
      result
        `shouldBe` 
        [t|
          <work/817> a crm:E22_Human-Made_Object .
          <work/817> crm:P108i_was_produced_by <work/817/production> .
          <work/817/production> a crm:E12_Production .
          <work/817/production> crm:P14_carried_out_by <work/817/production/actor/anonymous/1> .
          <work/817/production/actor/anonymous/1> a crm:E39_Actor .
          <work/817/production/actor/anonymous/1> crm:P2_has_type pharos-meta:anonymous_author .
          <work/817/production/actor/anonymous/1> crm:P107i_is_current_or_former_member_of <group/anonymous/anonimo_romano_sec_vi> .
          <group/anonymous/anonimo_romano_sec_vi> a crm:E74_Group .
          <group/anonymous/anonimo_romano_sec_vi> crm:P1_is_identified_by <group/anonymous/anonimo_romano_sec_vi/name> .
          <group/anonymous/anonimo_romano_sec_vi/name> a crm:E41_Appellation .
          <group/anonymous/anonimo_romano_sec_vi/name> crm:P2_has_type pharos-meta:preferred_name .
          <group/anonymous/anonimo_romano_sec_vi/name> crm:P190_has_symbolic_content "Anonimo romano sec. VI" .
          <group/anonymous/anonimo_romano_sec_vi> crm:P2_has_type pharos-meta:anonymous_author_group .
          <work/817/production/actor/anonymous/1> crm:P1_is_identified_by <work/817/production/actor/anonymous/1/name> .
          <work/817/production/actor/anonymous/1/name> a crm:E41_Appellation .
          <work/817/production/actor/anonymous/1/name> crm:P2_has_type pharos-meta:preferred_name .
          <work/817/production/actor/anonymous/1/name> crm:P190_has_symbolic_content "Anonimo romano sec. VI" .
          <work/817/production> crm:P14_carried_out_by <work/817/production/actor/anonymous/2> .
          <work/817/production/actor/anonymous/2> a crm:E39_Actor .
          <work/817/production/actor/anonymous/2> crm:P2_has_type pharos-meta:anonymous_author .
          <work/817/production/actor/anonymous/2> crm:P107i_is_current_or_former_member_of <group/anonymous/anonimo_romano_sec_vii> .
          <group/anonymous/anonimo_romano_sec_vii> a crm:E74_Group .
          <group/anonymous/anonimo_romano_sec_vii> crm:P1_is_identified_by <group/anonymous/anonimo_romano_sec_vii/name> .
          <group/anonymous/anonimo_romano_sec_vii/name> a crm:E41_Appellation .
          <group/anonymous/anonimo_romano_sec_vii/name> crm:P2_has_type pharos-meta:preferred_name .
          <group/anonymous/anonimo_romano_sec_vii/name> crm:P190_has_symbolic_content "Anonimo romano sec. VII" .
          <group/anonymous/anonimo_romano_sec_vii> crm:P2_has_type pharos-meta:anonymous_author_group .
          <work/817/production/actor/anonymous/2> crm:P1_is_identified_by <work/817/production/actor/anonymous/2/name> .
          <work/817/production/actor/anonymous/2/name> a crm:E41_Appellation .
          <work/817/production/actor/anonymous/2/name> crm:P2_has_type pharos-meta:preferred_name .
          <work/817/production/actor/anonymous/2/name> crm:P190_has_symbolic_content "Anonimo romano sec. VII" .
          <work/817/production> crm:P14_carried_out_by <work/817/production/actor/anonymous/3> .
          <work/817/production/actor/anonymous/3> a crm:E39_Actor .
          <work/817/production/actor/anonymous/3> crm:P141i_was_assigned_by <work/817/production/actor/anonymous/3/attribution_assignment> .
          <work/817/production/actor/anonymous/3/attribution_assignment> a crm:E13_Attribute_Assignment .
          <work/817/production/actor/anonymous/3/attribution_assignment> crm:P140_assigned_attribute_to <work/817/production> .
          <work/817/production> a crm:E12_Production .
          <work/817/production/actor/anonymous/3/attribution_assignment> crm:P177_assigned_property_of_type crm:P14_carried_out_by .
          crm:P14_carried_out_by a crm:E55_Type .
          <work/817/production/actor/anonymous/3/attribution_assignment> crm:P2_has_type pharos-meta:attribution .
          <work/817/production/actor/anonymous/3> crm:P2_has_type pharos-meta:anonymous_author .
          <work/817/production/actor/anonymous/3> crm:P107i_is_current_or_former_member_of <group/anonymous/anonimo_bizantino_sec_viii> .
          <group/anonymous/anonimo_bizantino_sec_viii> a crm:E74_Group .
          <group/anonymous/anonimo_bizantino_sec_viii> crm:P1_is_identified_by <group/anonymous/anonimo_bizantino_sec_viii/name> .
          <group/anonymous/anonimo_bizantino_sec_viii/name> a crm:E41_Appellation .
          <group/anonymous/anonimo_bizantino_sec_viii/name> crm:P2_has_type pharos-meta:preferred_name .
          <group/anonymous/anonimo_bizantino_sec_viii/name> crm:P190_has_symbolic_content "Anonimo bizantino sec. VIII" .
          <group/anonymous/anonimo_bizantino_sec_viii> crm:P2_has_type pharos-meta:anonymous_author_group .
          <work/817/production/actor/anonymous/3> crm:P1_is_identified_by <work/817/production/actor/anonymous/3/name> .
          <work/817/production/actor/anonymous/3/name> a crm:E41_Appellation .
          <work/817/production/actor/anonymous/3/name> crm:P2_has_type pharos-meta:preferred_name .
          <work/817/production/actor/anonymous/3/name> crm:P190_has_symbolic_content "Anonimo bizantino sec. VIII" .
        |]

  describe "production date mappings" $ do
    it "correctly maps multiple comma-separated dates with different centuries and parts of century" $ do
      let mapping = baseMapping +> [productionLinks]
      let zeriMultipleDatesXml = [w|
        <RISULTATI>
          <SCHEDA sercdoa="2140">
            <PARAGRAFO etichetta="DATING">
              <DTZG etichetta="Century">sec. XII,sec. XIII,sec. XV</DTZG>
              <DTZS etichetta="Part of century">prima metà,ultimo quarto</DTZS>
              <DTSI etichetta="From">1100,1200,1475</DTSI>
              <DTSF etichetta="To">1149,1299,1499</DTSF>
            </PARAGRAFO>
          </SCHEDA>
        </RISULTATI>
      |]
      result <- processXMLStringAsSet mapping zeriBaseUri zeriMultipleDatesXml
      result `shouldBe`
        [t|
          <work/2140> a crm:E22_Human-Made_Object .
          <work/2140> crm:P108i_was_produced_by <work/2140/production> .
          <work/2140/production> a crm:E12_Production .
          <work/2140/production> crm:P4_has_time-span <work/2140/production/date/1> .
          <work/2140/production/date/1> a crm:E52_Time-Span .
          <work/2140/production/date/1> crm:P1_is_identified_by <work/2140/production/date/1/appellation/preferred_name> .
          <work/2140/production/date/1> crm:P82a_begin_of_the_begin "1100-01-01T00:00:00Z"^^xsd:dateTime .
          <work/2140/production/date/1> crm:P82b_end_of_the_end "1149-12-31T23:59:59Z"^^xsd:dateTime .
          <work/2140/production/date/1/appellation/preferred_name> a crm:E41_Appellation .
          <work/2140/production/date/1/appellation/preferred_name> crm:P190_has_symbolic_content "1100 - 1149" .
          <work/2140/production/date/1/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
          <work/2140/production> crm:P4_has_time-span <work/2140/production/date/2> .
          <work/2140/production/date/2> a crm:E52_Time-Span .
          <work/2140/production/date/2> crm:P1_is_identified_by <work/2140/production/date/2/appellation/preferred_name> .
          <work/2140/production/date/2> crm:P82a_begin_of_the_begin "1200-01-01T00:00:00Z"^^xsd:dateTime .
          <work/2140/production/date/2> crm:P82b_end_of_the_end "1299-12-31T23:59:59Z"^^xsd:dateTime .
          <work/2140/production/date/2/appellation/preferred_name> a crm:E41_Appellation .
          <work/2140/production/date/2/appellation/preferred_name> crm:P190_has_symbolic_content "1200 - 1299" .
          <work/2140/production/date/2/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
          <work/2140/production> crm:P4_has_time-span <work/2140/production/date/3> .
          <work/2140/production/date/3> a crm:E52_Time-Span .
          <work/2140/production/date/3> crm:P1_is_identified_by <work/2140/production/date/3/appellation/preferred_name> .
          <work/2140/production/date/3> crm:P82a_begin_of_the_begin "1475-01-01T00:00:00Z"^^xsd:dateTime .
          <work/2140/production/date/3> crm:P82b_end_of_the_end "1499-12-31T23:59:59Z"^^xsd:dateTime .
          <work/2140/production/date/3/appellation/preferred_name> a crm:E41_Appellation .
          <work/2140/production/date/3/appellation/preferred_name> crm:P190_has_symbolic_content "1475 - 1499" .
          <work/2140/production/date/3/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
        |]

    it "correctly maps a single full date (YYYY/MM/DD)" $ do
      let mapping = baseMapping +> [productionLinks]
      let zeriSingleFullDateXml = [w|
        <RISULTATI>
          <SCHEDA sercdoa="106052">
              <PARAGRAFO etichetta="DATING">
                <DTZG etichetta="Century">sec. XX</DTZG>
                <DTZS etichetta="Part of century">secondo quarto</DTZS>
                <DTSI etichetta="From">1937/09/18</DTSI>
                <DTSF etichetta="To">1937/09/18</DTSF>
              </PARAGRAFO>
          </SCHEDA>
        </RISULTATI>
      |]
      result <- processXMLStringAsSet mapping zeriBaseUri zeriSingleFullDateXml
      result `shouldBe`
        [t|
          <work/106052> a crm:E22_Human-Made_Object .
          <work/106052> crm:P108i_was_produced_by <work/106052/production> .
          <work/106052/production> a crm:E12_Production .
          <work/106052/production> crm:P4_has_time-span <work/106052/production/date> .
          <work/106052/production/date> a crm:E52_Time-Span .
          <work/106052/production/date> crm:P1_is_identified_by <work/106052/production/date/appellation/preferred_name> .
          <work/106052/production/date> crm:P82a_begin_of_the_begin "1937-09-18T00:00:00Z"^^xsd:dateTime .
          <work/106052/production/date> crm:P82b_end_of_the_end "1937-09-18T23:59:59Z"^^xsd:dateTime .
          <work/106052/production/date/appellation/preferred_name> a crm:E41_Appellation .
          <work/106052/production/date/appellation/preferred_name> crm:P190_has_symbolic_content "1937/09/18" .
          <work/106052/production/date/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
        |]

    it "correctly maps a single date with year and month (YYYY/MM)" $ do
      let mapping = baseMapping +> [productionLinks]
      let zeriYearMonthDateXml = [w|
        <RISULTATI>
          <SCHEDA sercdoa="104354">
            <PARAGRAFO etichetta="DATING">
              <DTZG etichetta="Century">sec. XX</DTZG>
              <DTZS etichetta="Part of century">inizio</DTZS>
              <DTSI etichetta="From">1907/05</DTSI>
              <DTSF etichetta="To">1907/05</DTSF>
            </PARAGRAFO>
          </SCHEDA>
        </RISULTATI>
      |]
      result <- processXMLStringAsSet mapping zeriBaseUri zeriYearMonthDateXml
      result `shouldBe`
        [t|
          <work/104354> a crm:E22_Human-Made_Object .
          <work/104354> crm:P108i_was_produced_by <work/104354/production> .
          <work/104354/production> a crm:E12_Production .
          <work/104354/production> crm:P4_has_time-span <work/104354/production/date> .
          <work/104354/production/date> a crm:E52_Time-Span .
          <work/104354/production/date> crm:P1_is_identified_by <work/104354/production/date/appellation/preferred_name> .
          <work/104354/production/date> crm:P82a_begin_of_the_begin "1907-05-01T00:00:00Z"^^xsd:dateTime .
          <work/104354/production/date> crm:P82b_end_of_the_end "1907-05-31T23:59:59Z"^^xsd:dateTime .
          <work/104354/production/date/appellation/preferred_name> a crm:E41_Appellation .
          <work/104354/production/date/appellation/preferred_name> crm:P190_has_symbolic_content "1907/05" .
          <work/104354/production/date/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
        |]

    it "correctly maps multiple comma-separated dates with qualifiers" $ do
      let mapping = baseMapping +> [productionLinks]
      let zeriMultipleDatesWithQualifiersXml = [w|
        <RISULTATI>
          <SCHEDA sercdoa="3247">
              <PARAGRAFO etichetta="DATING">
                <DTZG etichetta="Century">sec. XIII,sec. XIV</DTZG>
                <DTZS etichetta="Part of century">terzo quarto,inizio</DTZS>
                <DTSI etichetta="From">1265,1310</DTSI>
                <DTSV etichetta="Degree of approximation">ca.,ca.</DTSV>
                <DTSF etichetta="To">1265,1310</DTSF>
                <DTSL etichetta="Degree of approximation">ca.,ca.</DTSL>
                <ADT etichetta="Different dating">sec. XIII (1290-1299)</ADT>
              </PARAGRAFO>
          </SCHEDA>
        </RISULTATI>
      |]
      result <- processXMLStringAsSet mapping zeriBaseUri zeriMultipleDatesWithQualifiersXml
      result `shouldBe`
        [t|
          <work/3247> a crm:E22_Human-Made_Object .
          <work/3247> a crm:E22_Human-Made_Object .
          <work/3247> crm:P108i_was_produced_by <work/3247/production> .
          <work/3247/production> a crm:E12_Production .
          <work/3247/production> crm:P4_has_time-span <work/3247/production/date/1> .
          <work/3247/production/date/1> a crm:E52_Time-Span .
          <work/3247/production/date/1> crm:P1_is_identified_by <work/3247/production/date/1/appellation/preferred_name> .
          <work/3247/production/date/1> crm:P82a_begin_of_the_begin "1263-01-01T00:00:00Z"^^xsd:dateTime .
          <work/3247/production/date/1> crm:P82b_end_of_the_end "1267-12-31T23:59:59Z"^^xsd:dateTime .
          <work/3247/production/date/1/appellation/preferred_name> a crm:E41_Appellation .
          <work/3247/production/date/1/appellation/preferred_name> crm:P190_has_symbolic_content "1265 ca." .
          <work/3247/production/date/1/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
          <work/3247/production> crm:P4_has_time-span <work/3247/production/date/2> .
          <work/3247/production/date/2> a crm:E52_Time-Span .
          <work/3247/production/date/2> crm:P1_is_identified_by <work/3247/production/date/2/appellation/preferred_name> .
          <work/3247/production/date/2> crm:P82a_begin_of_the_begin "1308-01-01T00:00:00Z"^^xsd:dateTime .
          <work/3247/production/date/2> crm:P82b_end_of_the_end "1312-12-31T23:59:59Z"^^xsd:dateTime .
          <work/3247/production/date/2/appellation/preferred_name> a crm:E41_Appellation .
          <work/3247/production/date/2/appellation/preferred_name> crm:P190_has_symbolic_content "1310 ca." .
          <work/3247/production/date/2/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
        |]

    it "correctly maps a single date with 'post' and 'ca.' qualifiers" $ do
      let mapping = baseMapping +> [productionLinks]
      let zeriPostCaDateXml = [w|
        <RISULTATI>
          <SCHEDA sercdoa="28203">
            <PARAGRAFO etichetta="DATING">
              <DTZG etichetta="Century">sec. XVI</DTZG>
              <DTZS etichetta="Part of century">inizio</DTZS>
              <DTSI etichetta="From">1504</DTSI>
              <DTSV etichetta="Degree of approximation">post</DTSV>
              <DTSF etichetta="To">1510</DTSF>
              <DTSL etichetta="Degree of approximation">ca.</DTSL>
            </PARAGRAFO>
          </SCHEDA>
        </RISULTATI>
      |]
      result <- processXMLStringAsSet mapping zeriBaseUri zeriPostCaDateXml
      result `shouldBe`
        [t|
          <work/28203> a crm:E22_Human-Made_Object .
          <work/28203> a crm:E22_Human-Made_Object .
          <work/28203> crm:P108i_was_produced_by <work/28203/production> .
          <work/28203/production> a crm:E12_Production .
          <work/28203/production> crm:P4_has_time-span <work/28203/production/date> .
          <work/28203/production/date> a crm:E52_Time-Span .
          <work/28203/production/date> crm:P1_is_identified_by <work/28203/production/date/appellation/preferred_name> .
          <work/28203/production/date> crm:P82a_begin_of_the_begin "1502-01-01T00:00:00Z"^^xsd:dateTime .
          <work/28203/production/date> crm:P82b_end_of_the_end "1510-12-31T23:59:59Z"^^xsd:dateTime .
          <work/28203/production/date/appellation/preferred_name> a crm:E41_Appellation .
          <work/28203/production/date/appellation/preferred_name> crm:P190_has_symbolic_content "1504 ca. - 1510 post" .
          <work/28203/production/date/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
        |]

    it "correctly maps a single date with 'ca.' and 'ante' qualifiers and century range" $ do
      let mapping = baseMapping +> [productionLinks]
      let zeriCaAnteCenturyRangeXml = [w|
        <RISULTATI>
          <SCHEDA sercdoa="83698">
              <PARAGRAFO etichetta="DATING">
                <DTZG etichetta="Century">sec. XIX/ XX</DTZG>
                <DTSI etichetta="From">1870</DTSI>
                <DTSV etichetta="Degree of approximation">ca.</DTSV>
                <DTSF etichetta="To">1929</DTSF>
                <DTSL etichetta="Degree of approximation">ante</DTSL>
              </PARAGRAFO>
          </SCHEDA>
        </RISULTATI>
      |]
      result <- processXMLStringAsSet mapping zeriBaseUri zeriCaAnteCenturyRangeXml
      result `shouldBe`
        [t|
          <work/83698> a crm:E22_Human-Made_Object .
          <work/83698> a crm:E22_Human-Made_Object .
          <work/83698> crm:P108i_was_produced_by <work/83698/production> .
          <work/83698/production> a crm:E12_Production .
          <work/83698/production> crm:P4_has_time-span <work/83698/production/date> .
          <work/83698/production/date> a crm:E52_Time-Span .
          <work/83698/production/date> crm:P1_is_identified_by <work/83698/production/date/appellation/preferred_name> .
          <work/83698/production/date> crm:P82a_begin_of_the_begin "1870-01-01T00:00:00Z"^^xsd:dateTime .
          <work/83698/production/date> crm:P82b_end_of_the_end "1931-12-31T23:59:59Z"^^xsd:dateTime .
          <work/83698/production/date/appellation/preferred_name> a crm:E41_Appellation .
          <work/83698/production/date/appellation/preferred_name> crm:P190_has_symbolic_content "1870 ante - 1929 ca." .
          <work/83698/production/date/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
        |]

    it "correctly maps a single date with 'post' and 'ante' qualifiers" $ do
      let mapping = baseMapping +> [productionLinks]
      let zeriPostAnteDateXml = [w|
        <RISULTATI>
          <SCHEDA sercdoa="26854">
            <PARAGRAFO etichetta="DATING">
              <DTZG etichetta="Century">sec. XV</DTZG>
              <DTZS etichetta="Part of century">terzo quarto</DTZS>
              <DTSI etichetta="From">1450</DTSI>
              <DTSV etichetta="Degree of approximation">post</DTSV>
              <DTSF etichetta="To">1459</DTSF>
              <DTSL etichetta="Degree of approximation">ante</DTSL>
            </PARAGRAFO>
          </SCHEDA>
        </RISULTATI>
      |]
      result <- processXMLStringAsSet mapping zeriBaseUri zeriPostAnteDateXml
      result `shouldBe`
        [t|
          <work/26854> a crm:E22_Human-Made_Object .
          <work/26854> a crm:E22_Human-Made_Object .
          <work/26854> crm:P108i_was_produced_by <work/26854/production> .
          <work/26854/production> a crm:E12_Production .
          <work/26854/production> crm:P4_has_time-span <work/26854/production/date> .
          <work/26854/production/date> a crm:E52_Time-Span .
          <work/26854/production/date> crm:P1_is_identified_by <work/26854/production/date/appellation/preferred_name> .
          <work/26854/production/date> crm:P82a_begin_of_the_begin "1450-01-01T00:00:00Z"^^xsd:dateTime .
          <work/26854/production/date> crm:P82b_end_of_the_end "1459-12-31T23:59:59Z"^^xsd:dateTime .
          <work/26854/production/date/appellation/preferred_name> a crm:E41_Appellation .
          <work/26854/production/date/appellation/preferred_name> crm:P190_has_symbolic_content "1450 ante - 1459 post" .
          <work/26854/production/date/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
        |]
