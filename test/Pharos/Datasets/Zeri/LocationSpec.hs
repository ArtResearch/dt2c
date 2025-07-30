module Pharos.Datasets.Zeri.LocationSpec (spec) where

import Test.Hspec
import CommonImports
import Mappings.Mappings.Work.Location (location)
import Mappings.Mappings.Work.WorkMappings (baseMapping, zeriBaseUri)

spec :: Spec
spec = describe "Zeri Location mapping" $ do
  it "should process PVCS (Country) only" $ do
    let mapping = baseMapping +> [location]
    let zeriXml = [w|
      <RISULTATI>
        <SCHEDA sercdoa="10142">
          <PARAGRAFO etichetta="LOCATION">
            <PVCS>Svizzera</PVCS>
          </PARAGRAFO>
        </SCHEDA>
      </RISULTATI>
    |]
    result <- processXMLStringAsSet mapping zeriBaseUri zeriXml
    result `shouldBe` [t|
<http://vocab.getty.edu/tgn/7011731> a crm:E53_Place .
<place/country/svizzera> a crm:E53_Place .
<place/country/svizzera> crm:P1_is_identified_by <place/country/svizzera/appellation/preferred_name> .
<place/country/svizzera> crm:P2_has_type pharos-meta:country .
<place/country/svizzera> crm:P55i_currently_holds <work/10142> .
<place/country/svizzera> custom:sameAs <http://vocab.getty.edu/tgn/7011731> .
<place/country/svizzera/appellation/preferred_name> a crm:E41_Appellation .
<place/country/svizzera/appellation/preferred_name> crm:P190_has_symbolic_content "Svizzera" .
<place/country/svizzera/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<work/10142> a crm:E22_Human-Made_Object .
|]

  it "should process PVCS (Country) -> PVCR (Region)" $ do
    let mapping = baseMapping +> [location]
    let zeriXml = [w|
      <RISULTATI>
        <SCHEDA sercdoa="11130">
          <PARAGRAFO etichetta="LOCATION">
            <PVCS>Italia</PVCS>
            <PVCR>Umbria</PVCR>
          </PARAGRAFO>
        </SCHEDA>
      </RISULTATI>
    |]
    result <- processXMLStringAsSet mapping zeriBaseUri zeriXml
    result `shouldBe` [t|
<http://vocab.getty.edu/tgn/1000080> a crm:E53_Place .
<http://vocab.getty.edu/tgn/7003125> a crm:E53_Place .
<place/country/italia> a crm:E53_Place .
<place/country/italia> crm:P1_is_identified_by <place/country/italia/appellation/preferred_name> .
<place/country/italia> crm:P2_has_type pharos-meta:country .
<place/country/italia> crm:P89i_contains <place/country/italia/region/umbria> .
<place/country/italia> custom:sameAs <http://vocab.getty.edu/tgn/1000080> .
<place/country/italia/appellation/preferred_name> a crm:E41_Appellation .
<place/country/italia/appellation/preferred_name> crm:P190_has_symbolic_content "Italia" .
<place/country/italia/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<place/country/italia/region/umbria> a crm:E53_Place .
<place/country/italia/region/umbria> crm:P1_is_identified_by <place/country/italia/region/umbria/appellation/preferred_name> .
<place/country/italia/region/umbria> crm:P2_has_type <vocab/meta/region> .
<place/country/italia/region/umbria> crm:P55i_currently_holds <work/11130> .
<place/country/italia/region/umbria> custom:sameAs <http://vocab.getty.edu/tgn/7003125> .
<place/country/italia/region/umbria/appellation/preferred_name> a crm:E41_Appellation .
<place/country/italia/region/umbria/appellation/preferred_name> crm:P190_has_symbolic_content "Umbria" .
<place/country/italia/region/umbria/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<work/11130> a crm:E22_Human-Made_Object .
|]

  it "should process PVCS (Country) -> PVCR (Region) -> PVCP (District)" $ do
    let mapping = baseMapping +> [location]
    let zeriXml = [w|
      <RISULTATI>
        <SCHEDA sercdoa="11533">
          <PARAGRAFO etichetta="LOCATION">
            <PVCS>Italia</PVCS>
            <PVCR>Campania</PVCR>
            <PVCP>Napoli</PVCP>
          </PARAGRAFO>
        </SCHEDA>
      </RISULTATI>
    |]
    result <- processXMLStringAsSet mapping zeriBaseUri zeriXml
    result `shouldBe` [t|
<http://vocab.getty.edu/tgn/1000080> a crm:E53_Place .
<http://vocab.getty.edu/tgn/7003005> a crm:E53_Place .
<http://vocab.getty.edu/tgn/7003013> a crm:E53_Place .
<place/country/italia> a crm:E53_Place .
<place/country/italia> crm:P1_is_identified_by <place/country/italia/appellation/preferred_name> .
<place/country/italia> crm:P2_has_type pharos-meta:country .
<place/country/italia> crm:P89i_contains <place/country/italia/region/campania> .
<place/country/italia> custom:sameAs <http://vocab.getty.edu/tgn/1000080> .
<place/country/italia/appellation/preferred_name> a crm:E41_Appellation .
<place/country/italia/appellation/preferred_name> crm:P190_has_symbolic_content "Italia" .
<place/country/italia/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<place/country/italia/region/campania> a crm:E53_Place .
<place/country/italia/region/campania> crm:P1_is_identified_by <place/country/italia/region/campania/appellation/preferred_name> .
<place/country/italia/region/campania> crm:P2_has_type <vocab/meta/region> .
<place/country/italia/region/campania> crm:P89i_contains <place/country/italia/region/campania/district/napoli> .
<place/country/italia/region/campania> custom:sameAs <http://vocab.getty.edu/tgn/7003005> .
<place/country/italia/region/campania/appellation/preferred_name> a crm:E41_Appellation .
<place/country/italia/region/campania/appellation/preferred_name> crm:P190_has_symbolic_content "Campania" .
<place/country/italia/region/campania/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<place/country/italia/region/campania/district/napoli> a crm:E53_Place .
<place/country/italia/region/campania/district/napoli> crm:P1_is_identified_by <place/country/italia/region/campania/district/napoli/appellation/preferred_name> .
<place/country/italia/region/campania/district/napoli> crm:P2_has_type <vocab/meta/district> .
<place/country/italia/region/campania/district/napoli> crm:P55i_currently_holds <work/11533> .
<place/country/italia/region/campania/district/napoli> custom:sameAs <http://vocab.getty.edu/tgn/7003013> .
<place/country/italia/region/campania/district/napoli/appellation/preferred_name> a crm:E41_Appellation .
<place/country/italia/region/campania/district/napoli/appellation/preferred_name> crm:P190_has_symbolic_content "Napoli" .
<place/country/italia/region/campania/district/napoli/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<work/11533> a crm:E22_Human-Made_Object .
|]

  it "should process PVCS (Country) -> PVCR (Region) -> PVCP (District) -> PVCC (Town)" $ do
    let mapping = baseMapping +> [location]
    let zeriXml = [w|
      <RISULTATI>
        <SCHEDA sercdoa="10229">
          <PARAGRAFO etichetta="LOCATION">
            <PVCS>Italia</PVCS>
            <PVCR>Toscana</PVCR>
            <PVCP>Siena</PVCP>
            <PVCC>Siena</PVCC>
          </PARAGRAFO>
        </SCHEDA>
      </RISULTATI>
    |]
    result <- processXMLStringAsSet mapping zeriBaseUri zeriXml
    result `shouldBe` [t|
<http://vocab.getty.edu/tgn/1000080> a crm:E53_Place .
<http://vocab.getty.edu/tgn/7003168> a crm:E53_Place .
<http://vocab.getty.edu/tgn/7009760> a crm:E53_Place .
<http://vocab.getty.edu/tgn/7011179> a crm:E53_Place .
<place/country/italia> a crm:E53_Place .
<place/country/italia> crm:P1_is_identified_by <place/country/italia/appellation/preferred_name> .
<place/country/italia> crm:P2_has_type pharos-meta:country .
<place/country/italia> crm:P89i_contains <place/country/italia/region/toscana> .
<place/country/italia> custom:sameAs <http://vocab.getty.edu/tgn/1000080> .
<place/country/italia/appellation/preferred_name> a crm:E41_Appellation .
<place/country/italia/appellation/preferred_name> crm:P190_has_symbolic_content "Italia" .
<place/country/italia/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<place/country/italia/region/toscana> a crm:E53_Place .
<place/country/italia/region/toscana> crm:P1_is_identified_by <place/country/italia/region/toscana/appellation/preferred_name> .
<place/country/italia/region/toscana> crm:P2_has_type <vocab/meta/region> .
<place/country/italia/region/toscana> crm:P89i_contains <place/country/italia/region/toscana/district/siena> .
<place/country/italia/region/toscana> custom:sameAs <http://vocab.getty.edu/tgn/7009760> .
<place/country/italia/region/toscana/appellation/preferred_name> a crm:E41_Appellation .
<place/country/italia/region/toscana/appellation/preferred_name> crm:P190_has_symbolic_content "Toscana" .
<place/country/italia/region/toscana/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<place/country/italia/region/toscana/district/siena> a crm:E53_Place .
<place/country/italia/region/toscana/district/siena> crm:P1_is_identified_by <place/country/italia/region/toscana/district/siena/appellation/preferred_name> .
<place/country/italia/region/toscana/district/siena> crm:P2_has_type <vocab/meta/district> .
<place/country/italia/region/toscana/district/siena> crm:P89i_contains <place/country/italia/region/toscana/district/siena/town/siena> .
<place/country/italia/region/toscana/district/siena> custom:sameAs <http://vocab.getty.edu/tgn/7003168> .
<place/country/italia/region/toscana/district/siena/appellation/preferred_name> a crm:E41_Appellation .
<place/country/italia/region/toscana/district/siena/appellation/preferred_name> crm:P190_has_symbolic_content "Siena" .
<place/country/italia/region/toscana/district/siena/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<place/country/italia/region/toscana/district/siena/town/siena> a crm:E53_Place .
<place/country/italia/region/toscana/district/siena/town/siena> crm:P1_is_identified_by <place/country/italia/region/toscana/district/siena/town/siena/appellation/preferred_name> .
<place/country/italia/region/toscana/district/siena/town/siena> crm:P2_has_type <vocab/meta/town> .
<place/country/italia/region/toscana/district/siena/town/siena> crm:P55i_currently_holds <work/10229> .
<place/country/italia/region/toscana/district/siena/town/siena> custom:sameAs <http://vocab.getty.edu/tgn/7011179> .
<place/country/italia/region/toscana/district/siena/town/siena/appellation/preferred_name> a crm:E41_Appellation .
<place/country/italia/region/toscana/district/siena/town/siena/appellation/preferred_name> crm:P190_has_symbolic_content "Siena" .
<place/country/italia/region/toscana/district/siena/town/siena/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<work/10229> a crm:E22_Human-Made_Object .
|]

  it "should process PVCS (Country) -> PVCR (Region) -> PVCP (District) -> PVCC (Town) -> PVCL (Village)" $ do
    let mapping = baseMapping +> [location]
    let zeriXml = [w|
      <RISULTATI>
        <SCHEDA sercdoa="12112">
          <PARAGRAFO etichetta="LOCATION">
            <PVCS>Italia</PVCS>
            <PVCR>Umbria</PVCR>
            <PVCP>Perugia</PVCP>
            <PVCC>Sellano</PVCC>
            <PVCL>Montesanto</PVCL>
          </PARAGRAFO>
        </SCHEDA>
      </RISULTATI>
    |]
    result <- processXMLStringAsSet mapping zeriBaseUri zeriXml
    result `shouldBe` [t|
<http://vocab.getty.edu/tgn/1000080> a crm:E53_Place .
<http://vocab.getty.edu/tgn/7003125> a crm:E53_Place .
<http://vocab.getty.edu/tgn/7003170> a crm:E53_Place .
<http://vocab.getty.edu/tgn/7005052> a crm:E53_Place .
<place/country/italia> a crm:E53_Place .
<place/country/italia> crm:P1_is_identified_by <place/country/italia/appellation/preferred_name> .
<place/country/italia> crm:P2_has_type pharos-meta:country .
<place/country/italia> crm:P89i_contains <place/country/italia/region/umbria> .
<place/country/italia> custom:sameAs <http://vocab.getty.edu/tgn/1000080> .
<place/country/italia/appellation/preferred_name> a crm:E41_Appellation .
<place/country/italia/appellation/preferred_name> crm:P190_has_symbolic_content "Italia" .
<place/country/italia/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<place/country/italia/region/umbria> a crm:E53_Place .
<place/country/italia/region/umbria> crm:P1_is_identified_by <place/country/italia/region/umbria/appellation/preferred_name> .
<place/country/italia/region/umbria> crm:P2_has_type <vocab/meta/region> .
<place/country/italia/region/umbria> crm:P89i_contains <place/country/italia/region/umbria/district/perugia> .
<place/country/italia/region/umbria> custom:sameAs <http://vocab.getty.edu/tgn/7003125> .
<place/country/italia/region/umbria/appellation/preferred_name> a crm:E41_Appellation .
<place/country/italia/region/umbria/appellation/preferred_name> crm:P190_has_symbolic_content "Umbria" .
<place/country/italia/region/umbria/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<place/country/italia/region/umbria/district/perugia> a crm:E53_Place .
<place/country/italia/region/umbria/district/perugia> crm:P1_is_identified_by <place/country/italia/region/umbria/district/perugia/appellation/preferred_name> .
<place/country/italia/region/umbria/district/perugia> crm:P2_has_type <vocab/meta/district> .
<place/country/italia/region/umbria/district/perugia> crm:P89i_contains <place/country/italia/region/umbria/district/perugia/town/sellano> .
<place/country/italia/region/umbria/district/perugia> custom:sameAs <http://vocab.getty.edu/tgn/7003170> .
<place/country/italia/region/umbria/district/perugia/appellation/preferred_name> a crm:E41_Appellation .
<place/country/italia/region/umbria/district/perugia/appellation/preferred_name> crm:P190_has_symbolic_content "Perugia" .
<place/country/italia/region/umbria/district/perugia/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<place/country/italia/region/umbria/district/perugia/town/sellano> a crm:E53_Place .
<place/country/italia/region/umbria/district/perugia/town/sellano> crm:P1_is_identified_by <place/country/italia/region/umbria/district/perugia/town/sellano/appellation/preferred_name> .
<place/country/italia/region/umbria/district/perugia/town/sellano> crm:P2_has_type <vocab/meta/town> .
<place/country/italia/region/umbria/district/perugia/town/sellano> crm:P89i_contains <place/country/italia/region/umbria/district/perugia/town/sellano/village/montesanto> .
<place/country/italia/region/umbria/district/perugia/town/sellano> custom:sameAs <http://vocab.getty.edu/tgn/7005052> .
<place/country/italia/region/umbria/district/perugia/town/sellano/appellation/preferred_name> a crm:E41_Appellation .
<place/country/italia/region/umbria/district/perugia/town/sellano/appellation/preferred_name> crm:P190_has_symbolic_content "Sellano" .
<place/country/italia/region/umbria/district/perugia/town/sellano/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<place/country/italia/region/umbria/district/perugia/town/sellano/village/montesanto> a crm:E53_Place .
<place/country/italia/region/umbria/district/perugia/town/sellano/village/montesanto> crm:P1_is_identified_by <place/country/italia/region/umbria/district/perugia/town/sellano/village/montesanto/appellation/preferred_name> .
<place/country/italia/region/umbria/district/perugia/town/sellano/village/montesanto> crm:P2_has_type <vocab/meta/village> .
<place/country/italia/region/umbria/district/perugia/town/sellano/village/montesanto> crm:P55i_currently_holds <work/12112> .
<place/country/italia/region/umbria/district/perugia/town/sellano/village/montesanto/appellation/preferred_name> a crm:E41_Appellation .
<place/country/italia/region/umbria/district/perugia/town/sellano/village/montesanto/appellation/preferred_name> crm:P190_has_symbolic_content "Montesanto" .
<place/country/italia/region/umbria/district/perugia/town/sellano/village/montesanto/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<work/12112> a crm:E22_Human-Made_Object .
|]

  it "should process PVCS (Country) -> PVCR (Region) -> PVCP (District) -> PVCL (Village, no Town)" $ do
    let mapping = baseMapping +> [location]
    let zeriXml = [w|
      <RISULTATI>
        <SCHEDA sercdoa="12437">
          <PARAGRAFO etichetta="LOCATION">
            <PVCS>Italia</PVCS>
            <PVCR>Toscana</PVCR>
            <PVCP>Firenze</PVCP>
            <PVCL>Vigliano</PVCL>
          </PARAGRAFO>
        </SCHEDA>
      </RISULTATI>
    |]
    result <- processXMLStringAsSet mapping zeriBaseUri zeriXml
    result `shouldBe` [t|
<http://vocab.getty.edu/tgn/1000080> a crm:E53_Place .
<http://vocab.getty.edu/tgn/7003163> a crm:E53_Place .
<http://vocab.getty.edu/tgn/7009760> a crm:E53_Place .
<place/country/italia> a crm:E53_Place .
<place/country/italia> crm:P1_is_identified_by <place/country/italia/appellation/preferred_name> .
<place/country/italia> crm:P2_has_type pharos-meta:country .
<place/country/italia> crm:P89i_contains <place/country/italia/region/toscana> .
<place/country/italia> custom:sameAs <http://vocab.getty.edu/tgn/1000080> .
<place/country/italia/appellation/preferred_name> a crm:E41_Appellation .
<place/country/italia/appellation/preferred_name> crm:P190_has_symbolic_content "Italia" .
<place/country/italia/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<place/country/italia/region/toscana> a crm:E53_Place .
<place/country/italia/region/toscana> crm:P1_is_identified_by <place/country/italia/region/toscana/appellation/preferred_name> .
<place/country/italia/region/toscana> crm:P2_has_type <vocab/meta/region> .
<place/country/italia/region/toscana> crm:P89i_contains <place/country/italia/region/toscana/district/firenze> .
<place/country/italia/region/toscana> custom:sameAs <http://vocab.getty.edu/tgn/7009760> .
<place/country/italia/region/toscana/appellation/preferred_name> a crm:E41_Appellation .
<place/country/italia/region/toscana/appellation/preferred_name> crm:P190_has_symbolic_content "Toscana" .
<place/country/italia/region/toscana/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<place/country/italia/region/toscana/district/firenze> a crm:E53_Place .
<place/country/italia/region/toscana/district/firenze> crm:P1_is_identified_by <place/country/italia/region/toscana/district/firenze/appellation/preferred_name> .
<place/country/italia/region/toscana/district/firenze> crm:P2_has_type <vocab/meta/district> .
<place/country/italia/region/toscana/district/firenze> crm:P89i_contains <place/country/italia/region/toscana/district/firenze/village/vigliano> .
<place/country/italia/region/toscana/district/firenze> custom:sameAs <http://vocab.getty.edu/tgn/7003163> .
<place/country/italia/region/toscana/district/firenze/appellation/preferred_name> a crm:E41_Appellation .
<place/country/italia/region/toscana/district/firenze/appellation/preferred_name> crm:P190_has_symbolic_content "Firenze" .
<place/country/italia/region/toscana/district/firenze/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<place/country/italia/region/toscana/district/firenze/village/vigliano> a crm:E53_Place .
<place/country/italia/region/toscana/district/firenze/village/vigliano> crm:P1_is_identified_by <place/country/italia/region/toscana/district/firenze/village/vigliano/appellation/preferred_name> .
<place/country/italia/region/toscana/district/firenze/village/vigliano> crm:P2_has_type <vocab/meta/village> .
<place/country/italia/region/toscana/district/firenze/village/vigliano> crm:P55i_currently_holds <work/12437> .
<place/country/italia/region/toscana/district/firenze/village/vigliano/appellation/preferred_name> a crm:E41_Appellation .
<place/country/italia/region/toscana/district/firenze/village/vigliano/appellation/preferred_name> crm:P190_has_symbolic_content "Vigliano" .
<place/country/italia/region/toscana/district/firenze/village/vigliano/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<work/12437> a crm:E22_Human-Made_Object .
|]

  it "should process PVCS (Country) -> PVCR (Region) -> PVCC (Town, no District)" $ do
    let mapping = baseMapping +> [location]
    let zeriXml = [w|
      <RISULTATI>
        <SCHEDA sercdoa="10284">
          <PARAGRAFO etichetta="LOCATION">
            <PVCS>Italia</PVCS>
            <PVCR>Toscana</PVCR>
            <PVCC>Siena</PVCC>
          </PARAGRAFO>
        </SCHEDA>
      </RISULTATI>
    |]
    result <- processXMLStringAsSet mapping zeriBaseUri zeriXml
    result `shouldBe` [t|
<http://vocab.getty.edu/tgn/1000080> a crm:E53_Place .
<http://vocab.getty.edu/tgn/7009760> a crm:E53_Place .
<http://vocab.getty.edu/tgn/7011179> a crm:E53_Place .
<place/country/italia> a crm:E53_Place .
<place/country/italia> crm:P1_is_identified_by <place/country/italia/appellation/preferred_name> .
<place/country/italia> crm:P2_has_type pharos-meta:country .
<place/country/italia> crm:P89i_contains <place/country/italia/region/toscana> .
<place/country/italia> custom:sameAs <http://vocab.getty.edu/tgn/1000080> .
<place/country/italia/appellation/preferred_name> a crm:E41_Appellation .
<place/country/italia/appellation/preferred_name> crm:P190_has_symbolic_content "Italia" .
<place/country/italia/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<place/country/italia/region/toscana> a crm:E53_Place .
<place/country/italia/region/toscana> crm:P1_is_identified_by <place/country/italia/region/toscana/appellation/preferred_name> .
<place/country/italia/region/toscana> crm:P2_has_type <vocab/meta/region> .
<place/country/italia/region/toscana> crm:P89i_contains <place/country/italia/region/toscana/town/siena> .
<place/country/italia/region/toscana> custom:sameAs <http://vocab.getty.edu/tgn/7009760> .
<place/country/italia/region/toscana/appellation/preferred_name> a crm:E41_Appellation .
<place/country/italia/region/toscana/appellation/preferred_name> crm:P190_has_symbolic_content "Toscana" .
<place/country/italia/region/toscana/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<place/country/italia/region/toscana/town/siena> a crm:E53_Place .
<place/country/italia/region/toscana/town/siena> crm:P1_is_identified_by <place/country/italia/region/toscana/town/siena/appellation/preferred_name> .
<place/country/italia/region/toscana/town/siena> crm:P2_has_type <vocab/meta/town> .
<place/country/italia/region/toscana/town/siena> crm:P55i_currently_holds <work/10284> .
<place/country/italia/region/toscana/town/siena> custom:sameAs <http://vocab.getty.edu/tgn/7011179> .
<place/country/italia/region/toscana/town/siena/appellation/preferred_name> a crm:E41_Appellation .
<place/country/italia/region/toscana/town/siena/appellation/preferred_name> crm:P190_has_symbolic_content "Siena" .
<place/country/italia/region/toscana/town/siena/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<work/10284> a crm:E22_Human-Made_Object .
|]

  it "should process PVCS (Country) -> PVCR (Region) -> PVCC (Town) -> PVCL (Village, no District)" $ do
    let mapping = baseMapping +> [location]
    let zeriXml = [w|
      <RISULTATI>
        <SCHEDA sercdoa="21799">
          <PARAGRAFO etichetta="LOCATION">
            <PVCS>Stati Uniti d'America</PVCS>
            <PVCR>New York</PVCR>
            <PVCC>New York (NY)</PVCC>
            <PVCL>Queens</PVCL>
          </PARAGRAFO>
        </SCHEDA>
      </RISULTATI>
    |]
    result <- processXMLStringAsSet mapping zeriBaseUri zeriXml
    result `shouldBe` [t|
<http://vocab.getty.edu/tgn/1002814> a crm:E53_Place .
<http://vocab.getty.edu/tgn/7007567> a crm:E53_Place .
<http://vocab.getty.edu/tgn/7007568> a crm:E53_Place .
<http://vocab.getty.edu/tgn/7012149> a crm:E53_Place .
<place/country/stati_uniti_d_america> a crm:E53_Place .
<place/country/stati_uniti_d_america> crm:P1_is_identified_by <place/country/stati_uniti_d_america/appellation/preferred_name> .
<place/country/stati_uniti_d_america> crm:P2_has_type pharos-meta:country .
<place/country/stati_uniti_d_america> crm:P89i_contains <place/country/stati_uniti_d_america/region/new_york> .
<place/country/stati_uniti_d_america> custom:sameAs <http://vocab.getty.edu/tgn/7012149> .
<place/country/stati_uniti_d_america/appellation/preferred_name> a crm:E41_Appellation .
<place/country/stati_uniti_d_america/appellation/preferred_name> crm:P190_has_symbolic_content "Stati Uniti d'America" .
<place/country/stati_uniti_d_america/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<place/country/stati_uniti_d_america/region/new_york> a crm:E53_Place .
<place/country/stati_uniti_d_america/region/new_york> crm:P1_is_identified_by <place/country/stati_uniti_d_america/region/new_york/appellation/preferred_name> .
<place/country/stati_uniti_d_america/region/new_york> crm:P2_has_type <vocab/meta/region> .
<place/country/stati_uniti_d_america/region/new_york> crm:P89i_contains <place/country/stati_uniti_d_america/region/new_york/town/new_york_ny> .
<place/country/stati_uniti_d_america/region/new_york> custom:sameAs <http://vocab.getty.edu/tgn/7007568> .
<place/country/stati_uniti_d_america/region/new_york/appellation/preferred_name> a crm:E41_Appellation .
<place/country/stati_uniti_d_america/region/new_york/appellation/preferred_name> crm:P190_has_symbolic_content "New York" .
<place/country/stati_uniti_d_america/region/new_york/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<place/country/stati_uniti_d_america/region/new_york/town/new_york_ny> a crm:E53_Place .
<place/country/stati_uniti_d_america/region/new_york/town/new_york_ny> crm:P1_is_identified_by <place/country/stati_uniti_d_america/region/new_york/town/new_york_ny/appellation/preferred_name> .
<place/country/stati_uniti_d_america/region/new_york/town/new_york_ny> crm:P2_has_type <vocab/meta/town> .
<place/country/stati_uniti_d_america/region/new_york/town/new_york_ny> crm:P89i_contains <place/country/stati_uniti_d_america/region/new_york/town/new_york_ny/village/queens> .
<place/country/stati_uniti_d_america/region/new_york/town/new_york_ny> custom:sameAs <http://vocab.getty.edu/tgn/7007567> .
<place/country/stati_uniti_d_america/region/new_york/town/new_york_ny/appellation/preferred_name> a crm:E41_Appellation .
<place/country/stati_uniti_d_america/region/new_york/town/new_york_ny/appellation/preferred_name> crm:P190_has_symbolic_content "New York (NY)" .
<place/country/stati_uniti_d_america/region/new_york/town/new_york_ny/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<place/country/stati_uniti_d_america/region/new_york/town/new_york_ny/village/queens> a crm:E53_Place .
<place/country/stati_uniti_d_america/region/new_york/town/new_york_ny/village/queens> crm:P1_is_identified_by <place/country/stati_uniti_d_america/region/new_york/town/new_york_ny/village/queens/appellation/preferred_name> .
<place/country/stati_uniti_d_america/region/new_york/town/new_york_ny/village/queens> crm:P2_has_type <vocab/meta/village> .
<place/country/stati_uniti_d_america/region/new_york/town/new_york_ny/village/queens> crm:P55i_currently_holds <work/21799> .
<place/country/stati_uniti_d_america/region/new_york/town/new_york_ny/village/queens> custom:sameAs <http://vocab.getty.edu/tgn/1002814> .
<place/country/stati_uniti_d_america/region/new_york/town/new_york_ny/village/queens/appellation/preferred_name> a crm:E41_Appellation .
<place/country/stati_uniti_d_america/region/new_york/town/new_york_ny/village/queens/appellation/preferred_name> crm:P190_has_symbolic_content "Queens" .
<place/country/stati_uniti_d_america/region/new_york/town/new_york_ny/village/queens/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<work/21799> a crm:E22_Human-Made_Object .
|]

  it "should process PVCS (Country) -> PVCC (Town, no Region/District)" $ do
    let mapping = baseMapping +> [location]
    let zeriXml = [w|
      <RISULTATI>
        <SCHEDA sercdoa="100027">
          <PARAGRAFO etichetta="LOCATION">
            <PVCS>Messico</PVCS>
            <PVCC>Città del Messico</PVCC>
          </PARAGRAFO>
        </SCHEDA>
      </RISULTATI>
    |]
    result <- processXMLStringAsSet mapping zeriBaseUri zeriXml
    result `shouldBe` [t|
<http://vocab.getty.edu/tgn/7005560> a crm:E53_Place .
<http://vocab.getty.edu/tgn/7007227> a crm:E53_Place .
<place/country/messico> a crm:E53_Place .
<place/country/messico> crm:P1_is_identified_by <place/country/messico/appellation/preferred_name> .
<place/country/messico> crm:P2_has_type pharos-meta:country .
<place/country/messico> crm:P89i_contains <place/country/messico/town/citta_del_messico> .
<place/country/messico> custom:sameAs <http://vocab.getty.edu/tgn/7005560> .
<place/country/messico/appellation/preferred_name> a crm:E41_Appellation .
<place/country/messico/appellation/preferred_name> crm:P190_has_symbolic_content "Messico" .
<place/country/messico/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<place/country/messico/town/citta_del_messico> a crm:E53_Place .
<place/country/messico/town/citta_del_messico> crm:P1_is_identified_by <place/country/messico/town/citta_del_messico/appellation/preferred_name> .
<place/country/messico/town/citta_del_messico> crm:P2_has_type <vocab/meta/town> .
<place/country/messico/town/citta_del_messico> crm:P55i_currently_holds <work/100027> .
<place/country/messico/town/citta_del_messico> custom:sameAs <http://vocab.getty.edu/tgn/7007227> .
<place/country/messico/town/citta_del_messico/appellation/preferred_name> a crm:E41_Appellation .
<place/country/messico/town/citta_del_messico/appellation/preferred_name> crm:P190_has_symbolic_content "Città del Messico" .
<place/country/messico/town/citta_del_messico/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<work/100027> a crm:E22_Human-Made_Object .
|]

  it "should process PVCS (Country) -> PVCC (Town) -> PVCL (Village, no Region/District)" $ do
    let mapping = baseMapping +> [location]
    let zeriXml = [w|
      <RISULTATI>
        <SCHEDA sercdoa="28307">
          <PARAGRAFO etichetta="LOCATION">
            <PVCS>Regno Unito</PVCS>
            <PVCC>Londra</PVCC>
            <PVCL>Richmond</PVCL>
          </PARAGRAFO>
        </SCHEDA>
      </RISULTATI>
    |]
    result <- processXMLStringAsSet mapping zeriBaseUri zeriXml
    result `shouldBe` [t|
<http://vocab.getty.edu/tgn/7008136> a crm:E53_Place .
<http://vocab.getty.edu/tgn/7008591> a crm:E53_Place .
<http://vocab.getty.edu/tgn/7012405> a crm:E53_Place .
<place/country/regno_unito> a crm:E53_Place .
<place/country/regno_unito> crm:P1_is_identified_by <place/country/regno_unito/appellation/preferred_name> .
<place/country/regno_unito> crm:P2_has_type pharos-meta:country .
<place/country/regno_unito> crm:P89i_contains <place/country/regno_unito/town/londra> .
<place/country/regno_unito> custom:sameAs <http://vocab.getty.edu/tgn/7008591> .
<place/country/regno_unito/appellation/preferred_name> a crm:E41_Appellation .
<place/country/regno_unito/appellation/preferred_name> crm:P190_has_symbolic_content "Regno Unito" .
<place/country/regno_unito/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<place/country/regno_unito/town/londra> a crm:E53_Place .
<place/country/regno_unito/town/londra> crm:P1_is_identified_by <place/country/regno_unito/town/londra/appellation/preferred_name> .
<place/country/regno_unito/town/londra> crm:P2_has_type <vocab/meta/town> .
<place/country/regno_unito/town/londra> crm:P89i_contains <place/country/regno_unito/town/londra/village/richmond> .
<place/country/regno_unito/town/londra> custom:sameAs <http://vocab.getty.edu/tgn/7008136> .
<place/country/regno_unito/town/londra/appellation/preferred_name> a crm:E41_Appellation .
<place/country/regno_unito/town/londra/appellation/preferred_name> crm:P190_has_symbolic_content "Londra" .
<place/country/regno_unito/town/londra/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<place/country/regno_unito/town/londra/village/richmond> a crm:E53_Place .
<place/country/regno_unito/town/londra/village/richmond> crm:P1_is_identified_by <place/country/regno_unito/town/londra/village/richmond/appellation/preferred_name> .
<place/country/regno_unito/town/londra/village/richmond> crm:P2_has_type <vocab/meta/village> .
<place/country/regno_unito/town/londra/village/richmond> crm:P55i_currently_holds <work/28307> .
<place/country/regno_unito/town/londra/village/richmond> custom:sameAs <http://vocab.getty.edu/tgn/7012405> .
<place/country/regno_unito/town/londra/village/richmond/appellation/preferred_name> a crm:E41_Appellation .
<place/country/regno_unito/town/londra/village/richmond/appellation/preferred_name> crm:P190_has_symbolic_content "Richmond" .
<place/country/regno_unito/town/londra/village/richmond/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<work/28307> a crm:E22_Human-Made_Object .
|]

  it "should process LCDN when value is institution (M)" $ do
    -- in this test we also check that institution URI is relative to the most specific place
    -- in this case Londra (London)
    let mapping = baseMapping +> [location]
    let zeriXml = [w|
      <RISULTATI>
        <SCHEDA sercdoa="106811">
          <PARAGRAFO etichetta="LOCATION">
            <PVCS etichetta="Country">Regno Unito</PVCS>
            <PVCC etichetta="Town / Municipality">Londra</PVCC>
            <LDCN etichetta="Repository">The British Museum</LDCN>
          </PARAGRAFO>
        </SCHEDA>
      </RISULTATI>
    |]
    result <- processXMLStringAsSet mapping zeriBaseUri zeriXml
    result `shouldBe` [t|
<http://vocab.getty.edu/tgn/7008136> a crm:E53_Place .
<http://vocab.getty.edu/tgn/7008591> a crm:E53_Place .
<http://www.wikidata.org/entity/Q6373> a crm:E53_Place .
<place/country/regno_unito> a crm:E53_Place .
<place/country/regno_unito> crm:P1_is_identified_by <place/country/regno_unito/appellation/preferred_name> .
<place/country/regno_unito> crm:P2_has_type pharos-meta:country .
<place/country/regno_unito> crm:P89i_contains <place/country/regno_unito/town/londra> .
<place/country/regno_unito> custom:sameAs <http://vocab.getty.edu/tgn/7008591> .
<place/country/regno_unito/appellation/preferred_name> a crm:E41_Appellation .
<place/country/regno_unito/appellation/preferred_name> crm:P190_has_symbolic_content "Regno Unito" .
<place/country/regno_unito/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<place/country/regno_unito/town/londra> a crm:E53_Place .
<place/country/regno_unito/town/londra> crm:P1_is_identified_by <place/country/regno_unito/town/londra/appellation/preferred_name> .
<place/country/regno_unito/town/londra> crm:P2_has_type <vocab/meta/town> .
<place/country/regno_unito/town/londra> crm:P74i_is_current_or_former_residence_of <place/country/regno_unito/town/londra/actor/repository_institution/the_british_museum> .
<place/country/regno_unito/town/londra> custom:sameAs <http://vocab.getty.edu/tgn/7008136> .
<place/country/regno_unito/town/londra/actor/repository_institution/the_british_museum> a crm:E39_Actor .
<place/country/regno_unito/town/londra/actor/repository_institution/the_british_museum> crm:P1_is_identified_by <place/country/regno_unito/town/londra/actor/repository_institution/the_british_museum/appellation/preferred_name> .
<place/country/regno_unito/town/londra/actor/repository_institution/the_british_museum> crm:P2_has_type <vocab/meta/repository_institution> .
<place/country/regno_unito/town/londra/actor/repository_institution/the_british_museum> crm:P50i_is_current_keeper_of <work/106811> .
<place/country/regno_unito/town/londra/actor/repository_institution/the_british_museum> custom:sameAs <http://www.wikidata.org/entity/Q6373> .
<place/country/regno_unito/town/londra/actor/repository_institution/the_british_museum/appellation/preferred_name> a crm:E41_Appellation .
<place/country/regno_unito/town/londra/actor/repository_institution/the_british_museum/appellation/preferred_name> crm:P190_has_symbolic_content "The British Museum" .
<place/country/regno_unito/town/londra/actor/repository_institution/the_british_museum/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<place/country/regno_unito/town/londra/appellation/preferred_name> a crm:E41_Appellation .
<place/country/regno_unito/town/londra/appellation/preferred_name> crm:P190_has_symbolic_content "Londra" .
<place/country/regno_unito/town/londra/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<work/106811> a crm:E22_Human-Made_Object .
|]

  it "should process LCDN when value is actor (A)" $ do
    let mapping = baseMapping +> [location]
    let zeriXml = [w|
      <RISULTATI>
        <SCHEDA sercdoa="16393">
          <PARAGRAFO etichetta="LOCATION">
            <PVCS etichetta="Country">Paesi Bassi</PVCS>
            <PVCC etichetta="Town / Municipality">L'Aja</PVCC>
            <LDCN etichetta="Repository">A.B. van Hengel</LDCN>
            <LDCS etichetta="Precise location">segnalato nel 1965</LDCS>
          </PARAGRAFO>
        </SCHEDA>
      </RISULTATI>
    |]
    result <- processXMLStringAsSet mapping zeriBaseUri zeriXml
    result `shouldBe` [t|
<http://vocab.getty.edu/tgn/7006810> a crm:E53_Place .
<http://vocab.getty.edu/tgn/7016845> a crm:E53_Place .
<place/country/paesi_bassi> a crm:E53_Place .
<place/country/paesi_bassi> crm:P1_is_identified_by <place/country/paesi_bassi/appellation/preferred_name> .
<place/country/paesi_bassi> crm:P2_has_type pharos-meta:country .
<place/country/paesi_bassi> crm:P89i_contains <place/country/paesi_bassi/town/l_aja> .
<place/country/paesi_bassi> custom:sameAs <http://vocab.getty.edu/tgn/7016845> .
<place/country/paesi_bassi/appellation/preferred_name> a crm:E41_Appellation .
<place/country/paesi_bassi/appellation/preferred_name> crm:P190_has_symbolic_content "Paesi Bassi" .
<place/country/paesi_bassi/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<place/country/paesi_bassi/town/l_aja> a crm:E53_Place .
<place/country/paesi_bassi/town/l_aja> crm:P1_is_identified_by <place/country/paesi_bassi/town/l_aja/appellation/preferred_name> .
<place/country/paesi_bassi/town/l_aja> crm:P2_has_type <vocab/meta/town> .
<place/country/paesi_bassi/town/l_aja> crm:P74i_is_current_or_former_residence_of <place/country/paesi_bassi/town/l_aja/actor/repository_actor/a_b_van_hengel> .
<place/country/paesi_bassi/town/l_aja> custom:sameAs <http://vocab.getty.edu/tgn/7006810> .
<place/country/paesi_bassi/town/l_aja/actor/repository_actor/a_b_van_hengel> a crm:E39_Actor .
<place/country/paesi_bassi/town/l_aja/actor/repository_actor/a_b_van_hengel> crm:P1_is_identified_by <place/country/paesi_bassi/town/l_aja/actor/repository_actor/a_b_van_hengel/appellation/preferred_name> .
<place/country/paesi_bassi/town/l_aja/actor/repository_actor/a_b_van_hengel> crm:P2_has_type <vocab/meta/repository_actor> .
<place/country/paesi_bassi/town/l_aja/actor/repository_actor/a_b_van_hengel> crm:P50i_is_current_keeper_of <work/16393> .
<place/country/paesi_bassi/town/l_aja/actor/repository_actor/a_b_van_hengel/appellation/preferred_name> a crm:E41_Appellation .
<place/country/paesi_bassi/town/l_aja/actor/repository_actor/a_b_van_hengel/appellation/preferred_name> crm:P190_has_symbolic_content "A.B. van Hengel" .
<place/country/paesi_bassi/town/l_aja/actor/repository_actor/a_b_van_hengel/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<place/country/paesi_bassi/town/l_aja/appellation/preferred_name> a crm:E41_Appellation .
<place/country/paesi_bassi/town/l_aja/appellation/preferred_name> crm:P190_has_symbolic_content "L'Aja" .
<place/country/paesi_bassi/town/l_aja/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<work/16393> a crm:E22_Human-Made_Object .
|]

  it "should process LCDN when value is action house (AH)" $ do
    let mapping = baseMapping +> [location]
    let zeriXml = [w|
      <RISULTATI>
        <SCHEDA sercdoa="8280">
          <PARAGRAFO etichetta="LOCATION">
            <PVCS etichetta="Country">Stati Uniti d&#039;America</PVCS>
            <PVCR etichetta="Region / Federal State">New York</PVCR>
            <PVCC etichetta="Town / Municipality">New York (NY)</PVCC>
            <LDCN etichetta="Repository">American Art Galleries (?)</LDCN>
          </PARAGRAFO>
        </SCHEDA>
      </RISULTATI>
    |]
    result <- processXMLStringAsSet mapping zeriBaseUri zeriXml
    result `shouldBe` [t|
<http://vocab.getty.edu/tgn/7007567> a crm:E53_Place .
<http://vocab.getty.edu/tgn/7007568> a crm:E53_Place .
<http://vocab.getty.edu/tgn/7012149> a crm:E53_Place .
<place/country/stati_uniti_d_america> a crm:E53_Place .
<place/country/stati_uniti_d_america> crm:P1_is_identified_by <place/country/stati_uniti_d_america/appellation/preferred_name> .
<place/country/stati_uniti_d_america> crm:P2_has_type pharos-meta:country .
<place/country/stati_uniti_d_america> crm:P89i_contains <place/country/stati_uniti_d_america/region/new_york> .
<place/country/stati_uniti_d_america> custom:sameAs <http://vocab.getty.edu/tgn/7012149> .
<place/country/stati_uniti_d_america/appellation/preferred_name> a crm:E41_Appellation .
<place/country/stati_uniti_d_america/appellation/preferred_name> crm:P190_has_symbolic_content "Stati Uniti d'America" .
<place/country/stati_uniti_d_america/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<place/country/stati_uniti_d_america/region/new_york> a crm:E53_Place .
<place/country/stati_uniti_d_america/region/new_york> crm:P1_is_identified_by <place/country/stati_uniti_d_america/region/new_york/appellation/preferred_name> .
<place/country/stati_uniti_d_america/region/new_york> crm:P2_has_type <vocab/meta/region> .
<place/country/stati_uniti_d_america/region/new_york> crm:P89i_contains <place/country/stati_uniti_d_america/region/new_york/town/new_york_ny> .
<place/country/stati_uniti_d_america/region/new_york> custom:sameAs <http://vocab.getty.edu/tgn/7007568> .
<place/country/stati_uniti_d_america/region/new_york/appellation/preferred_name> a crm:E41_Appellation .
<place/country/stati_uniti_d_america/region/new_york/appellation/preferred_name> crm:P190_has_symbolic_content "New York" .
<place/country/stati_uniti_d_america/region/new_york/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<place/country/stati_uniti_d_america/region/new_york/town/new_york_ny> a crm:E53_Place .
<place/country/stati_uniti_d_america/region/new_york/town/new_york_ny> crm:P1_is_identified_by <place/country/stati_uniti_d_america/region/new_york/town/new_york_ny/appellation/preferred_name> .
<place/country/stati_uniti_d_america/region/new_york/town/new_york_ny> crm:P2_has_type <vocab/meta/town> .
<place/country/stati_uniti_d_america/region/new_york/town/new_york_ny> crm:P74i_is_current_or_former_residence_of <place/country/stati_uniti_d_america/region/new_york/town/new_york_ny/actor/repository_actor/american_art_galleries_q> .
<place/country/stati_uniti_d_america/region/new_york/town/new_york_ny> custom:sameAs <http://vocab.getty.edu/tgn/7007567> .
<place/country/stati_uniti_d_america/region/new_york/town/new_york_ny/actor/repository_actor/american_art_galleries_q> a crm:E39_Actor .
<place/country/stati_uniti_d_america/region/new_york/town/new_york_ny/actor/repository_actor/american_art_galleries_q> crm:P1_is_identified_by <place/country/stati_uniti_d_america/region/new_york/town/new_york_ny/actor/repository_actor/american_art_galleries_q/appellation/preferred_name> .
<place/country/stati_uniti_d_america/region/new_york/town/new_york_ny/actor/repository_actor/american_art_galleries_q> crm:P2_has_type <vocab/meta/repository_actor> .
<place/country/stati_uniti_d_america/region/new_york/town/new_york_ny/actor/repository_actor/american_art_galleries_q> crm:P50i_is_current_keeper_of <work/8280> .
<place/country/stati_uniti_d_america/region/new_york/town/new_york_ny/actor/repository_actor/american_art_galleries_q/appellation/preferred_name> a crm:E41_Appellation .
<place/country/stati_uniti_d_america/region/new_york/town/new_york_ny/actor/repository_actor/american_art_galleries_q/appellation/preferred_name> crm:P190_has_symbolic_content "American Art Galleries (?)" .
<place/country/stati_uniti_d_america/region/new_york/town/new_york_ny/actor/repository_actor/american_art_galleries_q/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<place/country/stati_uniti_d_america/region/new_york/town/new_york_ny/appellation/preferred_name> a crm:E41_Appellation .
<place/country/stati_uniti_d_america/region/new_york/town/new_york_ny/appellation/preferred_name> crm:P190_has_symbolic_content "New York (NY)" .
<place/country/stati_uniti_d_america/region/new_york/town/new_york_ny/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<work/8280> a crm:E22_Human-Made_Object .
|]


  it "should process LCDN when value is collection (C)" $ do
    let mapping = baseMapping +> [location]
    let zeriXml = [w|
      <RISULTATI>
        <SCHEDA sercdoa="42278">
          <PARAGRAFO etichetta="LOCATION">
            <PVCS etichetta="Country">Italia</PVCS>
            <PVCR etichetta="Region / Federal State">Lombardia</PVCR>
            <PVCP etichetta="District">Sondrio</PVCP>
            <PVCC etichetta="Town / Municipality">Sondrio</PVCC>
            <LDCN etichetta="Repository">Banca Popolare di Sondrio</LDCN>
          </PARAGRAFO>
        </SCHEDA>
      </RISULTATI>
    |]
    result <- processXMLStringAsSet mapping zeriBaseUri zeriXml
    result `shouldBe` [t|
<http://vocab.getty.edu/tgn/1000080> a crm:E53_Place .
<http://vocab.getty.edu/tgn/7003152> a crm:E53_Place .
<http://vocab.getty.edu/tgn/7003237> a crm:E53_Place .
<http://vocab.getty.edu/tgn/7006032> a crm:E53_Place .
<place/country/italia> a crm:E53_Place .
<place/country/italia> crm:P1_is_identified_by <place/country/italia/appellation/preferred_name> .
<place/country/italia> crm:P2_has_type pharos-meta:country .
<place/country/italia> crm:P89i_contains <place/country/italia/region/lombardia> .
<place/country/italia> custom:sameAs <http://vocab.getty.edu/tgn/1000080> .
<place/country/italia/appellation/preferred_name> a crm:E41_Appellation .
<place/country/italia/appellation/preferred_name> crm:P190_has_symbolic_content "Italia" .
<place/country/italia/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<place/country/italia/region/lombardia> a crm:E53_Place .
<place/country/italia/region/lombardia> crm:P1_is_identified_by <place/country/italia/region/lombardia/appellation/preferred_name> .
<place/country/italia/region/lombardia> crm:P2_has_type <vocab/meta/region> .
<place/country/italia/region/lombardia> crm:P89i_contains <place/country/italia/region/lombardia/district/sondrio> .
<place/country/italia/region/lombardia> custom:sameAs <http://vocab.getty.edu/tgn/7003237> .
<place/country/italia/region/lombardia/appellation/preferred_name> a crm:E41_Appellation .
<place/country/italia/region/lombardia/appellation/preferred_name> crm:P190_has_symbolic_content "Lombardia" .
<place/country/italia/region/lombardia/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<place/country/italia/region/lombardia/district/sondrio> a crm:E53_Place .
<place/country/italia/region/lombardia/district/sondrio> crm:P1_is_identified_by <place/country/italia/region/lombardia/district/sondrio/appellation/preferred_name> .
<place/country/italia/region/lombardia/district/sondrio> crm:P2_has_type <vocab/meta/district> .
<place/country/italia/region/lombardia/district/sondrio> crm:P89i_contains <place/country/italia/region/lombardia/district/sondrio/town/sondrio> .
<place/country/italia/region/lombardia/district/sondrio> custom:sameAs <http://vocab.getty.edu/tgn/7003152> .
<place/country/italia/region/lombardia/district/sondrio/appellation/preferred_name> a crm:E41_Appellation .
<place/country/italia/region/lombardia/district/sondrio/appellation/preferred_name> crm:P190_has_symbolic_content "Sondrio" .
<place/country/italia/region/lombardia/district/sondrio/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<place/country/italia/region/lombardia/district/sondrio/town/sondrio> a crm:E53_Place .
<place/country/italia/region/lombardia/district/sondrio/town/sondrio> crm:P1_is_identified_by <place/country/italia/region/lombardia/district/sondrio/town/sondrio/appellation/preferred_name> .
<place/country/italia/region/lombardia/district/sondrio/town/sondrio> crm:P2_has_type <vocab/meta/town> .
<place/country/italia/region/lombardia/district/sondrio/town/sondrio> crm:P55i_currently_holds <place/country/italia/region/lombardia/district/sondrio/town/sondrio/collection/banca_popolare_di_sondrio> .
<place/country/italia/region/lombardia/district/sondrio/town/sondrio> custom:sameAs <http://vocab.getty.edu/tgn/7006032> .
<place/country/italia/region/lombardia/district/sondrio/town/sondrio/appellation/preferred_name> a crm:E41_Appellation .
<place/country/italia/region/lombardia/district/sondrio/town/sondrio/appellation/preferred_name> crm:P190_has_symbolic_content "Sondrio" .
<place/country/italia/region/lombardia/district/sondrio/town/sondrio/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<place/country/italia/region/lombardia/district/sondrio/town/sondrio/collection/banca_popolare_di_sondrio> a crm:E22_Human-Made_Object .
<place/country/italia/region/lombardia/district/sondrio/town/sondrio/collection/banca_popolare_di_sondrio> crm:P1_is_identified_by <place/country/italia/region/lombardia/district/sondrio/town/sondrio/collection/banca_popolare_di_sondrio/appellation/preferred_name> .
<place/country/italia/region/lombardia/district/sondrio/town/sondrio/collection/banca_popolare_di_sondrio> crm:P2_has_type <vocab/meta/repository_actor> .
<place/country/italia/region/lombardia/district/sondrio/town/sondrio/collection/banca_popolare_di_sondrio> crm:P46_is_composed_of <work/42278> .
<place/country/italia/region/lombardia/district/sondrio/town/sondrio/collection/banca_popolare_di_sondrio/appellation/preferred_name> a crm:E41_Appellation .
<place/country/italia/region/lombardia/district/sondrio/town/sondrio/collection/banca_popolare_di_sondrio/appellation/preferred_name> crm:P190_has_symbolic_content "Banca Popolare di Sondrio" .
<place/country/italia/region/lombardia/district/sondrio/town/sondrio/collection/banca_popolare_di_sondrio/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<work/42278> a crm:E22_Human-Made_Object .
|]

  it "should process LCDN when value is building (B)" $ do
    -- in this test we also check that institution URI is relative to the most specific place
    let mapping = baseMapping +> [location]
    let zeriXml = [w|
      <RISULTATI>
        <SCHEDA sercdoa="57730">
          <PARAGRAFO etichetta="LOCATION">
            <PVCS etichetta="Country">Francia</PVCS>
            <PVCR etichetta="Region / Federal State">Île-de-France</PVCR>
            <PVCC etichetta="Town / Municipality">Parigi</PVCC>
            <LDCN etichetta="Repository">Cathédrale de Notre-Dame</LDCN>
          </PARAGRAFO>
        </SCHEDA>
      </RISULTATI>
    |]
    result <- processXMLStringAsSet mapping zeriBaseUri zeriXml
    result `shouldBe` [t|
<http://vocab.getty.edu/tgn/1000070> a crm:E53_Place .
<http://vocab.getty.edu/tgn/7002883> a crm:E53_Place .
<http://vocab.getty.edu/tgn/7008038> a crm:E53_Place .
<place/country/francia> a crm:E53_Place .
<place/country/francia> crm:P1_is_identified_by <place/country/francia/appellation/preferred_name> .
<place/country/francia> crm:P2_has_type pharos-meta:country .
<place/country/francia> crm:P89i_contains <place/country/francia/region/i_le_de_france> .
<place/country/francia> custom:sameAs <http://vocab.getty.edu/tgn/1000070> .
<place/country/francia/appellation/preferred_name> a crm:E41_Appellation .
<place/country/francia/appellation/preferred_name> crm:P190_has_symbolic_content "Francia" .
<place/country/francia/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<place/country/francia/region/i_le_de_france> a crm:E53_Place .
<place/country/francia/region/i_le_de_france> crm:P1_is_identified_by <place/country/francia/region/i_le_de_france/appellation/preferred_name> .
<place/country/francia/region/i_le_de_france> crm:P2_has_type <vocab/meta/region> .
<place/country/francia/region/i_le_de_france> crm:P89i_contains <place/country/francia/region/i_le_de_france/town/parigi> .
<place/country/francia/region/i_le_de_france> custom:sameAs <http://vocab.getty.edu/tgn/7002883> .
<place/country/francia/region/i_le_de_france/appellation/preferred_name> a crm:E41_Appellation .
<place/country/francia/region/i_le_de_france/appellation/preferred_name> crm:P190_has_symbolic_content "Île-de-France" .
<place/country/francia/region/i_le_de_france/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<place/country/francia/region/i_le_de_france/town/parigi> a crm:E53_Place .
<place/country/francia/region/i_le_de_france/town/parigi> crm:P1_is_identified_by <place/country/francia/region/i_le_de_france/town/parigi/appellation/preferred_name> .
<place/country/francia/region/i_le_de_france/town/parigi> crm:P2_has_type <vocab/meta/town> .
<place/country/francia/region/i_le_de_france/town/parigi> crm:P89i_contains <place/country/francia/region/i_le_de_france/town/parigi/building_place/cathe_drale_de_notre_dame> .
<place/country/francia/region/i_le_de_france/town/parigi> custom:sameAs <http://vocab.getty.edu/tgn/7008038> .
<place/country/francia/region/i_le_de_france/town/parigi/appellation/preferred_name> a crm:E41_Appellation .
<place/country/francia/region/i_le_de_france/town/parigi/appellation/preferred_name> crm:P190_has_symbolic_content "Parigi" .
<place/country/francia/region/i_le_de_france/town/parigi/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<place/country/francia/region/i_le_de_france/town/parigi/building_place/cathe_drale_de_notre_dame> a crm:E53_Place .
<place/country/francia/region/i_le_de_france/town/parigi/building_place/cathe_drale_de_notre_dame> crm:P156i_is_occupied_by <place/country/francia/region/i_le_de_france/town/parigi/building_place/cathe_drale_de_notre_dame/building> .
<place/country/francia/region/i_le_de_france/town/parigi/building_place/cathe_drale_de_notre_dame> crm:P55i_currently_holds <work/57730> .
<place/country/francia/region/i_le_de_france/town/parigi/building_place/cathe_drale_de_notre_dame/building> a crm:E22_Human-Made_Object .
<place/country/francia/region/i_le_de_france/town/parigi/building_place/cathe_drale_de_notre_dame/building> crm:P1_is_identified_by <place/country/francia/region/i_le_de_france/town/parigi/building_place/cathe_drale_de_notre_dame/building/appellation/pharos_preferred_name> .
<place/country/francia/region/i_le_de_france/town/parigi/building_place/cathe_drale_de_notre_dame/building> crm:P1_is_identified_by <place/country/francia/region/i_le_de_france/town/parigi/building_place/cathe_drale_de_notre_dame/building/appellation/preferred_name> .
<place/country/francia/region/i_le_de_france/town/parigi/building_place/cathe_drale_de_notre_dame/building> crm:P2_has_type pharos-meta:built_work .
<place/country/francia/region/i_le_de_france/town/parigi/building_place/cathe_drale_de_notre_dame/building> crm:P46_is_composed_of <work/57730> .
<place/country/francia/region/i_le_de_france/town/parigi/building_place/cathe_drale_de_notre_dame/building/appellation/pharos_preferred_name> a crm:E41_Appellation .
<place/country/francia/region/i_le_de_france/town/parigi/building_place/cathe_drale_de_notre_dame/building/appellation/pharos_preferred_name> crm:P190_has_symbolic_content "Cathédrale de Notre-Dame, Parigi" .
<place/country/francia/region/i_le_de_france/town/parigi/building_place/cathe_drale_de_notre_dame/building/appellation/pharos_preferred_name> crm:P2_has_type pharos-meta:pharos_preferred_name .
<place/country/francia/region/i_le_de_france/town/parigi/building_place/cathe_drale_de_notre_dame/building/appellation/preferred_name> a crm:E41_Appellation .
<place/country/francia/region/i_le_de_france/town/parigi/building_place/cathe_drale_de_notre_dame/building/appellation/preferred_name> crm:P190_has_symbolic_content "Cathédrale de Notre-Dame" .
<place/country/francia/region/i_le_de_france/town/parigi/building_place/cathe_drale_de_notre_dame/building/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<work/57730> a crm:E22_Human-Made_Object .
|]


  it "should process LCDN when value is building (B) in a building (B)" $ do
    let mapping = baseMapping +> [location]
    let zeriXml = [w|
      <RISULTATI>
        <SCHEDA sercdoa="79948">
          <PARAGRAFO etichetta="LOCATION">
            <PVCS etichetta="Country">Italia</PVCS>
            <PVCR etichetta="Region / Federal State">Lazio</PVCR>
            <PVCP etichetta="District">Roma</PVCP>
            <PVCC etichetta="Town / Municipality">Roma</PVCC>
            <LDCN etichetta="Repository">Abbazia delle Tre Fontane, Chiesa di S. Paolo</LDCN>
          </PARAGRAFO>
        </SCHEDA>
      </RISULTATI>
    |]
    result <- processXMLStringAsSet mapping zeriBaseUri zeriXml
    result `shouldBe` [t|
<http://vocab.getty.edu/tgn/1000080> a crm:E53_Place .
<http://vocab.getty.edu/tgn/7000874> a crm:E53_Place .
<http://vocab.getty.edu/tgn/7003080> a crm:E53_Place .
<http://vocab.getty.edu/tgn/7003138> a crm:E53_Place .
<place/country/italia> a crm:E53_Place .
<place/country/italia> crm:P1_is_identified_by <place/country/italia/appellation/preferred_name> .
<place/country/italia> crm:P2_has_type pharos-meta:country .
<place/country/italia> crm:P89i_contains <place/country/italia/region/lazio> .
<place/country/italia> custom:sameAs <http://vocab.getty.edu/tgn/1000080> .
<place/country/italia/appellation/preferred_name> a crm:E41_Appellation .
<place/country/italia/appellation/preferred_name> crm:P190_has_symbolic_content "Italia" .
<place/country/italia/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<place/country/italia/region/lazio> a crm:E53_Place .
<place/country/italia/region/lazio> crm:P1_is_identified_by <place/country/italia/region/lazio/appellation/preferred_name> .
<place/country/italia/region/lazio> crm:P2_has_type <vocab/meta/region> .
<place/country/italia/region/lazio> crm:P89i_contains <place/country/italia/region/lazio/district/roma> .
<place/country/italia/region/lazio> custom:sameAs <http://vocab.getty.edu/tgn/7003080> .
<place/country/italia/region/lazio/appellation/preferred_name> a crm:E41_Appellation .
<place/country/italia/region/lazio/appellation/preferred_name> crm:P190_has_symbolic_content "Lazio" .
<place/country/italia/region/lazio/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<place/country/italia/region/lazio/district/roma> a crm:E53_Place .
<place/country/italia/region/lazio/district/roma> crm:P1_is_identified_by <place/country/italia/region/lazio/district/roma/appellation/preferred_name> .
<place/country/italia/region/lazio/district/roma> crm:P2_has_type <vocab/meta/district> .
<place/country/italia/region/lazio/district/roma> crm:P89i_contains <place/country/italia/region/lazio/district/roma/town/roma> .
<place/country/italia/region/lazio/district/roma> custom:sameAs <http://vocab.getty.edu/tgn/7003138> .
<place/country/italia/region/lazio/district/roma/appellation/preferred_name> a crm:E41_Appellation .
<place/country/italia/region/lazio/district/roma/appellation/preferred_name> crm:P190_has_symbolic_content "Roma" .
<place/country/italia/region/lazio/district/roma/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<place/country/italia/region/lazio/district/roma/town/roma> a crm:E53_Place .
<place/country/italia/region/lazio/district/roma/town/roma> crm:P1_is_identified_by <place/country/italia/region/lazio/district/roma/town/roma/appellation/preferred_name> .
<place/country/italia/region/lazio/district/roma/town/roma> crm:P2_has_type <vocab/meta/town> .
<place/country/italia/region/lazio/district/roma/town/roma> crm:P89i_contains <place/country/italia/region/lazio/district/roma/town/roma/building_place/abbazia_delle_tre_fontane> .
<place/country/italia/region/lazio/district/roma/town/roma> custom:sameAs <http://vocab.getty.edu/tgn/7000874> .
<place/country/italia/region/lazio/district/roma/town/roma/appellation/preferred_name> a crm:E41_Appellation .
<place/country/italia/region/lazio/district/roma/town/roma/appellation/preferred_name> crm:P190_has_symbolic_content "Roma" .
<place/country/italia/region/lazio/district/roma/town/roma/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<place/country/italia/region/lazio/district/roma/town/roma/building_place/abbazia_delle_tre_fontane> a crm:E53_Place .
<place/country/italia/region/lazio/district/roma/town/roma/building_place/abbazia_delle_tre_fontane> crm:P156i_is_occupied_by <place/country/italia/region/lazio/district/roma/town/roma/building_place/abbazia_delle_tre_fontane/building> .
<place/country/italia/region/lazio/district/roma/town/roma/building_place/abbazia_delle_tre_fontane> crm:P89i_contains <place/country/italia/region/lazio/district/roma/town/roma/building_place/abbazia_delle_tre_fontane/building_place/chiesa_di_s_paolo> .
<place/country/italia/region/lazio/district/roma/town/roma/building_place/abbazia_delle_tre_fontane/building> a crm:E22_Human-Made_Object .
<place/country/italia/region/lazio/district/roma/town/roma/building_place/abbazia_delle_tre_fontane/building> crm:P1_is_identified_by <place/country/italia/region/lazio/district/roma/town/roma/building_place/abbazia_delle_tre_fontane/building/appellation/pharos_preferred_name> .
<place/country/italia/region/lazio/district/roma/town/roma/building_place/abbazia_delle_tre_fontane/building> crm:P1_is_identified_by <place/country/italia/region/lazio/district/roma/town/roma/building_place/abbazia_delle_tre_fontane/building/appellation/preferred_name> .
<place/country/italia/region/lazio/district/roma/town/roma/building_place/abbazia_delle_tre_fontane/building> crm:P2_has_type pharos-meta:built_work .
<place/country/italia/region/lazio/district/roma/town/roma/building_place/abbazia_delle_tre_fontane/building> crm:P46_is_composed_of <work/79948> .
<place/country/italia/region/lazio/district/roma/town/roma/building_place/abbazia_delle_tre_fontane/building/appellation/pharos_preferred_name> a crm:E41_Appellation .
<place/country/italia/region/lazio/district/roma/town/roma/building_place/abbazia_delle_tre_fontane/building/appellation/pharos_preferred_name> crm:P190_has_symbolic_content "Abbazia delle Tre Fontane, Roma" .
<place/country/italia/region/lazio/district/roma/town/roma/building_place/abbazia_delle_tre_fontane/building/appellation/pharos_preferred_name> crm:P2_has_type pharos-meta:pharos_preferred_name .
<place/country/italia/region/lazio/district/roma/town/roma/building_place/abbazia_delle_tre_fontane/building/appellation/preferred_name> a crm:E41_Appellation .
<place/country/italia/region/lazio/district/roma/town/roma/building_place/abbazia_delle_tre_fontane/building/appellation/preferred_name> crm:P190_has_symbolic_content "Abbazia delle Tre Fontane" .
<place/country/italia/region/lazio/district/roma/town/roma/building_place/abbazia_delle_tre_fontane/building/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<place/country/italia/region/lazio/district/roma/town/roma/building_place/abbazia_delle_tre_fontane/building_place/chiesa_di_s_paolo> a crm:E53_Place .
<place/country/italia/region/lazio/district/roma/town/roma/building_place/abbazia_delle_tre_fontane/building_place/chiesa_di_s_paolo> crm:P156i_is_occupied_by <place/country/italia/region/lazio/district/roma/town/roma/building_place/abbazia_delle_tre_fontane/building_place/chiesa_di_s_paolo/building> .
<place/country/italia/region/lazio/district/roma/town/roma/building_place/abbazia_delle_tre_fontane/building_place/chiesa_di_s_paolo> crm:P55i_currently_holds <work/79948> .
<place/country/italia/region/lazio/district/roma/town/roma/building_place/abbazia_delle_tre_fontane/building_place/chiesa_di_s_paolo/building> a crm:E22_Human-Made_Object .
<place/country/italia/region/lazio/district/roma/town/roma/building_place/abbazia_delle_tre_fontane/building_place/chiesa_di_s_paolo/building> crm:P1_is_identified_by <place/country/italia/region/lazio/district/roma/town/roma/building_place/abbazia_delle_tre_fontane/building_place/chiesa_di_s_paolo/building/appellation/pharos_preferred_name> .
<place/country/italia/region/lazio/district/roma/town/roma/building_place/abbazia_delle_tre_fontane/building_place/chiesa_di_s_paolo/building> crm:P1_is_identified_by <place/country/italia/region/lazio/district/roma/town/roma/building_place/abbazia_delle_tre_fontane/building_place/chiesa_di_s_paolo/building/appellation/preferred_name> .
<place/country/italia/region/lazio/district/roma/town/roma/building_place/abbazia_delle_tre_fontane/building_place/chiesa_di_s_paolo/building> crm:P2_has_type pharos-meta:built_work .
<place/country/italia/region/lazio/district/roma/town/roma/building_place/abbazia_delle_tre_fontane/building_place/chiesa_di_s_paolo/building> crm:P46_is_composed_of <work/79948> .
<place/country/italia/region/lazio/district/roma/town/roma/building_place/abbazia_delle_tre_fontane/building_place/chiesa_di_s_paolo/building/appellation/pharos_preferred_name> a crm:E41_Appellation .
<place/country/italia/region/lazio/district/roma/town/roma/building_place/abbazia_delle_tre_fontane/building_place/chiesa_di_s_paolo/building/appellation/pharos_preferred_name> crm:P190_has_symbolic_content "Chiesa di S. Paolo, Roma" .
<place/country/italia/region/lazio/district/roma/town/roma/building_place/abbazia_delle_tre_fontane/building_place/chiesa_di_s_paolo/building/appellation/pharos_preferred_name> crm:P2_has_type pharos-meta:pharos_preferred_name .
<place/country/italia/region/lazio/district/roma/town/roma/building_place/abbazia_delle_tre_fontane/building_place/chiesa_di_s_paolo/building/appellation/preferred_name> a crm:E41_Appellation .
<place/country/italia/region/lazio/district/roma/town/roma/building_place/abbazia_delle_tre_fontane/building_place/chiesa_di_s_paolo/building/appellation/preferred_name> crm:P190_has_symbolic_content "Chiesa di S. Paolo" .
<place/country/italia/region/lazio/district/roma/town/roma/building_place/abbazia_delle_tre_fontane/building_place/chiesa_di_s_paolo/building/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<work/79948> a crm:E22_Human-Made_Object .
|]

  it "monaco lookup bug" $ do
    -- zeri sometimes use non-breaking spaces in values
    -- this test checks that we normalize them to regular spaces
    -- and then reconciliation lookup works as expected
    let mapping = baseMapping +> [location]
    let zeriXml = [w|
      <RISULTATI>
        <SCHEDA sercdoa="105439">
          <PARAGRAFO etichetta="LOCATION">
            <PVCS etichetta="Country">Principato di Monaco</PVCS>
            <PVCC etichetta="Town / Municipality">Montecarlo</PVCC>
            <LDCN etichetta="Repository">Salocchi</LDCN>
          </PARAGRAFO>
        </SCHEDA>
      </RISULTATI>
    |]
    result <- processXMLStringAsSet mapping zeriBaseUri zeriXml
    result `shouldBe` [t|
<http://vocab.getty.edu/tgn/7005758> a crm:E53_Place .
<http://www.wikidata.org/entity/Q45240> a crm:E53_Place .
<place/country/principato_di_monaco> a crm:E53_Place .
<place/country/principato_di_monaco> crm:P1_is_identified_by <place/country/principato_di_monaco/appellation/preferred_name> .
<place/country/principato_di_monaco> crm:P2_has_type pharos-meta:country .
<place/country/principato_di_monaco> crm:P89i_contains <place/country/principato_di_monaco/town/montecarlo> .
<place/country/principato_di_monaco> custom:sameAs <http://vocab.getty.edu/tgn/7005758> .
<place/country/principato_di_monaco/appellation/preferred_name> a crm:E41_Appellation .
<place/country/principato_di_monaco/appellation/preferred_name> crm:P190_has_symbolic_content "Principato di Monaco" .
<place/country/principato_di_monaco/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<place/country/principato_di_monaco/town/montecarlo> a crm:E53_Place .
<place/country/principato_di_monaco/town/montecarlo> crm:P1_is_identified_by <place/country/principato_di_monaco/town/montecarlo/appellation/preferred_name> .
<place/country/principato_di_monaco/town/montecarlo> crm:P2_has_type <vocab/meta/town> .
<place/country/principato_di_monaco/town/montecarlo> crm:P74i_is_current_or_former_residence_of <place/country/principato_di_monaco/town/montecarlo/actor/repository_actor/salocchi> .
<place/country/principato_di_monaco/town/montecarlo> custom:sameAs <http://www.wikidata.org/entity/Q45240> .
<place/country/principato_di_monaco/town/montecarlo/actor/repository_actor/salocchi> a crm:E39_Actor .
<place/country/principato_di_monaco/town/montecarlo/actor/repository_actor/salocchi> crm:P1_is_identified_by <place/country/principato_di_monaco/town/montecarlo/actor/repository_actor/salocchi/appellation/preferred_name> .
<place/country/principato_di_monaco/town/montecarlo/actor/repository_actor/salocchi> crm:P2_has_type <vocab/meta/repository_actor> .
<place/country/principato_di_monaco/town/montecarlo/actor/repository_actor/salocchi> crm:P50i_is_current_keeper_of <work/105439> .
<place/country/principato_di_monaco/town/montecarlo/actor/repository_actor/salocchi/appellation/preferred_name> a crm:E41_Appellation .
<place/country/principato_di_monaco/town/montecarlo/actor/repository_actor/salocchi/appellation/preferred_name> crm:P190_has_symbolic_content "Salocchi" .
<place/country/principato_di_monaco/town/montecarlo/actor/repository_actor/salocchi/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<place/country/principato_di_monaco/town/montecarlo/appellation/preferred_name> a crm:E41_Appellation .
<place/country/principato_di_monaco/town/montecarlo/appellation/preferred_name> crm:P190_has_symbolic_content "Montecarlo" .
<place/country/principato_di_monaco/town/montecarlo/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<work/105439> a crm:E22_Human-Made_Object .
|]
