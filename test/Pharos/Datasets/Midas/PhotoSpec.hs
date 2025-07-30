module Pharos.Datasets.Midas.PhotoSpec (spec) where

import CommonImports
import Test.Hspec
import Midas.Mappings.Work (baseMapping, midasBaseUri, nestedWorks)
import Midas.Mappings.Photo (photoLinks)
import qualified Data.Text as T

spec :: Spec
spec = describe "Midas photo mappings" $ do
  let links datasetName = [photoLinks (T.pack datasetName)]
  let createMapping datasetName = baseMapping +> links datasetName ++ (nestedWorks $ (\_ -> links datasetName))

  describe "Marburg" $ do
    let currentMapping = createMapping "marburg"

    it "don't include photos where a8541a matches 'kein Download'" $ do
      let xmlInput = [w|
        <obj>
          <a5000>20648760</a5000>
          <a8450 modifier="Foto" did="67304284" lvl="2">
            <a8470>193.471</a8470>
            <a8460>Foto Marburg</a8460>
            <a8490>unbekannt</a8490>
            <a8494>um 1953/1960?</a8494>
            <a8500>Außenaufnahme</a8500>
            <a8510>Gesamtansicht</a8510>
            <a8541a>kein Download</a8541a>
            <asr01>Deutschland</asr01>
            <asr02>Marburg</asr02>
            <asr03>Öffentlicher Profanbau</asr03>
            <asr04>Bildungsbau</asr04>
            <asr05>Universität</asr05>
            <asr06>Kunstinstitut (Hülsen-Haus)</asr06>
            <asr07>Garten</asr07>
            <a8555>mi02052d13</a8555>
            <asr00>09238490</asr00>
            <a8494a edp:augmented="no:reference">1948</a8494a>
            <a8494e edp:augmented="no:reference">1960</a8494e>
          </a8450>

          <a8450 modifier="Foto" did="67304305" lvl="2">
            <a8470>fmd10017326</a8470>
            <a8540>fmd10017326</a8540>
            <a8460>Foto Marburg</a8460>
            <a8490>Scheidt, Thomas</a8490>
            <a8494>2020.04.24</a8494>
            <a8482>color</a8482>
            <a8500>Außenaufnahme</a8500>
            <a8510>Seitenansicht</a8510>
            <a8478>born digital</a8478>
            <a85fm>DEHIO-digital  Kunstdenkmäler in Deutschland</a85fm>
            <a8543c>http://creativecommons.org/licenses/by-nc-sa/4.0/</a8543c>
            <a8494a edp:augmented="no:reference">2020</a8494a>
            <a8494e edp:augmented="no:reference">2020</a8494e>
          </a8450>
        </obj>
      |]

      result <- processXMLStringAsSet currentMapping midasBaseUri xmlInput
      result `shouldBe` [t|
<actor/foto_marburg> crm:P1_is_identified_by <actor/foto_marburg/appellation/preferred_name> .
<actor/foto_marburg> a crm:E39_Actor .
<actor/foto_marburg/appellation/preferred_name> crm:P190_has_symbolic_content "Foto Marburg" .
<actor/foto_marburg/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<actor/foto_marburg/appellation/preferred_name> a crm:E41_Appellation .
<actor/photographer/scheidt_thomas> crm:P1_is_identified_by <actor/photographer/scheidt_thomas/appellation/preferred_name> .
<actor/photographer/scheidt_thomas> crm:P2_has_type pharos-meta:photographer .
<actor/photographer/scheidt_thomas> custom:sameAs <https://artresearch.net/resource/pharos/actor/photographer/scheidt_thomas> .
<actor/photographer/scheidt_thomas> a crm:E39_Actor .
<actor/photographer/scheidt_thomas/appellation/preferred_name> crm:P190_has_symbolic_content "Scheidt, Thomas" .
<actor/photographer/scheidt_thomas/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<actor/photographer/scheidt_thomas/appellation/preferred_name> a crm:E41_Appellation .
<http://creativecommons.org/licenses/by-nc-sa/4.0/> crm:P2_has_type pharos-meta:license_type .
<http://creativecommons.org/licenses/by-nc-sa/4.0/> a crm:E55_Type .
<https://artresearch.net/resource/e31/marburg> a crm:E31_Document .
<https://artresearch.net/resource/pharos/actor/photographer/scheidt_thomas> a crm:E39_Actor .
<https://iiif.artresearch.net/iiif/3/marburg/fmd10017326.jpg/full/max/0/default.jpg> crm:P2_has_type pharos-meta:photo_file_url .
<https://iiif.artresearch.net/iiif/3/marburg/fmd10017326.jpg/full/max/0/default.jpg> image-api:storage-id "marburg" .
<https://iiif.artresearch.net/iiif/3/marburg/fmd10017326.jpg/full/max/0/default.jpg> a crm:E42_Identifier .
<marburg/photo/fmd10017326> crm:P104_is_subject_to <marburg/photo/fmd10017326/rights> .
<marburg/photo/fmd10017326> crm:P108i_was_produced_by <marburg/photo/fmd10017326/production> .
<marburg/photo/fmd10017326> crm:P138i_has_representation <marburg/photo/fmd10017326/visual_item> .
<marburg/photo/fmd10017326> crm:P2_has_type <vocab/photo-type/foto> .
<marburg/photo/fmd10017326> crm:P50_has_current_keeper <actor/foto_marburg> .
<marburg/photo/fmd10017326> crm:P70i_is_documented_in <https://artresearch.net/resource/e31/marburg> .
<marburg/photo/fmd10017326> custom:imageOrder "2"^^xsd:integer .
<marburg/photo/fmd10017326> a crm:E22_Human-Made_Object .
<marburg/photo/fmd10017326/production> crm:P14_carried_out_by <actor/photographer/scheidt_thomas> .
<marburg/photo/fmd10017326/production> a crm:E12_Production .
<marburg/photo/fmd10017326/rights> crm:P2_has_type <http://creativecommons.org/licenses/by-nc-sa/4.0/> .
<marburg/photo/fmd10017326/rights> a crm:E30_Right .
<marburg/photo/fmd10017326/visual_item> crm:P165i_is_incorporated_in <marburg/photo/fmd10017326/visual_item/image> .
<marburg/photo/fmd10017326/visual_item> a crm:E36_Visual_Item .
<marburg/photo/fmd10017326/visual_item/image> crm:P1_is_identified_by <https://iiif.artresearch.net/iiif/3/marburg/fmd10017326.jpg/full/max/0/default.jpg> .
<marburg/photo/fmd10017326/visual_item/image> crm:P2_has_type pharos-meta:digital_image .
<marburg/photo/fmd10017326/visual_item/image> a crm:D1_Digital_Object .
<vocab/photo-type/foto> crm:P127_has_broader_term pharos-meta:photograph .
<vocab/photo-type/foto> crm:P1_is_identified_by <vocab/photo-type/foto/appellation/preferred_name> .
<vocab/photo-type/foto> a crm:E55_Type .
<vocab/photo-type/foto/appellation/preferred_name> crm:P190_has_symbolic_content "Foto" .
<vocab/photo-type/foto/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<vocab/photo-type/foto/appellation/preferred_name> a crm:E41_Appellation .
<work/20648760> crm:P138i_has_representation <work/20648760/visual_item> .
<work/20648760> a crm:E22_Human-Made_Object .
<work/20648760/visual_item> crm:P65i_is_shown_by <marburg/photo/fmd10017326> .
<work/20648760/visual_item> a crm:E36_Visual_Item .
      |]

    it "don't include photos where where a8541 matches 'gesperrt'" $ do
      let xmlInput = [w|
        <obj>
          <a5000>20837569</a5000>
          <a8450 modifier="Foto" did="63868931" lvl="2">
            <a8408>00420652</a8408>
            <a8470>1.566.740</a8470>
            <a8540>fm1566740</a8540>
            <a8460>Foto Marburg</a8460>
            <a8561>Schmidt-Glassner2001</a8561>
            <a85fm>Projekt historische fotografische Negative</a85fm>
            <a8541>gesperrt</a8541>
            <a8543c>http://rightsstatements.org/vocab/InC/1.0/</a8543c>
          </a8450>
        </obj>
      |]

      result <- processXMLStringAsSet currentMapping midasBaseUri xmlInput
      result `shouldBe` [t|
<work/20837569> a crm:E22_Human-Made_Object .
      |]


  -- Test cases for Hertziana
  describe "Hertziana dataset" $ do
    let currentMapping = createMapping "hertziana"

    it "correctly maps photos for Hertziana record" $ do
      let xmlInput = [w|
        <obj>
          <a5000>08095383</a5000>
          <a8450 modifier="Foto">
            <a8460>Braun, Adolphe</a8460>
            <a8470>229</a8470>
            <a8540 edp:checksum="" edp:lastupdate="2017-10-27T12:04:17Z" edp:originalfilename="" edp:originalfilesource="" edp:updateby="glorius@biblhertz.it" edp:virtualfilename="">bh000293</a8540>
            <a84bh>000293</a84bh>
            <a8515>raro-art</a8515>
            <a8496>1912/1915</a8496>
            <a8487>Kohledruck</a8487>
            <a8490>Braun, ?</a8490>
            <a8541>freigegeben</a8541>
            <a8579>Vollständig aufgeklebt; K: Rs. Stempel a</a8579>
            <a8543c>http://creativecommons.org/publicdomain/mark/1.0/</a8543c>
          </a8450>
          <a8450 modifier="Foto">
            <a8460>Graphische Sammlung Albertina</a8460>
            <a8540 edp:checksum="" edp:lastupdate="2018-05-07T15:06:23Z" edp:originalfilename="" edp:originalfilesource="" edp:updateby="glorius@biblhertz.it" edp:virtualfilename="">bh478301</a8540>
            <a84bh>478301</a84bh>
            <a8515>art</a8515>
            <a8496>1998.04</a8496>
            <a8482>s/w</a8482>
            <a8541>gesperrt</a8541>
            <a8543c>https://www.deutsche-digitale-bibliothek.de/content/lizenzen/rv-ez/</a8543c>
          </a8450>
        </obj>
      |]

      result <- processXMLStringAsSet currentMapping midasBaseUri xmlInput
      result `shouldBe` [t|
<actor/braun_adolphe> crm:P1_is_identified_by <actor/braun_adolphe/appellation/preferred_name> .
<actor/braun_adolphe> a crm:E39_Actor .
<actor/braun_adolphe/appellation/preferred_name> crm:P190_has_symbolic_content "Braun, Adolphe" .
<actor/braun_adolphe/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<actor/braun_adolphe/appellation/preferred_name> a crm:E41_Appellation .
<actor/graphische_sammlung_albertina> crm:P1_is_identified_by <actor/graphische_sammlung_albertina/appellation/preferred_name> .
<actor/graphische_sammlung_albertina> a crm:E39_Actor .
<actor/graphische_sammlung_albertina/appellation/preferred_name> crm:P190_has_symbolic_content "Graphische Sammlung Albertina" .
<actor/graphische_sammlung_albertina/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<actor/graphische_sammlung_albertina/appellation/preferred_name> a crm:E41_Appellation .
<actor/photographer/braun_q> crm:P1_is_identified_by <actor/photographer/braun_q/appellation/preferred_name> .
<actor/photographer/braun_q> crm:P2_has_type pharos-meta:photographer .
<actor/photographer/braun_q> custom:sameAs <http://www.wikidata.org/entity/Q365683> .
<actor/photographer/braun_q> a crm:E39_Actor .
<actor/photographer/braun_q/appellation/preferred_name> crm:P190_has_symbolic_content "Braun, ?" .
<actor/photographer/braun_q/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<actor/photographer/braun_q/appellation/preferred_name> a crm:E41_Appellation .
<hertziana/photo/bh000293> crm:P104_is_subject_to <hertziana/photo/bh000293/rights> .
<hertziana/photo/bh000293> crm:P108i_was_produced_by <hertziana/photo/bh000293/production> .
<hertziana/photo/bh000293> crm:P138i_has_representation <hertziana/photo/bh000293/visual_item> .
<hertziana/photo/bh000293> crm:P2_has_type <vocab/photo-type/foto> .
<hertziana/photo/bh000293> crm:P50_has_current_keeper <actor/braun_adolphe> .
<hertziana/photo/bh000293> crm:P70i_is_documented_in <https://artresearch.net/resource/e31/hertziana> .
<hertziana/photo/bh000293> custom:imageOrder "1"^^xsd:integer .
<hertziana/photo/bh000293> a crm:E22_Human-Made_Object .
<hertziana/photo/bh000293/production> crm:P14_carried_out_by <actor/photographer/braun_q> .
<hertziana/photo/bh000293/production> a crm:E12_Production .
<hertziana/photo/bh000293/rights> crm:P2_has_type <http://creativecommons.org/publicdomain/mark/1.0/> .
<hertziana/photo/bh000293/rights> a crm:E30_Right .
<hertziana/photo/bh000293/visual_item> crm:P165i_is_incorporated_in <hertziana/photo/bh000293/visual_item/image> .
<hertziana/photo/bh000293/visual_item> a crm:E36_Visual_Item .
<hertziana/photo/bh000293/visual_item/image> crm:P1_is_identified_by <https://hertz-foto-os1.biblhertz.it/iiif/3/bh000293/full/max/0/default.jpg> .
<hertziana/photo/bh000293/visual_item/image> crm:P2_has_type pharos-meta:digital_image .
<hertziana/photo/bh000293/visual_item/image> a crm:D1_Digital_Object .
<hertziana/photo/bh478301> crm:P104_is_subject_to <hertziana/photo/bh478301/rights> .
<hertziana/photo/bh478301> crm:P138i_has_representation <hertziana/photo/bh478301/visual_item> .
<hertziana/photo/bh478301> crm:P2_has_type <vocab/photo-type/foto> .
<hertziana/photo/bh478301> crm:P50_has_current_keeper <actor/graphische_sammlung_albertina> .
<hertziana/photo/bh478301> crm:P70i_is_documented_in <https://artresearch.net/resource/e31/hertziana> .
<hertziana/photo/bh478301> custom:imageOrder "2"^^xsd:integer .
<hertziana/photo/bh478301> a crm:E22_Human-Made_Object .
<hertziana/photo/bh478301/rights> crm:P2_has_type <http://www.deutsche-digitale-bibliothek.de/content/lizenzen/rv-ez/> .
<hertziana/photo/bh478301/rights> a crm:E30_Right .
<hertziana/photo/bh478301/visual_item> crm:P165i_is_incorporated_in <hertziana/photo/bh478301/visual_item/image> .
<hertziana/photo/bh478301/visual_item> a crm:E36_Visual_Item .
<hertziana/photo/bh478301/visual_item/image> crm:P1_is_identified_by <https://hertz-foto-os1.biblhertz.it/iiif/3/bh478301/full/max/0/default.jpg> .
<hertziana/photo/bh478301/visual_item/image> crm:P2_has_type pharos-meta:digital_image .
<hertziana/photo/bh478301/visual_item/image> crm:P2_has_type <vocab/meta/iiif_not_available> .
<hertziana/photo/bh478301/visual_item/image> a crm:D1_Digital_Object .
<http://creativecommons.org/publicdomain/mark/1.0/> crm:P2_has_type pharos-meta:license_type .
<http://creativecommons.org/publicdomain/mark/1.0/> a crm:E55_Type .
<http://www.deutsche-digitale-bibliothek.de/content/lizenzen/rv-ez/> crm:P2_has_type pharos-meta:license_type .
<http://www.deutsche-digitale-bibliothek.de/content/lizenzen/rv-ez/> a crm:E55_Type .
<http://www.wikidata.org/entity/Q365683> a crm:E39_Actor .
<https://artresearch.net/resource/e31/hertziana> a crm:E31_Document .
<https://hertz-foto-os1.biblhertz.it/iiif/3/bh000293/full/max/0/default.jpg> crm:P2_has_type pharos-meta:photo_file_url .
<https://hertz-foto-os1.biblhertz.it/iiif/3/bh000293/full/max/0/default.jpg> image-api:storage-id "hertziana" .
<https://hertz-foto-os1.biblhertz.it/iiif/3/bh000293/full/max/0/default.jpg> a crm:E42_Identifier .
<https://hertz-foto-os1.biblhertz.it/iiif/3/bh478301/full/max/0/default.jpg> crm:P2_has_type pharos-meta:photo_file_url .
<https://hertz-foto-os1.biblhertz.it/iiif/3/bh478301/full/max/0/default.jpg> image-api:storage-id "hertziana" .
<https://hertz-foto-os1.biblhertz.it/iiif/3/bh478301/full/max/0/default.jpg> a crm:E42_Identifier .
<vocab/photo-type/foto> crm:P127_has_broader_term pharos-meta:photograph .
<vocab/photo-type/foto> crm:P1_is_identified_by <vocab/photo-type/foto/appellation/preferred_name> .
<vocab/photo-type/foto> a crm:E55_Type .
<vocab/photo-type/foto/appellation/preferred_name> crm:P190_has_symbolic_content "Foto" .
<vocab/photo-type/foto/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<vocab/photo-type/foto/appellation/preferred_name> a crm:E41_Appellation .
<work/08095383> crm:P138i_has_representation <work/08095383/visual_item> .
<work/08095383> a crm:E22_Human-Made_Object .
<work/08095383/visual_item> crm:P65i_is_shown_by <hertziana/photo/bh000293> .
<work/08095383/visual_item> crm:P65i_is_shown_by <hertziana/photo/bh478301> .
<work/08095383/visual_item> a crm:E36_Visual_Item .
      |]

  -- Test cases for KHI
  describe "KHI dataset" $ do
    let currentMapping = createMapping "khi"

    it "map only photos with image but not verso" $ do
      let xmlInput = [w|
        <obj>
          <a5000>70011551</a5000>
          <a8450>Foto<a8496>2022.03.29</a8496>
                <a84fl>628984</a84fl>
                <a8515>Stadtbau</a8515>
                <a8460>Farbenphotographische Gesellschaft m.b.H.</a8460>
                <a8490>Hildebrand, Hans?</a8490>
                <a8494>vor 1919</a8494>
                <a8478>Autotypie</a8478>
                <a8596>Cimelia Photographica (Sonderbestand) &amp; Stereoskopie</a8596>
                <a8497>Kauf</a8497>
                <a8480>9 x 18,1 cm (Karton)</a8480>
                <a8482>color</a8482>
                <a8510>Straßenansicht</a8510>
                
                <a854r>Stereoskopie</a854r>
                
                
                
                
                
                
                
            <a8494a edp:augmented="no:reference">1909</a8494a><a8494e edp:augmented="no:reference">1919</a8494e></a8450>
            <a8450>Foto<a8496>2022.05.02</a8496>
                <a84fl>628984scan</a84fl>
                <a8460>KHI-Farbenphotographische Gesellschaft m.b.H.</a8460>
                <a8490>Hildebrand, Hans?</a8490>
                <a8490>Digitallabor KHI nach historischer Vorlage</a8490>
                <a8494>vor 1919</a8494>
                <a8494>2022.04</a8494>
                <a8478>Vorlage Autotypie</a8478>
                <a8596>Cimelia Photographica (Vorlage Sonderbestand) &amp; Stereoskopie</a8596>
                <a8497>Fotokampagne KHI</a8497>
                <a8480>Vorlage 9 x 18,1 cm (Karton)</a8480>
                <a8482>color</a8482>
                <a854g>tif</a854g>
                <a854t>Phase One XF IQ4</a854t>
                <a8540 edp:checksum="" edp:lastupdate="2022-05-02T16:32:58Z" edp:originalfilename="" edp:originalfilesource="" edp:updateby="dagmar.keultjes@khi.fi.it" edp:virtualfilename="">fld0015051a_p</a8540>
                <a8577>Kunsthistorisches Institut in Florenz – Max-Planck-Institut</a8577>
                <a8510>Historischer Trägerkarton mit Straßenansicht, Stereoskopie</a8510>
                <a8579>P002779_2021-027_CIMELIA_Stereos_Jerusal</a8579>
                
                
                
                
                
                
                <a8543c>CC0</a8543c>
                
                
            <a8494a edp:augmented="no:reference">1909</a8494a><a8494e edp:augmented="no:reference">1919</a8494e></a8450>
            <a8450>Foto<a8496>2022.04.06</a8496>
                <a84fl>628984v</a84fl>
                <a8460>Farbenphotographische Gesellschaft m.b.H.</a8460>
                <a8490>Hildebrand, Hans?</a8490>
                <a8487>Karton</a8487>
                <a8596>Cimelia Photographica (Sonderbestand) &amp; Stereoskopie</a8596>
                <a8480>9 x 18,1 cm (Karton)</a8480>
                <a8510>Rückseite des historischen Trägerkartons mit Textaufdruck, Stereoskopie</a8510>
            </a8450>
            <a8450>Foto<a8496>2022.05.02</a8496>
                <a84fl>628984vscan</a84fl>
                <a8460>KHI-Farbenphotographische Gesellschaft m.b.H.</a8460>
                <a8490>Digitallabor KHI nach historischer Vorlage</a8490>
                <a8494>2022.04</a8494>
                <a8487>Vorlage Karton</a8487>
                <a8596>Cimelia Photographica (Vorlage Sonderbestand) &amp; Stereoskopie</a8596>
                <a8497>Fotokampagne KHI</a8497>
                <a8480>Vorlage 9 x 18,1 cm (Karton)</a8480>
                <a8482>color</a8482>
                <a854g>tif</a854g>
                <a854t>Phase One XF IQ4</a854t>
                <a8540 edp:checksum="" edp:lastupdate="2022-05-02T16:56:04Z" edp:originalfilename="" edp:originalfilesource="" edp:updateby="dagmar.keultjes@khi.fi.it" edp:virtualfilename="">fld0015051b_p</a8540>
                <a8577>Kunsthistorisches Institut in Florenz – Max-Planck-Institut</a8577>
                <a8510>Rückseite des historischen Trägerkartons mit Textaufdruck, Stereoskopie</a8510>
                <a8579>P002779_2021-027_CIMELIA_Stereos_Jerusal</a8579>
                
                
                
                
                
                
                <a8543c>CC0</a8543c>
                
                
            <a8494a edp:augmented="no:reference">2022</a8494a><a8494e edp:augmented="no:reference">2022</a8494e></a8450>
            <a8450>Foto<a8496>2022.03.30</a8496>
                <a84fl>628986</a84fl>
                <a8515>Stadtbau</a8515>
                <a8460>Berry, Kelley et Chadwick</a8460>
                <a8490>Rau, William Herman</a8490>
                <a8494>1903</a8494>
                <a8487>Silbergelatineabzug</a8487>
                <a8596>Cimelia Photographica (Sonderbestand) &amp; Stereoskopie &amp; retuschiert</a8596>
                <a8497>Kauf</a8497>
                <a8480>8,9 x 17,8 cm (Karton)</a8480>
                <a8482>monochrom</a8482>
                <a8510>Straßenansicht, Stereoskopie</a8510>
                
                <a854r>Stereoskopie</a854r>
                
                
                
                <a8488>Bildsilberoxidation</a8488>
                <a8488>Vergilbungen</a8488>
                
                
                
                
                
                
                
                
            <a3000 edp:augmented="kue::70000482">70000482</a3000><a31nn edp:augmented="kue::70000482">William Herman Rau</a31nn><a8494a edp:augmented="no:reference">1903</a8494a><a8494e edp:augmented="no:reference">1903</a8494e></a8450>
            <a8450>Foto<a8496>2022.05.02</a8496>
                <a84fl>628986scan</a84fl>
                <a8460>KHI-Berry, Kelley et Chadwick</a8460>
                <a8490>Rau, William Herman</a8490>
                <a8490>Digitallabor KHI nach historischer Vorlage</a8490>
                <a8494>1903</a8494>
                <a8494>2022.04</a8494>
                <a8487>Vorlage Silbergelatineabzug</a8487>
                <a8596>Cimelia Photographica (Vorlage Sonderbestand) &amp; Stereoskopie &amp; retuschiert</a8596>
                <a8497>Fotokampagne KHI</a8497>
                <a8480>Vorlage 8,9 x 17,8 cm (Karton)</a8480>
                <a8482>color</a8482>
                <a854g>tif</a854g>
                <a854t>Phase One XF IQ4</a854t>
                <a8540 edp:checksum="" edp:lastupdate="2022-05-02T16:24:57Z" edp:originalfilename="" edp:originalfilesource="" edp:updateby="dagmar.keultjes@khi.fi.it" edp:virtualfilename="">fld0015039a_p</a8540>
                <a8577>Kunsthistorisches Institut in Florenz – Max-Planck-Institut</a8577>
                <a8510>Historischer Trägerkarton mit Straßenansicht, Stereoskopie</a8510>
                <a8579>P002779_2021-027_CIMELIA_Stereos_Jerusal</a8579>
                <a8543c>CC0</a8543c>
                
                
            <a3000 edp:augmented="kue::70000482">70000482</a3000><a31nn edp:augmented="kue::70000482">William Herman Rau</a31nn><a8494a edp:augmented="no:reference">1903</a8494a><a8494e edp:augmented="no:reference">1903</a8494e></a8450>
            <a8450>Foto<a8496>2022.04.06</a8496>
                <a84fl>628986v</a84fl>
                <a8460>Berry, Kelley et Chadwick</a8460>
                <a8490>Rau, William Herman</a8490>
                <a8487>Karton</a8487>
                <a8596>Cimelia Photographica (Sonderbestand) &amp; Stereoskopie</a8596>
                <a8480>8,9 x 17,8 cm (Karton)</a8480>
                <a8510>Rückseite des historischen Trägerkartons mit Textaufdruck, Stereoskopie</a8510>
                
                
                
                
            <a3000 edp:augmented="kue::70000482">70000482</a3000><a31nn edp:augmented="kue::70000482">William Herman Rau</a31nn></a8450>
            <a8450>Foto<a8496>2022.05.02</a8496>
                <a84fl>628986vscan</a84fl>
                <a8460>KHI-Berry, Kelley et Chadwick</a8460>
                <a8490>Digitallabor KHI nach historischer Vorlage</a8490>
                <a8494>2022.04</a8494>
                <a8487>Vorlage Karton</a8487>
                <a8596>Cimelia Photographica (Vorlage Sonderbestand) &amp; Stereoskopie</a8596>
                <a8497>Fotokampagne KHI</a8497>
                <a8480>Vorlage 8,9 x 17,8 cm (Karton)</a8480>
                <a8482>color</a8482>
                <a854g>tif</a854g>
                <a854t>Phase One XF IQ4</a854t>
                <a8540 edp:checksum="" edp:lastupdate="2022-05-02T16:29:01Z" edp:originalfilename="" edp:originalfilesource="" edp:updateby="dagmar.keultjes@khi.fi.it" edp:virtualfilename="">fld0015039b_p</a8540>
                <a8577>Kunsthistorisches Institut in Florenz – Max-Planck-Institut</a8577>
                <a8510>Rückseite des historischen Trägerkartons mit Textaufdruck, Stereoskopie</a8510>
                <a8579>P002779_2021-027_CIMELIA_Stereos_Jerusal</a8579>
                <a8543c>CC0</a8543c>
                <a8494a edp:augmented="no:reference">2022</a8494a><a8494e edp:augmented="no:reference">2022</a8494e></a8450>
        </obj>
      |]

      result <- processXMLStringAsSet currentMapping midasBaseUri xmlInput
      result `shouldBe` [t|
<actor/kunsthistorisches_institut_in_florenz_max_planck_institut> crm:P1_is_identified_by <actor/kunsthistorisches_institut_in_florenz_max_planck_institut/appellation/preferred_name> .
<actor/kunsthistorisches_institut_in_florenz_max_planck_institut> a crm:E39_Actor .
<actor/kunsthistorisches_institut_in_florenz_max_planck_institut/appellation/preferred_name> crm:P190_has_symbolic_content "Kunsthistorisches Institut in Florenz – Max-Planck-Institut" .
<actor/kunsthistorisches_institut_in_florenz_max_planck_institut/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<actor/kunsthistorisches_institut_in_florenz_max_planck_institut/appellation/preferred_name> a crm:E41_Appellation .
<actor/photographer/digitallabor_khi_nach_historischer_vorlage> crm:P1_is_identified_by <actor/photographer/digitallabor_khi_nach_historischer_vorlage/appellation/preferred_name> .
<actor/photographer/digitallabor_khi_nach_historischer_vorlage> crm:P2_has_type pharos-meta:photographer .
<actor/photographer/digitallabor_khi_nach_historischer_vorlage> custom:sameAs <http://www.wikidata.org/entity/Q872398> .
<actor/photographer/digitallabor_khi_nach_historischer_vorlage> a crm:E39_Actor .
<actor/photographer/digitallabor_khi_nach_historischer_vorlage/appellation/preferred_name> crm:P190_has_symbolic_content "Digitallabor KHI nach historischer Vorlage" .
<actor/photographer/digitallabor_khi_nach_historischer_vorlage/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<actor/photographer/digitallabor_khi_nach_historischer_vorlage/appellation/preferred_name> a crm:E41_Appellation .
<actor/photographer/hildebrand_hans_q> crm:P1_is_identified_by <actor/photographer/hildebrand_hans_q/appellation/preferred_name> .
<actor/photographer/hildebrand_hans_q> crm:P2_has_type pharos-meta:photographer .
<actor/photographer/hildebrand_hans_q> custom:sameAs <https://artresearch.net/resource/pharos/actor/photographer/hildebrand_hans> .
<actor/photographer/hildebrand_hans_q> a crm:E39_Actor .
<actor/photographer/hildebrand_hans_q/appellation/preferred_name> crm:P190_has_symbolic_content "Hildebrand, Hans?" .
<actor/photographer/hildebrand_hans_q/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<actor/photographer/hildebrand_hans_q/appellation/preferred_name> a crm:E41_Appellation .
<actor/photographer/rau_william_herman> crm:P1_is_identified_by <actor/photographer/rau_william_herman/appellation/preferred_name> .
<actor/photographer/rau_william_herman> crm:P2_has_type pharos-meta:photographer .
<actor/photographer/rau_william_herman> custom:sameAs <http://www.wikidata.org/entity/Q8010564> .
<actor/photographer/rau_william_herman> a crm:E39_Actor .
<actor/photographer/rau_william_herman/appellation/preferred_name> crm:P190_has_symbolic_content "Rau, William Herman" .
<actor/photographer/rau_william_herman/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<actor/photographer/rau_william_herman/appellation/preferred_name> a crm:E41_Appellation .
<http://creativecommons.org/publicdomain/zero/1.0/> crm:P2_has_type pharos-meta:license_type .
<http://creativecommons.org/publicdomain/zero/1.0/> a crm:E55_Type .
<http://www.wikidata.org/entity/Q8010564> a crm:E39_Actor .
<http://www.wikidata.org/entity/Q872398> a crm:E39_Actor .
<https://artresearch.net/resource/e31/khi> a crm:E31_Document .
<https://artresearch.net/resource/pharos/actor/photographer/hildebrand_hans> a crm:E39_Actor .
<https://iiif.khi.fi.it/iiif/2/photothek%2Ffld0015039a_p.tif/full/max/0/default.jpg> crm:P2_has_type pharos-meta:photo_file_url .
<https://iiif.khi.fi.it/iiif/2/photothek%2Ffld0015039a_p.tif/full/max/0/default.jpg> image-api:storage-id "khi" .
<https://iiif.khi.fi.it/iiif/2/photothek%2Ffld0015039a_p.tif/full/max/0/default.jpg> a crm:E42_Identifier .
<https://iiif.khi.fi.it/iiif/2/photothek%2Ffld0015051a_p.tif/full/max/0/default.jpg> crm:P2_has_type pharos-meta:photo_file_url .
<https://iiif.khi.fi.it/iiif/2/photothek%2Ffld0015051a_p.tif/full/max/0/default.jpg> image-api:storage-id "khi" .
<https://iiif.khi.fi.it/iiif/2/photothek%2Ffld0015051a_p.tif/full/max/0/default.jpg> a crm:E42_Identifier .
<khi/photo/fld0015039a_p> crm:P104_is_subject_to <khi/photo/fld0015039a_p/rights> .
<khi/photo/fld0015039a_p> crm:P108i_was_produced_by <khi/photo/fld0015039a_p/production> .
<khi/photo/fld0015039a_p> crm:P138i_has_representation <khi/photo/fld0015039a_p/visual_item> .
<khi/photo/fld0015039a_p> crm:P50_has_current_keeper <actor/kunsthistorisches_institut_in_florenz_max_planck_institut> .
<khi/photo/fld0015039a_p> crm:P70i_is_documented_in <https://artresearch.net/resource/e31/khi> .
<khi/photo/fld0015039a_p> custom:imageOrder "6"^^xsd:integer .
<khi/photo/fld0015039a_p> a crm:E22_Human-Made_Object .
<khi/photo/fld0015039a_p/production> crm:P14_carried_out_by <actor/photographer/digitallabor_khi_nach_historischer_vorlage> .
<khi/photo/fld0015039a_p/production> crm:P14_carried_out_by <actor/photographer/rau_william_herman> .
<khi/photo/fld0015039a_p/production> a crm:E12_Production .
<khi/photo/fld0015039a_p/rights> crm:P2_has_type <http://creativecommons.org/publicdomain/zero/1.0/> .
<khi/photo/fld0015039a_p/rights> a crm:E30_Right .
<khi/photo/fld0015039a_p/visual_item> crm:P165i_is_incorporated_in <khi/photo/fld0015039a_p/visual_item/image> .
<khi/photo/fld0015039a_p/visual_item> a crm:E36_Visual_Item .
<khi/photo/fld0015039a_p/visual_item/image> crm:P1_is_identified_by <https://iiif.khi.fi.it/iiif/2/photothek%2Ffld0015039a_p.tif/full/max/0/default.jpg> .
<khi/photo/fld0015039a_p/visual_item/image> crm:P2_has_type pharos-meta:digital_image .
<khi/photo/fld0015039a_p/visual_item/image> a crm:D1_Digital_Object .
<khi/photo/fld0015051a_p> crm:P104_is_subject_to <khi/photo/fld0015051a_p/rights> .
<khi/photo/fld0015051a_p> crm:P108i_was_produced_by <khi/photo/fld0015051a_p/production> .
<khi/photo/fld0015051a_p> crm:P138i_has_representation <khi/photo/fld0015051a_p/visual_item> .
<khi/photo/fld0015051a_p> crm:P50_has_current_keeper <actor/kunsthistorisches_institut_in_florenz_max_planck_institut> .
<khi/photo/fld0015051a_p> crm:P70i_is_documented_in <https://artresearch.net/resource/e31/khi> .
<khi/photo/fld0015051a_p> custom:imageOrder "2"^^xsd:integer .
<khi/photo/fld0015051a_p> a crm:E22_Human-Made_Object .
<khi/photo/fld0015051a_p/production> crm:P14_carried_out_by <actor/photographer/digitallabor_khi_nach_historischer_vorlage> .
<khi/photo/fld0015051a_p/production> crm:P14_carried_out_by <actor/photographer/hildebrand_hans_q> .
<khi/photo/fld0015051a_p/production> a crm:E12_Production .
<khi/photo/fld0015051a_p/rights> crm:P2_has_type <http://creativecommons.org/publicdomain/zero/1.0/> .
<khi/photo/fld0015051a_p/rights> a crm:E30_Right .
<khi/photo/fld0015051a_p/visual_item> crm:P165i_is_incorporated_in <khi/photo/fld0015051a_p/visual_item/image> .
<khi/photo/fld0015051a_p/visual_item> a crm:E36_Visual_Item .
<khi/photo/fld0015051a_p/visual_item/image> crm:P1_is_identified_by <https://iiif.khi.fi.it/iiif/2/photothek%2Ffld0015051a_p.tif/full/max/0/default.jpg> .
<khi/photo/fld0015051a_p/visual_item/image> crm:P2_has_type pharos-meta:digital_image .
<khi/photo/fld0015051a_p/visual_item/image> a crm:D1_Digital_Object .
<work/70011551> crm:P138i_has_representation <work/70011551/visual_item> .
<work/70011551> a crm:E22_Human-Made_Object .
<work/70011551/visual_item> crm:P65i_is_shown_by <khi/photo/fld0015039a_p> .
<work/70011551/visual_item> crm:P65i_is_shown_by <khi/photo/fld0015051a_p> .
<work/70011551/visual_item> a crm:E36_Visual_Item .
      |]
