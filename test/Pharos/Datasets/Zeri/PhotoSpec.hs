module Pharos.Datasets.Zeri.PhotoSpec (spec) where

import CommonImports
import Test.Hspec
import qualified Mappings.Mappings.Work.WorkMappings as WorkMappings
import qualified Mappings.Mappings.Work.Photo as WorkPhoto
import qualified Mappings.Mappings.Photo.Photo as PhotoMappings

spec :: Spec
spec = describe "Zeri photo mappings" $ do
  it "simple link from artwork to photo" $ do
    let mapping = WorkMappings.baseMapping +> [WorkPhoto.photoLink]
    let xml = [w|
      <RISULTATI>
        <SCHEDA sercdoa="10363">
          <ALLEGATI>
            <FOTO note="Raffaelli, Luigi , Orvieto, Cattedrale. Affreschi restaurati dell&amp;#039;abside, la volta - insieme" sercdf="24017" progr="1" altezza="230" larghezza="171">/40000/30400/30374.jpg</FOTO>
            <FOTO note="Istituto Centrale per il Catalogo e la Documentazione: Fototeca Nazionale , Orvieto - Duomo - Volta a crociera dell&amp;#039;abside con la gloria di Maria. Ugolino di Prete Ilario - insieme" sercdf="24018" progr="2" altezza="230" larghezza="175">/40000/30400/30375.jpg</FOTO>              
            <FOTO note="Todini, Filippo , Orvieto, Duomo. Volta del Coro. Ugolino di Prete Ilario - particolare" sercdf="24019" progr="3" altezza="180" larghezza="240">/40000/30400/30376.jpg</FOTO>
            <FOTO note="Istituto Centrale per il Catalogo e la Documentazione: Fototeca Nazionale , Orvieto - Duomo - Incoronazione della Vergine, affresco della volta dell&amp;#039;abside (particolare). Ugolino di Prete Ilario - particolare" sercdf="24020" progr="4" altezza="235" larghezza="175">/40000/30400/30378.jpg</FOTO>
            <FOTO note="Istituto Centrale per il Catalogo e la Documentazione: Fototeca Nazionale , Orvieto - Duomo - Incoronazione della Vergine, affresco della volta dell&amp;#039;abside. Ugolino del Prete Ilario - particolare" sercdf="24021" progr="5" altezza="230" larghezza="173">/40000/30400/30377.jpg</FOTO>
            <FOTO note="Istituto Centrale per il Catalogo e la Documentazione: Fototeca Nazionale , Orvieto - Duomo - Lo spirito santo circondato da cori angelici, affresco della volta dell&amp;#039;abside. Ugolino di Prete Ilario - particolare" sercdf="24022" progr="6" altezza="173" larghezza="232">/40000/30400/30380.jpg</FOTO>
            <FOTO note="Istituto Centrale per il Catalogo e la Documentazione: Fototeca Nazionale , Orvieto - Duomo - Cristo portato in sedia dagli angeli - affresco della volta dell&amp;#039;abside. Ugolino di Prete Ilario - particolare" sercdf="24023" progr="7" altezza="232" larghezza="175">/40000/30400/30379.jpg</FOTO>
            <FOTO note="Istituto Centrale per il Catalogo e la Documentazione: Fototeca Nazionale , Orvieto - Duomo - Cristo portato in sedia dagli angeli (partic.). Affresco della volta dell&amp;#039;abside. Ugolino di Prete Ilario - particolare" sercdf="24024" progr="8" altezza="174" larghezza="230">/40000/30400/30381.jpg</FOTO>
            <FOTO note="Istituto Centrale per il Catalogo e la Documentazione: Fototeca Nazionale , Ugolino di Prete Ilario - sec. XIV - Gerarchie angeliche - particolare" sercdf="24025" progr="9" altezza="175" larghezza="230">/40000/30400/30382.jpg</FOTO>
            <FOTO note="Istituto Centrale per il Catalogo e la Documentazione: Fototeca Nazionale , Orvieto - Duomo - Gerarchie angeliche di troni, podestà, cherubini, serafini - affresco della volta dell&amp;#039;abside. Ugolino di Prete Ilario - particolare" sercdf="24026" progr="10" altezza="172" larghezza="230">/40000/30400/30383.jpg</FOTO>            
          </ALLEGATI>
        </SCHEDA>
      </RISULTATI>
    |]
    
    result <- processXMLStringAsSet mapping WorkMappings.zeriBaseUri xml
    
    result `shouldBe` [t|
<work/10363> crm:P138i_has_representation <work/10363/visual_item> .
<work/10363> a crm:E22_Human-Made_Object .
<work/10363/visual_item> crm:P65i_is_shown_by <work/10363/visual_item/photo/24017> .
<work/10363/visual_item> crm:P65i_is_shown_by <work/10363/visual_item/photo/24018> .
<work/10363/visual_item> crm:P65i_is_shown_by <work/10363/visual_item/photo/24019> .
<work/10363/visual_item> crm:P65i_is_shown_by <work/10363/visual_item/photo/24020> .
<work/10363/visual_item> crm:P65i_is_shown_by <work/10363/visual_item/photo/24021> .
<work/10363/visual_item> crm:P65i_is_shown_by <work/10363/visual_item/photo/24022> .
<work/10363/visual_item> crm:P65i_is_shown_by <work/10363/visual_item/photo/24023> .
<work/10363/visual_item> crm:P65i_is_shown_by <work/10363/visual_item/photo/24024> .
<work/10363/visual_item> crm:P65i_is_shown_by <work/10363/visual_item/photo/24025> .
<work/10363/visual_item> crm:P65i_is_shown_by <work/10363/visual_item/photo/24026> .
<work/10363/visual_item> a crm:E36_Visual_Item .
<work/10363/visual_item/photo/24017> custom:imageOrder "1"^^xsd:integer .
<work/10363/visual_item/photo/24017> a crm:E22_Human-Made_Object .
<work/10363/visual_item/photo/24018> custom:imageOrder "2"^^xsd:integer .
<work/10363/visual_item/photo/24018> a crm:E22_Human-Made_Object .
<work/10363/visual_item/photo/24019> custom:imageOrder "3"^^xsd:integer .
<work/10363/visual_item/photo/24019> a crm:E22_Human-Made_Object .
<work/10363/visual_item/photo/24020> custom:imageOrder "4"^^xsd:integer .
<work/10363/visual_item/photo/24020> a crm:E22_Human-Made_Object .
<work/10363/visual_item/photo/24021> custom:imageOrder "5"^^xsd:integer .
<work/10363/visual_item/photo/24021> a crm:E22_Human-Made_Object .
<work/10363/visual_item/photo/24022> custom:imageOrder "6"^^xsd:integer .
<work/10363/visual_item/photo/24022> a crm:E22_Human-Made_Object .
<work/10363/visual_item/photo/24023> custom:imageOrder "7"^^xsd:integer .
<work/10363/visual_item/photo/24023> a crm:E22_Human-Made_Object .
<work/10363/visual_item/photo/24024> custom:imageOrder "8"^^xsd:integer .
<work/10363/visual_item/photo/24024> a crm:E22_Human-Made_Object .
<work/10363/visual_item/photo/24025> custom:imageOrder "9"^^xsd:integer .
<work/10363/visual_item/photo/24025> a crm:E22_Human-Made_Object .
<work/10363/visual_item/photo/24026> custom:imageOrder "10"^^xsd:integer .
<work/10363/visual_item/photo/24026> a crm:E22_Human-Made_Object .
    |]

  it "map photo object" $ do
    let mapping = PhotoMappings.photoMapping
    let xml = [w|
      <RISULTATI>
        <SCHEDA sercdf="24017" sercdoa="10363">
          <ELENCO_FOTO>
            <FOTO note="Raffaelli, Luigi , Orvieto, Cattedrale. Affreschi restaurati dell&#039;abside, la volta - insieme" sercdf="24017" altezza="230" larghezza="171">/40000/30400/30374.jpg</FOTO>
          </ELENCO_FOTO>
          <PARAGRAFO etichetta="PHOTOGRAPHERS">
            <RIPETIZIONE>      
              <AUFN etichetta="Photographer">Raffaelli, Luigi </AUFN>
              <AUFI etichetta="Address">Premiata Fotografia Luigi Raffaelli-Armoni. Orvieto</AUFI>
              <AUFM etichetta="Reason for attribution">timbro</AUFM>
              <AUFA etichetta="Personal data">1860/ 1929</AUFA>
            </RIPETIZIONE>
          </PARAGRAFO>  
        </SCHEDA>
      </RISULTATI>
    |]
    
    result <- processXMLStringAsSet mapping WorkMappings.zeriBaseUri xml
    
    result `shouldBe` [t|
<actor/photographer/raffaelli_luigi> a crm:E39_Actor .
<actor/photographer/raffaelli_luigi> crm:P1_is_identified_by <actor/photographer/raffaelli_luigi/appellation/preferred_name> .
<actor/photographer/raffaelli_luigi> crm:P2_has_type pharos-meta:photographer .
<actor/photographer/raffaelli_luigi> custom:sameAs <https://artresearch.net/resource/pharos/actor/photographer/raffaelli_luigi> .
<actor/photographer/raffaelli_luigi/appellation/preferred_name> a crm:E41_Appellation .
<actor/photographer/raffaelli_luigi/appellation/preferred_name> crm:P190_has_symbolic_content "Raffaelli, Luigi" .
<actor/photographer/raffaelli_luigi/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<http://creativecommons.org/licenses/by-nc-nd/4.0/> a crm:E55_Type .
<https://artresearch.net/resource/e31/zeri> a crm:E31_Document .
<https://artresearch.net/resource/pharos/actor/photographer/raffaelli_luigi> a crm:E39_Actor .
<https://artresearch.net/resource/provider/zeri> a crm:E39_Actor .
<https://iiif.artresearch.net/iiif/3/zeri/40000/30400/30374.jpg/full/full/0/default.jpg> a crm:E42_Identifier .
<https://iiif.artresearch.net/iiif/3/zeri/40000/30400/30374.jpg/full/full/0/default.jpg> crm:P2_has_type pharos-meta:photo_file_url .
<https://iiif.artresearch.net/iiif/3/zeri/40000/30400/30374.jpg/full/full/0/default.jpg> image-api:storage-id "zeri" .
<work/10363/visual_item/photo/24017> a crm:E22_Human-Made_Object .
<work/10363/visual_item/photo/24017> crm:P104_is_subject_to <work/10363/visual_item/photo/24017/rights> .
<work/10363/visual_item/photo/24017> crm:P108i_was_produced_by <work/10363/visual_item/photo/24017/production> .
<work/10363/visual_item/photo/24017> crm:P138i_has_representation <work/10363/visual_item/photo/24017/visual_item> .
<work/10363/visual_item/photo/24017> crm:P2_has_type pharos-meta:photographic_print .
<work/10363/visual_item/photo/24017> crm:P50_has_current_keeper <https://artresearch.net/resource/provider/zeri> .
<work/10363/visual_item/photo/24017> crm:P70i_is_documented_in <https://artresearch.net/resource/e31/zeri> .
<work/10363/visual_item/photo/24017/production> a crm:E12_Production .
<work/10363/visual_item/photo/24017/production> crm:P14_carried_out_by <actor/photographer/raffaelli_luigi> .
<work/10363/visual_item/photo/24017/rights> a crm:E30_Right .
<work/10363/visual_item/photo/24017/rights> crm:P2_has_type <http://creativecommons.org/licenses/by-nc-nd/4.0/> .
<work/10363/visual_item/photo/24017/visual_item> a crm:E36_Visual_Item .
<work/10363/visual_item/photo/24017/visual_item> crm:P165i_is_incorporated_in <work/10363/visual_item/photo/24017/visual_item/image> .
<work/10363/visual_item/photo/24017/visual_item/image> a crm:D1_Digital_Object .
<work/10363/visual_item/photo/24017/visual_item/image> crm:P1_is_identified_by <https://iiif.artresearch.net/iiif/3/zeri/40000/30400/30374.jpg/full/full/0/default.jpg> .
<work/10363/visual_item/photo/24017/visual_item/image> crm:P2_has_type pharos-meta:digital_image .
    |]
