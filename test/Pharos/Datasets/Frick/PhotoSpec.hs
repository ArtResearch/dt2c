module Pharos.Datasets.Frick.PhotoSpec (spec) where

import CommonImports
import Test.Hspec
import Pharos.Datasets.Frick.Mappings.Photo (photoLinks)
import Pharos.Datasets.Frick.Mappings.Work (baseMapping, frickBaseUri)


spec :: Spec
spec = describe "photo mappings" $ do

  let mapping = baseMapping +> photoLinks

  it "should process FARL Negative witht digital image" $ do
    let frickXml = [w|
      <record>
        <controlfield tag="001">991013393139707141</controlfield>
          <datafield tag="590" ind1="9" ind2=" ">
            <subfield code="a">Photograph,</subfield>
            <subfield code="b">Thurman Rotan</subfield>
            <subfield code="d">56590</subfield>
            <subfield code="f">good</subfield>
            <subfield code="k">Frick Art Reference Library, New York (FARL) negative.</subfield>
            <subfield code="n">m</subfield>
          </datafield>
        <datafield tag="590" ind1="0" ind2=" ">
          <subfield code="a">Digital image,</subfield>
          <subfield code="g">56590_POST.tif</subfield> 
          <subfield code="h">negative.</subfield>
        </datafield>
      </record>
    |]
    result <- processXMLStringAsSet mapping frickBaseUri frickXml
    result `shouldBe` [t|
<actor/photographer/thurman_rotan> a crm:E39_Actor .
<actor/photographer/thurman_rotan> crm:P1_is_identified_by <actor/photographer/thurman_rotan/appellation/preferred_name> .
<actor/photographer/thurman_rotan> crm:P2_has_type pharos-meta:photographer .
<actor/photographer/thurman_rotan> custom:sameAs <http://www.wikidata.org/entity/Q42315270> .
<actor/photographer/thurman_rotan/appellation/preferred_name> a crm:E41_Appellation .
<actor/photographer/thurman_rotan/appellation/preferred_name> crm:P190_has_symbolic_content "Thurman Rotan" .
<actor/photographer/thurman_rotan/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<http://rightsstatements.org/vocab/UND/1.0/> a crm:E55_Type .
<http://www.wikidata.org/entity/Q42315270> a crm:E39_Actor .
<https://artresearch.net/resource/e31/frick> a crm:E31_Document .
<https://artresearch.net/resource/provider/frick> a crm:E39_Actor .
<https://iiif.artresearch.net/iiif/3/frick/56590_POST.tif/full/full/0/default.jpg> a crm:E42_Identifier .
<https://iiif.artresearch.net/iiif/3/frick/56590_POST.tif/full/full/0/default.jpg> crm:P2_has_type pharos-meta:photo_file_url .
<https://iiif.artresearch.net/iiif/3/frick/56590_POST.tif/full/full/0/default.jpg> image-api:storage-id "frick" .
<work/991013393139707141> a crm:E22_Human-Made_Object .
<work/991013393139707141> crm:P138i_has_representation <work/991013393139707141/visual_item> .
<work/991013393139707141/visual_item> a crm:E36_Visual_Item .
<work/991013393139707141/visual_item> crm:P65i_is_shown_by <work/991013393139707141/visual_item/photo/farl_negative/56590> .
<work/991013393139707141/visual_item/photo/farl_negative/56590> a crm:E22_Human-Made_Object .
<work/991013393139707141/visual_item/photo/farl_negative/56590> crm:P104_is_subject_to <work/991013393139707141/visual_item/photo/farl_negative/56590/rights> .
<work/991013393139707141/visual_item/photo/farl_negative/56590> crm:P108i_was_produced_by <work/991013393139707141/visual_item/photo/farl_negative/56590/production> .
<work/991013393139707141/visual_item/photo/farl_negative/56590> crm:P138i_has_representation <work/991013393139707141/visual_item/photo/farl_negative/56590/visual_item> .
<work/991013393139707141/visual_item/photo/farl_negative/56590> crm:P1_is_identified_by <work/991013393139707141/visual_item/photo/farl_negative/56590/id/farl-negative-number> .
<work/991013393139707141/visual_item/photo/farl_negative/56590> crm:P2_has_type pharos-meta:photograph_negative .
<work/991013393139707141/visual_item/photo/farl_negative/56590> crm:P50_has_current_keeper <https://artresearch.net/resource/provider/frick> .
<work/991013393139707141/visual_item/photo/farl_negative/56590> crm:P70i_is_documented_in <https://artresearch.net/resource/e31/frick> .
<work/991013393139707141/visual_item/photo/farl_negative/56590> custom:imageOrder "1"^^xsd:integer .
<work/991013393139707141/visual_item/photo/farl_negative/56590/id/farl-negative-number> a crm:E42_Identifier .
<work/991013393139707141/visual_item/photo/farl_negative/56590/id/farl-negative-number> crm:P190_has_symbolic_content "56590" .
<work/991013393139707141/visual_item/photo/farl_negative/56590/id/farl-negative-number> crm:P2_has_type <vocab/meta/farl-negative-number> .
<work/991013393139707141/visual_item/photo/farl_negative/56590/production> a crm:E12_Production .
<work/991013393139707141/visual_item/photo/farl_negative/56590/production> crm:P14_carried_out_by <actor/photographer/thurman_rotan> .
<work/991013393139707141/visual_item/photo/farl_negative/56590/rights> a crm:E30_Right .
<work/991013393139707141/visual_item/photo/farl_negative/56590/rights> crm:P2_has_type <http://rightsstatements.org/vocab/UND/1.0/> .
<work/991013393139707141/visual_item/photo/farl_negative/56590/visual_item> a crm:E36_Visual_Item .
<work/991013393139707141/visual_item/photo/farl_negative/56590/visual_item> crm:P165i_is_incorporated_in <work/991013393139707141/visual_item/photo/farl_negative/56590/visual_item/image> .
<work/991013393139707141/visual_item/photo/farl_negative/56590/visual_item/image> a crm:D1_Digital_Object .
<work/991013393139707141/visual_item/photo/farl_negative/56590/visual_item/image> crm:P1_is_identified_by <https://iiif.artresearch.net/iiif/3/frick/56590_POST.tif/full/full/0/default.jpg> .
<work/991013393139707141/visual_item/photo/farl_negative/56590/visual_item/image> crm:P2_has_type pharos-meta:digital_image .
    |]


  it "should process photographic print witht digital image" $ do
    -- when we have one photo object and multiple digital images 
    -- at the moment, we should associate only image that has 01. in the name
    -- e.g 3107100116035_001.tif
    -- for other images we don't know what they represent, e.g recto or verso, etc.
    let frickXml = [w|
      <record>
        <controlfield tag="001">991007483569707141</controlfield>
        <datafield tag="590" ind1="9" ind2=" ">
          <subfield code="a">Photograph,</subfield>
          <subfield code="b">Museum of Fine Arts, Boston photograph</subfield>
          <subfield code="c">B9360</subfield>
          <subfield code="j">Purchase,</subfield>
          <subfield code="k">Museum of Fine Arts, Boston, Boston.</subfield>
        </datafield>
        <datafield tag="590" ind1="0" ind2=" ">
          <subfield code="a">Digital image,</subfield>
          <subfield code="g">3107100116035_001.tif</subfield>
          <subfield code="h">(image).</subfield>
        </datafield>
        <datafield tag="590" ind1="0" ind2=" ">
          <subfield code="a">Digital image,</subfield>
          <subfield code="g">3107100116035_002.tif</subfield>
          <subfield code="h">(image).</subfield>
        </datafield>
        <datafield tag="590" ind1="0" ind2=" ">
          <subfield code="a">Digital image,</subfield>
          <subfield code="g">3107100116035_003.tif</subfield>
          <subfield code="h">(documentation).</subfield>
        </datafield>
      </record>
    |]
    result <- processXMLStringAsSet mapping frickBaseUri frickXml
    result `shouldBe` [t|
<actor/photographer/museum_of_fine_arts_boston_photograph> a crm:E39_Actor .
<actor/photographer/museum_of_fine_arts_boston_photograph> crm:P1_is_identified_by <actor/photographer/museum_of_fine_arts_boston_photograph/appellation/preferred_name> .
<actor/photographer/museum_of_fine_arts_boston_photograph> crm:P2_has_type pharos-meta:photographer .
<actor/photographer/museum_of_fine_arts_boston_photograph> custom:sameAs <http://www.wikidata.org/entity/Q49133> .
<actor/photographer/museum_of_fine_arts_boston_photograph/appellation/preferred_name> a crm:E41_Appellation .
<actor/photographer/museum_of_fine_arts_boston_photograph/appellation/preferred_name> crm:P190_has_symbolic_content "Museum of Fine Arts, Boston photograph" .
<actor/photographer/museum_of_fine_arts_boston_photograph/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<http://rightsstatements.org/vocab/UND/1.0/> a crm:E55_Type .
<http://www.wikidata.org/entity/Q49133> a crm:E39_Actor .
<https://artresearch.net/resource/e31/frick> a crm:E31_Document .
<https://artresearch.net/resource/provider/frick> a crm:E39_Actor .
<https://iiif.artresearch.net/iiif/3/frick/3107100116035_001.tif/full/full/0/default.jpg> a crm:E42_Identifier .
<https://iiif.artresearch.net/iiif/3/frick/3107100116035_001.tif/full/full/0/default.jpg> crm:P2_has_type pharos-meta:photo_file_url .
<https://iiif.artresearch.net/iiif/3/frick/3107100116035_001.tif/full/full/0/default.jpg> image-api:storage-id "frick" .
<work/991007483569707141> a crm:E22_Human-Made_Object .
<work/991007483569707141> crm:P138i_has_representation <work/991007483569707141/visual_item> .
<work/991007483569707141/visual_item> a crm:E36_Visual_Item .
<work/991007483569707141/visual_item> crm:P65i_is_shown_by <work/991007483569707141/visual_item/photo/print/1> .
<work/991007483569707141/visual_item/photo/print/1> a crm:E22_Human-Made_Object .
<work/991007483569707141/visual_item/photo/print/1> crm:P104_is_subject_to <work/991007483569707141/visual_item/photo/print/1/rights> .
<work/991007483569707141/visual_item/photo/print/1> crm:P108i_was_produced_by <work/991007483569707141/visual_item/photo/print/1/production> .
<work/991007483569707141/visual_item/photo/print/1> crm:P138i_has_representation <work/991007483569707141/visual_item/photo/print/1/visual_item> .
<work/991007483569707141/visual_item/photo/print/1> crm:P2_has_type pharos-meta:photographic_print .
<work/991007483569707141/visual_item/photo/print/1> crm:P50_has_current_keeper <https://artresearch.net/resource/provider/frick> .
<work/991007483569707141/visual_item/photo/print/1> crm:P70i_is_documented_in <https://artresearch.net/resource/e31/frick> .
<work/991007483569707141/visual_item/photo/print/1> custom:imageOrder "1"^^xsd:integer .
<work/991007483569707141/visual_item/photo/print/1/production> a crm:E12_Production .
<work/991007483569707141/visual_item/photo/print/1/production> crm:P14_carried_out_by <actor/photographer/museum_of_fine_arts_boston_photograph> .
<work/991007483569707141/visual_item/photo/print/1/rights> a crm:E30_Right .
<work/991007483569707141/visual_item/photo/print/1/rights> crm:P2_has_type <http://rightsstatements.org/vocab/UND/1.0/> .
<work/991007483569707141/visual_item/photo/print/1/visual_item> a crm:E36_Visual_Item .
<work/991007483569707141/visual_item/photo/print/1/visual_item> crm:P165i_is_incorporated_in <work/991007483569707141/visual_item/photo/print/1/visual_item/image> .
<work/991007483569707141/visual_item/photo/print/1/visual_item/image> a crm:D1_Digital_Object .
<work/991007483569707141/visual_item/photo/print/1/visual_item/image> crm:P1_is_identified_by <https://iiif.artresearch.net/iiif/3/frick/3107100116035_001.tif/full/full/0/default.jpg> .
<work/991007483569707141/visual_item/photo/print/1/visual_item/image> crm:P2_has_type pharos-meta:digital_image .
    |]


  it "multiple photographs and negatives" $ do
    -- when we have many photo objects and many digital images 
    -- at the moment, we should associate only image that has 01. in the name
    -- e.g 3107100116035_001.tif with the first photo object.
    -- For other images we don't know what photograph they represent.
    let frickXml = [w|
      <record>
        <controlfield tag="001">991003705449707141</controlfield>
        <datafield tag="590" ind1="1" ind2=" ">
            <subfield code="a">Palazzo Comunale,</subfield>
            <subfield code="b">San Gimignano,</subfield>
            <subfield code="d">Italy,</subfield>
            <subfield code="g">public.</subfield>
          </datafield>
          <datafield tag="590" ind1="9" ind2=" ">
            <subfield code="a">Photograph,</subfield>
            <subfield code="b">Sansoni</subfield>
            <subfield code="d">22231</subfield>
            <subfield code="f">taped</subfield>
            <subfield code="k">Frick Art Reference Library, New York (FARL) negative</subfield>
            <subfield code="m">(whole view).</subfield>
            <subfield code="n">m</subfield>
          </datafield>
          <datafield tag="590" ind1="9" ind2=" ">
            <subfield code="a">Photograph,</subfield>
            <subfield code="b">Sansoni</subfield>
            <subfield code="d">22232</subfield>
            <subfield code="f">taped</subfield>
            <subfield code="k">Frick Art Reference Library, New York (FARL) negative</subfield>
            <subfield code="m">(detail, right side).</subfield>
            <subfield code="n">2</subfield>
          </datafield>
          <datafield tag="590" ind1="9" ind2=" ">
            <subfield code="a">Photograph,</subfield>
            <subfield code="b">Sansoni</subfield>
            <subfield code="d">22233</subfield>
            <subfield code="f">taped</subfield>
            <subfield code="k">Frick Art Reference Library, New York (FARL) negative</subfield>
            <subfield code="m">(detail, right center).</subfield>
            <subfield code="n">1</subfield>
          </datafield>
          <datafield tag="590" ind1="9" ind2=" ">
            <subfield code="a">Photograph,</subfield>
            <subfield code="b">Sansoni</subfield>
            <subfield code="d">22234</subfield>
            <subfield code="f">taped</subfield>
            <subfield code="k">Frick Art Reference Library, New York (FARL) negative</subfield>
            <subfield code="m">(detail, center).</subfield>
            <subfield code="n">1</subfield>
          </datafield>
          <datafield tag="590" ind1="9" ind2=" ">
            <subfield code="a">Photograph,</subfield>
            <subfield code="b">Sansoni</subfield>
            <subfield code="d">22235</subfield>
            <subfield code="f">taped</subfield>
            <subfield code="k">Frick Art Reference Library, New York (FARL) negative</subfield>
            <subfield code="m">(detail, left center).</subfield>
            <subfield code="n">1</subfield>
          </datafield>
          <datafield tag="590" ind1="9" ind2=" ">
            <subfield code="a">Photograph,</subfield>
            <subfield code="b">Sansoni</subfield>
            <subfield code="d">22236</subfield>
            <subfield code="f">taped</subfield>
            <subfield code="k">Frick Art Reference Library, New York (FARL) negative</subfield>
            <subfield code="m">(detail, right center).</subfield>
            <subfield code="n">2</subfield>
          </datafield>
          <datafield tag="590" ind1="9" ind2=" ">
            <subfield code="a">Photograph,</subfield>
            <subfield code="b">Alinari, Florence</subfield>
            <subfield code="c">9588</subfield>
            <subfield code="j">Purchase,</subfield>
            <subfield code="k">Alinari, Florence</subfield>
            <subfield code="m">(detail).</subfield>
            <subfield code="n">m</subfield>
          </datafield>
          <datafield tag="590" ind1="9" ind2=" ">
            <subfield code="a">Photograph,</subfield>
            <subfield code="b">Giacomo Brogi</subfield>
            <subfield code="c">15300,</subfield>
            <subfield code="j">Purchase,</subfield>
            <subfield code="k">Giacomo Brogi.</subfield>
            <subfield code="n">m</subfield>
          </datafield>
          <datafield tag="590" ind1="9" ind2=" ">
            <subfield code="a">Photograph,</subfield>
            <subfield code="b">Giacomo Brogi</subfield>
            <subfield code="c">15301,</subfield>
            <subfield code="j">Purchase,</subfield>
            <subfield code="k">Giacomo Brogi</subfield>
            <subfield code="m">(detail, center group).</subfield>
            <subfield code="n">m</subfield>
          </datafield>
          <datafield tag="590" ind1="0" ind2=" ">
            <subfield code="a">Digital image,</subfield>
            <subfield code="g">22231_POST.tif</subfield>
            <subfield code="h">negative</subfield>
            <subfield code="m">(whole view).</subfield>
          </datafield>
          <datafield tag="590" ind1="0" ind2=" ">
            <subfield code="a">Digital image,</subfield>
            <subfield code="g">22232_POST.tif</subfield>
            <subfield code="h">negative</subfield>
            <subfield code="m">(detail, right side).</subfield>
          </datafield>
          <datafield tag="590" ind1="0" ind2=" ">
            <subfield code="a">Digital image,</subfield>
            <subfield code="g">22233_POST.tif</subfield>
            <subfield code="h">negative</subfield>
            <subfield code="m">(detail, right center).</subfield>
          </datafield>
          <datafield tag="590" ind1="0" ind2=" ">
            <subfield code="a">Digital image,</subfield>
            <subfield code="g">22234_POST.tif</subfield>
            <subfield code="h">negative</subfield>
            <subfield code="m">(detail, center).</subfield>
          </datafield>
          <datafield tag="590" ind1="0" ind2=" ">
            <subfield code="a">Digital image,</subfield>
            <subfield code="g">22235_POST.tif</subfield>
            <subfield code="h">negative</subfield>
            <subfield code="m">(detail, left center).</subfield>
          </datafield>
        <datafield tag="590" ind1="0" ind2=" ">
            <subfield code="a">Digital image,</subfield>
            <subfield code="g">22236_POST.tif</subfield>
            <subfield code="h">negative</subfield>
            <subfield code="m">(detail, left side).</subfield>
          </datafield>
          <datafield tag="590" ind1="0" ind2=" ">
            <subfield code="a">Digital image,</subfield>
            <subfield code="g">31071000057806.tif</subfield>
            <subfield code="h">(documentation).</subfield>
          </datafield>
          <datafield tag="590" ind1="0" ind2=" ">
            <subfield code="a">Digital image,</subfield>
            <subfield code="g">31071000057806_a_POST.tif</subfield>
            <subfield code="h">(image)..</subfield>
          </datafield>
          <datafield tag="590" ind1="0" ind2=" ">
            <subfield code="a">Digital image,</subfield>
            <subfield code="g">31071000065486.tif</subfield>
            <subfield code="h">(documentation).</subfield>
          </datafield>
          <datafield tag="590" ind1="0" ind2=" ">
            <subfield code="a">Digital image,</subfield>
            <subfield code="g">31071000061071.tif</subfield>
            <subfield code="h">(documentation).</subfield>
          </datafield>
          <datafield tag="590" ind1="0" ind2=" ">
            <subfield code="a">Digital image,</subfield>
            <subfield code="g">31071000061071_a_POST.tif</subfield>
            <subfield code="h">(image).</subfield>
          </datafield>
          <datafield tag="590" ind1="0" ind2=" ">
            <subfield code="a">Digital image,</subfield>
            <subfield code="g">4107100212645_0001.jp2</subfield>
          </datafield>
          <datafield tag="590" ind1="0" ind2=" ">
            <subfield code="a">Digital image,</subfield>
            <subfield code="g">4107100212645_0002.jp2</subfield>
          </datafield>
          <datafield tag="590" ind1="0" ind2=" ">
            <subfield code="a">Digital image,</subfield>
            <subfield code="g">4107100212645_0003.jp2</subfield>
          </datafield>
          <datafield tag="590" ind1="0" ind2=" ">
            <subfield code="a">Digital image,</subfield>
            <subfield code="g">4107100212645_0004.jp2</subfield>
          </datafield>
          <datafield tag="590" ind1="0" ind2=" ">
            <subfield code="a">Digital image,</subfield>
            <subfield code="g">4107100212645_0005.jp2</subfield>
          </datafield>
          <datafield tag="590" ind1="0" ind2=" ">
            <subfield code="a">Digital image,</subfield>
            <subfield code="g">4107100212645_0006.jp2</subfield>
          </datafield>
          <datafield tag="590" ind1="0" ind2=" ">
            <subfield code="a">Digital image,</subfield>
            <subfield code="g">4107100212645_0007.jp2</subfield>
          </datafield>
          <datafield tag="590" ind1="0" ind2=" ">
            <subfield code="a">Digital image,</subfield>
            <subfield code="g">4107100212645_0008.jp2</subfield>
          </datafield>
          <datafield tag="590" ind1="0" ind2=" ">
            <subfield code="a">Digital image,</subfield>
            <subfield code="g">4107100212645_0009.jp2</subfield>
          </datafield>
          <datafield tag="590" ind1="0" ind2=" ">
            <subfield code="a">Digital image,</subfield>
            <subfield code="g">4107100212645_0010.jp2</subfield>
          </datafield>
          <datafield tag="590" ind1="0" ind2=" ">
            <subfield code="a">Digital image,</subfield>
            <subfield code="g">4107100212645_0011.jp2</subfield>
          </datafield>
          <datafield tag="590" ind1="0" ind2=" ">
            <subfield code="a">Digital image,</subfield>
            <subfield code="g">4107100212645_0012.jp2</subfield>
          </datafield>
          <datafield tag="590" ind1="0" ind2=" ">
            <subfield code="a">Digital image,</subfield>
            <subfield code="g">4107100212645_0013.jp2</subfield>
          </datafield>
          <datafield tag="590" ind1="0" ind2=" ">
            <subfield code="a">Digital image,</subfield>
            <subfield code="g">4107100212645_0014.jp2</subfield>
          </datafield>
          <datafield tag="590" ind1="0" ind2=" ">
            <subfield code="a">Digital image,</subfield>
            <subfield code="g">4107100212645_0015.jp2</subfield>
          </datafield>
          <datafield tag="590" ind1="0" ind2=" ">
            <subfield code="a">Digital image,</subfield>
            <subfield code="g">4107100212645_0016.jp2</subfield>
          </datafield>
          <datafield tag="590" ind1="0" ind2=" ">
            <subfield code="a">Digital image,</subfield>
            <subfield code="g">4107100212645_0017.jp2</subfield>
          </datafield>
      </record>
    |]
    result <- processXMLStringAsSet mapping frickBaseUri frickXml
    result `shouldBe` [t|
<actor/photographer/alinari_florence> a crm:E39_Actor .
<actor/photographer/alinari_florence> crm:P1_is_identified_by <actor/photographer/alinari_florence/appellation/preferred_name> .
<actor/photographer/alinari_florence> crm:P2_has_type pharos-meta:photographer .
<actor/photographer/alinari_florence> custom:sameAs <http://www.wikidata.org/entity/Q644689> .
<actor/photographer/alinari_florence/appellation/preferred_name> a crm:E41_Appellation .
<actor/photographer/alinari_florence/appellation/preferred_name> crm:P190_has_symbolic_content "Alinari, Florence" .
<actor/photographer/alinari_florence/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<actor/photographer/giacomo_brogi> a crm:E39_Actor .
<actor/photographer/giacomo_brogi> crm:P1_is_identified_by <actor/photographer/giacomo_brogi/appellation/preferred_name> .
<actor/photographer/giacomo_brogi> crm:P2_has_type pharos-meta:photographer .
<actor/photographer/giacomo_brogi> custom:sameAs <http://www.wikidata.org/entity/Q85724796> .
<actor/photographer/giacomo_brogi/appellation/preferred_name> a crm:E41_Appellation .
<actor/photographer/giacomo_brogi/appellation/preferred_name> crm:P190_has_symbolic_content "Giacomo Brogi" .
<actor/photographer/giacomo_brogi/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<actor/photographer/sansoni> a crm:E39_Actor .
<actor/photographer/sansoni> crm:P1_is_identified_by <actor/photographer/sansoni/appellation/preferred_name> .
<actor/photographer/sansoni> crm:P2_has_type pharos-meta:photographer .
<actor/photographer/sansoni> custom:sameAs <http://www.wikidata.org/entity/Q106652385> .
<actor/photographer/sansoni/appellation/preferred_name> a crm:E41_Appellation .
<actor/photographer/sansoni/appellation/preferred_name> crm:P190_has_symbolic_content "Sansoni" .
<actor/photographer/sansoni/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<http://rightsstatements.org/vocab/UND/1.0/> a crm:E55_Type .
<http://www.wikidata.org/entity/Q106652385> a crm:E39_Actor .
<http://www.wikidata.org/entity/Q644689> a crm:E39_Actor .
<http://www.wikidata.org/entity/Q85724796> a crm:E39_Actor .
<https://artresearch.net/resource/e31/frick> a crm:E31_Document .
<https://artresearch.net/resource/provider/frick> a crm:E39_Actor .
<https://iiif.artresearch.net/iiif/3/frick/22231_POST.tif/full/full/0/default.jpg> a crm:E42_Identifier .
<https://iiif.artresearch.net/iiif/3/frick/22231_POST.tif/full/full/0/default.jpg> crm:P2_has_type pharos-meta:photo_file_url .
<https://iiif.artresearch.net/iiif/3/frick/22231_POST.tif/full/full/0/default.jpg> image-api:storage-id "frick" .
<https://iiif.artresearch.net/iiif/3/frick/22232_POST.tif/full/full/0/default.jpg> a crm:E42_Identifier .
<https://iiif.artresearch.net/iiif/3/frick/22232_POST.tif/full/full/0/default.jpg> crm:P2_has_type pharos-meta:photo_file_url .
<https://iiif.artresearch.net/iiif/3/frick/22232_POST.tif/full/full/0/default.jpg> image-api:storage-id "frick" .
<https://iiif.artresearch.net/iiif/3/frick/22233_POST.tif/full/full/0/default.jpg> a crm:E42_Identifier .
<https://iiif.artresearch.net/iiif/3/frick/22233_POST.tif/full/full/0/default.jpg> crm:P2_has_type pharos-meta:photo_file_url .
<https://iiif.artresearch.net/iiif/3/frick/22233_POST.tif/full/full/0/default.jpg> image-api:storage-id "frick" .
<https://iiif.artresearch.net/iiif/3/frick/22234_POST.tif/full/full/0/default.jpg> a crm:E42_Identifier .
<https://iiif.artresearch.net/iiif/3/frick/22234_POST.tif/full/full/0/default.jpg> crm:P2_has_type pharos-meta:photo_file_url .
<https://iiif.artresearch.net/iiif/3/frick/22234_POST.tif/full/full/0/default.jpg> image-api:storage-id "frick" .
<https://iiif.artresearch.net/iiif/3/frick/22235_POST.tif/full/full/0/default.jpg> a crm:E42_Identifier .
<https://iiif.artresearch.net/iiif/3/frick/22235_POST.tif/full/full/0/default.jpg> crm:P2_has_type pharos-meta:photo_file_url .
<https://iiif.artresearch.net/iiif/3/frick/22235_POST.tif/full/full/0/default.jpg> image-api:storage-id "frick" .
<https://iiif.artresearch.net/iiif/3/frick/22236_POST.tif/full/full/0/default.jpg> a crm:E42_Identifier .
<https://iiif.artresearch.net/iiif/3/frick/22236_POST.tif/full/full/0/default.jpg> crm:P2_has_type pharos-meta:photo_file_url .
<https://iiif.artresearch.net/iiif/3/frick/22236_POST.tif/full/full/0/default.jpg> image-api:storage-id "frick" .
<work/991003705449707141> a crm:E22_Human-Made_Object .
<work/991003705449707141> crm:P138i_has_representation <work/991003705449707141/visual_item> .
<work/991003705449707141/visual_item> a crm:E36_Visual_Item .
<work/991003705449707141/visual_item> crm:P65i_is_shown_by <work/991003705449707141/visual_item/photo/farl_negative/22231> .
<work/991003705449707141/visual_item> crm:P65i_is_shown_by <work/991003705449707141/visual_item/photo/farl_negative/22232> .
<work/991003705449707141/visual_item> crm:P65i_is_shown_by <work/991003705449707141/visual_item/photo/farl_negative/22233> .
<work/991003705449707141/visual_item> crm:P65i_is_shown_by <work/991003705449707141/visual_item/photo/farl_negative/22234> .
<work/991003705449707141/visual_item> crm:P65i_is_shown_by <work/991003705449707141/visual_item/photo/farl_negative/22235> .
<work/991003705449707141/visual_item> crm:P65i_is_shown_by <work/991003705449707141/visual_item/photo/farl_negative/22236> .
<work/991003705449707141/visual_item> crm:P65i_is_shown_by <work/991003705449707141/visual_item/photo/print/7> .
<work/991003705449707141/visual_item> crm:P65i_is_shown_by <work/991003705449707141/visual_item/photo/print/8> .
<work/991003705449707141/visual_item> crm:P65i_is_shown_by <work/991003705449707141/visual_item/photo/print/9> .
<work/991003705449707141/visual_item/photo/farl_negative/22231> a crm:E22_Human-Made_Object .
<work/991003705449707141/visual_item/photo/farl_negative/22231> crm:P104_is_subject_to <work/991003705449707141/visual_item/photo/farl_negative/22231/rights> .
<work/991003705449707141/visual_item/photo/farl_negative/22231> crm:P108i_was_produced_by <work/991003705449707141/visual_item/photo/farl_negative/22231/production> .
<work/991003705449707141/visual_item/photo/farl_negative/22231> crm:P138i_has_representation <work/991003705449707141/visual_item/photo/farl_negative/22231/visual_item> .
<work/991003705449707141/visual_item/photo/farl_negative/22231> crm:P1_is_identified_by <work/991003705449707141/visual_item/photo/farl_negative/22231/id/farl-negative-number> .
<work/991003705449707141/visual_item/photo/farl_negative/22231> crm:P2_has_type pharos-meta:photograph_negative .
<work/991003705449707141/visual_item/photo/farl_negative/22231> crm:P50_has_current_keeper <https://artresearch.net/resource/provider/frick> .
<work/991003705449707141/visual_item/photo/farl_negative/22231> crm:P70i_is_documented_in <https://artresearch.net/resource/e31/frick> .
<work/991003705449707141/visual_item/photo/farl_negative/22231> custom:imageOrder "1"^^xsd:integer .
<work/991003705449707141/visual_item/photo/farl_negative/22231/id/farl-negative-number> a crm:E42_Identifier .
<work/991003705449707141/visual_item/photo/farl_negative/22231/id/farl-negative-number> crm:P190_has_symbolic_content "22231" .
<work/991003705449707141/visual_item/photo/farl_negative/22231/id/farl-negative-number> crm:P2_has_type <vocab/meta/farl-negative-number> .
<work/991003705449707141/visual_item/photo/farl_negative/22231/production> a crm:E12_Production .
<work/991003705449707141/visual_item/photo/farl_negative/22231/production> crm:P14_carried_out_by <actor/photographer/sansoni> .
<work/991003705449707141/visual_item/photo/farl_negative/22231/rights> a crm:E30_Right .
<work/991003705449707141/visual_item/photo/farl_negative/22231/rights> crm:P2_has_type <http://rightsstatements.org/vocab/UND/1.0/> .
<work/991003705449707141/visual_item/photo/farl_negative/22231/visual_item> a crm:E36_Visual_Item .
<work/991003705449707141/visual_item/photo/farl_negative/22231/visual_item> crm:P165i_is_incorporated_in <work/991003705449707141/visual_item/photo/farl_negative/22231/visual_item/image> .
<work/991003705449707141/visual_item/photo/farl_negative/22231/visual_item/image> a crm:D1_Digital_Object .
<work/991003705449707141/visual_item/photo/farl_negative/22231/visual_item/image> crm:P1_is_identified_by <https://iiif.artresearch.net/iiif/3/frick/22231_POST.tif/full/full/0/default.jpg> .
<work/991003705449707141/visual_item/photo/farl_negative/22231/visual_item/image> crm:P2_has_type pharos-meta:digital_image .
<work/991003705449707141/visual_item/photo/farl_negative/22232> a crm:E22_Human-Made_Object .
<work/991003705449707141/visual_item/photo/farl_negative/22232> crm:P104_is_subject_to <work/991003705449707141/visual_item/photo/farl_negative/22232/rights> .
<work/991003705449707141/visual_item/photo/farl_negative/22232> crm:P108i_was_produced_by <work/991003705449707141/visual_item/photo/farl_negative/22232/production> .
<work/991003705449707141/visual_item/photo/farl_negative/22232> crm:P138i_has_representation <work/991003705449707141/visual_item/photo/farl_negative/22232/visual_item> .
<work/991003705449707141/visual_item/photo/farl_negative/22232> crm:P1_is_identified_by <work/991003705449707141/visual_item/photo/farl_negative/22232/id/farl-negative-number> .
<work/991003705449707141/visual_item/photo/farl_negative/22232> crm:P2_has_type pharos-meta:photograph_negative .
<work/991003705449707141/visual_item/photo/farl_negative/22232> crm:P50_has_current_keeper <https://artresearch.net/resource/provider/frick> .
<work/991003705449707141/visual_item/photo/farl_negative/22232> crm:P70i_is_documented_in <https://artresearch.net/resource/e31/frick> .
<work/991003705449707141/visual_item/photo/farl_negative/22232> custom:imageOrder "2"^^xsd:integer .
<work/991003705449707141/visual_item/photo/farl_negative/22232/id/farl-negative-number> a crm:E42_Identifier .
<work/991003705449707141/visual_item/photo/farl_negative/22232/id/farl-negative-number> crm:P190_has_symbolic_content "22232" .
<work/991003705449707141/visual_item/photo/farl_negative/22232/id/farl-negative-number> crm:P2_has_type <vocab/meta/farl-negative-number> .
<work/991003705449707141/visual_item/photo/farl_negative/22232/production> a crm:E12_Production .
<work/991003705449707141/visual_item/photo/farl_negative/22232/production> crm:P14_carried_out_by <actor/photographer/sansoni> .
<work/991003705449707141/visual_item/photo/farl_negative/22232/rights> a crm:E30_Right .
<work/991003705449707141/visual_item/photo/farl_negative/22232/rights> crm:P2_has_type <http://rightsstatements.org/vocab/UND/1.0/> .
<work/991003705449707141/visual_item/photo/farl_negative/22232/visual_item> a crm:E36_Visual_Item .
<work/991003705449707141/visual_item/photo/farl_negative/22232/visual_item> crm:P165i_is_incorporated_in <work/991003705449707141/visual_item/photo/farl_negative/22232/visual_item/image> .
<work/991003705449707141/visual_item/photo/farl_negative/22232/visual_item/image> a crm:D1_Digital_Object .
<work/991003705449707141/visual_item/photo/farl_negative/22232/visual_item/image> crm:P1_is_identified_by <https://iiif.artresearch.net/iiif/3/frick/22232_POST.tif/full/full/0/default.jpg> .
<work/991003705449707141/visual_item/photo/farl_negative/22232/visual_item/image> crm:P2_has_type pharos-meta:digital_image .
<work/991003705449707141/visual_item/photo/farl_negative/22233> a crm:E22_Human-Made_Object .
<work/991003705449707141/visual_item/photo/farl_negative/22233> crm:P104_is_subject_to <work/991003705449707141/visual_item/photo/farl_negative/22233/rights> .
<work/991003705449707141/visual_item/photo/farl_negative/22233> crm:P108i_was_produced_by <work/991003705449707141/visual_item/photo/farl_negative/22233/production> .
<work/991003705449707141/visual_item/photo/farl_negative/22233> crm:P138i_has_representation <work/991003705449707141/visual_item/photo/farl_negative/22233/visual_item> .
<work/991003705449707141/visual_item/photo/farl_negative/22233> crm:P1_is_identified_by <work/991003705449707141/visual_item/photo/farl_negative/22233/id/farl-negative-number> .
<work/991003705449707141/visual_item/photo/farl_negative/22233> crm:P2_has_type pharos-meta:photograph_negative .
<work/991003705449707141/visual_item/photo/farl_negative/22233> crm:P50_has_current_keeper <https://artresearch.net/resource/provider/frick> .
<work/991003705449707141/visual_item/photo/farl_negative/22233> crm:P70i_is_documented_in <https://artresearch.net/resource/e31/frick> .
<work/991003705449707141/visual_item/photo/farl_negative/22233> custom:imageOrder "3"^^xsd:integer .
<work/991003705449707141/visual_item/photo/farl_negative/22233/id/farl-negative-number> a crm:E42_Identifier .
<work/991003705449707141/visual_item/photo/farl_negative/22233/id/farl-negative-number> crm:P190_has_symbolic_content "22233" .
<work/991003705449707141/visual_item/photo/farl_negative/22233/id/farl-negative-number> crm:P2_has_type <vocab/meta/farl-negative-number> .
<work/991003705449707141/visual_item/photo/farl_negative/22233/production> a crm:E12_Production .
<work/991003705449707141/visual_item/photo/farl_negative/22233/production> crm:P14_carried_out_by <actor/photographer/sansoni> .
<work/991003705449707141/visual_item/photo/farl_negative/22233/rights> a crm:E30_Right .
<work/991003705449707141/visual_item/photo/farl_negative/22233/rights> crm:P2_has_type <http://rightsstatements.org/vocab/UND/1.0/> .
<work/991003705449707141/visual_item/photo/farl_negative/22233/visual_item> a crm:E36_Visual_Item .
<work/991003705449707141/visual_item/photo/farl_negative/22233/visual_item> crm:P165i_is_incorporated_in <work/991003705449707141/visual_item/photo/farl_negative/22233/visual_item/image> .
<work/991003705449707141/visual_item/photo/farl_negative/22233/visual_item/image> a crm:D1_Digital_Object .
<work/991003705449707141/visual_item/photo/farl_negative/22233/visual_item/image> crm:P1_is_identified_by <https://iiif.artresearch.net/iiif/3/frick/22233_POST.tif/full/full/0/default.jpg> .
<work/991003705449707141/visual_item/photo/farl_negative/22233/visual_item/image> crm:P2_has_type pharos-meta:digital_image .
<work/991003705449707141/visual_item/photo/farl_negative/22234> a crm:E22_Human-Made_Object .
<work/991003705449707141/visual_item/photo/farl_negative/22234> crm:P104_is_subject_to <work/991003705449707141/visual_item/photo/farl_negative/22234/rights> .
<work/991003705449707141/visual_item/photo/farl_negative/22234> crm:P108i_was_produced_by <work/991003705449707141/visual_item/photo/farl_negative/22234/production> .
<work/991003705449707141/visual_item/photo/farl_negative/22234> crm:P138i_has_representation <work/991003705449707141/visual_item/photo/farl_negative/22234/visual_item> .
<work/991003705449707141/visual_item/photo/farl_negative/22234> crm:P1_is_identified_by <work/991003705449707141/visual_item/photo/farl_negative/22234/id/farl-negative-number> .
<work/991003705449707141/visual_item/photo/farl_negative/22234> crm:P2_has_type pharos-meta:photograph_negative .
<work/991003705449707141/visual_item/photo/farl_negative/22234> crm:P50_has_current_keeper <https://artresearch.net/resource/provider/frick> .
<work/991003705449707141/visual_item/photo/farl_negative/22234> crm:P70i_is_documented_in <https://artresearch.net/resource/e31/frick> .
<work/991003705449707141/visual_item/photo/farl_negative/22234> custom:imageOrder "4"^^xsd:integer .
<work/991003705449707141/visual_item/photo/farl_negative/22234/id/farl-negative-number> a crm:E42_Identifier .
<work/991003705449707141/visual_item/photo/farl_negative/22234/id/farl-negative-number> crm:P190_has_symbolic_content "22234" .
<work/991003705449707141/visual_item/photo/farl_negative/22234/id/farl-negative-number> crm:P2_has_type <vocab/meta/farl-negative-number> .
<work/991003705449707141/visual_item/photo/farl_negative/22234/production> a crm:E12_Production .
<work/991003705449707141/visual_item/photo/farl_negative/22234/production> crm:P14_carried_out_by <actor/photographer/sansoni> .
<work/991003705449707141/visual_item/photo/farl_negative/22234/rights> a crm:E30_Right .
<work/991003705449707141/visual_item/photo/farl_negative/22234/rights> crm:P2_has_type <http://rightsstatements.org/vocab/UND/1.0/> .
<work/991003705449707141/visual_item/photo/farl_negative/22234/visual_item> a crm:E36_Visual_Item .
<work/991003705449707141/visual_item/photo/farl_negative/22234/visual_item> crm:P165i_is_incorporated_in <work/991003705449707141/visual_item/photo/farl_negative/22234/visual_item/image> .
<work/991003705449707141/visual_item/photo/farl_negative/22234/visual_item/image> a crm:D1_Digital_Object .
<work/991003705449707141/visual_item/photo/farl_negative/22234/visual_item/image> crm:P1_is_identified_by <https://iiif.artresearch.net/iiif/3/frick/22234_POST.tif/full/full/0/default.jpg> .
<work/991003705449707141/visual_item/photo/farl_negative/22234/visual_item/image> crm:P2_has_type pharos-meta:digital_image .
<work/991003705449707141/visual_item/photo/farl_negative/22235> a crm:E22_Human-Made_Object .
<work/991003705449707141/visual_item/photo/farl_negative/22235> crm:P104_is_subject_to <work/991003705449707141/visual_item/photo/farl_negative/22235/rights> .
<work/991003705449707141/visual_item/photo/farl_negative/22235> crm:P108i_was_produced_by <work/991003705449707141/visual_item/photo/farl_negative/22235/production> .
<work/991003705449707141/visual_item/photo/farl_negative/22235> crm:P138i_has_representation <work/991003705449707141/visual_item/photo/farl_negative/22235/visual_item> .
<work/991003705449707141/visual_item/photo/farl_negative/22235> crm:P1_is_identified_by <work/991003705449707141/visual_item/photo/farl_negative/22235/id/farl-negative-number> .
<work/991003705449707141/visual_item/photo/farl_negative/22235> crm:P2_has_type pharos-meta:photograph_negative .
<work/991003705449707141/visual_item/photo/farl_negative/22235> crm:P50_has_current_keeper <https://artresearch.net/resource/provider/frick> .
<work/991003705449707141/visual_item/photo/farl_negative/22235> crm:P70i_is_documented_in <https://artresearch.net/resource/e31/frick> .
<work/991003705449707141/visual_item/photo/farl_negative/22235> custom:imageOrder "5"^^xsd:integer .
<work/991003705449707141/visual_item/photo/farl_negative/22235/id/farl-negative-number> a crm:E42_Identifier .
<work/991003705449707141/visual_item/photo/farl_negative/22235/id/farl-negative-number> crm:P190_has_symbolic_content "22235" .
<work/991003705449707141/visual_item/photo/farl_negative/22235/id/farl-negative-number> crm:P2_has_type <vocab/meta/farl-negative-number> .
<work/991003705449707141/visual_item/photo/farl_negative/22235/production> a crm:E12_Production .
<work/991003705449707141/visual_item/photo/farl_negative/22235/production> crm:P14_carried_out_by <actor/photographer/sansoni> .
<work/991003705449707141/visual_item/photo/farl_negative/22235/rights> a crm:E30_Right .
<work/991003705449707141/visual_item/photo/farl_negative/22235/rights> crm:P2_has_type <http://rightsstatements.org/vocab/UND/1.0/> .
<work/991003705449707141/visual_item/photo/farl_negative/22235/visual_item> a crm:E36_Visual_Item .
<work/991003705449707141/visual_item/photo/farl_negative/22235/visual_item> crm:P165i_is_incorporated_in <work/991003705449707141/visual_item/photo/farl_negative/22235/visual_item/image> .
<work/991003705449707141/visual_item/photo/farl_negative/22235/visual_item/image> a crm:D1_Digital_Object .
<work/991003705449707141/visual_item/photo/farl_negative/22235/visual_item/image> crm:P1_is_identified_by <https://iiif.artresearch.net/iiif/3/frick/22235_POST.tif/full/full/0/default.jpg> .
<work/991003705449707141/visual_item/photo/farl_negative/22235/visual_item/image> crm:P2_has_type pharos-meta:digital_image .
<work/991003705449707141/visual_item/photo/farl_negative/22236> a crm:E22_Human-Made_Object .
<work/991003705449707141/visual_item/photo/farl_negative/22236> crm:P104_is_subject_to <work/991003705449707141/visual_item/photo/farl_negative/22236/rights> .
<work/991003705449707141/visual_item/photo/farl_negative/22236> crm:P108i_was_produced_by <work/991003705449707141/visual_item/photo/farl_negative/22236/production> .
<work/991003705449707141/visual_item/photo/farl_negative/22236> crm:P138i_has_representation <work/991003705449707141/visual_item/photo/farl_negative/22236/visual_item> .
<work/991003705449707141/visual_item/photo/farl_negative/22236> crm:P1_is_identified_by <work/991003705449707141/visual_item/photo/farl_negative/22236/id/farl-negative-number> .
<work/991003705449707141/visual_item/photo/farl_negative/22236> crm:P2_has_type pharos-meta:photograph_negative .
<work/991003705449707141/visual_item/photo/farl_negative/22236> crm:P50_has_current_keeper <https://artresearch.net/resource/provider/frick> .
<work/991003705449707141/visual_item/photo/farl_negative/22236> crm:P70i_is_documented_in <https://artresearch.net/resource/e31/frick> .
<work/991003705449707141/visual_item/photo/farl_negative/22236> custom:imageOrder "6"^^xsd:integer .
<work/991003705449707141/visual_item/photo/farl_negative/22236/id/farl-negative-number> a crm:E42_Identifier .
<work/991003705449707141/visual_item/photo/farl_negative/22236/id/farl-negative-number> crm:P190_has_symbolic_content "22236" .
<work/991003705449707141/visual_item/photo/farl_negative/22236/id/farl-negative-number> crm:P2_has_type <vocab/meta/farl-negative-number> .
<work/991003705449707141/visual_item/photo/farl_negative/22236/production> a crm:E12_Production .
<work/991003705449707141/visual_item/photo/farl_negative/22236/production> crm:P14_carried_out_by <actor/photographer/sansoni> .
<work/991003705449707141/visual_item/photo/farl_negative/22236/rights> a crm:E30_Right .
<work/991003705449707141/visual_item/photo/farl_negative/22236/rights> crm:P2_has_type <http://rightsstatements.org/vocab/UND/1.0/> .
<work/991003705449707141/visual_item/photo/farl_negative/22236/visual_item> a crm:E36_Visual_Item .
<work/991003705449707141/visual_item/photo/farl_negative/22236/visual_item> crm:P165i_is_incorporated_in <work/991003705449707141/visual_item/photo/farl_negative/22236/visual_item/image> .
<work/991003705449707141/visual_item/photo/farl_negative/22236/visual_item/image> a crm:D1_Digital_Object .
<work/991003705449707141/visual_item/photo/farl_negative/22236/visual_item/image> crm:P1_is_identified_by <https://iiif.artresearch.net/iiif/3/frick/22236_POST.tif/full/full/0/default.jpg> .
<work/991003705449707141/visual_item/photo/farl_negative/22236/visual_item/image> crm:P2_has_type pharos-meta:digital_image .
<work/991003705449707141/visual_item/photo/print/7> a crm:E22_Human-Made_Object .
<work/991003705449707141/visual_item/photo/print/7> crm:P104_is_subject_to <work/991003705449707141/visual_item/photo/print/7/rights> .
<work/991003705449707141/visual_item/photo/print/7> crm:P108i_was_produced_by <work/991003705449707141/visual_item/photo/print/7/production> .
<work/991003705449707141/visual_item/photo/print/7> crm:P2_has_type pharos-meta:photographic_print .
<work/991003705449707141/visual_item/photo/print/7> crm:P50_has_current_keeper <https://artresearch.net/resource/provider/frick> .
<work/991003705449707141/visual_item/photo/print/7> crm:P70i_is_documented_in <https://artresearch.net/resource/e31/frick> .
<work/991003705449707141/visual_item/photo/print/7> custom:imageOrder "7"^^xsd:integer .
<work/991003705449707141/visual_item/photo/print/7/production> a crm:E12_Production .
<work/991003705449707141/visual_item/photo/print/7/production> crm:P14_carried_out_by <actor/photographer/alinari_florence> .
<work/991003705449707141/visual_item/photo/print/7/rights> a crm:E30_Right .
<work/991003705449707141/visual_item/photo/print/7/rights> crm:P2_has_type <http://rightsstatements.org/vocab/UND/1.0/> .
<work/991003705449707141/visual_item/photo/print/8> a crm:E22_Human-Made_Object .
<work/991003705449707141/visual_item/photo/print/8> crm:P104_is_subject_to <work/991003705449707141/visual_item/photo/print/8/rights> .
<work/991003705449707141/visual_item/photo/print/8> crm:P108i_was_produced_by <work/991003705449707141/visual_item/photo/print/8/production> .
<work/991003705449707141/visual_item/photo/print/8> crm:P2_has_type pharos-meta:photographic_print .
<work/991003705449707141/visual_item/photo/print/8> crm:P50_has_current_keeper <https://artresearch.net/resource/provider/frick> .
<work/991003705449707141/visual_item/photo/print/8> crm:P70i_is_documented_in <https://artresearch.net/resource/e31/frick> .
<work/991003705449707141/visual_item/photo/print/8> custom:imageOrder "8"^^xsd:integer .
<work/991003705449707141/visual_item/photo/print/8/production> a crm:E12_Production .
<work/991003705449707141/visual_item/photo/print/8/production> crm:P14_carried_out_by <actor/photographer/giacomo_brogi> .
<work/991003705449707141/visual_item/photo/print/8/rights> a crm:E30_Right .
<work/991003705449707141/visual_item/photo/print/8/rights> crm:P2_has_type <http://rightsstatements.org/vocab/UND/1.0/> .
<work/991003705449707141/visual_item/photo/print/9> a crm:E22_Human-Made_Object .
<work/991003705449707141/visual_item/photo/print/9> crm:P104_is_subject_to <work/991003705449707141/visual_item/photo/print/9/rights> .
<work/991003705449707141/visual_item/photo/print/9> crm:P108i_was_produced_by <work/991003705449707141/visual_item/photo/print/9/production> .
<work/991003705449707141/visual_item/photo/print/9> crm:P2_has_type pharos-meta:photographic_print .
<work/991003705449707141/visual_item/photo/print/9> crm:P50_has_current_keeper <https://artresearch.net/resource/provider/frick> .
<work/991003705449707141/visual_item/photo/print/9> crm:P70i_is_documented_in <https://artresearch.net/resource/e31/frick> .
<work/991003705449707141/visual_item/photo/print/9> custom:imageOrder "9"^^xsd:integer .
<work/991003705449707141/visual_item/photo/print/9/production> a crm:E12_Production .
<work/991003705449707141/visual_item/photo/print/9/production> crm:P14_carried_out_by <actor/photographer/giacomo_brogi> .
<work/991003705449707141/visual_item/photo/print/9/rights> a crm:E30_Right .
<work/991003705449707141/visual_item/photo/print/9/rights> crm:P2_has_type <http://rightsstatements.org/vocab/UND/1.0/> .
    |]



  it "photgrapher reconciliation" $ do
    let frickXml = [w|
<record>
  <leader>03391nkc a2200709 a 4500</leader>
  <controlfield tag="001">991012945339707141</controlfield>
  <datafield tag="590" ind1="1" ind2=" ">
    <subfield code="a">Museum of Fine Arts, Boston,</subfield>
    <subfield code="b">Boston,</subfield>
    <subfield code="c">Massachusetts,</subfield>
    <subfield code="d">United States,</subfield>
    <subfield code="f">2069,</subfield>
    <subfield code="g">public.</subfield>
  </datafield>
  <datafield tag="590" ind1="9" ind2=" ">
    <subfield code="a">Photograph,</subfield>
    <subfield code="c">B10997</subfield>
    <subfield code="j">Purchase,</subfield>
    <subfield code="k">Museum of Fine Arts, Boston</subfield>
    <subfield code="l">8/25/1948</subfield>
    <subfield code="n">m</subfield>
  </datafield>
  <datafield tag="590" ind1="9" ind2=" ">
    <subfield code="a">Photograph,</subfield>
    <subfield code="b">Gernsheim Corpus Photographicum</subfield>
    <subfield code="c">97099</subfield>
    <subfield code="i">page: 2069</subfield>
    <subfield code="j">Purchase,</subfield>
    <subfield code="k">Gernsheim Subscription,</subfield>
    <subfield code="l">10/1/1982</subfield>
    <subfield code="m">(as "Portrait of Francois de Croix")</subfield>
    <subfield code="n">1</subfield>
  </datafield>
  <datafield tag="590" ind1="0" ind2=" ">
    <subfield code="a">Digital image,</subfield>
    <subfield code="g">4107100367974_0001.jp2</subfield>
  </datafield>
  <datafield tag="590" ind1="0" ind2=" ">
    <subfield code="a">Digital image,</subfield>
    <subfield code="g">4107100367974_0002.jp2</subfield>
  </datafield>
  <datafield tag="590" ind1="0" ind2=" ">
    <subfield code="a">Digital image,</subfield>
    <subfield code="g">4107100367974_0003.jp2</subfield>
  </datafield>
  <datafield tag="590" ind1="0" ind2=" ">
    <subfield code="a">Digital image,</subfield>
    <subfield code="g">4107100367974_0004.jp2</subfield>
  </datafield>
  <datafield tag="590" ind1="0" ind2=" ">
    <subfield code="a">Digital image,</subfield>
    <subfield code="g">4107100367974_0005.jp2</subfield>
  </datafield>
  <datafield tag="590" ind1="0" ind2=" ">
    <subfield code="a">Digital image,</subfield>
    <subfield code="g">4107100367974_0006.jp2</subfield>
  </datafield>
</record>
    |]
    result <- processXMLStringAsSet mapping frickBaseUri frickXml
    result `shouldBe` [t|
<actor/photographer/gernsheim_corpus_photographicum> a crm:E39_Actor .
<actor/photographer/gernsheim_corpus_photographicum> crm:P1_is_identified_by <actor/photographer/gernsheim_corpus_photographicum/appellation/preferred_name> .
<actor/photographer/gernsheim_corpus_photographicum> crm:P2_has_type pharos-meta:photographer .
<actor/photographer/gernsheim_corpus_photographicum> custom:sameAs <https://artresearch.net/resource/pharos/actor/photographer/gernsheim_corpus_photographicum> .
<actor/photographer/gernsheim_corpus_photographicum/appellation/preferred_name> a crm:E41_Appellation .
<actor/photographer/gernsheim_corpus_photographicum/appellation/preferred_name> crm:P190_has_symbolic_content "Gernsheim Corpus Photographicum" .
<actor/photographer/gernsheim_corpus_photographicum/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<http://rightsstatements.org/vocab/UND/1.0/> a crm:E55_Type .
<https://artresearch.net/resource/e31/frick> a crm:E31_Document .
<https://artresearch.net/resource/pharos/actor/photographer/gernsheim_corpus_photographicum> a crm:E39_Actor .
<https://artresearch.net/resource/provider/frick> a crm:E39_Actor .
<https://iiif.artresearch.net/iiif/3/frick/4107100367974_0001.jp2/full/full/0/default.jpg> a crm:E42_Identifier .
<https://iiif.artresearch.net/iiif/3/frick/4107100367974_0001.jp2/full/full/0/default.jpg> crm:P2_has_type pharos-meta:photo_file_url .
<https://iiif.artresearch.net/iiif/3/frick/4107100367974_0001.jp2/full/full/0/default.jpg> image-api:storage-id "frick" .
<work/991012945339707141> a crm:E22_Human-Made_Object .
<work/991012945339707141> crm:P138i_has_representation <work/991012945339707141/visual_item> .
<work/991012945339707141/visual_item> a crm:E36_Visual_Item .
<work/991012945339707141/visual_item> crm:P65i_is_shown_by <work/991012945339707141/visual_item/photo/print/1> .
<work/991012945339707141/visual_item> crm:P65i_is_shown_by <work/991012945339707141/visual_item/photo/print/2> .
<work/991012945339707141/visual_item/photo/print/1> a crm:E22_Human-Made_Object .
<work/991012945339707141/visual_item/photo/print/1> crm:P104_is_subject_to <work/991012945339707141/visual_item/photo/print/1/rights> .
<work/991012945339707141/visual_item/photo/print/1> crm:P138i_has_representation <work/991012945339707141/visual_item/photo/print/1/visual_item> .
<work/991012945339707141/visual_item/photo/print/1> crm:P2_has_type pharos-meta:photographic_print .
<work/991012945339707141/visual_item/photo/print/1> crm:P50_has_current_keeper <https://artresearch.net/resource/provider/frick> .
<work/991012945339707141/visual_item/photo/print/1> crm:P70i_is_documented_in <https://artresearch.net/resource/e31/frick> .
<work/991012945339707141/visual_item/photo/print/1> custom:imageOrder "1"^^xsd:integer .
<work/991012945339707141/visual_item/photo/print/1/rights> a crm:E30_Right .
<work/991012945339707141/visual_item/photo/print/1/rights> crm:P2_has_type <http://rightsstatements.org/vocab/UND/1.0/> .
<work/991012945339707141/visual_item/photo/print/1/visual_item> a crm:E36_Visual_Item .
<work/991012945339707141/visual_item/photo/print/1/visual_item> crm:P165i_is_incorporated_in <work/991012945339707141/visual_item/photo/print/1/visual_item/image> .
<work/991012945339707141/visual_item/photo/print/1/visual_item/image> a crm:D1_Digital_Object .
<work/991012945339707141/visual_item/photo/print/1/visual_item/image> crm:P1_is_identified_by <https://iiif.artresearch.net/iiif/3/frick/4107100367974_0001.jp2/full/full/0/default.jpg> .
<work/991012945339707141/visual_item/photo/print/1/visual_item/image> crm:P2_has_type pharos-meta:digital_image .
<work/991012945339707141/visual_item/photo/print/2> a crm:E22_Human-Made_Object .
<work/991012945339707141/visual_item/photo/print/2> crm:P104_is_subject_to <work/991012945339707141/visual_item/photo/print/2/rights> .
<work/991012945339707141/visual_item/photo/print/2> crm:P108i_was_produced_by <work/991012945339707141/visual_item/photo/print/2/production> .
<work/991012945339707141/visual_item/photo/print/2> crm:P2_has_type pharos-meta:photographic_print .
<work/991012945339707141/visual_item/photo/print/2> crm:P50_has_current_keeper <https://artresearch.net/resource/provider/frick> .
<work/991012945339707141/visual_item/photo/print/2> crm:P70i_is_documented_in <https://artresearch.net/resource/e31/frick> .
<work/991012945339707141/visual_item/photo/print/2> custom:imageOrder "2"^^xsd:integer .
<work/991012945339707141/visual_item/photo/print/2/production> a crm:E12_Production .
<work/991012945339707141/visual_item/photo/print/2/production> crm:P14_carried_out_by <actor/photographer/gernsheim_corpus_photographicum> .
<work/991012945339707141/visual_item/photo/print/2/rights> a crm:E30_Right .
<work/991012945339707141/visual_item/photo/print/2/rights> crm:P2_has_type <http://rightsstatements.org/vocab/UND/1.0/> .
    |]
