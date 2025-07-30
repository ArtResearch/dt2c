module Pharos.Datasets.Midas.GeoSpec (spec) where

import Test.Hspec
import CommonImports
import Midas.Mappings.Work (midasBaseUri)
import MidasGeo.Mappings.Geo (geoMapping)

spec :: Spec
spec = describe "Midas Geo Mappings" $ do
  it "part of the city" $ do
    let mapping = geoMapping
    let xml = [w|
        <geo>
          <a2000>00260852</a2000>
          <a9920>2004.01.14</a9920>
          <a2050>Arcetri</a2050>
          <a2055>Ortsteil</a2055>
          <a2110>Firenze</a2110>
          <a2190>Toscana &amp; Toskana</a2190>
          <a2194>Italien</a2194>
        </geo>
    |]
    
    result <- processXMLStringAsSet mapping midasBaseUri xml
    
    result `shouldBe` [t|
<http://vocab.getty.edu/tgn/5002851> a crm:E53_Place .
<http://vocab.getty.edu/tgn/7009760> a crm:E53_Place .
<place/00003281> a crm:E53_Place .
<place/00003281> crm:P1_is_identified_by <place/00003281/appellation/preferred_name> .
<place/00003281> crm:P2_has_type pharos-meta:geographical_entity .
<place/00003281> custom:sameAs <http://vocab.getty.edu/tgn/7009760> .
<place/00003281/appellation/preferred_name> a crm:E41_Appellation .
<place/00003281/appellation/preferred_name> crm:P190_has_symbolic_content "Toscana & Toskana" .
<place/00003281/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<place/00260852> a crm:E53_Place .
<place/00260852> crm:P1_is_identified_by <place/00260852/appellation/preferred_name> .
<place/00260852> crm:P1_is_identified_by <place/00260852/id/geo_id> .
<place/00260852> crm:P89_falls_within <place/00003281> .
<place/00260852> custom:sameAs <http://vocab.getty.edu/tgn/5002851> .
<place/00260852/appellation/preferred_name> a crm:E41_Appellation .
<place/00260852/appellation/preferred_name> crm:P190_has_symbolic_content "Arcetri" .
<place/00260852/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<place/00260852/id/geo_id> a crm:E42_Identifier .
<place/00260852/id/geo_id> crm:P190_has_symbolic_content "00260852" .
<place/00260852/id/geo_id> crm:P2_has_type <vocab/meta/geo_id> .
    |]


  it "city in a region without geo id" $ do
    let mapping = geoMapping
    let xml = [w|
        <geo>
          <a2000>20320751</a2000>
          <a2050>Vitry-sur-Orne</a2050>
          <a2190>Lorraine</a2190>
          <a2194>Frankreich</a2194>
        </geo>
    |]
    
    result <- processXMLStringAsSet mapping midasBaseUri xml
    
    result `shouldBe` [t|
<http://vocab.getty.edu/tgn/7002888> a crm:E53_Place .
<http://vocab.getty.edu/tgn/7596326> a crm:E53_Place .
<place/20320751> a crm:E53_Place .
<place/20320751> crm:P1_is_identified_by <place/20320751/appellation/preferred_name> .
<place/20320751> crm:P1_is_identified_by <place/20320751/id/geo_id> .
<place/20320751> crm:P89_falls_within <place/lorraine> .
<place/20320751> custom:sameAs <http://vocab.getty.edu/tgn/7596326> .
<place/20320751/appellation/preferred_name> a crm:E41_Appellation .
<place/20320751/appellation/preferred_name> crm:P190_has_symbolic_content "Vitry-sur-Orne" .
<place/20320751/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
<place/20320751/id/geo_id> a crm:E42_Identifier .
<place/20320751/id/geo_id> crm:P190_has_symbolic_content "20320751" .
<place/20320751/id/geo_id> crm:P2_has_type <vocab/meta/geo_id> .
<place/lorraine> a crm:E53_Place .
<place/lorraine> crm:P1_is_identified_by <place/lorraine/appellation/preferred_name> .
<place/lorraine> crm:P2_has_type pharos-meta:geographical_entity .
<place/lorraine> custom:sameAs <http://vocab.getty.edu/tgn/7002888> .
<place/lorraine/appellation/preferred_name> a crm:E41_Appellation .
<place/lorraine/appellation/preferred_name> crm:P190_has_symbolic_content "Lorraine" .
<place/lorraine/appellation/preferred_name> crm:P2_has_type pharos-meta:preferred_name .
    |]
