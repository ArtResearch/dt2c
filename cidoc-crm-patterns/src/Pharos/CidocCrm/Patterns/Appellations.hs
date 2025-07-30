module Pharos.CidocCrm.Patterns.Appellations where

import CommonImports

appellation_0_N appellationType xPath =
  P1 ---> (E41, relativeUriT ("/appellation/{i}/" <> localName appellationType) [("i", i)]) ==> [
    P2 --> (E55, appellationType),
    P190 --> literal xPath
  ]

appellation_0_1 appellationType xPath =
  P1 ---> (E41, relativeUri ("/appellation/" <> localName appellationType)) ==> [
    P2 --> (E55, appellationType),
    P190 --> literal xPath
  ]

appellation_0_1_fn appellationType xPath fn =
  P1 ---> (E41, relativeUri ("/appellation/" <> localName appellationType)) ==> [
    P2 --> (E55, appellationType),
    P190 --> literalFn xPath fn 
  ]
