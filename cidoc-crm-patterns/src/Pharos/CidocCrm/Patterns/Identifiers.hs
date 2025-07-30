module Pharos.CidocCrm.Patterns.Identifiers where

import CommonImports

identifier_0_1 idType = identifier_0_1_fn idType id

identifier_0_1_marc idType = identifier_0_1_fn_generic idType [x|marc-value()|] id

identifier_0_1_fn idType fn = identifier_0_1_fn_generic idType [x|text()|] fn

identifier_0_1_fn_generic idType xPath fn =
  P1 ---> (E42, relativeUri ("/id/" <> localName idType)) ==> [ 
    P2 --> (E55, idType),
    P190 --> literalFn xPath fn
  ]