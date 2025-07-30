module Midas.Mappings.Subjects (subjectLinks) where

import CommonImports
import Pharos.CidocCrm.Patterns.Iconclass (iconclassMapping)

subjectLinks :: [PathTree E22_]
subjectLinks = [
  -- in Hertziana and some Marburg data a5500 is a plain XML element without subfields
  -- so we need to extract value from element and not from normalized attribute
  when (not_ $ exists [x|a5500/@modifier|]) (
    [x|a5500|] @> iconography "primary"
  ),
  when (exists [x|a5500/@modifier|]) (
    [x|a5500/@modifier|] @> iconography "primary"
  ),

  -- in Marburg secondary iconography can have multiple values separated by '&'
  -- but only when there is no ICO reference
  when (not_ $ exists [x|a5510/@modifier|]) (
    [x|tokenize(a5510/text(), '&')|] @> iconography "secondary"
  ),
  when (exists [x|a5510/@modifier|]) (
    [x|a5510/@modifier|] @> iconography "secondary"
  )
 ]

iconography typ =
  P65 ---> (E36, relativeUriT ("/visual_item/subject/" <> typ <> "/{i}") [("i", i)]) ==> [
    midasIconclassMapping
  ]

midasIconclassMapping :: XMLNode -> PathTree E36_
midasIconclassMapping = iconclassMapping P2 . nodeText
