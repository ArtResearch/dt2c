module Pmc.Mappings.Subjects (subjectLinks) where

import qualified Data.Text as T
import System.IO.Unsafe (unsafePerformIO)

import CommonImports
import qualified Vocabularies.PHAROS as P
import Pharos.CidocCrm.Patterns.Appellations (appellation_0_1)
import Pharos.CidocCrm.Patterns.Iconclass (iconclassMapping)
import Pharos.Utils.CsvHashTable (LocationReconciliationHash, loadLocationReconciliationHash, lookupValues)

subjectLinks :: [PathTree E22_]
subjectLinks = [
  [x|lido:descriptiveMetadata/lido:objectRelationWrap/lido:subjectWrap/lido:subjectSet/lido:subject/lido:subjectConcept|] @> (
    P65 ---> (E36, relativeUriT "/visual_item/subject/{i}" [("i", i)]) ==> [
      [x|lido:term|] @> (
        P2 ---> (E55, templateUri "subject/{name}" [("name", [x|text()|])]) ==> [
          P2 --> (E55, P.subject),
          appellation_0_1 P.preferred_name [x|text()|],
          [x|text()|] @> (\n -> maybe NullTree id (subjectIconclassMapping $ nodeText n))

        ]
      )
    ]
  ),
  
  [x|lido:descriptiveMetadata/lido:objectIdentificationWrap/lido:titleWrap/lido:titleSet/lido:appellationValue/text()|] @> (
    maybe NullTree baseSubject . titleIconclassMapping . nodeText
  )
  ]

baseSubject iconclassReconciliation = 
  P65 ---> (E36, relativeUri "/visual_item/subject/from_title" ) ==> [
    P2 ---> (E55, templateUri "subject/from_title/{name}" [("name", [x|text()|])]) ==> [
      P2 --> (E55, P.subject),
      appellation_0_1 P.preferred_name [x|text()|],
      iconclassReconciliation
    ]
  ]

-- Iconclass
subjectIconclassMapping subject = 
  case lookupValues subjectIconclassTable [subject] of
    [Just exact, _] -> Just $ iconclassMapping SameAs exact
    [_, Just broader] -> Just $ iconclassMapping P127 broader
    _ -> Nothing

subjectIconclassTable :: LocationReconciliationHash
subjectIconclassTable = unsafePerformIO $ loadLocationReconciliationHash
    "datasets/pmc/resources/subjects/subject-concept.csv"
    [T.pack "subject"]
    [T.pack "iconclass_exact", T.pack "iconclass_broader"]
{-# NOINLINE subjectIconclassTable #-}


titleIconclassMapping subject = 
  case lookupValues titleIconclassTable [subject] of
    [Just exact, _] -> Just $ iconclassMapping SameAs exact
    [_, Just broader] -> Just $ iconclassMapping P127 broader
    _ -> Nothing

titleIconclassTable :: LocationReconciliationHash
titleIconclassTable = unsafePerformIO $ loadLocationReconciliationHash
    "datasets/pmc/resources/subjects/title.csv"
    [T.pack "subject"]
    [T.pack "exact", T.pack "broader"]
{-# NOINLINE titleIconclassTable #-}