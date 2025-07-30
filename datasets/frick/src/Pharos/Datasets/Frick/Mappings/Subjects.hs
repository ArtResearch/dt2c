module Pharos.Datasets.Frick.Mappings.Subjects  where

import Data.Text as T
import System.IO.Unsafe (unsafePerformIO)

import CommonImports
import qualified Vocabularies.PHAROS as P
import Pharos.CidocCrm.Patterns.Appellations
import Pharos.CidocCrm.Patterns.Iconclass (iconclassMapping)
import Pharos.Utils.CsvHashTable (LocationReconciliationHash, loadLocationReconciliationHash, lookupValues)


subjects :: [PathTree 'E22_]
subjects = 
  [
    -- speciall mapping for self-portraits (we should do generalized reconciliation of subjects from titles)
    [x|datafield[@tag='245']/subfield[@code='a']/marc-value()|] @> (
      maybe NullTree baseSubject . titleIconclassMapping . nodeText
    ),
    [x|datafield[@tag='650' and @ind2='7']|] @> (
      P65 ---> (E36, relativeUriT "/visual_item/subject/{i}" [("i", i)]) ==> [
        [x|subfield[@code='a']|] @> (
          P2 ---> (E55, templateUri "subject/{name}" [("name", [x|marc-value()|])]) ==> [
            P2 --> (E55, P.subject),
            appellation_0_1 P.preferred_name [x|marc-value()|],
            [x|marc-value()|] @> generateBroader
          ]
        )
      ]
    )
  ]

generateBroader :: XMLNode -> PathTree E55_
generateBroader node =
  let initialItems = Prelude.map T.strip $ T.splitOn ":" $ nodeText node
  in generateBroaderInternal initialItems
  where
    generateBroaderInternal :: [Text] -> PathTree E55_
    generateBroaderInternal itemsList
      | Prelude.length itemsList < 2 = NullTree -- Base case: if the list is too short, it has no broader term by this logic.
      | otherwise =
          let broaderTermItems = Prelude.init itemsList -- Takes all but the last element
              broaderTermName  = T.intercalate " : " broaderTermItems
          in P127 ---> (E55, templateUri "subject/{name}" [("name", broaderTermName)]) ==> [
               P2 --> (E55, P.subject), -- Assign type P.subject to the E55 for the broader term
               appellation_0_1 P.preferred_name broaderTermName, -- Assign preferred name to the E55 for the broader term
               generateBroaderInternal broaderTermItems -- Recursively find broader terms for this broaderTerm
             ]

-- iconclass
baseSubject iconclassReconciliation = 
  P65 ---> (E36, relativeUri "/visual_item/subject/from_title" ) ==> [
    P2 ---> (E55, templateUri "subject/from_title/{name}" [("name", [x|marc-value()|])]) ==> [
      P2 --> (E55, P.subject),
      appellation_0_1 P.preferred_name [x|marc-value()|],
      iconclassReconciliation
    ]
  ]

-- Iconclass Reconciliation Cache
titleIconclassMapping subject = 
  case lookupValues iconclassTable [subject] of
    [Just exact, _] -> Just $ iconclassMapping SameAs exact
    [_, Just broader] -> Just $ iconclassMapping P127 broader
    _ -> Nothing

iconclassTable :: LocationReconciliationHash
iconclassTable = unsafePerformIO $ loadLocationReconciliationHash
    "datasets/frick/resources/subjects/245_a_iconclass.csv"
    [T.pack "subject"]
    [T.pack "exact", T.pack "broader"]
{-# NOINLINE iconclassTable #-}
