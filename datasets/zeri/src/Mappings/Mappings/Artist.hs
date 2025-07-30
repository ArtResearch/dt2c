{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
module Mappings.Mappings.Artist where

import GHC.IO (unsafePerformIO)
import CommonImports
import qualified Vocabularies.PHAROS as P
import qualified Data.Text as T
import qualified Mappings.Vocabulary as ZeriMeta
import Data.Maybe (fromMaybe)
import Mappings.NamedUris (production)
import Pharos.Utils.CsvHashTable (LocationReconciliationHash, lookupLocation, loadLocationReconciliationHash)

-- | Helper function for creating an attribution mapping with E13 Attribute Assignment
-- This handles uncertain attributions and can be reused for different types of authors
-- E12 → P140i → E13 → P141 → (author, depends on other qualifiers)
--                   |→ P177 → P14
--                   |→ P14 → <Institution, e.g Frick>
--                   |→ P2 → E55 "attributed to, etc."
-- Example of how to use namedUri to reference the production entity:
uncertainAttribution :: forall src. (IsSubClassOf src 'E1_ ~ 'True) => PathTree src
uncertainAttribution =
  [x|AUTS|] @> when (or_ [
    starts [x|text()|] "attr.",
    contains [x|text()|] "(?)"
  ]) (P141i ---> (E13, relativeUri "/attribution_assignment") ==> [
    P140 --> (E12, typedNamedUri production),
    P177 --> (E55, constUri "crm:P14_carried_out_by"),
    P2 --> (E55, P.attribution)
  ])

-- | Main artist mapping function using pattern matching
artistMapping :: XMLNode -> PathTree E12_
artistMapping node =
  -- Extract text values once for efficiency
  let autnText = fromMaybe "" $ evalXPathAsText node [x|AUTN/text()|]
      autsText = fromMaybe "" $ evalXPathAsText node [x|AUTS/text()|]

      -- Optional: extract other commonly used values
      hasAttr = T.isPrefixOf "attr." autsText
      hasQuestionMark = T.isInfixOf "(?)" autsText
      hasAssistants = T.isPrefixOf "e " autsText
  in
    -- Pattern matching with guards for efficient branching
    case () of

      -- Anonymous person author mapping
      -- When AUTN starts with "Anonimo". 
      -- Because there can be more than one anonymous author for the same artwork we build URI
      -- with index relative to work production URI.
      _ | T.isPrefixOf "Anonimo" autnText ->
            P14 ---> (E39, relativeUriT "/actor/anonymous/{index}" [("index", i)]) ==> [
              uncertainAttribution,
              P2 --> (E55, P.anonymous),
              P107i ---> (E74, templateUri "group/anonymous/{group_id}" [("group_id", autnText)]) ==> [
                P1 ---> (E41, relativeUri "/name") ==> [
                  P2 --> (E55, P.preferred_name),
                  P190 --> literal autnText
                ],
                P2 --> (E55, P.anonymous_group)
              ],
              (P1 ---> (E41, relativeUri "/name")) ==> [
                P2 --> (E55, P.preferred_name),
                P190 --> literal autnText
              ]
            ]

        -- Corporate body author mapping
        | T.isPrefixOf "Maestranze" autnText || T.isPrefixOf "Manifattura" autnText ->
            P14 ---> (E74, relativeUriT "/actor/anonymous/{group_id}" [("group_id", autnText)]) ==> [
              uncertainAttribution,
              P2 --> (E55, P.anonymous),
              P107i ---> (E74, templateUri "group/anonymous/{group_id}" [("group_id", autnText)]) ==> [
                P1 ---> (E41, relativeUri "/name") ==> [
                  P2 --> (E55, P.preferred_name),
                  P190 --> literal autnText
                ],
                P2 --> (E55, P.anonymous_group)
              ],
              (P1 ---> (E41, relativeUri "/name")) ==> [
                P2 --> (E55, P.preferred_name),
                P190 --> literal autnText
              ]
            ]

        -- Artist with assistants mapping
        | hasAssistants ->
            -- We can include both mappings for artist with assistants here
            -- First mapping (named artist)
            P14 ---> (E21, templateUri "person/{id}" [("id", autnText)]) ==> [
              (P1 ---> (E41, relativeUri "/name")) ==> [
                P2 --> (E55, P.preferred_name),
                P190 --> literal autnText
              ],
              SameAs --> (E39, uriMultiExprFn [[x|AUTN/text()|], [x|AUTP/text()|]] reconciledArtistUri)
            ]

        -- Uncertain anonymous related to artist mapping
        | hasQuestionMark && not hasAttr && not hasAssistants ->
            P14 ---> (E39, relativeUri "/actor/anonymous") ==> [
              P1 ---> (E41, relativeUri "/name") ==> [
                P2 --> (E55, P.preferred_name),
                P190 --> literal (autsText <> autnText)
              ],

              P107i --> (E74, templateUri "group/anonymous/{relationship}_{artist}" [
                ("relationship", autsText),
                ("artist", autnText)
              ]),
              --P15 ---> (E39, templateUri "actor/{id}" [("id", autnText)]) ==> [
              --  P1 ---> (E41, relativeUri "/name") ==> [
              --    P2 --> (E55, P.preferred_name),
              --    P190 --> literal autnText
              --  ]
              --],
              P1 ---> (E41, relativeUri "/name") ==> [
                P2 --> (E55, P.preferred_name),
                P190 --> literal (autsText <> autnText)
              ],
              (P2 ---> (E55, relativeUri "/relationship_type")) ==> [
                P2 --> (E55, ZeriMeta.auts)
              ],
              P2 --> (E55, relativeUri "/attribution_qualifier")
            ]

        -- Anonymous related to artist mapping (standard relationship)
        | not (T.null autsText) && not hasQuestionMark && not hasAttr && not hasAssistants ->
            P14 ---> (E39, relativeUri "/person/anonymous") ==> [
              P107i --> (E74, templateUri "group/anonymous/{relationship}_{artist}" [
                ("relationship", autsText),
                ("artist", autnText)
              ]),
              (P1 ---> (E41, relativeUri "/name")) ==> [
                P2 --> (E55, P.preferred_name),
                P190 --> literal autnText
              ],
              (P2 ---> (E55, relativeUri "/relationship_type")) ==> [
                P2 --> (E55, ZeriMeta.auts)
              ]
            ]

        -- Regular author mapping (default case)
        | otherwise ->
            P14 ---> (E39, templateUri "actor/{id}" [("id", autnText)]) ==> [
              [x|AUTC|] @> uncertainAttribution,
              (P1 ---> (E41, relativeUri "/name")) ==> [
                P2 --> (E55, P.preferred_name),
                P190 --> literal autnText
              ],
              SameAs --> (E39, uriMultiExprFn [[x|AUTN/text()|], [x|AUTP/text()|]] reconciledArtistUri)
            ]

-- | Zeri artist mapping
-- Artist information is stored in PARAGRAFO[@etichetta="AUTHOR"]/RIPETIZIONE
-- We need to handle both regular artists and anonymous artists
-- We also need to handle AUTS field for connection with artist
artistTrees :: [PathTree E12_]
artistTrees =
  [[x|PARAGRAFO[@etichetta="AUTHOR"]/RIPETIZIONE|] @> artistMapping]


-- Artist Reconciliation Cache
reconciledArtistUri = unsafePerformIO . lookupLocation artistTable
{-# NOINLINE reconciledArtistUri #-}

artistTable :: LocationReconciliationHash
artistTable = unsafePerformIO $ loadLocationReconciliationHash
    "datasets/zeri/resources/actors/artists.csv"
    [T.pack "name", T.pack "pseudonym"]
    [T.pack "ulan", T.pack "wikidata", T.pack "viaf"]
{-# NOINLINE artistTable #-}
