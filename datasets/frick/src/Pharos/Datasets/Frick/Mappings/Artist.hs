module Pharos.Datasets.Frick.Mappings.Artist (productionLinks, artistLinks) where

import qualified Data.Text as T
import GHC.IO (unsafePerformIO)
import CommonImports
import Pharos.CidocCrm.Patterns.Appellations
import Pharos.CidocCrm.Patterns.Dates (createDateLabel)
import qualified Vocabularies.PHAROS as P
import Pharos.Utils.CsvHashTable (LocationReconciliationHash, lookupLocation, loadLocationReconciliationHash)

import Control.Monad (guard)
import Data.Char (isDigit)
import Debug.Trace (trace)
import Text.Read (readMaybe)


productionLinks = [
    productionDate,
    productionPlace
  ] ++ artistLinks


  --[x|datafield[@tag='100']|] @> when (or_ [
  --  not_ (exists [x| subfield[@code='j'] |]),
  --  equals [x|subfield[@code='j']/text()|] "attributed to",
  --  equals [x|subfield[@code='j']/text()|] "contributor"
  -- ]) (


-- | Artist/Creator links
artistLinks :: [PathTree E22_]
artistLinks = [
  [x|datafield[@tag='100']|] @> (
    P108i ---> (E12, relativeUri "/production") ==> [

      -- Attribution handling
      [x|subfield[@code='j']|] @> [
        when (
          equals [x|marc-value()|] "attributed to"
        ) (
          P140i ---> (E13, relativeUri "/attribution_assignment") ==> [
            P141 --> (E39, actorUri),
            P177 --> (E55, constUri "crm:P14_carried_out_by"),
            P2 --> (E55, P.attributed_to)
          ]
        ),
        when (
          not_ $ equals [x|marc-value()|] "attributed to"
        ) (
            P14 ---> (E39, relativeUri "/actor/anonymous") ==> [
              P107i ---> 
                (E74, templateUri "group/anonymous/{relationship}_{artist}" [
                  ("relationship", [x|marc-value()|]),
                  ("artist", [x|../subfield[@code='a']/marc-value()|])
                ]) ==> [
                  appellation_0_1 P.preferred_name [x|concat(marc-value(),../subfield[@code='a']/marc-value())|]
                ],
              (P1 ---> (E41, relativeUri "/name")) ==> [
                P2 --> (E55, P.preferred_name),
                P190 --> literal [x|marc-value()|]
              ],
              (P2 ---> (E55, relativeUri "/relationship_type")) ==> [
                P2 --> (E55, P.preferred_name)
              ],
              P2 --> (E55, relativeUri "/attribution_qualifier")
            ]     
        )
      ],

      P14 ---> (E39, actorUri) ==> [
          appellation_0_1 P.preferred_name [x|subfield[@code='a']/marc-value()|],
          
          -- reconciliation to ULAN/Wikidata/VIAF/GND
          SameAs --> (E39, uriMultiExprFn [[x|subfield[@code='a']/marc-value()|], [x|subfield[@code='d']/marc-value()|]] reconciledArtistUri)
        ]
    ]
    )
  ]

actorUri = 
  templateUri "actor/{id}"
    [("id", [x|concat(subfield[@code='a']/marc-value(), subfield[@code='c']/marc-value(), subfield[@code='d']/marc-value())|])]

-- | Production date mapping combining 008 and 260
productionDate :: PathTree E22_
productionDate =
  [x|controlfield[@tag='008']|] @> (
    P108i ---> (E12, relativeUri "/production") ==> [
      P4 ---> (E52, relativeUri "/timespan") ==> [
        [x|text()|] @> earliestLatestDate      ]
    ]
  )

earliestLatestDate :: XMLNode -> [PathTree E52_]
earliestLatestDate node =
  let code = nodeText node
      -- Production date appellation from 260_c
      dateAppellation = evalXPathAsText node [x|../datafield[@tag='260']/subfield[@code='c']/marc-value()|]
      maxAllowedYear = 2025 :: Int

      extractBetween :: T.Text -> Maybe T.Text
      extractBetween text = do
          -- 1. Split the text at the start marker.
          -- `afterStart` will be empty if the marker is not found.
          let (_, afterStart) = T.breakOn (T.pack "k") text

          -- 2. Guard against the start marker not being found.
          guard (not (T.null afterStart))

          -- 3. Get the text after the start marker by dropping the marker itself.
          let textToSearch = T.drop (T.length (T.pack "k")) afterStart

          -- 4. In the remaining text, find the end marker.
          -- `result` is the text we want. `afterEnd` is used to check for success.
          let (result, afterEnd) = T.breakOn (T.pack "xx") textToSearch

          -- 5. Guard against the end marker not being found.
          guard (not (T.null afterEnd))

          -- 6. If both guards passed, return the result.
          return result

      -- Attempt to parse and validate the date string.
      -- Returns Maybe (earliestYearString, latestYearString)
      parseAndValidateDateString :: T.Text -> Maybe (T.Text, T.Text)
      parseAndValidateDateString fullCode = do
        -- Extract the relevant part of the code string
        dateChars <- extractBetween fullCode
        
        -- Ensure the extracted part is 8 digits
        guard (T.length dateChars == 8 && T.all isDigit dateChars)

        let earliestYearStr = T.take 4 dateChars
        let latestYearStr   = T.drop 4 dateChars

        -- Parse years to integers
        earliestY <- readMaybe (T.unpack earliestYearStr) :: Maybe Int
        latestY   <- readMaybe (T.unpack latestYearStr)   :: Maybe Int

        -- Validate chronological order and year bounds
        guard (earliestY <= latestY && earliestY <= maxAllowedYear && latestY <= maxAllowedYear)

        return (earliestYearStr, latestYearStr)

  in case parseAndValidateDateString code of
       Just (earliestStr, latestStr) ->
         [ 
           P82a --> dateTime earliestStr (Year, Lower),
           P82b --> dateTime latestStr (Year, Upper),

           -- if there is no explicit date appellation, we create one based on the earliest/latest years
           appellation_0_1 P.preferred_name (case dateAppellation of
                                               Just text -> text
                                               Nothing -> createDateLabel earliestStr latestStr)
         ]
       Nothing ->
         trace ("Invalid earliest/latest production date: " ++ T.unpack code) [NullTree]

-- | Production place from 260
productionPlace :: PathTree E22_
productionPlace =
  [x|datafield[@tag='260']/subfield[@code='a']|] @> (
    P108i ---> (E12, relativeUri "/production") ==> [
      P7 ---> (E53, templateUri "place/{id}" [("id", [x|marc-value()|])]) ==> [
        P1 ---> (E41, templateUri "place/{id}/appellation" [("id", [x|marc-value()|])]) ==> [
          P190 --> literal [x|marc-value()|]
        ]
      ]
    ]
  )

-- Artist Reconciliation Cache
reconciledArtistUri = unsafePerformIO . lookupLocation artistTable
{-# NOINLINE reconciledArtistUri #-}

artistTable :: LocationReconciliationHash
artistTable = unsafePerformIO $ loadLocationReconciliationHash
    "datasets/frick/resources/actors/artists.csv"
    [T.pack "name", T.pack "date"]
    [T.pack "ulan", T.pack "wikidata", T.pack "viaf", T.pack "loc"]
{-# NOINLINE artistTable #-}
