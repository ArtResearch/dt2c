{-# LANGUAGE FlexibleContexts #-}

module Mappings.Mappings.Work.Dates where

import qualified Data.Text as T
import qualified Vocabularies.PHAROS as P
import Pharos.CidocCrm.Patterns (appellation_0_1)
import Data.Char (isDigit)
import CommonImports hiding (DateType, Year, FullDate, DateBound)
import Pharos.Utils.Date (DateType(..), DateBound(..), xsdDateTimeFromDateType)
import Data.Maybe (fromMaybe)

import Data.Time.Calendar (fromGregorianValid)
import Text.Read (readMaybe)
import Data.Maybe (isJust)
import qualified Data.Text.Encoding as TE

-- Helper to safely read an Int from Text
safeReadInt :: T.Text -> Maybe Int
safeReadInt = readMaybe . T.unpack

-- Represents qualifiers for dates like "ca.", "ante", "post".
data ZeriDateQualifier
    = ZDQ_Ca
    | ZDQ_Post
    | ZDQ_Ante
    | ZDQ_Uncertain
    | ZDQ_None
    deriving (Show, Eq)

-- Parses qualifier text into ZeriDateQualifier
parseQualifier :: T.Text -> ZeriDateQualifier
parseQualifier qText
    | qText == T.pack "ca." = ZDQ_Ca
    | qText == T.pack "ca.,ca." = ZDQ_Ca -- "ca.,ca." treated as "ca."
    | qText == T.pack "post" = ZDQ_Post
    | qText == T.pack "ante" = ZDQ_Ante
    | qText == T.pack "(?)" = ZDQ_Uncertain
    | otherwise = ZDQ_None

-- Represents a parsed date component: (Original Text, DateType, Qualifier)
type ZeriParsedDate = (T.Text, DateType, ZeriDateQualifier)

-- Data type to represent the structure of input dates, using ZeriParsedDate
data DateInputType
    = SingleDate ZeriParsedDate ZeriParsedDate
    | DateList [(ZeriParsedDate, ZeriParsedDate)]
    deriving (Show)

-- ProcessedDateInfo is removed as it's redundant.
-- The original full DTSI/DTSF strings are not used after parsing into components.
-- DateInputType will be returned directly by parseInputDates.

-- Error function for invalid dates
invalidDateError :: T.Text -> String -> a
invalidDateError dateStr specificMsg = error $ "Invalid date format for '" ++ T.unpack dateStr ++ "': " ++ specificMsg

-- Helper function to validate a year string
isValidYear :: T.Text -> Bool
isValidYear y = T.length y > 0 && T.length y <= 4 && T.all isDigit y

-- Parses a single date component (YYYY, YYYY/MM, YYYY/MM/DD) into ZeriParsedDate
-- Year can be 1-4 digits. All years in Common Era.
-- Now takes a ZeriDateQualifier to store with the parsed date.
parseSingleDateComponent :: T.Text -> ZeriDateQualifier -> ZeriParsedDate
parseSingleDateComponent dateStr qualifier =
    let textAndMaybeIntParts = map (\p -> (p, safeReadInt p)) (T.splitOn (T.pack "/") dateStr)
        mkZeriParsedDate dt = (dateStr, dt, qualifier)
    in case textAndMaybeIntParts of
        -- YYYY/MM/DD
        [(yStr, Just y), (_mStr, Just m), (_dStr, Just d)] | isValidYear yStr ->
            if isJust (fromGregorianValid (fromIntegral y) m d)
            then mkZeriParsedDate (DTYearMonthDay y m d)
            else invalidDateError dateStr "Invalid day for given month/year."

        -- YYYY/MM
        [(yStr, Just y), (_mStr, Just m)] | isValidYear yStr ->
            if m >= 1 && m <= 12
            then mkZeriParsedDate (DTYearMonth y m)
            else invalidDateError dateStr "Month out of range (1-12)."

        -- YYYY
        [(yStr, Just y)] | isValidYear yStr -> mkZeriParsedDate (DTYear y)

        _ -> invalidDateError dateStr "Invalid date component. Ensure format is YYYY, YYYY/MM, or YYYY/MM/DD with valid numeric parts."

-- Parses DTSI, DTSL (DTSI qualifier), DTSF, DTSV (DTSF qualifier) texts into DateInputType
parseInputDates :: T.Text -> T.Text -> T.Text -> T.Text -> DateInputType
parseInputDates dtsiText dtslText dtsfText dtsvText =
    let qualFrom = parseQualifier dtslText
        qualTo   = parseQualifier dtsvText
        isCommaSeparated = T.isInfixOf (T.pack ",") dtsiText || T.isInfixOf (T.pack ",") dtsfText
    in if isCommaSeparated then
           let fromParts = T.splitOn (T.pack ",") dtsiText
               toParts = T.splitOn (T.pack ",") dtsfText
           in if length fromParts /= length toParts then
                  error $ "DTSI and DTSF have different number of comma-separated parts. DTSI: " ++ T.unpack dtsiText ++ ", DTSF: " ++ T.unpack dtsfText
              else DateList $ zip (map (\txt -> parseSingleDateComponent txt qualFrom) fromParts)
                                  (map (\txt -> parseSingleDateComponent txt qualTo) toParts)
       else
           SingleDate (parseSingleDateComponent dtsiText qualFrom) (parseSingleDateComponent dtsfText qualTo)

-- Adjusts DateType year based on qualifier and bound (start/end of interval)
--
-- @Francesca:
-- 
-- We have been VERY ‘generous’ in giving chronological extremes in our original data, 
-- so you can consider narrower intervals:
-- In case of “ca.” or “(?),” you can implicitly include a margin of 5 years in the search (two years before, the same year and 2 years after)
-- In case of “ante,” "post" you can include both extremes in the calculation, but not widen the range further. 
-- Thus:
--   in the case of "1870 ca. - 1929 ante," the interval to search would be January 1 1868 - December 31, 1929.
--   in the case of "1870 post. - 1929 ca.," the interval to search would be January 1 1870 - December 31, 1931.
--   in case of "1321 ca. - 1321 ca. it would be 1319-1323
getAdjustedDate :: DateType -> ZeriDateQualifier -> DateBound -> DateType
getAdjustedDate dt qual bound =
  let adjustYearFn = case qual of
        ZDQ_Ca -> case bound of
          DateLowerBound -> \y -> y - 2
          DateUpperBound -> \y -> y + 2
        ZDQ_Uncertain -> case bound of
          DateLowerBound -> \y -> y - 2
          DateUpperBound -> \y -> y + 2
        _ -> id

      applyAdjustment :: (Int -> Int) -> DateType -> DateType
      applyAdjustment f (DTYear y) = DTYear (f y)
      applyAdjustment f (DTYearMonth y m) = DTYearMonth (f y) m
      applyAdjustment f (DTYearMonthDay y m d) = DTYearMonthDay (f y) m d
      applyAdjustment _ dmy@(DTDayMonthYear _ _ _) = dmy -- Should not be hit by current Zeri parsing logic

  in if qual == ZDQ_None then dt else applyAdjustment adjustYearFn dt

-- Converts qualifier to text for display in appellation
qualifierToLabelText :: ZeriDateQualifier -> T.Text
qualifierToLabelText ZDQ_Ca = T.pack " ca."
qualifierToLabelText ZDQ_Ante = T.pack " ante"
qualifierToLabelText ZDQ_Post = T.pack " post"
qualifierToLabelText ZDQ_Uncertain = T.pack " (?)"
qualifierToLabelText ZDQ_None = T.empty

-- Creates a human-readable date appellation including qualifiers
createZeriDateAppellation :: ZeriParsedDate -> ZeriParsedDate -> T.Text
createZeriDateAppellation (fromOrigStr, _, fromQual) (toOrigStr, _, toQual)
    -- If from and to dates and qualifiers are identical, show only one.
    | fromOrigStr == toOrigStr && fromQual == toQual = formatDatePart fromOrigStr fromQual
    | T.null fromOrigStr                             = T.pack "- " <> formatDatePart toOrigStr toQual
    | T.null toOrigStr                               = formatDatePart fromOrigStr fromQual <> T.pack " -"

    -- Default case: both 'from' and 'to' are non-empty and are different in some aspect (date or qualifier).
    | otherwise                                      = formatDatePart fromOrigStr fromQual <> T.pack " - " <> formatDatePart toOrigStr toQual
  where
    formatDatePart :: T.Text -> ZeriDateQualifier -> T.Text
    formatDatePart dateStr qual = dateStr <> qualifierToLabelText qual

createE52TimeSpanNode :: T.Text -> T.Text -> T.Text -> T.Text -> PathTree E12_
createE52TimeSpanNode uriSuffix beginXsdDateTime endXsdDateTime labelStr =
    P4 ---> (E52, relativeUri (TE.encodeUtf8 uriSuffix)) ==> [
        P82a --> dateTime beginXsdDateTime DateTime,
        P82b --> dateTime endXsdDateTime DateTime,
        appellation_0_1 P.preferred_name labelStr
    ]

-- Generates E52 TimeSpan nodes from DateInputType
generateTimeSpans :: DateInputType -> [PathTree E12_]
generateTimeSpans dateInput =
    case dateInput of
        SingleDate fromZpd@(_, fromDT, fromQual) toZpd@(_, toDT, toQual) ->
            let fromEffDT_Lower = getAdjustedDate fromDT fromQual DateLowerBound
                toEffDT_Upper   = getAdjustedDate toDT   toQual   DateUpperBound

                fromXsdDateTime = xsdDateTimeFromDateType fromEffDT_Lower False DateLowerBound -- Pass False for isBCE
                toXsdDateTime   = xsdDateTimeFromDateType toEffDT_Upper   False DateUpperBound   -- Pass False for isBCE
                
                appellationLabel = createZeriDateAppellation fromZpd toZpd
            in [createE52TimeSpanNode (T.pack "/date") fromXsdDateTime toXsdDateTime appellationLabel]

        DateList datePairs ->
            let processPair :: Int -> (ZeriParsedDate, ZeriParsedDate) -> PathTree E12_
                processPair idx (fromZpd@(_, fromDT, fromQual), toZpd@(_, toDT, toQual)) =
                    let fromEffDT_Lower = getAdjustedDate fromDT fromQual DateLowerBound
                        toEffDT_Upper   = getAdjustedDate toDT   toQual   DateUpperBound

                        fromXsdDateTime = xsdDateTimeFromDateType fromEffDT_Lower False DateLowerBound -- Pass False for isBCE
                        toXsdDateTime   = xsdDateTimeFromDateType toEffDT_Upper   False DateUpperBound   -- Pass False for isBCE
                        
                        appellationLabelPart = createZeriDateAppellation fromZpd toZpd
                        uriSuffix = T.pack ("/date/" <> show idx)
                    in createE52TimeSpanNode uriSuffix fromXsdDateTime toXsdDateTime appellationLabelPart
            in zipWith processPair [1..] datePairs

-- Updated mapDate function
mapProductionDate :: XMLNode -> [PathTree E12_]
mapProductionDate node =
    let
        maybeFromText = evalXPathAsText node [x|DTSI|]
        maybeFromQualText = evalXPathAsText node [x|DTSL|] -- Read DTSL for DTSI qualifier
        maybeToText   = evalXPathAsText node [x|DTSF|]
        maybeToQualText   = evalXPathAsText node [x|DTSV|] -- Read DTSV for DTSF qualifier
    in
        case (maybeFromText, maybeToText) of
            (Just fromText, Just toText) ->
                if T.null fromText && T.null toText then
                    [] -- Both DTSI/DTSF tags exist but are empty
                else
                    -- Use empty string if qualifier tags (DTSL, DTSV) are missing; parseQualifier handles "" as ZDQ_None.
                    let dtslText = fromMaybe T.empty maybeFromQualText
                        dtsvText = fromMaybe T.empty maybeToQualText
                        parsedDateValue = parseInputDates fromText dtslText toText dtsvText -- Pass qualifiers
                    in generateTimeSpans parsedDateValue
            _ -> [] -- One or both DTSI/DTSF tags are missing

-- TODO
mapAlternativeDates :: PathTree E12_
mapAlternativeDates =
    [x|ADT|] @> split "," @> \node -> do
        let dateText = nodeText node
        if T.null dateText then NullTree
        else NullTree -- todo here
