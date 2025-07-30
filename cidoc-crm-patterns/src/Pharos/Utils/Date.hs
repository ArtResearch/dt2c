{-# LANGUAGE FlexibleContexts #-}

module Pharos.Utils.Date (
    DateType(..),
    DateBound(..),
    formatYearForXsd,
    xsdDateTimeFromDateType
) where

import qualified Data.Text as T
import Data.Time.Calendar (gregorianMonthLength)
import Text.Printf (printf)

-- | Represents the structured information of a parsed date.
-- This type is copied from Midas.Mappings.Utils.DateParser.
-- The year component (e.g., in DTYear y) is the absolute magnitude.
-- BCE status is handled by a separate Bool flag passed to conversion functions.
data DateType =
    DTYear Int                 -- Stores the parsed year magnitude
  | DTYearMonth Int Int        -- Stores year magnitude, month
  | DTDayMonthYear Int Int Int -- Stores day, month, year magnitude
  | DTYearMonthDay Int Int Int -- Stores year magnitude, month, day
  deriving (Show, Eq)

-- | Specifies whether the date represents the lower or upper bound of a period.
-- Renamed from 'Bound' in Midas for clarity.
data DateBound = DateUpperBound | DateLowerBound deriving (Show, Eq)

-- | Helper to format a year integer for XSD, handling BCE and 4-digit padding.
--   Ensures that BCE years are formatted like "-YYYY" (e.g., -0300),
--   and CE years like "YYYY" (e.g., 0500 or 0000).
--   The 'yearMagnitude' parameter is expected to be non-negative.
formatYearForXsd :: Bool -> Int -> T.Text
formatYearForXsd isBce yearMagnitude = T.pack $
    let formattedMagnitude = printf "%04d" (abs yearMagnitude) -- Use abs for safety
    in if isBce && yearMagnitude /= 0 -- Year 0 is not considered BCE
       then "-" <> formattedMagnitude
       else formattedMagnitude

-- | Creates the final xsd:dateTime string from structured DateType.
-- Generalized from Midas.Mappings.Utils.DateParser.toDateTimeValue.
-- Takes DateType, a Bool for BCE status, and a DateBound.
xsdDateTimeFromDateType :: DateType -> Bool -> DateBound -> T.Text
xsdDateTimeFromDateType (DTYear yearVal) bce bound =
    let yearStr = formatYearForXsd bce yearVal
    in case bound of
        DateLowerBound -> yearStr <> T.pack "-01-01T00:00:00Z"
        DateUpperBound -> yearStr <> T.pack "-12-31T23:59:59Z"

xsdDateTimeFromDateType (DTYearMonth yearVal monthVal) bce bound =
    let yearStr = formatYearForXsd bce yearVal
        -- YearVal is magnitude. For gregorianMonthLength, it's the calendar year.
        calendarYear = fromIntegral yearVal
    in if monthVal < 1 || monthVal > 12 then error ("Invalid month: " ++ show monthVal) else
       case bound of
        DateLowerBound -> T.pack $ printf "%s-%02d-01T00:00:00Z" (T.unpack yearStr) monthVal
        DateUpperBound -> T.pack $ printf "%s-%02d-%02dT23:59:59Z" (T.unpack yearStr) monthVal (gregorianMonthLength calendarYear monthVal)

xsdDateTimeFromDateType (DTDayMonthYear dayVal monthVal yearVal) bce bound =
    let yearStr = formatYearForXsd bce yearVal
        calendarYear = fromIntegral yearVal
    in if monthVal < 1 || monthVal > 12 then error ("Invalid month: " ++ show monthVal)
       else if dayVal < 1 || dayVal > gregorianMonthLength calendarYear monthVal then error ("Invalid day: " ++ show dayVal ++ " for month " ++ show monthVal ++ " in year " ++ show yearVal)
       else
        let formattedDate = T.pack $ printf "%s-%02d-%02d" (T.unpack yearStr) monthVal dayVal
        in formattedDate <> (if bound == DateUpperBound then T.pack "T23:59:59Z" else T.pack "T00:00:00Z")

xsdDateTimeFromDateType (DTYearMonthDay yearVal monthVal dayVal) bce bound =
    let yearStr = formatYearForXsd bce yearVal
        calendarYear = fromIntegral yearVal
    in if monthVal < 1 || monthVal > 12 then error ("Invalid month: " ++ show monthVal)
       else if dayVal < 1 || dayVal > gregorianMonthLength calendarYear monthVal then error ("Invalid day: " ++ show dayVal ++ " for month " ++ show monthVal ++ " in year " ++ show yearVal)
       else
        let formattedDate = T.pack $ printf "%s-%02d-%02d" (T.unpack yearStr) monthVal dayVal
        in formattedDate <> (if bound == DateUpperBound then T.pack "T23:59:59Z" else T.pack "T00:00:00Z")
