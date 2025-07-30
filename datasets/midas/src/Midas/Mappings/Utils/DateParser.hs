{-# LANGUAGE LambdaCase #-}
-- | This module provides a robust parser for a single, specific date/year
--   string format. It is designed to be called after a higher-level split
--   has already separated multiple units. The parser converts a single
--   unit string into a `DateRangeResult` record containing the date range
--   and an uncertainty flag.
module Midas.Mappings.Utils.DateParser
    ( parseSingleUnitToRange
    , DateRangeResult(..)
    ) where

import qualified Data.Text as T
import           Data.Text (Text)
import           Data.Void (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char (char, space, space1, digitChar, string') -- Modified
import           Data.Time.Calendar (gregorianMonthLength) -- Will be used by the new shared utility
import           Control.Monad (guard, void, unless) -- Modified
import           Data.Maybe (isJust) -- Added
import           Text.Read (readMaybe) -- Added
import           Pharos.Utils.Date (DateType(..), DateBound(..), xsdDateTimeFromDateType) -- New import

-- SECTION: Data Types and Provided Helpers

-- | The final record returned by the parser.
data DateRangeResult = DateRangeResult
    { rangeStart  :: Text
    , rangeEnd    :: Text
    , uncertain   :: Bool
    } deriving (Show, Eq)

-- DateType is now imported from Pharos.Utils.Date
-- Bound is now DateBound, imported from Pharos.Utils.Date

-- | Qualifiers that add margins to a date.
data Modifier = Um | Vor | Nach | Bis | Ab | Seit | Gegen | Ca | UmNach | VorUm | None deriving (Show, Eq) -- Modified

-- | Represents a fully parsed "sub-piece" of a date string, including piece-level uncertainty.
data ParsedPiece = ParsedPiece
    { modifier         :: Modifier
    , isBCE            :: Bool
    , dateFields       :: DateType
    , isPieceUncertain :: Bool  -- New: uncertainty for this specific piece
    } deriving (Show, Eq)

-- | A parsed unit can be a single piece, a slash-separated range of pieces,
--   or a dash-separated range of units.
--   This structure implies precedence: slash binds tighter than dash.
--   e.g. A/B-C/D is parsed as (A/B) - (C/D).
data ParsedUnit =
    Single ParsedPiece
  | SlashRange ParsedPiece ParsedPiece  -- Slash operates on Pieces
  | DashRange ParsedUnit ParsedUnit      -- Dash operates on ParsedUnits
  deriving (Show, Eq)

-- | The intermediate representation of a unit, including overall uncertainty.
data FinalUnit = FinalUnit
    { finalUncertainty :: Bool -- True if overall '?' or any piece has '?'
    , unitData         :: ParsedUnit
    } deriving (Show, Eq)

-- SECTION: Main Public Function

-- |
-- Parses a single date unit string and returns a `DateRangeResult` record
-- containing the date range and uncertainty flag, or an error message.
--
-- Example: parseSingleUnitToRange (T.pack "ca. 1950?")
-- Returns: Right DateRangeResult {
--            rangeStart = "1940-01-01T00:00:00Z",
--            rangeEnd   = "1960-12-31T23:59:59Z",
--            uncertain  = True
--          }
-- Example: parseSingleUnitToRange (T.pack "invalid date")
-- Returns: Left "Error message..."

-- Helper to extract year from xsd:dateTime Text
extractYearFromXsd :: Text -> Either String Int
extractYearFromXsd t
    | T.null t = Left "Cannot extract year from empty date string"
    | otherwise =
        let isBce = T.head t == '-'
            -- Correctly handle potential "-YYYY" format by taking 4 chars after optional '-'
            yearTextSegment = if isBce then T.take 4 (T.drop 1 t) else T.take 4 t
        in case readMaybe (T.unpack yearTextSegment) of
            Nothing -> Left $ "Failed to parse year from segment: \"" <> T.unpack yearTextSegment <> "\" in date string: \"" <> T.unpack t <> "\""
            Just yearMag -> Right $ if isBce then negate yearMag else yearMag

parseSingleUnitToRange :: Text -> Either String DateRangeResult
parseSingleUnitToRange input = do
    -- Step 1: Parse the input string into abstract date structures
    (FinalUnit uncertaintyResult parsedUnit) <-
        case parse (pFinalUnit <* eof) "" input of
            Left bundle -> Left (errorBundlePretty bundle)
            Right fu -> Right fu

    -- Step 2: Convert parsed structures to start and end xsd:dateTime strings
    let (startText, endText) = parsedUnitToRangeTuple parsedUnit

    -- Step 3: Extract integer years for validation
    earliestYear <- extractYearFromXsd startText
    latestYear   <- extractYearFromXsd endText

    -- Step 4: Perform validations
    let maxAllowedYear = 2025 -- Dates must be strictly before 2025
    unless (earliestYear < maxAllowedYear) $
        Left $ "Validation Error: Earliest year " <> show earliestYear <> " is not before " <> show maxAllowedYear <> "."
    unless (latestYear < maxAllowedYear) $
        Left $ "Validation Error: Latest year " <> show latestYear <> " is not before " <> show maxAllowedYear <> "."
    unless (earliestYear <= latestYear) $
        Left $ "Validation Error: Earliest year " <> show earliestYear <> " is not chronologically before or same as latest year " <> show latestYear <> "."

    -- Step 5: If all validations pass, return the result
    Right DateRangeResult { rangeStart = startText, rangeEnd = endText, uncertain = uncertaintyResult }

-- SECTION: Megaparsec Grammar Definition

type Parser = Parsec Void Text

-- Helper to determine if any piece within a ParsedUnit is marked uncertain
getPieceLevelUncertainty :: ParsedUnit -> Bool
getPieceLevelUncertainty (Single p) = isPieceUncertain p
getPieceLevelUncertainty (SlashRange p1 p2) = isPieceUncertain p1 || isPieceUncertain p2 -- p1, p2 are ParsedPiece
getPieceLevelUncertainty (DashRange u1 u2) = getPieceLevelUncertainty u1 || getPieceLevelUncertainty u2 -- u1, u2 are ParsedUnit

-- | Parses a complete unit, including optional trailing '?' and considering piece-level '?'
pFinalUnit :: Parser FinalUnit
pFinalUnit = do
    space
    unit <- pUnit  -- pUnit is now the entry point for the new grammar
    space
    isTrailingQMark <- optional (char '?')
    space
    let pieceUncertainty = getPieceLevelUncertainty unit
    return $ FinalUnit (isTrailingQMark == Just '?' || pieceUncertainty) unit

-- | Top-level parser for a date unit. Parses dash-separated expressions.
--   Dash has lower precedence than slash. e.g., "A/B-C/D" is (A/B)-(C/D).
pUnit :: Parser ParsedUnit
pUnit = pDashExpression

-- | Parses expressions separated by '-', e.g., U1 - U2 - U3 etc.
--   Each component (U1, U2, etc.) must be a slash expression.
--   A slash expression is either a single piece (e.g., "1990", "1990?")
--   or two pieces separated by a slash (e.g., "1990/1995", "1990?/1995").
--   Uses foldl1 to ensure left-associativity for dashes, e.g., U1-U2-U3 becomes (U1-U2)-U3.
pDashExpression :: Parser ParsedUnit
pDashExpression =
    foldl1 DashRange <$> sepBy1 pSlashExpression (try (space *> oneOf ['-', '‐'] <* space)) -- Modified to accept '‐' (U+2010)

-- | Parses expressions separated by '/', e.g., P1 / P2.
--   P1 and P2 are individual pieces. Slash binds tighter than dash.
--   If no '/' is present, it parses a single piece.
--   Note: This parser only supports two pieces for a slash expression (P1 / P2), not P1 / P2 / P3.
pSlashExpression :: Parser ParsedUnit
pSlashExpression = do
    p1 <- pPiece
    maybeP2 <- optional (try (space *> char '/' <* space *> pPiece))
    case maybeP2 of
        Just p2 -> return $ SlashRange p1 p2
        Nothing -> return $ Single p1

-- | A piece is the core component: (optional modifier) + (date/year) + (optional bce) + (optional '?')
pInt :: Parser Int
pInt = read <$> some digitChar

-- Parses YYYY.MM.DD
pYMD :: Parser DateType
pYMD = do
    y <- pInt <* char '.'
    m <- pInt <* char '.'
    d <- pInt
    -- Validation using gregorianMonthLength for day validity
    guard (y >= 0 && y <= 9999 && m >= 1 && m <= 12 && d >= 1 && d <= gregorianMonthLength (fromIntegral y) m)
    return $ DTYearMonthDay y m d

-- Parses DD.MM.YYYY
pDMY :: Parser DateType
pDMY = do
    d <- pInt <* char '.'
    m <- pInt <* char '.'
    y <- pInt
    -- Validation using gregorianMonthLength for day validity
    guard (y >= 0 && y <= 9999 && m >= 1 && m <= 12 && d >= 1 && d <= gregorianMonthLength (fromIntegral y) m)
    return $ DTDayMonthYear d m y

-- Parses YYYY.MM
pYM :: Parser DateType
pYM = do
    y <- pInt <* char '.'
    m <- pInt
    guard (y >= 0 && y <= 9999 && m >= 1 && m <= 12) -- Validate year and month
    notFollowedBy (char '.' >> pInt) -- Distinguish from YYYY.MM.DD
    return $ DTYearMonth y m

pPiece :: Parser ParsedPiece
pPiece = do
    space
    modifier' <- optional (try pModifier) -- Modifier (e.g., "ca. ", "vor ")
    space
    -- Core date parsing: try modern date formats first, then plain year
    parsedDateFields <- try pModernDate <|> (DTYear <$> pInt) -- Date (e.g., "1990", "1990.01", "01.01.1990")
    space
    bce <- optional pBceMarker -- BCE marker (e.g., "ante")
    space
    isUncRaw <- optional pUncertaintySuffix -- Modified for extended uncertainty
    return $ ParsedPiece (maybe None id modifier') (bce == Just True) parsedDateFields (isJust isUncRaw) -- Modified

-- Helper for '( ? )'
pParenthesizedQ :: Parser Char
pParenthesizedQ = char '(' *> space *> char '?' <* space <* char ')'

-- Helper for '[ ? ]'
pBracketedQ :: Parser Char
pBracketedQ = char '[' *> space *> char '?' <* space <* char ']'

-- Combined uncertainty suffix parser
pUncertaintySuffix :: Parser ()
pUncertaintySuffix = void $ choice
    [ char '?'             -- Plain '?'
    , try pParenthesizedQ  -- '(?)'
    , try pBracketedQ      -- '[?]'
    ]

-- | Parses a modifier keyword. It must be followed by a space.
--   Modifiers are now case-insensitive.
pModifier :: Parser Modifier
pModifier = choice
    [ try (UmNach <$ (string' (T.pack "um/nach") <* space1)) -- New, case-insensitive
    , try (VorUm  <$ (string' (T.pack "vor/um") <* space1))  -- New, case-insensitive
    , Ca    <$ string' (T.pack "ca.") -- "ca." itself, space handled by pPiece, case-insensitive
    , Um    <$ (string' (T.pack "um")    <* space1) -- Case-insensitive
    , Vor   <$ (string' (T.pack "vor")   <* space1) -- Case-insensitive
    , Nach  <$ (string' (T.pack "nach")  <* space1) -- Case-insensitive
    , Bis   <$ (string' (T.pack "bis")   <* space1) -- Case-insensitive
    , Ab    <$ (string' (T.pack "ab")    <* space1) -- Case-insensitive
    , Seit  <$ (string' (T.pack "seit")  <* space1) -- Case-insensitive
    , Gegen <$ (string' (T.pack "gegen") <* space1) -- Case-insensitive
    ]

-- | Parses the "ante" BCE marker.
pBceMarker :: Parser Bool
pBceMarker = True <$ string' (T.pack "ante") -- Changed to string' for consistency

-- | Parses modern date formats like "DD.MM.YYYY", "YYYY.MM.DD", or "YYYY.MM".
pModernDate :: Parser DateType
pModernDate = choice
    [ try pYMD  -- YYYY.MM.DD
    , try pDMY  -- DD.MM.YYYY
    , try pYM   -- YYYY.MM
    ]

-- SECTION: Conversion Logic to xsd:dateTime

-- Helper to extract year for applyModifier from the structured DateType
getYearFromDateType :: DateType -> Int
getYearFromDateType (DTYear y) = y
getYearFromDateType (DTYearMonth y _) = y
getYearFromDateType (DTDayMonthYear _ _ y) = y
getYearFromDateType (DTYearMonthDay y _ _) = y

-- | Converts the parsed unit structure into a (start, end) tuple.
parsedUnitToRangeTuple :: ParsedUnit -> (Text, Text)
parsedUnitToRangeTuple pu =
    case pu of
      Single p ->
          let yearForModifier = getYearFromDateType (dateFields p)
              (modYearVal1, modYearVal2) = applyModifier (modifier p) yearForModifier -- e.g., (year-margin, year+margin)

              -- Determine effective start and end years considering BCE inversion for modifiers
              (effStartYear, effEndYear) =
                  if isBCE p && modifier p /= None then
                      -- For BCE with a modifier: year+margin is earlier, year-margin is later.
                      -- E.g., "um 100ante" (margin 2) -> range is 102ante to 98ante.
                      -- applyModifier returns (98, 102). So we use (modYearVal2, modYearVal1).
                      (modYearVal2, modYearVal1)
                  else
                      -- For CE, or BCE with no modifier: year-margin is earlier, year+margin is later.
                      -- Or if no modifier, modYearVal1 == modYearVal2 == yearForModifier.
                      (modYearVal1, modYearVal2)

              startInputDateType = case dateFields p of
                                     DTYear _ -> DTYear effStartYear -- Apply modifier only if it's a year-only date
                                     _        -> dateFields p -- For more specific dates, modifier affects year range but not d/m
              endInputDateType   = case dateFields p of
                                     DTYear _ -> DTYear effEndYear
                                     _        -> dateFields p
          in ( xsdDateTimeFromDateType startInputDateType (isBCE p) DateLowerBound
             , xsdDateTimeFromDateType endInputDateType (isBCE p) DateUpperBound )

      SlashRange p1 p2 -> -- p1, p2 are ParsedPiece. Interpreted as "from p1 to p2".
          let (p1Start, _p1End) = parsedUnitToRangeTuple (Single p1) -- Use start of first piece's range
              (_p2Start, p2End) = parsedUnitToRangeTuple (Single p2) -- Use end of second piece's range
          in (p1Start, p2End)

      DashRange u1 u2 -> -- u1, u2 are ParsedUnit. Result is start of u1 to end of u2.
          let (start1, _) = parsedUnitToRangeTuple u1 -- We only care about the start of the first unit
              (_, end2)   = parsedUnitToRangeTuple u2 -- We only care about the end of the second unit
          in (start1, end2)

-- | Applies margins based on the modifier. These are configurable.
applyModifier :: Modifier -> Int -> (Int, Int)
applyModifier modifier' year = case modifier' of -- Renamed mod to modifier'
    Um    -> (year - 2, year + 2)
    Ca    -> (year - 10, year + 10)
    Vor   -> (year - 100, year)
    Nach  -> (year, year + 100)
    UmNach -> (year - 2, year + 100) -- New
    VorUm  -> (year - 100, year + 2)  -- New
    _     -> (year, year)

-- The old formatYear and readAsInt are no longer needed as they've been replaced or made redundant.
