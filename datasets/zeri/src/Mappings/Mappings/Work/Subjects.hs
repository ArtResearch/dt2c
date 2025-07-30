module Mappings.Mappings.Work.Subjects where

import CommonImports
import qualified Vocabularies.PHAROS as P
import Pharos.CidocCrm.Patterns (appellation_0_1)
import Pharos.CidocCrm.Patterns.Iconclass (iconclassMapping)
import Pharos.Utils.CsvHashTable (LocationReconciliationHash, loadLocationReconciliationHash, lookupValue)

import System.IO.Unsafe (unsafePerformIO)
import Network.HTTP.Types.URI (urlEncode)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Char (isSpace)
import Data.Maybe (fromMaybe)
import Data.List (foldl')

subjectLink :: PathTree E22_
subjectLink = 
  [x|PARAGRAFO[@etichetta='OBJECT']/SGTI|] @> splitSubjectsOnComma @> (
    P65 ---> (E36, relativeUriT "/visual_item/subject/{i}" [("i", i)]) ==> [
      P2 ---> (E55, templateUri "subject/{name}" [("name", [x|text()|])]) ==> [
        P2 --> (E55, P.subject),
        appellation_0_1 P.preferred_name [x|text()|],
        [x|text()|] @> (\node -> maybe NullTree (iconclassMapping SameAs) (lookupIconClassCode (nodeText node)))
      ]
    ]
  )

-- Repository Reconciliation Cache
lookupIconClassCode subject = lookupValue iconclassTable [subject]

iconclassTable :: LocationReconciliationHash
iconclassTable = unsafePerformIO $ loadLocationReconciliationHash
    "datasets/zeri/resources/subjects/iconclass.csv"
    [T.pack "Value in SGTI (work of art record subject)"]
    [T.pack "cod ICONCLASS (Field DESI)"]
{-# NOINLINE iconclassTable #-}

-----------
-- |
-- Preprocessor to split subjects on commas, 
-- but only if the comma is not followed by whitespace.
splitSubjectsOnComma :: Preprocessor
splitSubjectsOnComma = Preprocessor {
    xpath = [x|text()|],
    transform = \node -> do
        let text = nodeText node
        return [makeTextNode (T.strip value) | value <- splitTextOnConditionalComma text]
}

-- Splits a Text string by commas, but only if they are not followed by whitespace.
splitTextOnConditionalComma :: T.Text -> [T.Text]
splitTextOnConditionalComma txt =
    let len = T.length txt
        -- Step 1: Find all indices of commas that satisfy the splitting condition.
        -- We iterate through all possible indices `[0 .. len - 1]`
        validCommaIndices = filter (isSplitPoint txt) [0 .. len - 1]

        -- Step 2: Partition the text using these valid comma indices.
        -- We use `foldl'` to efficiently accumulate segments.
        -- `(acc, currentStartIdx)`:
        --   `acc` is a reversed list of segments found so far.
        --   `currentStartIdx` is the starting index in `txt` for the current segment being built.
        (reversedSegments, lastStartIdx) =
            foldl' (splitter txt) ([], 0) validCommaIndices

        -- After the fold, the last segment remains. It goes from `lastStartIdx` to the end of `txt`.
        finalSegments = T.drop lastStartIdx txt : reversedSegments
    in
        -- Reverse the accumulated segments to get them in the correct order.
        reverse finalSegments
  where
    -- `splitter` is the accumulation function for `foldl'`.
    -- `fullText`: The original string (needed for slicing).
    -- `(acc, currentStartIdx)`: The accumulator from the previous step.
    -- `commaIdx`: The index of the current valid comma to split on.
    splitter :: T.Text -> ([T.Text], Int) -> Int -> ([T.Text], Int)
    splitter fullText (acc, currentStartIdx) commaIdx =
        -- The segment to extract goes from `currentStartIdx` up to `commaIdx - 1`.
        -- `T.take (commaIdx - currentStartIdx)` gives the length of this segment.
        let segment = T.take (commaIdx - currentStartIdx) (T.drop currentStartIdx fullText)
        in (segment : acc, commaIdx + 1) -- Add the segment and update `currentStartIdx` for the next segment.

-- Helper to safely get a character at a specific index.
-- Returns `Nothing` if the index is out of bounds.
charAtIndex :: T.Text -> Int -> Maybe Char
charAtIndex text index
  | index < 0 || index >= T.length text = Nothing
  | otherwise = Just $ T.index text index

-- Determines if a comma at a given index `i` in the text `t` is a valid split point.
-- A comma is considered a valid split point if:
-- 1. It is indeed a comma (`T.index t i == ','`).
-- 2. The character immediately after it (if it exists) is NOT whitespace.
--    If there is no character after it (i.e., comma is at the very end of the string),
--    it's considered valid on that side.
isSplitPoint :: T.Text -> Int -> Bool
isSplitPoint text index =
    T.index text index == ',' &&
    -- Check character after the comma:
    -- `fromMaybe True` means if there's no character after (e.g., comma is at the end),
    -- it's considered valid for splitting.
    (fromMaybe True $ fmap (not . isSpace) (charAtIndex text (index + 1)))
