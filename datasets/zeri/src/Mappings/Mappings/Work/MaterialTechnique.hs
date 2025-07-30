{-# LANGUAGE BangPatterns #-}
module Mappings.Mappings.Work.MaterialTechnique where

import CommonImports
import qualified Mappings.Vocabulary as ZeriMeta
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Hashtables as HT
import Control.Monad.Primitive (PrimState)
import qualified Control.Monad
import System.IO.Unsafe (unsafePerformIO)
import Pharos.Utils.CsvHashTable (resolveFilePath)


-- | Zeri MTC mapping
-- MTC element holds a strings which represents both material and technique.
-- We first split the string by comma, then check the type of each part,
-- base on a spreedsheet, and finally map it to the correct CIDOC-CRM pattern.
-- @see MTC sheet in https://docs.google.com/spreadsheets/d/19KBIEYefmsZF1p04k0Mq35yV1eS5a2smtWQcHSO_e0c/edit?usp=sharing
materialTechniqueLinks :: PathTree E22_
materialTechniqueLinks =
  [x|PARAGRAFO[@etichetta="OBJECT"]/MTC|] -- Sets the initial XPath context
  @> (                                   -- Applies to the ContextGroup created by @>
    (split "," >=> mtcTypeNormalizer)    -- This is the Preprocessor
    @> [                                -- Preprocessor on LHS of @> creates a ContextGroup with this preprocessor
      when (nodeNameIs "material") material, -- Ensure material/technique are wrapped if they are functions
      when (nodeNameIs "technique") technique -- or directly if they are PathTree
    ]
  )
-- Material mapping
material :: PathTree E22_
material =
  P45 ---> (E57, templateUri "vocab/{id}" [("id", [x|text()|])]) ==> [
    P2 --> (E55, ZeriMeta.mtc),
    P1 ---> (E41, relativeUri "/name") ==> [
      P190 --> literal [x|text()|]
    ]
  ]

-- Technique mapping
technique :: PathTree E22_
technique =
  P108i ---> (E12, relativeUri "/production") ==> [
    P32 ---> (E55, templateUri "vocab/{id}" [("id", [x|text()|])]) ==> [
      P2 --> (E55, ZeriMeta.mtc),
      P1 ---> (E41, relativeUri "/name") ==> [
        P190 --> literal [x|text()|]
      ]
    ]
  ]

-- MTC type-based normalizer
mtcTypeNormalizer = Preprocessor {
  xpath = [x|text()|],
  transform = \node -> do
    let text = nodeText node
    case lookupMTCType text of
      Just typ -> return [makeElementNode (Just node) typ [] [makeTextNode text]]
      Nothing -> return [] -- return nothing if not found
}


-- Lookup MTC type with O(1) performance
lookupMTCType :: T.Text -> Maybe T.Text
lookupMTCType value = unsafePerformIO $ do
  HT.lookup mtcTypesCache (T.strip value)
{-# NOINLINE lookupMTCType #-}

-- CSV record type for MTC types
data MTCType = MTCType
  { mtcValue :: !T.Text
  , mtcType :: !T.Text
  } deriving (Show)

-- Fast CSV parser for MTC types
parseMTCLine :: BS.ByteString -> MTCType
parseMTCLine line =
  let (value, rest) = BS.break (== 44) line -- 44 is ASCII for comma
      typeVal = if BS.null rest
                then BS.empty
                else BS.tail rest -- Skip the comma
  in MTCType
       (T.strip $ TE.decodeUtf8 value)
       (T.strip $ TE.decodeUtf8 typeVal)

-- Type alias for our Text-to-Text hashtable
type TextHashTable = HT.Dictionary (PrimState IO) VM.MVector T.Text VM.MVector T.Text

-- Load MTC types directly into a hashtable from CSV file
loadMTCTypes :: FilePath -> IO TextHashTable
loadMTCTypes filePath = do
  realFilePath <- resolveFilePath filePath
  !csvData <- BS.readFile realFilePath
  !ht <- HT.initialize 600  -- Initialize with known size
  
  let 
    -- Process one line at a time with direct indexing
    go !pos !end
      | pos >= end = return ()
      | otherwise = do
          let 
            !nextNL = case BS.elemIndex 10 (BS.drop pos csvData) of
                       Just n  -> pos + n
                       Nothing -> end
            !line = BS.take (nextNL - pos) (BS.drop pos csvData)
          
          -- Process non-empty lines
          Control.Monad.unless (BS.null line) $ do
            let !record = parseMTCLine line
            HT.insert ht (mtcValue record) (mtcType record)
          
          go (nextNL + 1) end

  go 0 (BS.length csvData)
  return ht

-- Cached MTC types for performance using hashtable for O(1) lookups
mtcTypesCache :: TextHashTable
mtcTypesCache = unsafePerformIO $ loadMTCTypes "datasets/zeri/resources/mtc-types.csv"
{-# NOINLINE mtcTypesCache #-}
