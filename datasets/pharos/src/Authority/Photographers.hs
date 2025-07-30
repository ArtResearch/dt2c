module Authority.Photographers (
  photographerUri
) where

import CommonImports

import qualified Data.Text as T
import qualified Pharos.Utils.CsvHashTable as CsvHT
import System.IO.Unsafe (unsafePerformIO)
import Data.Char (isSpace)

-- File path for the photographers CSV
photographersCsvPath :: FilePath
photographersCsvPath = "datasets/pharos/resources/actors/photographers.csv"

-- Column names
colPhotographer :: T.Text
colPhotographer = T.pack "photographer"

colInstitute :: T.Text
colInstitute = T.pack "institute"

colWikidata :: T.Text
colWikidata = T.pack "wikidata"

colSeparateEntity :: T.Text
colSeparateEntity = T.pack "separate entity"

-- Key columns for the hash table
keyColumns :: [T.Text]
keyColumns = [colPhotographer, colInstitute]

-- Value columns to retrieve
valueColumns :: [T.Text]
valueColumns = [colWikidata, colSeparateEntity]

-- Hash table for photographers
photographersTable :: CsvHT.LocationReconciliationHash
photographersTable = unsafePerformIO $ CsvHT.loadLocationReconciliationHash
    photographersCsvPath
    keyColumns
    valueColumns
{-# NOINLINE photographersTable #-}

photographerUri :: T.Text -> T.Text -> T.Text
photographerUri instituteName photographerName =
  case lookupPhotographerDetails photographerName instituteName of
    -- in photographers csv wikidata column can be "none" or empty
    (Just wikidataUri, _) | not (T.isInfixOf ";" wikidataUri) && not (T.isInfixOf "&" wikidataUri) && not (T.isInfixOf " or " wikidataUri) && not (T.isInfixOf "?" wikidataUri) && not (T.isInfixOf "none" wikidataUri)  -> T.dropAround isSpace wikidataUri
    (_, Just pharosEntity) -> "https://artresearch.net/resource/pharos/actor/photographer/" <> makeUrlFriendly pharosEntity
    _ -> ""

-- | Looks up photographer details by photographer name and institute.
-- Returns a tuple: (Maybe Wikidata URI, Maybe Separate Entity).
lookupPhotographerDetails :: T.Text  -- ^ Photographer name
                          -> T.Text  -- ^ Institute name
                          -> (Maybe T.Text, Maybe T.Text) -- ^ (Wikidata URI, Separate Entity)
lookupPhotographerDetails photographerName instituteName =
  let key = [photographerName, instituteName]
      results = CsvHT.lookupValues photographersTable key
  in case results of
      (wikidataMb : separateEntityMb : _) -> (wikidataMb, separateEntityMb)
      _                                   -> (Nothing, Nothing)
