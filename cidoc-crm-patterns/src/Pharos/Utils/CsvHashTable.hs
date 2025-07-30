{-# LANGUAGE ScopedTypeVariables #-}

module Pharos.Utils.CsvHashTable (
    LocationReconciliationHash,
    Key,
    CsvSourceConfig(..),
    loadLocationReconciliationHash,
    loadLocationReconciliationHashFromMultipleSources,
    lookupLocation,
    lookupValues,
    lookupValue, 
    resolveFilePath
) where

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import qualified Data.Csv as Csv
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Vector as V
import qualified Data.Vector.Hashtables as HT
import qualified Data.Vector.Mutable as VM
import Control.Monad.Primitive (PrimState)
import Control.Monad (forM_)
import Data.List (intercalate)
import qualified Data.Foldable
import Data.Maybe (fromMaybe, isJust)
import System.Environment (lookupEnv)
import System.FilePath ((</>), isAbsolute)
import System.IO.Unsafe (unsafePerformIO)

-- | The key for the reconciliation hash, composed of one or more Text values from CSV columns.
type Key = [T.Text]

-- | The hash table structure for location reconciliation.
-- Stores a vector of Maybe Text values for each key, corresponding to 'valueColumnNames'.
type LocationReconciliationHash = HT.Dictionary (PrimState IO) VM.MVector Key VM.MVector (V.Vector (Maybe T.Text))

-- Helper function to resolve the file path
resolveFilePath :: FilePath -> IO FilePath
resolveFilePath fp = do
    if isAbsolute fp then do
        return fp
    else do
        maybeBasePath <- lookupEnv "HS_TO_X3ML_BASE"
        let basePath = fromMaybe "." maybeBasePath
        let finalPath = basePath </> fp
        return finalPath

-- | Loads data from a CSV file into a hash table for reconciliation.
-- The hash table keys are formed from values in 'keyColumnNames', in order.
-- The hash table values are Text values, derived from the first non-empty column in 'valueColumnNames'.
-- Rows are skipped if any specified key column is missing, if any key part is an empty string,
-- or if no non-empty value is found in any of the 'valueColumnNames'.
loadLocationReconciliationHash :: FilePath         -- ^ Path to the CSV file.
                               -> [T.Text]         -- ^ Names of columns to form the composite key, in order.
                               -> [T.Text]         -- ^ Names of columns to search for the value, in order of preference.
                               -> IO (LocationReconciliationHash)
loadLocationReconciliationHash filePath keyColumnNames valueColumnNames = do
    actualFilePath <- resolveFilePath filePath
    csvData <- BL.readFile actualFilePath

    case Csv.decodeByName csvData of
        Left initialDecodeErr -> error $ "Error decoding CSV (" ++ actualFilePath ++ "): " ++ initialDecodeErr
        Right (headerBS_V, rows :: V.Vector Csv.NamedRecord) -> do

            let headerTexts = V.toList $ V.map TE.decodeUtf8 headerBS_V

            -- Validate that all specified column names are present in the CSV header.
            let missingKeyCols = filter (not . (`elem` headerTexts)) keyColumnNames
            let missingValueCols = filter (not . (`elem` headerTexts)) valueColumnNames

            let errorMessages =
                  (["Key columns not found in CSV header: " ++ intercalate ", " (map T.unpack missingKeyCols) | not (null missingKeyCols)]) ++
                  (["Value columns not found in CSV header: " ++ intercalate ", " (map T.unpack missingValueCols) | not (null missingValueCols)]) ++
                  (["List of value column names cannot be empty." | null valueColumnNames])

            if not (null errorMessages) then
                error $ "Error processing CSV headers for location reconciliation (" ++ actualFilePath ++ "):\n" ++ unlines errorMessages
            else do
                ht <- HT.initialize (V.length rows)

                forM_ rows $ \record -> do
                    let keyPartsBS_MaybeList = map (\name -> HashMap.lookup (TE.encodeUtf8 name) record) keyColumnNames
                    let mKeyTexts = mapM toValidKeyPartText keyPartsBS_MaybeList

                    case mKeyTexts of
                        Just keyTexts -> do
                            -- Get all values from the specified valueColumnNames
                            let valueTextsVec = findAllValueTexts record valueColumnNames
                            -- Insert if at least one value is Just something (i.e., not all are Nothing)
                            if V.any isJust valueTextsVec then
                                HT.insert ht keyTexts valueTextsVec
                            else
                                pure () -- Skip row if all value columns are empty or missing for this row
                        Nothing -> pure () -- Skip row due to invalid/missing key part(s)

                return ht

-- | Looks up a key in the LocationReconciliationHash.
-- Returns the first non-empty 'T.Text' value found in the stored vector of values,
-- based on the original order of 'valueColumnNames'.
-- Returns 'T.empty' if the key is not found or if all stored values are 'Nothing' or empty.
lookupLocation :: LocationReconciliationHash
               -> Key                      -- ^ The key values to look up (must match order of keyColumnNames at load time).
               -> IO T.Text
lookupLocation table key = return $ fromMaybe T.empty (lookupValue table key)

-- | Looks up a key in the LocationReconciliationHash and returns all stored values.
-- Uses unsafePerformIO for direct return without IO monad.
-- Returns a list of 'Maybe T.Text' corresponding to 'valueColumnNames' at load time.
-- Returns an empty list if the key is not found.
lookupValues :: LocationReconciliationHash
             -> Key          -- ^ The key values to look up.
             -> [Maybe T.Text] -- ^ List of Maybe Text values, or empty list if key not found.
lookupValues table key = unsafePerformIO $ do
    let lowerKey = map T.toLower key
    mValueVector <- HT.lookup table lowerKey
    case mValueVector of
        Nothing -> return []
        Just valueVector -> return $ V.toList valueVector

-- | Looks up a key in the LocationReconciliationHash and returns the first non-empty 'T.Text' value.
-- Uses unsafePerformIO for direct return without IO monad.
-- Returns 'Nothing' if the key is not found or if all stored values are 'Nothing' or empty.
lookupValue :: LocationReconciliationHash
       -> Key          -- ^ The key values to look up.
       -> Maybe T.Text -- ^ Maybe Text value, or Nothing if key not found or no suitable value.
lookupValue table key = unsafePerformIO $ do
    let lowerKey = map T.toLower key
    mValueVector <- HT.lookup table lowerKey
    case mValueVector of
        Nothing -> return Nothing
        Just valueVector ->
            -- Find the first Just Text in the vector
            let firstJustText = Data.Foldable.find isJust valueVector
            in case firstJustText of
                Just (Just text) -> return $ Just text -- Found a Just Text
                _                -> return Nothing    -- Vector was empty, all elements were Nothing, or find returned Nothing

-- Helper function to normalize spaces in a Text value
-- Trims leading/trailing whitespace and collapses internal whitespace sequences to a single space.
normalizeSpace :: T.Text -> T.Text
normalizeSpace = T.unwords . T.words

-- Helper: Converts a Maybe BS.ByteString from a CSV cell to Maybe Text for a key part.
-- Returns Nothing if the ByteString is Nothing (column not found).
-- An empty ByteString is converted to an empty Text, considered a valid key part.
-- The Text is normalized for spaces and then converted to lowercase.
toValidKeyPartText :: Maybe BS.ByteString -> Maybe T.Text
toValidKeyPartText Nothing = Nothing
toValidKeyPartText (Just bs) = Just (T.toLower (normalizeSpace (TE.decodeUtf8 bs))) -- Normalize spaces then convert to lowercase

-- Helper: Iterates through a list of column names and returns a Vector of Maybe Text
-- for each column in the given record.
findAllValueTexts :: Csv.NamedRecord -> [T.Text] -> V.Vector (Maybe T.Text)
findAllValueTexts record colNames =
    V.fromList $ map (\colName ->
        case HashMap.lookup (TE.encodeUtf8 colName) record of
            Just bs | not (BS.null bs) -> Just (TE.decodeUtf8 bs) -- Found a non-empty value
            _                          -> Nothing -- Column not found or value is empty
    ) colNames

-- | Configuration for a single CSV data source.
data CsvSourceConfig = CsvSourceConfig {
    csvFilePath         :: FilePath,    -- ^ Path to the CSV file.
    csvKeyColumnNames   :: [T.Text],    -- ^ Names of columns to form the composite key.
    csvValueColumnNames :: [T.Text]     -- ^ Names of columns to search for the value, in order of preference.
} deriving (Show)

-- | Loads data from multiple CSV files (specified by CsvSourceConfig) into a single hash table.
-- Keys from later files in the list will overwrite keys from earlier files if they are identical.
loadLocationReconciliationHashFromMultipleSources :: [CsvSourceConfig]
                                                  -> IO LocationReconciliationHash
loadLocationReconciliationHashFromMultipleSources configs = do
    -- Estimate initial size. This is a rough guess; it could be refined.
    -- For now, let's assume an average of 10000 rows per file if not empty, or a base of 1000.
    let estimatedSize = sum $ map (const 10000) $ filter (not . null . csvFilePath) configs -- Crude estimation
    ht <- HT.initialize (max 1000 estimatedSize) -- Ensure at least a small size

    forM_ configs $ \config -> do
        let configFilePath = csvFilePath config
        let keyColumnNames = csvKeyColumnNames config
        let valueColumnNames = csvValueColumnNames config

        -- Basic validation for this specific config
        if null configFilePath then
            error $ "Warning: Empty file path provided in CsvSourceConfig, skipping."
        else if null keyColumnNames then
            error $ "Warning: Empty keyColumnNames for " ++ configFilePath ++ ", skipping."
        else if null valueColumnNames then
            error $ "Warning: Empty valueColumnNames for " ++ configFilePath ++ ", skipping."
        else do
            actualFilePath <- resolveFilePath configFilePath
            csvDataExists <- BL.readFile actualFilePath -- This will throw an error if file doesn't exist, which is fine.
            case Csv.decodeByName csvDataExists of
                Left initialDecodeErr ->
                    error $ "Error decoding CSV (" ++ actualFilePath ++ "): " ++ initialDecodeErr ++ ". Skipping this source."
                Right (headerBS_V, rows :: V.Vector Csv.NamedRecord) -> do
                    let headerTexts = V.toList $ V.map TE.decodeUtf8 headerBS_V

                    let missingKeyCols = filter (not . (`elem` headerTexts)) keyColumnNames
                    let missingValueCols = filter (not . (`elem` headerTexts)) valueColumnNames

                    let errorMessagesConfig =
                          (["Key columns not found in CSV header for " ++ actualFilePath ++ ": " ++ intercalate ", " (map T.unpack missingKeyCols) | not (null missingKeyCols)]) ++
                          (["Value columns not found in CSV header for " ++ actualFilePath ++ ": " ++ intercalate ", " (map T.unpack missingValueCols) | not (null missingValueCols)])

                    if not (null errorMessagesConfig) then
                        error $ "Error processing CSV headers for " ++ actualFilePath ++ ":\n" ++ unlines errorMessagesConfig ++ ". Skipping this source."
                    else do
                        forM_ rows $ \record -> do
                            let keyPartsBS_MaybeList = map (\name -> HashMap.lookup (TE.encodeUtf8 name) record) keyColumnNames
                            let mKeyTexts = mapM toValidKeyPartText keyPartsBS_MaybeList

                            case mKeyTexts of
                                Just keyTexts -> do
                                    let valueTextsVec = findAllValueTexts record valueColumnNames
                                    if V.any isJust valueTextsVec then
                                        HT.insert ht keyTexts valueTextsVec
                                    else
                                        pure () -- Skip row if all value columns are empty or missing
                                Nothing -> pure () -- Skip row due to invalid/missing key part(s)
    return ht
