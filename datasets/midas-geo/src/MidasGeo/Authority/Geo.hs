module MidasGeo.Authority.Geo (
  lookupGeoId,         -- This is for the old geo.xml, remains as is
  lookupCountry,
  lookupRegion,
  lookupCity,
  lookupPlace,
  placeId,
  lookupPlaceReconciliation
) where

import CommonImports

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Text.Encoding.Error (lenientDecode)
import qualified Data.ByteString as BS
import qualified Data.Vector.Hashtables as HT
import qualified Data.Vector.Mutable as VM
import Control.Monad.Primitive (PrimState)
import qualified Pharos.Utils.CsvHashTable as CsvHT
import System.IO.Unsafe (unsafePerformIO)
import qualified Text.XML.Hexml as Hexml -- Keep for loadGeoData
import Control.Monad (forM_, unless)
import Data.List (find)
import Data.Maybe (listToMaybe)
import Data.Either (lefts)

type GeoCacheValue = T.Text
type GeoHashTable = HT.Dictionary (PrimState IO) VM.MVector T.Text VM.MVector GeoCacheValue

placeId idXPath cityXPath node = do
  let explicitGeoId = evalXPathAsText node idXPath
  case explicitGeoId of
    Just geoId -> (Just geoId, constUri $ "place/" <> geoId)
    Nothing -> do
      let placeNameMb = evalXPathAsText node cityXPath
      case placeNameMb of
        Just placeName ->
          case lookupGeoId placeName of
            Just geoIdFromLookup -> (Just geoIdFromLookup, constUri $ "place/" <> geoIdFromLookup)
            Nothing -> (Nothing, constUri $ "place/" <> makeUrlFriendly placeName)
        Nothing -> (Nothing, constUri "")

lookupPlaceReconciliation (Just geoid) _ = lookupPlace (Just geoid) []
lookupPlaceReconciliation Nothing name = lookupPlace Nothing ["", "", name]
lookupPlaceReconciliation _ _ = ""

-- Helper to extract text from a specific child element of a Hexml.Node
getTextFromChild :: Hexml.Node -> BS.ByteString -> Maybe T.Text
getTextFromChild parentNode childName =
    case listToMaybe (Hexml.childrenBy parentNode childName) of
        Nothing -> Nothing
        Just childNode ->
            let textParts = lefts (Hexml.contents childNode)
            in if null textParts
               then Nothing
               else Just $ T.strip $ TE.decodeUtf8With lenientDecode (BS.concat textParts)

-- Load Geo data from XML into a hashtable
loadGeoData :: FilePath -> IO GeoHashTable
loadGeoData filePath = do
  xmlBS <- BS.readFile filePath
  ht <- HT.initialize 50000

  case Hexml.parse xmlBS of
    Left err -> do
      putStrLn $ "Error parsing XML (" ++ filePath ++ "): " ++ show err
      pure ht -- Return empty or partially filled table on error
    Right doc -> do
      -- The root element in the example is <root>
      -- Hexml.children doc will give top-level nodes. We need to find "root".
      let rootNodes = Hexml.children doc
      case find (\n -> Hexml.name n == "root") rootNodes of
        Nothing -> do
          putStrLn $ "Error: <root> element not found in " ++ filePath
          pure ht
        Just rootNode -> do
          let geoNodes = Hexml.childrenBy rootNode "geo"
          forM_ geoNodes $ \geoNode -> do
            let maybeValue = getTextFromChild geoNode "a2000" -- This is the common value

            case maybeValue of
              Nothing -> pure () -- If there's no value, we can't insert anything for either key
              Just valueToCache -> do
                -- Process the primary key from <a2050>
                let maybeKeyPrimary = getTextFromChild geoNode "a2050"
                case maybeKeyPrimary of
                  Just keyPrimary ->
                    unless (T.null keyPrimary) $ HT.insert ht keyPrimary valueToCache
                  Nothing -> pure () -- No primary key, or it's empty

                -- Process the alternative key from <a2050a>
                let maybeKeyAlternative = getTextFromChild geoNode "a2050a"
                case maybeKeyAlternative of
                  Just keyAlternative ->
                    unless (T.null keyAlternative) $ HT.insert ht keyAlternative valueToCache
                  Nothing -> pure () -- No alternative key, or it's empty
          -- Print the number of items loaded into the cache
          count <- HT.size ht
          putStrLn $ "Loaded " ++ show count ++ " items into Geo cache from " ++ filePath
          pure ht

-- Cached Geo data for performance (from geo.xml)
geoDataCache :: GeoHashTable
geoDataCache = unsafePerformIO $ do
  resolvedPath <- CsvHT.resolveFilePath "datasets/midas-geo/resources/geo.xml"
  loadGeoData resolvedPath
{-# NOINLINE geoDataCache #-}

-- Lookup function for geo data (from geo.xml)
-- Returns Maybe GeoCacheValue
lookupGeoId :: T.Text -> Maybe GeoCacheValue
lookupGeoId key = unsafePerformIO $
  if T.isInfixOf (T.pack "&") key
  then do
    let parts = map T.strip $ T.splitOn (T.pack "&") key
    -- Helper function to find the first successful lookup
    let findFirst [] = pure Nothing
        findFirst (p:ps) = do
          result <- HT.lookup geoDataCache p
          case result of
            Just _  -> pure result
            Nothing -> findFirst ps
    findFirst parts
  else
    HT.lookup geoDataCache (T.strip key)
{-# NOINLINE lookupGeoId #-}

-- Configuration for CSV paths
countriesCsvPath :: FilePath
countriesCsvPath = "datasets/midas-geo/resources/countries.csv"

regionsCsvPath :: FilePath
regionsCsvPath = "datasets/midas-geo/resources/regions.csv"

citiesCsvPath :: FilePath
citiesCsvPath = "datasets/midas-geo/resources/cities.csv"

-- Column Name Placeholders (as per plan)
colCountryName :: T.Text
colCountryName = T.pack "country"

colRegionName :: T.Text
colRegionName = T.pack "region"

colCityName :: T.Text
colCityName = T.pack "city"

colGeoId :: T.Text
colGeoId = T.pack "geo_id"

colTgnUri :: T.Text
colTgnUri = T.pack "tgn_uri"

colWikidataUri :: T.Text
colWikidataUri = T.pack "wikidata_uri"

-- Preferred value columns for URI lookup
preferredUriValueCols :: [T.Text]
preferredUriValueCols = [colTgnUri, colWikidataUri]

-- --- Name-Based Lookup Tables ---

countryNameTable :: CsvHT.LocationReconciliationHash
countryNameTable = unsafePerformIO $ CsvHT.loadLocationReconciliationHash
    countriesCsvPath
    [colCountryName]
    preferredUriValueCols
{-# NOINLINE countryNameTable #-}

regionNameTable :: CsvHT.LocationReconciliationHash
regionNameTable = unsafePerformIO $ CsvHT.loadLocationReconciliationHash
    regionsCsvPath
    [colCountryName, colRegionName]
    preferredUriValueCols
{-# NOINLINE regionNameTable #-}

cityNameTable :: CsvHT.LocationReconciliationHash
cityNameTable = unsafePerformIO $ CsvHT.loadLocationReconciliationHash
    citiesCsvPath
    [colCountryName, colRegionName, colCityName]
    preferredUriValueCols
{-# NOINLINE cityNameTable #-}

-- --- Unified GeoID-Based Lookup Table ---

geoIdTable :: CsvHT.LocationReconciliationHash
geoIdTable = unsafePerformIO $ CsvHT.loadLocationReconciliationHashFromMultipleSources sources
  where
    sources = [
        CsvHT.CsvSourceConfig countriesCsvPath [colGeoId] preferredUriValueCols,
        CsvHT.CsvSourceConfig regionsCsvPath   [colGeoId] preferredUriValueCols,
        CsvHT.CsvSourceConfig citiesCsvPath    [colGeoId] preferredUriValueCols
      ]
{-# NOINLINE geoIdTable #-}

-- --- Lookup Functions ---

-- Helper to perform lookup and return T.Text
-- CsvHT.lookupLocation already returns T.Text (empty if not found) and handles key lowercasing.
performLookup :: CsvHT.LocationReconciliationHash -> [T.Text] -> T.Text
performLookup table keyParts = unsafePerformIO $ CsvHT.lookupLocation table keyParts

-- | Looks up a country by its geoId (if provided) or name.
-- Returns the primary URI (TGN then Wikidata) or an empty Text.
lookupCountry :: Maybe T.Text  -- ^ Optional geo_id
              -> [T.Text]      -- ^ List containing [countryName]
              -> T.Text
lookupCountry (Just gId) _ = performLookup geoIdTable [T.strip gId]
lookupCountry Nothing [countryName]  = performLookup countryNameTable [countryName]
lookupCountry Nothing _              = error "Invalid name parts for country"

-- | Looks up a region by its geoId (if provided) or names.
-- Returns the primary URI (TGN then Wikidata) or an empty Text.
lookupRegion :: Maybe T.Text   -- ^ Optional geo_id
             -> [T.Text]       -- ^ List containing [countryName, regionName]
             -> T.Text
lookupRegion (Just gId) _      = performLookup geoIdTable [T.strip gId]
lookupRegion Nothing [countryName, regionName] = performLookup regionNameTable [countryName, regionName]
lookupRegion Nothing _                   = error "Invalid name parts for region"

-- | Looks up a city by its geoId (if provided) or names.
-- Returns the primary URI (TGN then Wikidata) or an empty Text.
lookupCity :: Maybe T.Text    -- ^ Optional geo_id
           -> [T.Text]        -- ^ List containing [countryName, regionName, cityName]
           -> T.Text
lookupCity (Just gId) _                 = performLookup geoIdTable [T.strip gId]
lookupCity Nothing [countryName, regionName, cityName] = performLookup cityNameTable [countryName, regionName, cityName]
lookupCity Nothing _                              = error "Invalid name parts for city"

-- | Main lookup function that dispatches to specific level lookups.
-- Determines level based on the length of the placeNames list.
-- If geoId is provided, it's used; otherwise, names are used.
-- If placeNames is empty and no geoId is provided, returns empty Text.
-- If placeNames is empty but a geoId is provided, it attempts lookup using the geoId.
lookupPlace :: Maybe T.Text  -- ^ Optional geo_id
            -> [T.Text]      -- ^ List of place names [country, region?, city?]
            -> T.Text
lookupPlace (Just gId) [] = performLookup geoIdTable [T.strip gId] -- If placeNames is empty and geoId is present, lookup by geoId
lookupPlace Nothing    [] = T.empty -- No geoId and no placeNames, return empty
lookupPlace mGeoId placeNames
    | length placeNames == 3 = lookupCity mGeoId placeNames   -- [country, region, city]
    | length placeNames == 2 = lookupRegion mGeoId placeNames -- [country, region]
    | length placeNames == 1 = lookupCountry mGeoId placeNames -- [country]
    | otherwise              = T.empty -- Invalid number of place name parts
