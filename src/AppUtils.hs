{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module AppUtils (
    AppConfig(..),
    runApp
) where

import Engine (processXMLFile, EngineConfig(..), defaultEngineConfig)
import DSL (Mapping)
import CidocCRM (Class_(..))
import System.Environment (getArgs, getProgName)
import Control.Monad (void, zipWithM_, when)
import System.Directory (doesFileExist, doesDirectoryExist, listDirectory, createDirectoryIfMissing)
import System.FilePath ((</>), replaceExtension, takeDirectory, takeFileName)
import Data.List (isSuffixOf)
import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar, MVar)
import qualified Data.Text.IO as TIO
import GHC.Conc (numCapabilities, setNumCapabilities)
import Data.List.Split (chunksOf)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import qualified Data.Text as T
import RDF (Value, OutputFormat(..)) -- Import OutputFormat

-- | Configuration for a dataset processing application.
-- The type parameter 'c' represents the top-level CRM class for the mapping.
data AppConfig (c :: Class_) = AppConfig
    { appMapping         :: Mapping c
    , appBaseUri         :: Maybe T.Text
    , appVocabularyItems :: IO [Value] -- Action to get vocabulary items
    , appDatasetName     :: Maybe T.Text -- New field for dataset name
    }

-- | Process a single XML file using the provided configuration.
processFileWithConfig :: AppConfig c -> EngineConfig -> FilePath -> FilePath -> Maybe FilePath -> IO ()
processFileWithConfig config engConfig inFile mainOutFile maybeE13OutFile = do
    -- The datasetName is now part of engConfig.
    -- processXMLFile now handles writing to files directly.
    processXMLFile engConfig (appMapping config) (appBaseUri config) inFile mainOutFile maybeE13OutFile

-- | Process a single input file. Creates output directories if they don't exist,
-- then processes the XML.
processSingleFile :: AppConfig c -> EngineConfig -> FilePath -> FilePath -> Maybe FilePath -> IO ()
processSingleFile config engConfig inFile mainOutFilePath maybeE13OutFilePath = do
    createDirectoryIfMissing True (takeDirectory mainOutFilePath)
    mapM_ (createDirectoryIfMissing True . takeDirectory) maybeE13OutFilePath
    processFileWithConfig config engConfig inFile mainOutFilePath maybeE13OutFilePath

-- | Parse the --threads command-line argument.
parseThreadsArg :: [String] -> Int
parseThreadsArg args = case break (== "--threads") args of
    (_, "--threads":n:_) -> fromMaybe numCapabilities (readMaybe n)
    _ -> numCapabilities

-- | Split a list into n approximately equal chunks.
splitIntoChunks :: Int -> [a] -> [[a]]
splitIntoChunks n xs
    | n <= 0 = [xs] -- Avoid division by zero or negative chunks
    | otherwise =
        let len = length xs
            chunkSize = max 1 $ (len + n - 1) `div` n -- Ensure chunk size is at least 1
        in chunksOf chunkSize xs

-- | Process all XML files in an input directory concurrently.
processDirectory :: AppConfig c -> EngineConfig -> Int -> FilePath -> FilePath -> Maybe FilePath -> IO ()
processDirectory config engConfig numThreads' inDir mainOutDir maybeE13OutDir = do
    createDirectoryIfMissing True mainOutDir
    mapM_ (createDirectoryIfMissing True) maybeE13OutDir

    allEntries <- listDirectory inDir
    let xmlFiles = filter (isSuffixOf ".xml") allEntries
    if null xmlFiles
    then putStrLn $ "No .xml files found in directory: " ++ inDir
    else do
        putStrLn $ "Processing directory: " ++ inDir ++ " -> " ++ mainOutDir ++
                   (maybe "" (\e13d -> " (E13: " ++ e13d ++ ")") maybeE13OutDir) ++
                   " with " ++ show numThreads' ++ " threads..."
        setNumCapabilities numThreads' -- Set based on parsed arg or default

        -- Split files into batches based on number of threads
        let batches = splitIntoChunks numThreads' xmlFiles
        putStrLn $ "Distributing " ++ show (length xmlFiles) ++ " files into " ++ show (length batches) ++ " batches."

        -- Create MVars for synchronization
        doneVars <- mapM (const newEmptyMVar) batches

        -- Fork a thread for each batch
        zipWithM_ (processBatch config engConfig inDir mainOutDir maybeE13OutDir) batches doneVars

        -- Wait for all batches to complete
        mapM_ takeMVar doneVars
        putStrLn $ "Finished processing directory: " ++ inDir

-- | Helper function to process a batch of files in a separate thread.
processBatch :: AppConfig c -> EngineConfig -> FilePath -> FilePath -> Maybe FilePath -> [FilePath] -> MVar () -> IO ()
processBatch config engConfig inDir mainOutDir maybeE13OutDir batch doneVar = void $ forkIO $ do
    mapM_ (processOneFileInBatch config engConfig inDir mainOutDir maybeE13OutDir) batch
    putMVar doneVar ()

-- | Helper function to process a single file within a batch.
processOneFileInBatch :: AppConfig c -> EngineConfig -> FilePath -> FilePath -> Maybe FilePath -> FilePath -> IO ()
processOneFileInBatch config engConfig inDir mainOutDir maybeE13OutDir file = do
    let inFile = inDir </> file
    -- Determine output file extension based on EngineConfig
    let outExtension = case ecOutputFormat engConfig of
            TTL -> ".ttl"
            NTriples -> ".nt"
    let baseName = takeFileName file -- Use takeFileName to get just the file name for the output
    let mainOutFile = mainOutDir </> replaceExtension baseName outExtension
    let maybeE13OutFile = case maybeE13OutDir of
                            Just e13Dir -> Just (e13Dir </> replaceExtension baseName outExtension)
                            Nothing     -> Nothing
    processFileWithConfig config engConfig inFile mainOutFile maybeE13OutFile

-- Helper function to parse output format argument
parseOutputFormatArg :: [String] -> OutputFormat
parseOutputFormatArg args =
    go args TTL -- Default to TTL
  where
    go [] acc = acc
    go ("--output-format":"nt":rest) _ = NTriples -- if "nt" is found, it overrides
    go ("--output-format":"ntriples":rest) _ = NTriples -- if "ntriples" is found, it overrides
    go ("--output-format":"ttl":rest) _ = TTL -- if "ttl" is found, it overrides
    go (_:xs) acc = go xs acc -- continue searching

-- Helper function to parse --output-dir argument
parseOutputDirArg :: [String] -> Maybe FilePath
parseOutputDirArg [] = Nothing
parseOutputDirArg ("--output-dir":path:rest) = Just path
parseOutputDirArg (_:xs) = parseOutputDirArg xs

-- Helper function to parse --e13-output-dir argument
parseE13OutputDirArg :: [String] -> Maybe FilePath
parseE13OutputDirArg [] = Nothing
parseE13OutputDirArg ("--e13-output-dir":path:rest) = Just path
parseE13OutputDirArg (_:xs) = parseE13OutputDirArg xs

-- | Main application entry point. Parses arguments and calls the appropriate processing function.
runApp :: forall c. AppConfig c -> IO ()
runApp config = do
    args <- getArgs
    progName <- getProgName
    let numThreads = parseThreadsArg args
        genReverseProps = "--reverse-props" `elem` args
        materializeAligns = "--materialize-alignments" `elem` args
        generateE13 = "--generate-e13" `elem` args -- New flag
        outputFmt = parseOutputFormatArg args -- Parse the output format
        
        -- Parse dataset-name argument
        parseDatasetNameArg :: [String] -> Maybe T.Text
        parseDatasetNameArg [] = Nothing
        parseDatasetNameArg ("--dataset-name":name:rest) = Just (T.pack name)
        parseDatasetNameArg (_:xs) = parseDatasetNameArg xs
        
        datasetName = parseDatasetNameArg args
        
        -- Parse output directory arguments
        maybeOutputDir = parseOutputDirArg args
        maybeE13OutputDir = parseE13OutputDirArg args

        engineConf = defaultEngineConfig {
                        ecGenerateReverseProps = genReverseProps,
                        ecMaterializeAlignments = materializeAligns,
                        ecOutputFormat = outputFmt, -- Set the parsed output format
                        ecGenerateE13 = generateE13, -- Set the new flag
                        ecDatasetName = datasetName -- Set the parsed dataset name
                     }
        -- Update AppConfig with the dataset name
        -- Although appDatasetName is in AppConfig, EngineConfig's ecDatasetName is the one used by the Engine.
        -- We keep appDatasetName in AppConfig for potential direct use within AppUtils or if other app-level logic needs it.
        updatedConfig = config { appDatasetName = datasetName }

        -- Filter out recognized option flags and their arguments
        isOptionFlag arg = arg `elem` ["--threads", "--reverse-props", "--materialize-alignments", "--output-format", "--generate-e13", "--dataset-name", "--output-dir", "--e13-output-dir"] -- Added new flags
        isNumeric s = all (`elem` ['0'..'9']) s
        isOutputFormatValue s = s `elem` ["ttl", "nt", "ntriples"]

        filterOptions :: [String] -> [String] -> [String]
        filterOptions [] acc = reverse acc
        filterOptions (x:xs) acc
            | isOptionFlag x = 
                if x `elem` ["--threads", "--output-format", "--dataset-name", "--output-dir", "--e13-output-dir"] && not (null xs) then
                    -- Check specific conditions for arguments that take a value
                    if x == "--threads" && isNumeric (head xs) then filterOptions (drop 1 xs) acc
                    else if x == "--output-format" && isOutputFormatValue (head xs) then filterOptions (drop 1 xs) acc
                    else if x `elem` ["--dataset-name", "--output-dir", "--e13-output-dir"] then filterOptions (drop 1 xs) acc -- Consume value
                    else filterOptions xs acc -- Flag without valid value, or not one of these
                else filterOptions xs acc -- Flag that doesn't take a value, or end of list
            | otherwise = filterOptions xs (x:acc)

        nonOptionArgs = filterOptions args []

    case maybeOutputDir of
        Nothing -> do
            putStrLn "Error: --output-dir is a required argument."
            printUsage progName
        Just outputDir -> 
            case nonOptionArgs of                 
                [input] -> do
                    isFile <- doesFileExist input
                    isDir <- doesDirectoryExist input
                    
                    -- Determine output file extension based on EngineConfig
                    let outExtension = case ecOutputFormat engineConf of
                            TTL -> ".ttl"
                            NTriples -> ".nt"
                    
                    let effectiveE13OutputDir = if generateE13 then maybeE13OutputDir else Nothing

                    if isFile
                        then do
                            let mainOutFile = outputDir </> replaceExtension (takeFileName input) outExtension
                            let maybeE13OutFile = fmap (\e13d -> e13d </> replaceExtension (takeFileName input) outExtension) effectiveE13OutputDir
                            processSingleFile updatedConfig engineConf input mainOutFile maybeE13OutFile -- Use updatedConfig
                        else if isDir
                            then processDirectory updatedConfig engineConf numThreads input outputDir effectiveE13OutputDir -- Use updatedConfig
                            else putStrLn $ "Error: Input '" ++ input ++ "' is neither a file nor a directory."
                _ -> printUsage progName

printUsage :: String -> IO ()
printUsage progName = do
    putStrLn $ "Usage: " ++ progName ++ " <input-file-or-dir> --output-dir <dir> [--e13-output-dir <dir>] [OPTIONS]"
    putStrLn ""
    putStrLn "Required arguments:"
    putStrLn "  <input-file-or-dir>      Input XML file or directory containing XML files."
    putStrLn "  --output-dir <dir>       Directory to save the main RDF output."
    putStrLn ""
    putStrLn "Optional arguments for E13 generation:"
    putStrLn "  --e13-output-dir <dir>   Directory to save E13 triples. (Requires --generate-e13)"
    putStrLn "  --generate-e13           Generate E13 attribute assignment triples."
    putStrLn "                           (Requires --materialize-alignments, --output-format nt/ntriples, and --dataset-name)"
    putStrLn ""
    putStrLn "Other optional arguments:"
    putStrLn "  --threads N                Number of threads to use (default: number of CPU cores)."
    putStrLn "  --reverse-props            Optionally generate reverse properties (default: false)."
    putStrLn "  --materialize-alignments   Optionally materialize custom:sameAs alignments (default: false)."
    putStrLn "  --output-format <format>   Specify output format: ttl (default), nt, ntriples."
    putStrLn "  --dataset-name <name>      Specify the dataset name for URI generation."
    putStrLn $ "                     (Actual number of threads used: " ++ show numCapabilities ++ ")"
