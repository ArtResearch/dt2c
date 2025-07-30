{-# LANGUAGE NoOverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}

module Engine (
    processXMLFile,
    processXMLString,
    processXMLStringAsSet,
    processXMLStringWithConfig,
    processXMLStringAsSetWithConfig,
    evaluateMapping,
    Value(..),
    EngineConfig(..),
    defaultEngineConfig,
    OutputFormat(..) -- Export OutputFormat
    ) where

import DSL
import CidocCRM (propUri, unClassRef, getInverseOfProperty, Property_( P0 ))
import Generators (generateId, GenerationContext, makeContext, Generator(VocabGen), IsGenerator(..), unwrapGenerator)
import XML
import RDF (Value(..), Triple(..), formatTriples, OutputFormat(..), expandValue, valueToBuilder) -- Import OutputFormat, expandValue, valueToBuilder
import Util.ShowableSet

import qualified Data.ByteString.Char8 as BS
import qualified Text.XML.Hexml as X
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE -- Added for UTF-8 encoding/decoding
import Data.Set (fromList)
import Data.Maybe (catMaybes, fromMaybe, isJust, fromJust) -- Added for mkTriple logic, E13 bulk URI, and dataset name handling
import qualified Data.Map.Strict as Map
import Data.List (partition)

-- Crypto and hashing imports for E13
import Crypto.Hash (SHA256, Digest, hash)
import Data.ByteArray.Encoding (convertToBase, Base(Base16))
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text.Encoding as TE (encodeUtf8, decodeUtf8)
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB


-- | Configuration for the RDF generation engine
data EngineConfig = EngineConfig
    { ecGenerateReverseProps :: Bool  -- ^ Whether to generate reverse properties
    , ecMaterializeAlignments :: Bool -- ^ Whether to materialize custom:sameAs alignments
    , ecOutputFormat :: OutputFormat -- ^ The desired output format for RDF
    , ecGenerateE13 :: Bool           -- ^ Whether to generate E13 attribute assignment triples
    , ecDatasetName :: Maybe T.Text   -- ^ Optional dataset name for URI generation
    } deriving (Show, Eq)

-- | Default engine configuration
defaultEngineConfig :: EngineConfig
defaultEngineConfig = EngineConfig
    { ecGenerateReverseProps = False
    , ecMaterializeAlignments = False
    , ecOutputFormat = TTL -- Default to TTL output
    , ecGenerateE13 = False
    , ecDatasetName = Nothing -- Default dataset name to Nothing
    }

-- E13 Predicates
p140_assigned_attribute_to :: Value
p140_assigned_attribute_to = NonPrefixedURI (T.pack "http://www.cidoc-crm.org/cidoc-crm/P140_assigned_attribute_to")

p177_assigned_property_of_type :: Value
p177_assigned_property_of_type = NonPrefixedURI (T.pack "http://www.cidoc-crm.org/cidoc-crm/P177_assigned_property_of_type")

p141_assigned :: Value
p141_assigned = NonPrefixedURI (T.pack "http://www.cidoc-crm.org/cidoc-crm/P141_assigned")

p9i_forms_part_of :: Value
p9i_forms_part_of = NonPrefixedURI (T.pack "http://www.cidoc-crm.org/cidoc-crm/P9i_forms_part_of")

p134_continued :: Value
p134_continued = NonPrefixedURI (T.pack "http://www.cidoc-crm.org/cidoc-crm/P134_continued")

-- SHA256 helper
internalCalculateSha256 :: Text -> Text
internalCalculateSha256 inputText = TE.decodeUtf8 $ convertToBase Base16 (hash (TE.encodeUtf8 inputText) :: Digest SHA256)

-- Calculate E13 subject URI
calculateE13Subject :: Triple -> Maybe Text -> Value
calculateE13Subject triple baseUri =
  let (Triple s p o) = triple -- unexpanded
      -- RDF.valueToBuilder for NTriples needs an expanded value.
      -- It's used within formatTriples *after* expandTriple is called.
      sExpanded = RDF.expandValue baseUri s
      pExpanded = RDF.expandValue baseUri p
      oExpanded = RDF.expandValue baseUri o

      -- Helper to get the string representation for hashing, similar to SPARQL's STR()
      getComponentStringForHash :: Value -> Text
      getComponentStringForHash (NonPrefixedURI t) = t
      getComponentStringForHash (Literal t _)      = t -- STR(literal) returns its lexical form
      -- expandValue ensures PrefixedURI and RdfTypeVal are converted to NonPrefixedURI.
      -- EmptyURI is filtered by mkTriple for s and o. Predicates shouldn't be EmptyURI.
      getComponentStringForHash v                  = error $ "calculateE13Subject: Unexpected value type for E13 hash component: " ++ show v

      sComp = getComponentStringForHash sExpanded
      pComp = getComponentStringForHash pExpanded
      oComp = getComponentStringForHash oExpanded
      
      concatenated = sComp <> pComp <> oComp
      hashHex = internalCalculateSha256 concatenated
  in NonPrefixedURI (T.pack "https://artresearch.net/resource/e13/" <> hashHex)

-- Helper to determine if E13 triples should be generated for a given data triple
shouldGenerateE13ForTriple :: Triple -> Bool
shouldGenerateE13ForTriple (Triple _ p _) =
    case p of
        PrefixedURI text -> text `elem` whitelistedProperties
        _                -> False
  where
    whitelistedProperties = [
      T.pack "crm:P1_is_identified_by",
      T.pack "crm:P14_carried_out_by",
      T.pack "crm:P4_has_time-span",
      T.pack "crm:P50_has_current_keeper",
      T.pack "crm:P2_has_type",
      T.pack "crm:P45_consists_of",
      T.pack "crm:P32_used_general_technique",
      T.pack "crm:P55_has_current_location",
      T.pack "crm:P46_is_composed_of",
      T.pack "crm:P46i_forms_part_of"
      ]

-- Generate initial E13 triples
generateInitialE13Triples :: Value -> Triple -> Value -> [Triple]
generateInitialE13Triples e13Sub (Triple s p o) bulkUriVal = catMaybes [
    mkTriple e13Sub p140_assigned_attribute_to s,
    mkTriple e13Sub p177_assigned_property_of_type p,
    mkTriple e13Sub p141_assigned o,
    mkTriple e13Sub p9i_forms_part_of bulkUriVal
  ]

-- Generate E13 triples for materialized statements
generateMaterializedE13Triples :: Value -> Triple -> Value -> [Triple]
generateMaterializedE13Triples currentE13Sub (Triple s p o) originalE13Sub = catMaybes [
    mkTriple currentE13Sub p140_assigned_attribute_to s,
    mkTriple currentE13Sub p177_assigned_property_of_type p,
    mkTriple currentE13Sub p141_assigned o,
    mkTriple currentE13Sub p134_continued originalE13Sub
  ]

-- | Materializes 'custom:sameAs' and 'custom:objectSameAs' alignments and tracks changes.
-- Returns a list of (currentTriple, Maybe originalTriple)
materializeAlignmentsAndTrackChanges :: [Triple] -> [(Triple, Maybe Triple)]
materializeAlignmentsAndTrackChanges allTriples =
    let sameAsPredicate = PrefixedURI $ T.pack "custom:sameAs"
        objectSameAsPredicate = PrefixedURI $ T.pack "custom:objectSameAs"

        -- Partition into alignment triples and data triples
        (alignmentTriples, dataTriples) = partition
            (\(Triple _ p _) -> p == sameAsPredicate || p == objectSameAsPredicate)
            allTriples

        -- Further partition alignment triples by type
        sameAsTriples = filter (\(Triple _ p _) -> p == sameAsPredicate) alignmentTriples
        objectSameAsTriples = filter (\(Triple _ p _) -> p == objectSameAsPredicate) alignmentTriples

        -- Build replacement maps
        -- Map: original URI -> replacement URI for subjects of 'custom:sameAs'
        sameAsSubjectReplacements = Map.fromList $ map (\(Triple s _ o) -> (s, o)) sameAsTriples
        -- Map: original URI -> replacement URI for subjects of 'custom:objectSameAs'
        objectSameAsSubjectReplacements = Map.fromList $ map (\(Triple s _ o) -> (s, o)) objectSameAsTriples
        -- Universal map: original URI -> replacement URI (used for objects, priority to objectSameAs if overlap)
        universalReplacementMap = Map.union objectSameAsSubjectReplacements sameAsSubjectReplacements

        -- Transform data triples, tracking changes
        transformDataTriple :: Triple -> (Triple, Maybe Triple)
        transformDataTriple originalTriple@(Triple s p o) =
            let newS = case Map.lookup s objectSameAsSubjectReplacements of
                         Just replacementVal -> replacementVal -- Subject replaced if rule is objectSameAs
                         Nothing -> case Map.lookup s sameAsSubjectReplacements of
                                      Just _  -> s -- Subject kept intact if rule is sameAs
                                      Nothing -> s -- Subject not part of any alignment rule's subject, so kept

                newO = fromMaybe o (Map.lookup o universalReplacementMap) -- Object always replaced if a mapping exists in any alignment type

            in if newS /= s || newO /= o
               then (Triple newS p newO, Just originalTriple)
               else (originalTriple, Nothing)

        transformedDataTriples = map transformDataTriple dataTriples
    in
        -- Alignment triples themselves are not transformed by this process, so their 'Maybe OriginalTriple' is Nothing.
        map (\t -> (t, Nothing)) alignmentTriples ++ transformedDataTriples

-- Replaces subjects and objects based on 'custom:sameAs' and 'custom:objectSameAs' links.
-- Alignment triples themselves are not modified but are included in the result.
materializeAlignments :: [Triple] -> [Triple]
materializeAlignments allTriples =
    let sameAsPredicate = PrefixedURI $ T.pack "custom:sameAs"
        objectSameAsPredicate = PrefixedURI $ T.pack "custom:objectSameAs"

        -- Partition into alignment triples and data triples
        (alignmentTriples, dataTriples) = partition
            (\(Triple _ p _) -> p == sameAsPredicate || p == objectSameAsPredicate)
            allTriples
        
        -- Further partition alignment triples by type (though only their subjects and objects matter for maps)
        sameAsTriples = filter (\(Triple _ p _) -> p == sameAsPredicate) alignmentTriples
        objectSameAsTriples = filter (\(Triple _ p _) -> p == objectSameAsPredicate) alignmentTriples

        -- Build replacement maps (same logic as in materializeAlignmentsAndTrackChanges)
        sameAsSubjectReplacements = Map.fromList $ map (\(Triple s _ o) -> (s, o)) sameAsTriples
        objectSameAsSubjectReplacements = Map.fromList $ map (\(Triple s _ o) -> (s, o)) objectSameAsTriples
        universalReplacementMap = Map.union objectSameAsSubjectReplacements sameAsSubjectReplacements

        -- Transform a data triple
        transformDataTriple :: Triple -> Triple
        transformDataTriple (Triple s p o) =
            let newS = case Map.lookup s objectSameAsSubjectReplacements of
                         Just replacementVal -> replacementVal
                         Nothing -> case Map.lookup s sameAsSubjectReplacements of
                                      Just _  -> s
                                      Nothing -> s

                newO = fromMaybe o (Map.lookup o universalReplacementMap)
            in Triple newS p newO

        -- Apply transformation to all data triples
        transformedDataTriples = map transformDataTriple dataTriples
    in
        -- Combine original alignment triples with transformed data triples
        alignmentTriples ++ transformedDataTriples

-- | Process a node with the enhanced PathTree
processNode :: EngineConfig -> XMLNode -> Value -> Text -> Maybe XMLNode -> GenerationContext -> PathTree c -> IO ([Triple], GenerationContext)
processNode _ _ _ _ _ ctx NullTree = return ([], ctx)  -- NullTree produces no triples

-- Case for Node: Interpret XPath relative to current context 'node'
processNode engConfig node subj _domain _parentContext ctx (Node mRelativeXPath mcond mpreproc (prop, (classRef, genG)) mChildren) = do
    let contextNodes = case mRelativeXPath of
            Just relativeXPath -> evaluateXPathExpr node relativeXPath
            Nothing -> [node]

    let validNodes = case mcond of
            Nothing -> contextNodes
            Just condition -> evaluateCondition condition contextNodes

    processValidNodes validNodes ctx
  where
    processValidNodes [] ctx' = return ([], ctx')
    processValidNodes (vn:vns) ctx' = do
        (triples1, ctx1) <- processSingleValidNode vn ctx'
        (triples2, ctx2) <- processValidNodes vns ctx1
        return (triples1 ++ triples2, ctx2)

    processSingleValidNode ctxNode ctx' = do
        processedNodes <- case mpreproc of
            Nothing -> return [ctxNode]
            Just preprocessor -> transform preprocessor ctxNode
        
        processProcessedNodes processedNodes ctx'

    processProcessedNodes [] ctx' = return ([], ctx')
    processProcessedNodes (pn:pns) ctx' = do
        (triples1, ctx1) <- processSingleProcessedNode pn ctx'
        (triples2, ctx2) <- processProcessedNodes pns ctx1
        return (triples1 ++ triples2, ctx2)

    processSingleProcessedNode procNode ctx' = do
        let gen = unwrapGenerator genG
        (objIdForTarget, newCtxForTarget) <- case subj of
            NonPrefixedURI base -> generateId gen procNode base (xpathToByteString' mRelativeXPath) ctx'
            _                   -> generateId gen procNode T.empty (xpathToByteString' mRelativeXPath) ctx'

        let classUriTextForTarget = T.pack (unClassRef classRef)
        let typeTriplesForTarget = case objIdForTarget of
                                Literal _ _ -> []
                                EmptyURI    -> []
                                _           -> case gen of
                                                VocabGen {} -> []
                                                _           -> catMaybes [mkTriple objIdForTarget rdfType (PrefixedURI classUriTextForTarget)]

        (childTriples, finalCtxAfterChildren) <- case mChildren of
            Nothing -> return ([], newCtxForTarget)
            Just children -> processPathComponentsList engConfig children procNode objIdForTarget classUriTextForTarget (Just procNode) newCtxForTarget

        case prop of
            P0 -> return (typeTriplesForTarget ++ childTriples, finalCtxAfterChildren)
            _  -> do
                let predUri = PrefixedURI (propUri prop)
                let baseTriples = catMaybes [mkTriple subj predUri objIdForTarget]
                let reverseTriples = if ecGenerateReverseProps engConfig then
                                        case getInverseOfProperty prop of
                                          Just inverseProp -> catMaybes [mkTriple objIdForTarget (PrefixedURI (propUri inverseProp)) subj]
                                          Nothing          -> []
                                     else []
                return (baseTriples ++ typeTriplesForTarget ++ reverseTriples ++ childTriples, finalCtxAfterChildren)

-- Case for DynamicNode: Interpret XPath relative to current context 'node'
processNode engConfig node subj domain parentContext ctx (DynamicNode mRelativeXPath mcond mpreproc createTree) = do
    let contextNodes = case mRelativeXPath of
            Just relativeXPath -> evaluateXPathExpr node relativeXPath
            Nothing -> maybe [node] (const [node]) parentContext

    let validNodes = case mcond of
            Nothing -> contextNodes
            Just condition -> evaluateCondition condition contextNodes

    processValidNodes validNodes ctx
  where
    processValidNodes [] ctx' = return ([], ctx')
    processValidNodes (vn:vns) ctx' = do
        (triples1, ctx1) <- processSingleValidNode vn ctx'
        (triples2, ctx2) <- processValidNodes vns ctx1
        return (triples1 ++ triples2, ctx2)

    processSingleValidNode n ctx' = do
        processedNodes <- case mpreproc of
            Nothing -> return [n]
            Just preprocessor -> transform preprocessor n
        
        processProcessedNodes processedNodes ctx'

    processProcessedNodes [] ctx' = return ([], ctx')
    processProcessedNodes (pn:pns) ctx' = do
        (triples1, ctx1) <- processNode engConfig pn subj domain (Just pn) ctx' (createTree pn)
        (triples2, ctx2) <- processProcessedNodes pns ctx1
        return (triples1 ++ triples2, ctx2)

-- Case for ContextGroup: Evaluate base path once, then process children relative to results
processNode engConfig node subj domain parentContext ctx (ContextGroup mBaseXPath mCond mPreprocessor children) = do
    let initialContextNodes = case mBaseXPath of
            Just baseXPath -> evaluateXPathExpr node baseXPath
            Nothing -> maybe [node] (const [node]) parentContext

    let conditionedNodes = case mCond of
            Nothing -> initialContextNodes
            Just condition -> evaluateCondition condition initialContextNodes
    
    processGroupBaseNodes conditionedNodes ctx
  where
    processGroupBaseNodes [] ctx' = return ([], ctx')
    processGroupBaseNodes (bn:bns) ctx' = do
        (triples1, ctx1) <- processSingleGroupBaseNode bn ctx'
        (triples2, ctx2) <- processGroupBaseNodes bns ctx1
        return (triples1 ++ triples2, ctx2)

    processSingleGroupBaseNode baseNode currentCtx = do
        nodesForChildren <- case mPreprocessor of
            Nothing -> return [baseNode]
            Just preprocessor -> transform preprocessor baseNode
        
        processNodesForChildren nodesForChildren currentCtx

    processNodesForChildren [] ctx' = return ([], ctx')
    processNodesForChildren (childContextNode:remainingChildContextNodes) ctx' = do
        (triples1, ctx1) <- processPathComponentsList engConfig children childContextNode subj domain (Just childContextNode) ctx'
        (triples2, ctx2) <- processNodesForChildren remainingChildContextNodes ctx1
        return (triples1 ++ triples2, ctx2)

-- | Helper to process a list of PathComponents
processPathComponentsList :: EngineConfig -> [PathComponent c] -> XMLNode -> Value -> Text -> Maybe XMLNode -> GenerationContext -> IO ([Triple], GenerationContext)
processPathComponentsList _ [] _ _ _ _ ctx' = return ([], ctx')
processPathComponentsList engConfig' (pc:pcs) procNode objId domain' parentNodeCtx ctx' = do
    (triples1, ctx1) <- processPathComponent engConfig' pc procNode objId domain' parentNodeCtx ctx'
    (triples2, ctx2) <- processPathComponentsList engConfig' pcs procNode objId domain' parentNodeCtx ctx1
    return (triples1 ++ triples2, ctx2)

-- | Helper to process a single PathComponent
processPathComponent :: EngineConfig -> PathComponent c -> XMLNode -> Value -> Text -> Maybe XMLNode -> GenerationContext -> IO ([Triple], GenerationContext)
processPathComponent engConfig' (StaticTree pt) procNode objId domain' parentNodeCtx ctx' =
    processNode engConfig' procNode objId domain' parentNodeCtx ctx' pt
processPathComponent engConfig' (DynamicFunc fn) procNode objId domain' parentNodeCtx ctx' =
    processNode engConfig' procNode objId domain' parentNodeCtx ctx' (fn procNode)

-- | Helper function to safely convert Maybe XPathExpr to ByteString (used for ID generation context)
xpathToByteString' :: Maybe XPathExpr -> BS.ByteString
xpathToByteString' (Just expr) = xpathToByteString expr
xpathToByteString' Nothing = BS.pack "."  -- Default to current context '.' seems reasonable

-- | Evaluate a mapping
evaluateMapping :: EngineConfig -> Mapping c -> Maybe Text -> XMLNode -> IO [Triple]
evaluateMapping engConfig (Mapping (D classRef rootPath genG) pathTrees) _baseUri root = do -- gen renamed to genG
    let gen = unwrapGenerator genG -- unwrap here
    let rootNodes = evaluateXPathExpr root rootPath
    let initialCtx = makeContext root T.empty (xpathToByteString rootPath)
    (triples, _) <- processRootNodes engConfig rootNodes initialCtx gen -- pass unwrapped gen
    return triples
  where
    processRootNodes :: EngineConfig -> [XMLNode] -> GenerationContext -> Generator -> IO ([Triple], GenerationContext)
    processRootNodes _ [] ctx _ = return ([], ctx)
    processRootNodes engConfig' (rn:rns) ctx gen' = do -- pass unwrapped gen'
        (triples1, ctx1) <- processSingleRootNode engConfig' pathTrees rn ctx gen' -- pass unwrapped gen'
        (triples2, ctx2) <- processRootNodes engConfig' rns ctx1 gen' -- pass unwrapped gen'
        return (triples1 ++ triples2, ctx2)

    processSingleRootNode :: EngineConfig -> [PathTree c] -> XMLNode -> GenerationContext -> Generator -> IO ([Triple], GenerationContext)
    processSingleRootNode engConfig' treeDefs node ctx gen' = do -- pass unwrapped gen'
        (subjId, newCtx) <- generateId gen' node T.empty (xpathToByteString rootPath) ctx -- use unwrapped gen'
        let rootTypeTriples = case subjId of
                Literal _ _ -> []
                EmptyURI    -> []
                _           -> catMaybes [mkTriple subjId rdfType (PrefixedURI (T.pack (unClassRef classRef)))]
        (childTriples, finalCtx) <- processTreeDefsList engConfig' treeDefs node subjId newCtx
        return (rootTypeTriples ++ childTriples, finalCtx)

    processTreeDefsList :: EngineConfig -> [PathTree c] -> XMLNode -> Value -> GenerationContext -> IO ([Triple], GenerationContext)
    processTreeDefsList _ [] _ _ ctx = return ([], ctx)
    processTreeDefsList engConfig' (td:tds) node subjId ctx = do
        (triples1, ctx1) <- processNode engConfig' node subjId T.empty Nothing ctx td
        (triples2, ctx2) <- processTreeDefsList engConfig' tds node subjId ctx1
        return (triples1 ++ triples2, ctx2)

-- | Internal: Process XML string with EngineConfig
processXMLStringWithConfig :: EngineConfig -> Mapping c -> Maybe Text -> BS.ByteString -> IO (Text, Maybe Text)
processXMLStringWithConfig engConfig mapping baseUri content =
    case X.parse content of
        Left err -> error (T.unpack (TE.decodeUtf8 (BS.concat [BS.pack "Failed to parse XML: ", err])))
        Right doc -> do
            rawTriples <- evaluateMapping engConfig mapping baseUri (XMLNode doc 0 Nothing)

            let validDatasetNameProvided = case ecDatasetName engConfig of
                                               Just dsName -> not (T.null dsName)
                                               Nothing     -> False
            
            let e13Active = ecGenerateE13 engConfig &&
                            ecOutputFormat engConfig == NTriples &&
                            ecMaterializeAlignments engConfig &&
                            validDatasetNameProvided

            let bulkActivityUri = if e13Active then
                                      let dsName = fromJust (ecDatasetName engConfig) -- Safe due to e13Active check
                                      in NonPrefixedURI $ T.pack "https://artresearch.net/resource/" <> dsName <> T.pack "/cataloging_activity"
                                  else
                                      -- Fallback or default if E13 is not active or dataset name is not valid for it
                                      NonPrefixedURI $ fromMaybe T.empty baseUri <> T.pack "cataloging_activity"

            (mainTriplesToFormat, maybeE13TriplesToFormat) <- if e13Active then do
                -- Step 1: Filter raw triples and calculate E13 subjects only for qualified ones
                let qualifiedRawTriples = filter shouldGenerateE13ForTriple rawTriples
                let originalE13SubjectsMap = Map.fromList $ map (\t -> (t, calculateE13Subject t baseUri)) qualifiedRawTriples

                -- Step 2: Generate initial E13 triples only for qualified raw triples
                let initialE13s = concatMap (\t ->
                        -- t here is from qualifiedRawTriples, so it should be in the map.
                        case Map.lookup t originalE13SubjectsMap of
                            Just e13Sub -> generateInitialE13Triples e13Sub t bulkActivityUri
                            Nothing -> [] -- This case should ideally not be hit if logic is correct.
                        ) qualifiedRawTriples

                -- Step 3: Materialize alignments and track changes
                let materializedWithHistory = materializeAlignmentsAndTrackChanges rawTriples

                let (dataTriples, materializedE13s) = foldr
                        (\(currentTriple, mOriginalTriple) (accData, accE13) ->
                            let newE13Triples = if shouldGenerateE13ForTriple currentTriple then
                                                    case mOriginalTriple of
                                                        Just originalTriple -> -- This triple was changed by materialization
                                                            -- And its original form was qualified for E13
                                                            case Map.lookup originalTriple originalE13SubjectsMap of
                                                                Just originalE13Sub ->
                                                                    let currentE13Sub = calculateE13Subject currentTriple baseUri
                                                                    in generateMaterializedE13Triples currentE13Sub currentTriple originalE13Sub
                                                                Nothing -> [] -- Original was not qualified, so no "continued" E13
                                                        Nothing -> [] -- This triple was not changed (or is a sameAs triple itself)
                                                else [] -- Current (materialized) triple does not qualify for E13
                            in (currentTriple : accData, newE13Triples ++ accE13)
                        )
                        ([], [])
                        materializedWithHistory
                
                return (dataTriples, Just (initialE13s ++ materializedE13s))

            else if ecMaterializeAlignments engConfig then
                return (materializeAlignments rawTriples, Nothing)
            else
                return (rawTriples, Nothing)

            let mainOutputText = formatTriples (ecOutputFormat engConfig) baseUri mainTriplesToFormat
            let maybeE13OutputText = case maybeE13TriplesToFormat of
                                        Just e13s -> Just $ formatTriples (ecOutputFormat engConfig) baseUri e13s
                                        Nothing   -> Nothing
            
            return (mainOutputText, maybeE13OutputText)

-- | Public: Process XML string (uses defaultEngineConfig)
processXMLString :: Mapping c -> Maybe Text -> BS.ByteString -> IO Text
processXMLString mapping baseUri content = do
    (mainOutputText, _) <- processXMLStringWithConfig defaultEngineConfig mapping baseUri content
    return mainOutputText

-- | Internal: Map XML to RDF and return a set of triples with EngineConfig
processXMLStringAsSetWithConfig :: EngineConfig -> Mapping c -> Maybe Text -> BS.ByteString -> IO (ShowableSet Triple)
processXMLStringAsSetWithConfig engConfig mapping baseUri content =
    case X.parse content of
        Left err -> error (T.unpack (TE.decodeUtf8 (BS.concat [BS.pack "Failed to parse XML: ", err])))
        Right doc -> do
            triples <- evaluateMapping engConfig mapping baseUri (XMLNode doc 0 Nothing)
            let processedTriples = if ecMaterializeAlignments engConfig
                                   then materializeAlignments triples
                                   else triples
            return $ ShowableSet $ fromList processedTriples

-- | Public: Map XML to RDF and return a set of triples (uses defaultEngineConfig)
processXMLStringAsSet :: Mapping c -> Maybe Text -> BS.ByteString -> IO (ShowableSet Triple)
processXMLStringAsSet = processXMLStringAsSetWithConfig defaultEngineConfig


-- | Process XML file
processXMLFile :: EngineConfig -> Mapping c -> Maybe Text -> FilePath -> FilePath -> Maybe FilePath -> IO ()
processXMLFile engConfig mapping baseUri inputFile mainOutputFile maybeE13OutputFile = do
    content <- BS.readFile inputFile
    (mainOutputText, mE13OutputText) <- processXMLStringWithConfig engConfig mapping baseUri content
    
    TIO.writeFile mainOutputFile mainOutputText
    
    case (mE13OutputText, maybeE13OutputFile) of
        (Just e13Text, Just e13File) -> TIO.writeFile e13File e13Text
        _                            -> return () -- Do nothing if no E13 text or no E13 file path

-- | RDF type predicate
rdfType :: Value
rdfType = RdfTypeVal -- Use the new RdfTypeVal constructor

-- | Convert XPath expression to ByteString (UTF-8 encoded)
xpathToByteString :: XPathExpr -> BS.ByteString
xpathToByteString = TE.encodeUtf8 . T.pack . show

-- | Helper function to create a Triple only if subject and object are not EmptyURI
mkTriple :: Value -> Value -> Value -> Maybe Triple
mkTriple s _ o | s == EmptyURI || o == EmptyURI = Nothing
mkTriple s p o                                = Just (Triple s p o)
