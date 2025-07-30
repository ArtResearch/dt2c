{-# LANGUAGE NoOverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DeriveDataTypeable #-}

module RDF (
    Value(..),
    Triple(..),
    formatTriple,
    formatTriples,
    stringToTriple,
    OutputFormat(..), -- Export OutputFormat
    expandValue,      -- Export expandValue
    valueToBuilder    -- Export valueToBuilder
) where

import Data.Data (Data)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import Language.Haskell.TH.Syntax (Lift)
import qualified Data.Map.Strict as Map

-- | Output format for RDF serialization
data OutputFormat = TTL | NTriples
    deriving (Show, Eq, Read)

data Value
    = PrefixedURI Text     -- ^ URI with prefix like crm:P1
    | NonPrefixedURI Text  -- ^ URI that needs to be wrapped in <>
    | Literal Text (Maybe Value)    -- ^ Literal with value and optional datatype (datatype is now Maybe Value)
    | RdfTypeVal                  -- ^ Represents rdf:type, typically serialized as "a" in TTL
    | EmptyURI                     -- ^ Represents an empty or unresolvable URI
    deriving (Eq, Ord, Lift, Data)

data Triple = Triple Value Value Value deriving (Eq, Ord, Lift, Data)

-- Helper function for escaping literals, will be used by valueToBuilder and expansion logic
escapeLiteralChars :: Text -> Text
escapeLiteralChars = T.replace (T.pack "\t") (T.pack "\\t")
                   . T.replace (T.pack "\r") (T.pack "\\r")
                   . T.replace (T.pack "\n") (T.pack "\\n")
                   . T.replace (T.pack "\"") (T.pack "\\\"")
                   . T.replace (T.pack "\\") (T.pack "\\\\")  -- This must be applied first to the raw string

-- This Show instance will represent the TTL-like format for now.
-- The main serialization will be handled by formatTriple/formatTriples.
valueToBuilderTTL :: Value -> TB.Builder
valueToBuilderTTL (PrefixedURI uri) = TB.fromText uri
valueToBuilderTTL (NonPrefixedURI uri) = TB.singleton '<' <> TB.fromText uri <> TB.singleton '>'
valueToBuilderTTL (Literal val mValueDtype) =
    TB.singleton '"' <> TB.fromText (escapeLiteralChars val) <> TB.singleton '"' <>
    case mValueDtype of
        Just (PrefixedURI dtype) -> TB.fromText (T.pack "^^") <> TB.fromText dtype
        Just (NonPrefixedURI dtype) -> TB.fromText (T.pack "^^<") <> TB.fromText dtype <> TB.fromText (T.pack ">")
        -- Just (Literal _ _) -> error "Literal datatype cannot be a Literal itself" -- Or handle as error
        -- Just RdfTypeVal -> error "Literal datatype cannot be RdfTypeVal" -- Or handle as error
        -- Just EmptyURI -> error "Literal datatype cannot be EmptyURI" -- Or handle as error
        Nothing -> mempty
        _ -> error "Unsupported Value as literal datatype in TTL serialization" -- Catch-all for RdfTypeVal, Literal, EmptyURI if not explicitly handled
valueToBuilderTTL RdfTypeVal = TB.fromText (T.pack "a")
valueToBuilderTTL EmptyURI = error "valueToBuilderTTL: EmptyURI should not be rendered directly."

instance Show Value where
    show = TL.unpack . TB.toLazyText . valueToBuilderTTL

tripleToBuilderTTL :: Triple -> TB.Builder
tripleToBuilderTTL (Triple s p o) =
    valueToBuilderTTL s <> TB.singleton ' ' <>
    valueToBuilderTTL p <> TB.singleton ' ' <>
    valueToBuilderTTL o <> TB.fromText (T.pack " .")

instance Show Triple where
    show = TL.unpack . TB.toLazyText . tripleToBuilderTTL

-- | Main function to build a Value into a Text.Builder based on OutputFormat.
-- For NTriples, it expects values to have been pre-expanded by expandValue.
valueToBuilder :: OutputFormat -> Value -> TB.Builder
valueToBuilder TTL val = valueToBuilderTTL val -- Use existing TTL builder
valueToBuilder NTriples val = case val of
    NonPrefixedURI uri   -> TB.singleton '<' <> TB.fromText uri <> TB.singleton '>'
    Literal text mDtype ->
        TB.singleton '"' <> TB.fromText text <> TB.singleton '"' <> -- Text is assumed pre-escaped by expandValue
        case mDtype of
            Just (NonPrefixedURI dtypeIRI) -> TB.fromText (T.pack "^^<") <> TB.fromText dtypeIRI <> TB.fromText (T.pack ">")
            Nothing -> mempty
            Just otherDtype -> error $ "Invalid datatype for N-Triples literal: " ++ show otherDtype ++ ". Expected expanded NonPrefixedURI."
    RdfTypeVal -> error "RdfTypeVal should have been expanded to NonPrefixedURI for NTriples." -- Should not happen if pre-expanded
    PrefixedURI _ -> error "PrefixedURI should have been expanded for NTriples." -- Should not happen
    EmptyURI -> error "valueToBuilder (NTriples): EmptyURI should not be rendered directly."

-- | Main function to build a Triple into a Text.Builder.
tripleToBuilder :: OutputFormat -> Triple -> TB.Builder
tripleToBuilder format (Triple s p o) =
    valueToBuilder format s <> TB.singleton ' ' <>
    valueToBuilder format p <> TB.singleton ' ' <>
    valueToBuilder format o <> TB.fromText (T.pack " .")

-- | Formats a list of Triples into a single Text block.
formatTriples :: OutputFormat -> Maybe Text -> [Triple] -> Text
formatTriples format baseUri triplesToFormat =
    let maybeExpandedTriples = case format of
            NTriples -> map (expandTriple baseUri) triplesToFormat
            TTL      -> triplesToFormat -- No expansion needed for TTL
    in TL.toStrict $ TB.toLazyText $ mconcat $
        map (\t -> tripleToBuilder format t <> TB.singleton '\n') maybeExpandedTriples

-- | Formats a single Triple into its Text representation.
formatTriple :: OutputFormat -> Maybe Text -> Triple -> Text
formatTriple format baseUri tripleToFormat =
    let maybeExpandedTriple = case format of
            NTriples -> expandTriple baseUri tripleToFormat
            TTL      -> tripleToFormat
    in TL.toStrict $ TB.toLazyText $ tripleToBuilder format maybeExpandedTriple

-- Prefix map for URI expansion
prefixMap :: Map.Map Text Text
prefixMap = Map.fromList
    [ (T.pack "crm", T.pack "http://www.cidoc-crm.org/cidoc-crm/")
    , (T.pack "xsd", T.pack "http://www.w3.org/2001/XMLSchema#")
    , (T.pack "aat", T.pack "http://vocab.getty.edu/aat/")
    , (T.pack "custom", T.pack "https://artresearch.net/custom/")
    , (T.pack "pharos-meta", T.pack "https://artresearch.net/resource/pharos/vocab/meta/")
    , (T.pack "rdf", T.pack "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
    , (T.pack "image-api", T.pack "https://artresearch.net/image-api/image-api/")
    , (T.pack "skos", T.pack "http://www.w3.org/2004/02/skos/core#")
    ]

-- Full URI for rdf:type
rdfTypeURI :: Text
rdfTypeURI = T.pack "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"

-- | Expands a Value to its full N-Triples representation if applicable.
-- For N-Triples, all URIs become NonPrefixedURI, and literals are escaped.
-- The baseUri is used to resolve relative NonPrefixedURIs.
expandValue :: Maybe Text -> Value -> Value
expandValue baseUri val = case val of
    PrefixedURI pUri ->
        if pUri == T.pack "a" then NonPrefixedURI rdfTypeURI
        else
            let (prefix, T.drop 1 -> localName) = T.breakOn (T.pack ":") pUri
            in case Map.lookup prefix prefixMap of
                Just namespace -> NonPrefixedURI (namespace <> localName)
                Nothing        -> error $ "Prefix not found in map for N-Triples expansion: " ++ T.unpack prefix
    NonPrefixedURI uri ->
        if T.pack "http://" `T.isPrefixOf` uri || T.pack "https://" `T.isPrefixOf` uri then
            val -- Already a full URI
        else -- Relative URI
            case baseUri of
                Just b  -> NonPrefixedURI (b <> uri)
                Nothing -> error $ "Cannot resolve relative URI for N-Triples without base URI: " ++ T.unpack uri
    Literal text mDatatypeValue ->
        let escapedText = escapeLiteralChars text
            expandedDatatype = case mDatatypeValue of
                Just dtVal -> Just (expandValue baseUri dtVal) -- Recursively expand datatype URI
                Nothing    -> Nothing
        in Literal escapedText expandedDatatype
    RdfTypeVal -> NonPrefixedURI rdfTypeURI
    EmptyURI   -> EmptyURI -- Remains EmptyURI

-- | Expands all components of a Triple for N-Triples serialization.
expandTriple :: Maybe Text -> Triple -> Triple
expandTriple baseUri (Triple s p o) =
    Triple (expandValue baseUri s) (expandValue baseUri p) (expandValue baseUri o)

-- Helper function to parse a Value from the start of a Text input.
-- Returns the parsed Value and the remaining Text, or an error message on failure.
-- Note: This parseValue is for the stringToTriple function which expects TTL-like input.
-- The literal datatype parsing here will assume it's a simple Text representation
-- that can be wrapped in PrefixedURI or NonPrefixedURI as needed.
-- A more robust parser would be needed if complex nested Values were expected in datatype strings.
parseValue :: Text -> Either String (Value, Text)
parseValue input
    -- NonPrefixedURI: <...>
    | Just ('<', t) <- T.uncons input =
        let (uri, rest) = T.breakOn (T.pack ">") t
        in if T.null rest then Left "Missing closing '>' for NonPrefixedURI"
           else Right (NonPrefixedURI uri, T.tail rest) -- Consume '>'
    -- Literal: "..." or "..."^^...
    | Just ('"', t) <- T.uncons input =
        let (val, rest1) = T.breakOn (T.pack "\"") t -- Find closing quote
        in if T.null rest1 then Left "Missing closing '\"' for Literal"
           else let restAfterQuote = T.tail rest1 -- Consume '"'
                in if T.isPrefixOf (T.pack "^^") restAfterQuote then
                       let dtypePartStartingAfterCarats = T.drop 2 restAfterQuote -- Consumes "^^"
                       in case T.uncons dtypePartStartingAfterCarats of
                            Just ('<', afterOpeningBracket) -> -- Datatype is <IRI>
                                let (dtypeUri, restAfterClosingBracket) = T.breakOn (T.pack ">") afterOpeningBracket
                                in if T.null restAfterClosingBracket then Left "Missing closing '>' for Literal datatype URI"
                                   else Right (Literal val (Just (NonPrefixedURI dtypeUri)), T.stripStart (T.tail restAfterClosingBracket)) -- Consume '>' and strip leading spaces from rest
                            _ -> -- Datatype is PrefixedName or something else; parse up to space
                                let (dtype, rest2) = T.breakOn (T.pack " ") dtypePartStartingAfterCarats
                                in if T.null dtype
                                   then Left "Empty datatype for Literal after ^^"
                                   else Right (Literal val (Just (PrefixedURI dtype)), T.stripStart rest2)
                   else Right (Literal val Nothing, restAfterQuote)
    -- PrefixedURI or RdfTypeVal ("a"):
    -- Consumes up to the next space or the end of the string.
    -- If it's "a" and not part of a longer token, it's RdfTypeVal.
    | not (T.null input) =
        let (token, restOfInputAfterToken) = T.breakOn (T.pack " ") input
        in if T.null token
           then Left "Empty input for PrefixedURI/RdfTypeVal"
           else if token == T.pack "a"
                then Right (RdfTypeVal, T.stripStart restOfInputAfterToken)
                else Right (PrefixedURI token, T.stripStart restOfInputAfterToken)
    | otherwise = Left "Empty input to parseValue"

-- | Parses a Triple from its string representation (as generated by 'show').
-- Returns Left with an error message if parsing fails or if there's trailing input.
-- Example: stringToTriple "crm:E21_Person crm:P1_is_identified_by <http://example.org/id/123> ."
stringToTriple :: String -> Either String Triple
stringToTriple str = do
    let input = T.strip $ T.pack str -- Start with trimmed Text
    (s, T.stripStart -> rest1) <- parseValue input `wrapError` "subject"
    (p, T.stripStart -> rest2) <- parseValue rest1 `wrapError` "predicate"
    -- The rest should be the object followed by " ."
    let potentialObjectPart = T.stripEnd rest2 -- Remove trailing spaces first
    if not (T.isSuffixOf (T.pack " .") potentialObjectPart)
        then Left $ "Triple does not end with ' .': " ++ T.unpack potentialObjectPart -- Use T.unpack
    else do
        let objectPart = T.dropEnd 2 potentialObjectPart -- Remove " ."
        (o, T.strip -> rest3) <- parseValue objectPart `wrapError` "object"
        -- After parsing the object, there should be nothing left
        if not (T.null rest3)
            then Left $ "Trailing characters after object: " ++ T.unpack rest3 -- Use T.unpack
        else Right (Triple s p o)
  where
    -- Helper to add context to parsing errors
    wrapError :: Either String a -> String -> Either String a
    wrapError (Left err) context = Left $ "Failed to parse " ++ context ++ ": " ++ err
    wrapError (Right val) _      = Right val
