{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Generators (
    Generator(..),
    VocabGenerator(..),
    IsGenerator(..),
    GenerationContext(..),
    generateId,
    contextGen,
    applyContextGen,
    constUri,
    constXPathUri,
    uri,
    literal,
    literalFn,
    typedLiteral,
    uriFn,
    uriMultiExprFn,
    literalMultiExprFn, -- Added
    dateTime,
    makeContext,
    makeUrlFriendly,
    i,
    relativeUri,
    templateUri,
    relativeUriT,
    named,
    namedUri,
    vocab,
    IsTemplateValue(..),
    TemplateValue(..),
    DateBound(..),
    DateType(..),
    ToDateTime(..),
    NamedUriNamespace(..)
) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Char8 as BS
import CidocCRM (Class_(..), ClassRef(..), pattern LiteralType)
import Data.Char (toLower, isAlphaNum, isSpace) -- Added isSpace back
import XML ( XPathExpr, XMLNode(xmlNodeIndex), evaluateXPathExpr, nodeText) -- Added xmlNodeIndex to import
import RDF (Value( EmptyURI, Literal, NonPrefixedURI, PrefixedURI ))
import Data.Text.ICU (nfd)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Kind (Type)

-- | Newtype wrapper for vocabulary generators
newtype VocabGenerator = VocabGenerator { unVocabGenerator :: Generator }

-- | Typeclass to abstract over Generator and VocabGenerator
class IsGenerator g where
  unwrapGenerator :: g -> Generator

-- | Instance for Generator itself
instance IsGenerator Generator where
  unwrapGenerator = id

-- | Instance for VocabGenerator
instance IsGenerator VocabGenerator where
  unwrapGenerator = unVocabGenerator

-- | Type of date input
data DateType = Year | FullDate | DateTime
  deriving (Show, Eq)

-- | Date bound for datetime conversion
data DateBound = Upper | Lower
  deriving (Show, Eq)

-- | Context available during generation
data GenerationContext = GenerationContext
  { contextNode :: BS.ByteString  -- ^ Current xpath context
  , nodeIndex :: Int             -- ^ Index of current node in sequence
  , domainUri :: Text           -- ^ URI from the domain
  , namedUris :: Map Text Value  -- ^ Map of named URIs
  } deriving (Show)

-- | Template value source
data TemplateValue
  = ConstValue BS.ByteString       -- ^ Static string value
  | XPathValue XPathExpr          -- ^ Value from xpath expression
  | IndexValue

-- | Generator with access to context
-- Forward declaration for NamedUriNamespace
class NamedUriNamespace ns where
  data NamedUri ns :: Type
  uriToText :: NamedUri ns -> Text

-- | Show instance for NamedUri
instance NamedUriNamespace ns => Show (NamedUri ns) where
  show = T.unpack . uriToText

data Generator
  = UUIDGen
  | ConstantURIGen Text
  | ConstantXPathURIGen XPathExpr
  | forall a. (IsTemplateValue a) => LiteralGen a (Maybe Value)
  | forall val a. (IsTemplateValue val, ToDateTime a) => DateTimeValueGen val a  -- ^ DateTime generator, can be const or xpath
  | forall a. (IsTemplateValue a) => TemplateGen BS.ByteString [(BS.ByteString, a)]
  | ContextGen BS.ByteString      -- ^ Generator with full context access
  | RelativeUriGen BS.ByteString  -- ^ Append to domain URI  
  | IndexedUriGen BS.ByteString   -- ^ URI with index
  | forall a. (IsTemplateValue a) => RelativeUriTGen BS.ByteString [(BS.ByteString, a)]  -- ^ Template-based relative URI
  | NamedUriGen Text              -- ^ Reference a named URI by name (legacy)
  | NamedGen Text Generator       -- ^ Name the result of another generator (legacy)
  | forall ns. NamedUriNamespace ns => TypedNamedUriGen (NamedUri ns)  -- ^ Type-safe reference to a named URI
  | forall ns. NamedUriNamespace ns => TypedNamedGen (NamedUri ns) Generator  -- ^ Type-safe naming of a generator result
  | VocabGen { vocabBase :: Text, vocabLocalName :: Text, vocabValue :: Value } -- ^ Generator for vocabulary items (always of type E55_Type), pre-calculates Value
  | LiteralNodeFnGen XPathExpr (Text -> Text) -- ^ Literal from function applied to node
  | UriNodeFnGen XPathExpr (Text -> Text)     -- ^ URI from function applied to node
  | UriMultiNodeFnGen [XPathExpr] ([Text] -> Text) -- ^ URI from function applied to multiple nodes' texts
  | LiteralMultiNodeFnGen [XPathExpr] ([Text] -> Text) -- ^ Literal from function applied to multiple nodes' texts

class IsTemplateValue a where
    toTemplateValue :: a -> TemplateValue

instance IsTemplateValue TemplateValue where
    toTemplateValue = id

instance IsTemplateValue BS.ByteString where
    toTemplateValue = ConstValue

instance IsTemplateValue T.Text where
    toTemplateValue = ConstValue . TE.encodeUtf8

instance IsTemplateValue XPathExpr where
    toTemplateValue = XPathValue

i :: TemplateValue
i = IndexValue

-- | Create a context-aware generator
contextGen :: (GenerationContext -> Text) -> BS.ByteString -> Generator
contextGen _ = ContextGen

-- | Apply the context generator
applyContextGen :: Generator -> GenerationContext -> Maybe Text
applyContextGen (ContextGen _) _ = Nothing  -- Default implementation
applyContextGen _ _ = Nothing

-- | Create generation context for a node
makeContext :: XMLNode -> Text -> BS.ByteString -> GenerationContext
makeContext node domainUriVal xpath = GenerationContext
    { contextNode = xpath
    , nodeIndex = xmlNodeIndex node
    , domainUri = domainUriVal
    , namedUris = Map.empty
    }

-- | Generate an identifier based on the generator type
generateId :: Generator -> XMLNode -> Text -> BS.ByteString -> GenerationContext -> IO (Value, GenerationContext)
generateId UUIDGen _ _ _ ctx = pure (NonPrefixedURI (T.pack "uuid"), ctx) -- TODO: implement proper UUID generation
generateId (LiteralGen val dataType) node _ _ ctx =
    let templateVal = toTemplateValue val
    in case templateVal of
        ConstValue bs -> pure (Literal (TE.decodeUtf8 bs) dataType, ctx)
        XPathValue xp -> case evaluateXPathExpr node xp of
            []    -> pure (Literal (T.pack "unknown") dataType, ctx)
            (n:_) -> pure (Literal (nodeText n) dataType, ctx)
        IndexValue  -> pure (Literal (T.pack $ show (xmlNodeIndex node + 1)) dataType, ctx)
generateId (DateTimeValueGen val params) node _ _ ctx =
    let templateVal = toTemplateValue val
        xsdDateTime = Just (PrefixedURI $ T.pack "xsd:dateTime") -- Datatype is now a Value
    in case templateVal of
        ConstValue bs -> pure (Literal (toDateTimeValue (TE.decodeUtf8 bs) params) xsdDateTime, ctx)
        XPathValue xp -> case evaluateXPathExpr node xp of
            []    -> pure (Literal (T.pack "unknown") xsdDateTime, ctx)
            (n:_) -> pure (Literal (toDateTimeValue (nodeText n) params) xsdDateTime, ctx)
        IndexValue  -> error "DateTimeValueGen does not support IndexValue. Please use XPathValue or ConstValue."
generateId (TemplateGen template values) node _ _ ctx = do
    -- Always use the node's index + 1 for template substitution
    -- This ensures each node in a sequence gets its own index
    let nodeIdx = xmlNodeIndex node + 1
        -- Create a context with the node's index for template substitution
        idxCtx = ctx { nodeIndex = nodeIdx }
        replaced = foldl (replaceTemplateVar node idxCtx) template values
        -- Don't update the context's index - it should be maintained separately
        -- from the node's index
    pure (mkUriValue (TE.decodeUtf8 replaced), ctx)
  where
    replaceTemplateVar :: IsTemplateValue a => XMLNode -> GenerationContext -> BS.ByteString -> (BS.ByteString, a) -> BS.ByteString
    replaceTemplateVar currentNode ctx' tmpl (key, value') =
        let value = toTemplateValue value'
            placeholder = BS.concat [BS.pack "{", key, BS.pack "}"]
            replacement = case value of
                ConstValue v -> TE.encodeUtf8 $ makeUrlFriendly $ TE.decodeUtf8 v
                XPathValue xp -> case evaluateXPathExpr currentNode xp of
                    [] -> BS.empty
                    (n:_) -> TE.encodeUtf8 $ makeUrlFriendly $ nodeText n
                IndexValue -> BS.pack $ show $ nodeIndex ctx' + 1
        in bsReplace placeholder replacement tmpl

    bsReplace :: BS.ByteString -> BS.ByteString -> BS.ByteString -> BS.ByteString
    bsReplace old new input =
        let parts = BS.breakSubstring old input
        in case parts of
            (before, after)
                | BS.null after -> before
                | otherwise -> BS.concat [before, new, bsReplace old new (BS.drop (BS.length old) after)]
generateId (ContextGen _) _ _ _ ctx =
    pure (mkUriValue (T.concat [T.pack "context-", T.pack $ show $ nodeIndex ctx + 1]), ctx)
generateId (RelativeUriGen suffix) _ domain _ ctx =
    pure (mkUriValue (domain <> TE.decodeUtf8 suffix), ctx)
generateId (IndexedUriGen prefix) node _ _ ctx =
    pure (mkUriValue (TE.decodeUtf8 prefix <> T.pack (show $ xmlNodeIndex node + 1)), ctx)
generateId (ConstantURIGen uriText) _ _ _ ctx =
    pure (mkUriValue uriText, ctx)
generateId (VocabGen {vocabValue = value}) _ _ _ ctx =
    pure (value, ctx) -- Directly return the pre-calculated Value
generateId (ConstantXPathURIGen xpath) node _ _ ctx =
    case evaluateXPathExpr node xpath of
        [] -> pure (EmptyURI, ctx)
        (n:_) -> pure (mkUriValue (nodeText n), ctx)
generateId (RelativeUriTGen template values) node domain _ ctx = do
    -- Always use the node's index + 1 for template substitution
    -- This ensures each node in a sequence gets its own index
    let nodeIdx = xmlNodeIndex node + 1
        -- Create a context with the node's index for template substitution
        idxCtx = ctx { nodeIndex = nodeIdx }
        replaced = foldl (replaceTemplateVar node idxCtx) template values
        -- Don't update the context's index - it should be maintained separately
        -- from the node's index
    pure (mkUriValue (domain <> TE.decodeUtf8 replaced), ctx)
  where
    replaceTemplateVar :: IsTemplateValue a => XMLNode -> GenerationContext -> BS.ByteString -> (BS.ByteString, a) -> BS.ByteString
    replaceTemplateVar currentNode ctx' tmpl (key, value') =
        let value = toTemplateValue value'
            placeholder = BS.concat [BS.pack "{", key, BS.pack "}"]
            replacement = case value of
                ConstValue v -> TE.encodeUtf8 $ makeUrlFriendly $ TE.decodeUtf8 v
                XPathValue xp -> case evaluateXPathExpr currentNode xp of
                    [] -> BS.empty
                    (n:_) -> TE.encodeUtf8 $ makeUrlFriendly $ nodeText n
                IndexValue -> BS.pack $ show $ nodeIndex ctx'
        in bsReplace placeholder replacement tmpl

    bsReplace :: BS.ByteString -> BS.ByteString -> BS.ByteString -> BS.ByteString
    bsReplace old new input =
        let parts = BS.breakSubstring old input
        in case parts of
            (before, after)
                | BS.null after -> before
                | otherwise -> BS.concat [before, new, bsReplace old new (BS.drop (BS.length old) after)]
generateId (NamedUriGen name) _ _ _ ctx =
    case Map.lookup name (namedUris ctx) of
        Just value -> pure (value, ctx)
        Nothing -> pure (EmptyURI, ctx)
generateId (NamedGen name gen) node domain xpath ctx = do
    (value, ctx') <- generateId gen node domain xpath ctx
    let updatedCtx = ctx' { namedUris = Map.insert name value (namedUris ctx') }
    pure (value, updatedCtx)
generateId (TypedNamedUriGen namedUriVal) _ _ _ ctx =
    case Map.lookup (uriToText namedUriVal) (namedUris ctx) of
        Just value -> pure (value, ctx)
        Nothing -> pure (EmptyURI, ctx)
generateId (TypedNamedGen namedUriVal gen) node domain xpath ctx = do
    (value, ctx') <- generateId gen node domain xpath ctx
    let updatedCtx = ctx' { namedUris = Map.insert (uriToText namedUriVal) value (namedUris ctx') }
    pure (value, updatedCtx)
generateId (LiteralNodeFnGen xpath fn) node _ _ ctx =
    case evaluateXPathExpr node xpath of
        []    -> pure (Literal T.empty Nothing, ctx) -- Return empty literal if node not found
        (n:_) -> pure (Literal (fn $ nodeText n) Nothing, ctx)
generateId (UriNodeFnGen xpath fn) node _ _ ctx =
    case evaluateXPathExpr node xpath of
        []    -> pure (EmptyURI, ctx)
        (n:_) -> pure (mkUriValue (fn (nodeText n)), ctx)
generateId (UriMultiNodeFnGen xpaths fn) node _ _ ctx =
    let evaluatedTexts = map (extractText . evaluateXPathExpr node) xpaths
    in pure (mkUriValue (fn evaluatedTexts), ctx)
  where
    extractText :: [XMLNode] -> Text
    extractText []    = T.empty -- Default to empty string if XPath finds no node
    extractText (n:_) = nodeText n

generateId (LiteralMultiNodeFnGen xpaths fn) node _ _ ctx =
    let evaluatedTexts = map (extractText . evaluateXPathExpr node) xpaths
    in pure (Literal (fn evaluatedTexts) Nothing, ctx)
  where
    extractText :: [XMLNode] -> Text
    extractText []    = T.empty -- Default to empty string if XPath finds no node
    extractText (n:_) = nodeText n

-- Helper to convert Text to a URI Value, handling empty strings
mkUriValue :: Text -> Value
mkUriValue uriText
  | T.null uriText || T.all Data.Char.isSpace uriText = EmptyURI
  | T.count (T.pack ":") uriText == 1 && not (T.isInfixOf (T.singleton '/') uriText) = PrefixedURI uriText
  | otherwise = NonPrefixedURI uriText

-- Helper functions for creating generators
constUri :: Text -> Generator
constUri = ConstantURIGen

constXPathUri :: XPathExpr -> Generator
constXPathUri xPath = uriFn xPath id

uri :: XPathExpr -> Generator
uri = ConstantXPathURIGen

literal :: IsTemplateValue a => a -> (ClassRef 'Literal_, Generator)
literal val = (LiteralType, LiteralGen val Nothing)

-- | Create a literal generator with a specific data type
-- | This is useful for specifying the datatype of the literal, e.g., xsd:integer, etc.
typedLiteral :: IsTemplateValue a => a -> Value -> (ClassRef 'Literal_, Generator)
typedLiteral val dataType = (LiteralType, LiteralGen val (Just dataType))

-- | Create a literal generator applying a function to the selected node
literalFn :: XPathExpr -> (Text -> Text) -> (ClassRef 'Literal_, Generator)
literalFn path fn = (LiteralType, LiteralNodeFnGen path fn)

-- | Create a URI generator applying a function to the selected node's text content
uriFn :: XPathExpr -> (Text -> Text) -> Generator
uriFn = UriNodeFnGen

-- | Create a URI generator applying a function to the text content of selected nodes
uriMultiExprFn :: [XPathExpr] -> ([Text] -> Text) -> Generator
uriMultiExprFn = UriMultiNodeFnGen

-- | Create a literal generator applying a function to the text content of selected nodes
literalMultiExprFn :: [XPathExpr] -> ([Text] -> Text) -> (ClassRef 'Literal_, Generator)
literalMultiExprFn xpaths fn = (LiteralType, LiteralMultiNodeFnGen xpaths fn)

-- | Create a named generator
-- | Prefer typesafe version, see NamedUris
named :: Text -> Generator -> Generator
named = NamedGen

-- | Reference a named URI
-- | Prefer typesafe version, see NamedUris
namedUri :: Text -> Generator
namedUri = NamedUriGen

-- | Typeclass for all datetime handling
class ToDateTime a where
    toDateTimeValue :: Text -> a -> Text

instance ToDateTime DateType where
    toDateTimeValue str DateTime = str
    toDateTimeValue _ _ = error "Only DateTime type is allowed without bounds"

instance ToDateTime (DateType, DateBound) where
    toDateTimeValue str (Year, Upper) = str <> T.pack "-12-31T23:59:59Z"
    toDateTimeValue str (Year, Lower) = str <> T.pack "-01-01T00:00:00Z"
    toDateTimeValue str (FullDate, Upper) = str <> T.pack "T23:59:59Z"
    toDateTimeValue str (FullDate, Lower) = str <> T.pack "T00:00:00Z"
    toDateTimeValue str (DateTime, _) = str

-- | Create a datetime generator that handles both bounded and unbounded cases
dateTime :: (IsTemplateValue val, ToDateTime a) => val -> a -> (ClassRef 'Literal_, Generator)
dateTime val params = (LiteralType, DateTimeValueGen val params)

-- | Create a relative URI generator that appends the given suffix to the domain URI
relativeUri :: BS.ByteString -> Generator
relativeUri = RelativeUriGen

-- | Create a template URI generator with the given template and values
templateUri :: (IsTemplateValue a) => BS.ByteString -> [(BS.ByteString, a)] -> Generator
templateUri = TemplateGen

-- | Create a template-based relative URI generator with the given template and values
relativeUriT :: (IsTemplateValue a) => BS.ByteString -> [(BS.ByteString, a)] -> Generator
relativeUriT = RelativeUriTGen

-- | Create a vocabulary generator (always of type E55_Type)
vocab :: Text -> Text -> Generator
vocab base local =
    let fullUriText = base <> local
        value = mkUriValue fullUriText
    in VocabGen { vocabBase = base, vocabLocalName = local, vocabValue = value }

-- | Make text URL-friendly by converting to lowercase, removing diacritics,
-- and normalizing special characters to underscores
makeUrlFriendly :: Text -> Text
makeUrlFriendly input =
    let -- Convert to lowercase (input is already decoded from XML.hs)
        lowercased = T.map toLower input
        -- Handle German special characters
        germanHandled = T.concatMap handleGermanChars lowercased
        -- Normalize to NFD form (decompose diacritics)
        normalized = nfd germanHandled
        -- Filter out combining diacritical marks and non-alphanumeric chars
        -- Collapse consecutive underscores into a single one
        filtered = T.pack $ go $ T.unpack normalized
    in filtered
  where
    go [] = []
    go (c:cs)
      | isAlphaNum c = c : go cs
      | null cs = []  -- Skip trailing underscore
      | otherwise = case go cs of
          [] -> []  -- Skip trailing underscore
          rest@(next:_)
            | not (isAlphaNum next) -> rest  -- Skip consecutive underscores
            | otherwise -> '_' : rest  -- Keep single underscore between alphanumeric chars

    -- Handle German special characters
    handleGermanChars :: Char -> Text
    handleGermanChars c = case c of
        'ä' -> T.pack "ae"
        'ö' -> T.pack "oe"
        'ü' -> T.pack "ue"
        'ß' -> T.pack "ss"
        '&' -> T.pack "and"
        '?' -> T.pack "_q"
        _ -> T.singleton c
