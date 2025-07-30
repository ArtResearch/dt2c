{-# LANGUAGE NoOverloadedStrings #-}

module Vocabularies (
    writeVocabularyFile,
    extractValue
) where

import qualified Data.ByteString.Char8 as BS
import RDF (Value(..), Triple(..), formatTriples, OutputFormat(TTL)) -- Import OutputFormat(TTL)
import qualified Data.Text as T
import qualified Text.XML.Hexml as X
import Generators (generateId, VocabGenerator (unVocabGenerator))
import XML (XMLNode(..))

-- | RDF type predicate
rdfType :: Value
rdfType = PrefixedURI (T.pack "a")

-- | E55_Type class URI
e55Type :: Value
e55Type = PrefixedURI (T.pack "crm:E55_Type")

-- | Write vocabulary type statements to a file
-- Note: The list of items must now be generated within each dataset package's executable
writeVocabularyFile :: [Value] -> FilePath -> IO ()
writeVocabularyFile vocabItems outFile = do
    let typeTriples = map (\item -> Triple item rdfType e55Type) vocabItems
    -- Pass TTL format and Nothing for baseUri to formatTriples
    writeFile outFile (T.unpack (formatTriples TTL Nothing typeTriples))

-- | Extract the Value from a Generator
extractValue :: VocabGenerator -> IO Value
extractValue vocabGen = do
    -- Create a dummy XML node for the generator
    let dummyDoc = X.parse (BS.pack "<dummy></dummy>")
    let gen = unVocabGenerator vocabGen
    case dummyDoc of
        Left _ -> error "Failed to create dummy XML node"
        Right doc -> do
            let dummyNode = XMLNode doc 0 Nothing
            -- Generate the value with empty context
            (value, _) <- generateId gen dummyNode T.empty (BS.pack ".") emptyContext
            return value
  where
    emptyContext = error "Context should not be accessed during vocabulary value extraction"
