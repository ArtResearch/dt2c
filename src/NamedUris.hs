{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}

module NamedUris (
    NamedUriNamespace(..),
    typedNamed,
    typedNamedUri,
    getNamedUriText,
    lookupNamedUri,
    allNamedUris
) where

import Data.Text (Text)
import qualified Data.Map.Strict as Map
import RDF (Value)
import Generators (Generator(..), GenerationContext(..), NamedUriNamespace(..))

-- | Get all named URIs in this namespace with their text representations
allNamedUris :: NamedUriNamespace ns => [NamedUri ns] -> [(NamedUri ns, Text)]
allNamedUris = map (\uri -> (uri, uriToText uri))

-- | Smart constructor for naming the result of a generator
typedNamed :: NamedUriNamespace ns => NamedUri ns -> Generator -> Generator
typedNamed = TypedNamedGen

-- | Smart constructor for referencing a named URI
typedNamedUri :: NamedUriNamespace ns => NamedUri ns -> Generator
typedNamedUri = TypedNamedUriGen

-- | Helper function to get the text representation of a named URI
getNamedUriText :: NamedUriNamespace ns => NamedUri ns -> Text
getNamedUriText = uriToText

-- | Helper function to look up a named URI in the context
lookupNamedUri :: NamedUriNamespace ns => NamedUri ns -> GenerationContext -> Maybe Value
lookupNamedUri uri ctx = Map.lookup (uriToText uri) (namedUris ctx)
