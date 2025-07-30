{-# LANGUAGE RecordWildCards #-}

module Vocabularies.Base (
    (</>),
    localName
) where

import Generators (Generator(..), VocabGenerator(..), IsGenerator(..), unwrapGenerator, vocab)
import Data.Text (Text)
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as TE

-- | Create a VocabGenerator
(</>) :: Text -> Text -> VocabGenerator
base </> path = VocabGenerator (vocab base path)

-- | Safely extract the local name from a VocabGenerator
localName :: VocabGenerator -> BS.ByteString
localName vg = case unwrapGenerator vg of
    VocabGen {..} -> TE.encodeUtf8 vocabLocalName
    _other -> error "getVocabLocalName: Expected VocabGen inside VocabGenerator. Should never happen because of the type system."
