module Preprocessors where

import Data.Text (Text)
import qualified Data.Text as T
import XML
import DSL

-- | Common preprocessors
-- | Identity preprocessor that returns the node as is
identity :: Preprocessor
identity = Preprocessor {
    xpath = [x|.|],
    transform = \node -> return [node]
}

split :: String -> Preprocessor
split delimiter = Preprocessor {
    xpath = [x|text()|],
    transform = \node -> do
        let text = nodeText node
        return [makeTextNode (T.strip value) | value <- T.splitOn (T.pack delimiter) text]
}

normalizeText :: Text -> Text
normalizeText = T.unwords . T.words
