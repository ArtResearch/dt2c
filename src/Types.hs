{-# LANGUAGE NoOverloadedStrings #-}

module Types (
    OutputFormat(..)
) where

-- | Output format for RDF serialization
data OutputFormat = TTL | NTriples
    deriving (Show, Eq, Read)
