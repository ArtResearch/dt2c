module Util.ShowableSet (
  ShowableSet (..),
) where

import qualified Data.Set as Set
import Prelude

-- | A newtype wrapper for Data.Set to provide a custom Show instance.
newtype ShowableSet a = ShowableSet {unShowableSet :: Set.Set a}
  deriving (Eq, Ord) -- Derive other useful instances if needed

-- Helper function to remove leading/trailing quotes from a string
stripQuotes :: String -> String
stripQuotes s = case s of
    -- Check if it starts with " and has at least one character after it
    '"' : cs@(_:_) ->
        case reverse cs of
            -- Check if the reversed remaining string starts with " (meaning original ended with ")
            '"' : rs -> reverse rs -- Return the middle part
            _        -> s -- Doesn't end with quote, return original
    _            -> s -- Doesn't start with quote, or is empty/single quote, return original

-- | Custom Show instance for Set that prints each element on a new line, stripping quotes.
instance (Show a, Ord a) => Show (ShowableSet a) where
  show (ShowableSet s) = unlines (map (stripQuotes . show) $ Set.toList s)
