{-# LANGUAGE TemplateHaskell #-}

{- | Raw string literals, implemented using Template Haskell's quasiquotation
feature.
-}
module Util.QQ (
  w,
  t,
)
where

import Data.Either (partitionEithers)
import Data.List.Extra hiding (trim)
import qualified Data.Set as Set
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax (liftData)
import Prelude
import RDF (stringToTriple)
import Util.ShowableSet (ShowableSet (..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

{-
A quasiquoter that cleans up whitespace and common indent. Example:
x = [w|
        for(i=0;i<10;i++) {
          printf("Hello, world!\n");
        }
 |]
...gives...
x = (UTF-8 encoded ByteString for)
    "for(i=0;i<10;i++) {\n" <>
    "  printf(\"Hello, world!\\n\");\n" <>
    "}\n"
-}
w :: QuasiQuoter
w =
  QuasiQuoter
    { quoteExp = \s -> [| TE.encodeUtf8 (T.pack (unlines (whiteSpaceSanitize s))) |]
    , quotePat = \_ ->
        fail
          "illegal raw string QuasiQuote \
          \(allowed as expression only, used as a pattern)"
    , quoteType = \_ ->
        fail
          "illegal raw string QuasiQuote \
          \(allowed as expression only, used as a type)"
    , quoteDec = \_ ->
        fail
          "illegal raw string QuasiQuote \
          \(allowed as expression only, used as a declaration)"
    }

{-
A quasiquoter that cleans up whitespace, common indent, splits into lines,
and returns a Set of strings. Example:
x = [t|
        line one
        line two
        line one
 |]
...gives...
x = Set.fromList ["line one", "line two"]
-}
t :: QuasiQuoter
t =
  QuasiQuoter
    { quoteExp = textToSetExp
    , quotePat = \_ ->
        fail
          "illegal set QuasiQuote \
          \(allowed as expression only, used as a pattern)"
    , quoteType = \_ ->
        fail
          "illegal set QuasiQuote \
          \(allowed as expression only, used as a type)"
    , quoteDec = \_ ->
        fail
          "illegal set QuasiQuote \
          \(allowed as expression only, used as a declaration)"
    }

-- Helper function to generate the Set expression for Triples
textToSetExp :: String -> Q Exp
textToSetExp str = do
  let sanitizedLines = whiteSpaceSanitize str
  let parseResults = map stringToTriple sanitizedLines
  let (errors, triples) = partitionEithers parseResults

  -- If there are any parsing errors, fail the compilation
  case errors of
    (firstError : _) -> fail $ "Failed to parse triple: " ++ firstError
    [] -> do
      -- Lift the list of triples into a Template Haskell expression
      liftedTriples <- liftData triples
      -- Construct the expression: ShowableSet $ Set.fromList [Triple1, Triple2, ...]
      [|ShowableSet (Set.fromList $(return liftedTriples))|]

whiteSpaceSanitize :: String -> [String]
whiteSpaceSanitize str =
  let
    -- normalize newlines, remove trailing whitespace, remove leading and trailing empty lines
    ls = (dropWhile null . dropWhileEnd null) (trimEnd <$> lines str)

    minIndent =
      ( minimum
          . map (length . takeWhile (== ' ')) -- count leading spaces
          . filter (not . null) -- ignore empty lines
      )
        ls
  in
    (drop minIndent <$> ls)
