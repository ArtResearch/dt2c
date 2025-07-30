module Pharos.CidocCrm.Patterns.Iconclass (iconclassMapping) where

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Types.URI (urlEncode)
import Data.Char (isSpace)
import Control.Monad (guard)

import CommonImports
import qualified Vocabularies.PHAROS as P
import Pharos.CidocCrm.Patterns.Identifiers (identifier_0_1_fn)
import Pharos.CidocCrm.Patterns.Appellations (appellation_0_1_fn)

iconclassMapping p iconClassCode =
  case map (T.toUpper . T.strip) $ T.splitOn ":" iconClassCode of
    -- single iconclass term like:
    -- 49 D 52
    [mainSubject] -> singleIconclass p mainSubject []

    -- main, possibly complex, iconclass term like:
    -- 49 D 52 : 11 Q 71 3 : 61 F (SAN PIETRO) : 61 E (ROMA)
    (mainSubject : ss) -> 
      (singleIconclass p iconClassCode (
          [
            -- it is broader of the main subject, e.g 49 D 52
            singleIconclass P127 mainSubject []
          ]
          ++ map (\s -> singleIconclass P130 s []) ss
        )
      )
    [] -> NullTree

singleIconclass p iconClassCode additional =
  case extractParenthetical iconClassCode of
    Just (base, detail) -> 
      baseSingleIconclass p iconClassCode (additional ++ [
        appellation_0_1_fn P.preferred_name [x|text()|] (const detail),
        baseSingleIconclass P127 base []
      ])
    Nothing -> baseSingleIconclass p iconClassCode additional

baseSingleIconclass p iconClassCode additional =      
  p ---> (E55, constUri $ iconclassUri iconClassCode) ==> [
    P2 --> (E55, P.iconclass),
    identifier_0_1_fn P.preferred_identifier (const iconClassCode)
  ] ++ additional

-- | 
-- Create a canonical Iconclass URI from a subject string,
-- stripping all whitespace and transforming to uppercase.
iconclassUri :: T.Text -> T.Text
iconclassUri value =
  -- we need True in urlEncode to make sure that we also encod +, because it is encoded in canonical iconclass URIs
  -- see also https://hackage.haskell.org/package/http-types-0.12.4/docs/Network-HTTP-Types-URI.html#v:urlEncode
  T.pack "http://iconclass.org/" <>
    TE.decodeUtf8 (urlEncode True (TE.encodeUtf8 $ T.toUpper $ T.filter (not . isSpace) value))

-- Parses a string containing a single pair of parentheses like "Prefix(Detail)Suffix".
-- Returns a tuple: (Base, Detail)
-- Base: The original string with "(Detail)" replaced by "(...)".
-- Detail: The content found within the parentheses. Can be empty.
-- Returns Nothing if no parentheses are found, or if they are malformed (e.g., missing a closer).
--
-- Examples:
--   "11H(Francis)59" -> Just ("11H(...)59", "Francis")
--   "Text(Detail)End" -> Just ("Text(...)End", "Detail")
--   "Start(Detail)"   -> Just ("Start(...)", "Detail")
--   "(Detail)End"     -> Just ("(...)End", "Detail")
--   "Text()More"      -> Just ("Text(...)More", "")
--   "()"              -> Just ("(...)", "")
--   "NoParens"        -> Nothing
--   "MissingClose("   -> Nothing
--   "MissingOpen)Close" -> Nothing -- as breakOn "(" won't find it first
extractParenthetical :: T.Text -> Maybe (T.Text, T.Text) -- (BaseWithEllipsis, Detail)
extractParenthetical t = do
  let openParen  = T.pack "("
  let closeParen = T.pack ")"
  let ellipsis   = T.pack "(...)"

  -- 1. Split the string at the first '('.
  --    prefix: part before '(', e.g., "11H"
  --    restStartingWithOpen: part from '(' onwards, e.g., "(Francis)59"
  let (prefix, restStartingWithOpen) = T.breakOn openParen t

  -- 2. Guard: Ensure '(' was actually found. If not, restStartingWithOpen will be empty.
  guard (not (T.null restStartingWithOpen))

  -- 3. Remove the leading '(' from restStartingWithOpen.
  --    contentAfterOpen: e.g., "Francis)59"
  let contentAfterOpen = T.drop (T.length openParen) restStartingWithOpen

  -- 4. Split contentAfterOpen at the first ')'.
  --    detail: part before ')', e.g., "Francis"
  --    suffix: part from ')' onwards, e.g., ")59" (this still includes the ')' )
  let (detail, restStartingWithClose) = T.breakOn closeParen contentAfterOpen

  -- 5. Guard: Ensure ')' was actually found in contentAfterOpen.
  guard (not (T.null restStartingWithClose))

  -- 6. Guard: Ensure that the detail does not contain a '+'
  guard (not $ T.isInfixOf (T.pack "+") detail)

  -- 7. Remove the leading ')' from restStartingWithClose to get the true suffix.
  --    suffixWithoutParen: e.g., "59"
  let suffixWithoutParen = T.drop (T.length closeParen) restStartingWithClose
  
  -- 8. Construct the new base string.
  let baseWithEllipsis = prefix <> ellipsis <> suffixWithoutParen

  -- The detail is already extracted. It can be empty, e.g. for "Text()".
  -- No T.strip is applied to detail here, assuming it's not needed per strict parsing.
  -- If detail could have spaces like "(  Francis  )59", and you want "Francis",
  -- then `let finalDetail = T.strip detail` would be needed.
  -- For now, I'll assume detail is exactly what's between the parens.

  Just (baseWithEllipsis, detail)
