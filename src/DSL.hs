{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeOperators #-}

module DSL where

import qualified Data.Text as T
import Generators (Generator, IsGenerator(..))
import XML
import CidocCRM
import Data.Maybe (fromMaybe)
import Data.Typeable (eqT, Typeable)
import Data.Type.Equality ((:~:)(..))

-- | PathComponent allows a list of children to be either static PathTrees
-- | or functions that generate PathTrees from an XMLNode.
data PathComponent (c :: Class_) where
  StaticTree  :: PathTree c -> PathComponent c
  DynamicFunc :: (XMLNode -> PathTree c) -> PathComponent c

-- | Type class to easily convert various types to PathComponent.
class ToPathComponent a where
  type PathComponentResult a :: Class_
  toPathComponent :: a -> PathComponent (PathComponentResult a)

instance ToPathComponent (PathTree (c :: Class_)) where
  type PathComponentResult (PathTree c) = c
  toPathComponent = StaticTree

instance ToPathComponent (XMLNode -> PathTree (c :: Class_)) where
  type PathComponentResult (XMLNode -> PathTree c) = c
  toPathComponent = DynamicFunc

-- | Helper to convert a list of items to a list of PathComponents.
toPathComponents :: ToPathComponent a => [a] -> [PathComponent (PathComponentResult a)]
toPathComponents = map toPathComponent

-------------------------------------------------------------------------------
-- CONTEXT APPLIER TYPECLASS (for unified @> operator)
-------------------------------------------------------------------------------

class ContextApplier contextProvider input where
  type ResultClass contextProvider input :: Class_
  applyContext :: contextProvider -> input -> PathTree (ResultClass contextProvider input)

-- Instances for XPathExpr as contextProvider
instance ContextApplier XPathExpr (PathTree (c :: Class_)) where
  type ResultClass XPathExpr (PathTree c) = c
  applyContext newXPathExpr (Node mOldXPath cond preproc prop children) =
      Node (Just (prependXPathToMaybe newXPathExpr mOldXPath)) cond preproc prop children
  applyContext newXPathExpr (DynamicNode mOldXPath cond preproc createTree) =
      DynamicNode (Just (prependXPathToMaybe newXPathExpr mOldXPath)) cond preproc createTree
  applyContext newXPathExpr (ContextGroup mOldXPath cond mPreproc childrenGroup) =
      ContextGroup (Just (prependXPathToMaybe newXPathExpr mOldXPath)) cond mPreproc childrenGroup
  applyContext _ NullTree = NullTree

instance ContextApplier XPathExpr (XMLNode -> PathTree (c :: Class_)) where
  type ResultClass XPathExpr (XMLNode -> PathTree c) = c
  applyContext newXPathExpr func =
      DynamicNode (Just newXPathExpr) Nothing Nothing func

-- Instance for a function that returns a list of PathTrees
instance ContextApplier XPathExpr (XMLNode -> [PathTree (c :: Class_)]) where
  type ResultClass XPathExpr (XMLNode -> [PathTree c]) = c
  applyContext newXPathExpr listGeneratingFunc =
    DynamicNode (Just newXPathExpr) Nothing Nothing innerFunc
    where
      innerFunc :: XMLNode -> PathTree c
      innerFunc xmlNodeCtx =
        let generatedPathTrees = listGeneratingFunc xmlNodeCtx
            pathComponents = map StaticTree generatedPathTrees -- Use StaticTree to wrap each PathTree
        in ContextGroup Nothing Nothing Nothing pathComponents

-- If ToPathComponent is class ToPathComponent a (c :: Class_) | a -> c,
-- and PathComponentResult p is also c, then the constraint simplifies.
instance ToPathComponent p => ContextApplier XPathExpr [p] where
  type ResultClass XPathExpr [p] = PathComponentResult p
  applyContext newXPathExpr items =
      ContextGroup (Just newXPathExpr) Nothing Nothing (toPathComponents items)

-- Instances for Preprocessor as contextProvider
instance ToPathComponent p => ContextApplier Preprocessor [p] where
  type ResultClass Preprocessor [p] = PathComponentResult p
  applyContext preproc items =
      ContextGroup Nothing Nothing (Just preproc) (toPathComponents items)

instance ContextApplier Preprocessor (PathTree (c :: Class_)) where
  type ResultClass Preprocessor (PathTree c) = c
  applyContext preproc tree =
      ContextGroup Nothing Nothing (Just preproc) [toPathComponent tree]

instance ContextApplier Preprocessor (XMLNode -> PathTree (c :: Class_)) where
  type ResultClass Preprocessor (XMLNode -> PathTree c) = c
  applyContext preproc func =
      ContextGroup Nothing Nothing (Just preproc) [toPathComponent func]

-- | Core preprocessing types
data Preprocessor = Preprocessor {
    xpath :: XPathExpr,
    transform :: XMLNode -> IO [XMLNode]
}

-- | Condition type using XPath expressions
data Condition
    = Equals XPathExpr String    -- xpath and value to compare
    | Exists XPathExpr          -- xpath that should exist
    | Contains XPathExpr String  -- xpath contains a substring
    | StartsWith XPathExpr String -- xpath starts with a string
    | EqualsXPath XPathExpr XPathExpr
    | StartsWithXPath XPathExpr XPathExpr
    | ContainsXPath XPathExpr XPathExpr
    | And [Condition]        -- combine conditions with AND
    | Or [Condition]         -- combine conditions with OR
    | Not Condition         -- negate a condition
    | IsTrue Bool
    | IsFirst               -- new condition to take the first node
    | NodeNameIs String     -- new condition to check node name
    deriving (Show, Eq)

-- | Evaluate a condition, now taking Condition first and returning a list of matching nodes
evaluateCondition :: Condition -> [XMLNode] -> [XMLNode]
evaluateCondition (Equals xpathExpr value) nodes =
    filter (\node -> any (\n -> T.toLower (nodeText n) == T.toLower (T.pack value)) (evaluateXPathExpr node xpathExpr)) nodes
evaluateCondition (Exists xpathExpr) nodes =
    filter (\node -> not $ null $ evaluateXPathExpr node xpathExpr) nodes
evaluateCondition (Contains xpathExpr value) nodes =
    filter (\node -> any (\n -> T.isInfixOf (T.toLower (T.pack value)) (T.toLower (nodeText n))) (evaluateXPathExpr node xpathExpr)) nodes
evaluateCondition (StartsWith xpathExpr value) nodes =
    filter (\node -> any (\n -> T.isPrefixOf (T.toLower (T.pack value)) (T.toLower (nodeText n))) (evaluateXPathExpr node xpathExpr)) nodes
evaluateCondition (EqualsXPath xpathExpr1 xpathExpr2) nodes =
    filter (\node ->
        let texts1 = map (T.toLower . nodeText) (evaluateXPathExpr node xpathExpr1)
            texts2 = map (T.toLower . nodeText) (evaluateXPathExpr node xpathExpr2)
        in any (\t1 -> t1 `elem` texts2) texts1
    ) nodes
evaluateCondition (StartsWithXPath xpathExpr1 xpathExpr2) nodes =
    filter (\node ->
        let texts1 = map (T.toLower . nodeText) (evaluateXPathExpr node xpathExpr1)
            texts2 = map (T.toLower . nodeText) (evaluateXPathExpr node xpathExpr2)
        in any (\t1 -> any (\t2 -> T.isPrefixOf t2 t1) texts2) texts1
    ) nodes
evaluateCondition (ContainsXPath xpathExpr1 xpathExpr2) nodes =
    filter (\node ->
        let texts1 = map (T.toLower . nodeText) (evaluateXPathExpr node xpathExpr1)
            texts2 = map (T.toLower . nodeText) (evaluateXPathExpr node xpathExpr2)
        in any (\t1 -> any (\t2 -> T.isInfixOf t2 t1) texts2) texts1
    ) nodes
evaluateCondition (And conditions) nodes =
    filter (\node -> all (\cond -> not . null $ evaluateCondition cond [node]) conditions) nodes
evaluateCondition (Or conditions) nodes =
    filter (\node -> any (\cond -> not . null $ evaluateCondition cond [node]) conditions) nodes
evaluateCondition (Not condition) nodes =
    filter (\node -> null $ evaluateCondition condition [node]) nodes
evaluateCondition (IsTrue True) nodes = nodes
evaluateCondition (IsTrue False) _ = []
evaluateCondition IsFirst nodes = take 1 nodes
evaluateCondition (NodeNameIs name) nodes =
    filter (\node -> T.toLower (nodeName node) == T.toLower (T.pack name)) nodes

-- | Condition helper functions
equals :: XPathExpr -> String -> Condition
equals = Equals

exists :: XPathExpr -> Condition
exists = Exists

contains :: XPathExpr -> String -> Condition
contains = Contains

starts :: XPathExpr -> String -> Condition
starts = StartsWith

equalsXPath :: XPathExpr -> XPathExpr -> Condition
equalsXPath = EqualsXPath

startsWithXPath :: XPathExpr -> XPathExpr -> Condition
startsWithXPath = StartsWithXPath

containsXPath :: XPathExpr -> XPathExpr -> Condition
containsXPath = ContainsXPath

and_ :: [Condition] -> Condition
and_ = And

or_ :: [Condition] -> Condition
or_ = Or

not_ :: Condition -> Condition
not_ = Not

isTrue :: Bool -> Condition
isTrue = IsTrue

isFirst :: Condition
isFirst = IsFirst

nodeNameIs :: String -> Condition
nodeNameIs = NodeNameIs

-- | Type class for conditionable elements
class Conditionable a where
    withCondition :: Condition -> a -> a

-- | Condition application helper
when :: Conditionable a => Condition -> a -> a
when = withCondition

-- | Chain preprocessors
(>=>) :: Preprocessor -> Preprocessor -> Preprocessor
(>=>) p1 p2 = Preprocessor {
    xpath = xpath p1,
    transform = \node -> do
        nodes1 <- transform p1 node
        concat <$> mapM (transform p2) nodes1
}

-- | Type class for XPath inputs
class ToXPathExpr a where
    toXPathExpr :: a -> XPathExpr

instance ToXPathExpr XPathExpr where
    toXPathExpr = id

-- | Domain definition
data D c = D (ClassRef c) XPathExpr Generator

-- | Enhanced PathTree type that incorporates Link functionality
data PathTree (src :: Class_) where
  Node :: forall src tgt domain range g. (IsGenerator g, Typeable tgt) =>
         Maybe XPathExpr ->                -- Relative xpath context for this node (Nothing means use parent context)
         Maybe Condition ->                -- Optional condition
         Maybe Preprocessor ->             -- Optional preprocessor
         (Property_ domain range src tgt, (ClassRef tgt, g)) ->
         Maybe [PathComponent tgt] ->      -- Optional children, now PathComponents
         PathTree src
  DynamicNode :: forall src.
         Maybe XPathExpr ->                -- Relative xpath context for this node (Nothing means use parent context)
         Maybe Condition ->                -- Optional condition
         Maybe Preprocessor ->             -- Optional preprocessor
         (XMLNode -> PathTree src) ->      -- Function to create PathTree from node
         PathTree src
  ContextGroup :: Maybe XPathExpr ->       -- Base XPath for the group (optional, Nothing means use parent context)
                  Maybe Condition ->       -- Condition for the group (optional)
                  Maybe Preprocessor ->     -- Optional preprocessor for the group
                  [PathComponent src] ->    -- Child trees operating within this context, now PathComponents
                  PathTree src
  NullTree :: PathTree src                 -- Represents a null/empty mapping

-- | Mapping type (simplified)
data Mapping c = Mapping (D c) [PathTree c]

-------------------------------------------------------------------------------
-- XPATH HELPER FUNCTIONS
-------------------------------------------------------------------------------

-- | Helper function to combine two XPathExprs by concatenating their components.
combineXPath :: XPathExpr -> XPathExpr -> XPathExpr
combineXPath (XPathExpr comps1) (XPathExpr comps2) = XPathExpr (comps1 ++ comps2)

-- | Prepends a new XPath to an existing Maybe XPath.
-- If no existing XPath, the new one is used directly.
-- If an old XPath exists, the new one is prepended to it.
prependXPathToMaybe :: XPathExpr -> Maybe XPathExpr -> XPathExpr
prependXPathToMaybe newXP Nothing = newXP
prependXPathToMaybe newXP (Just oldXP) = combineXPath newXP oldXP

-------------------------------------------------------------------------------
-- OPERATORS
-------------------------------------------------------------------------------

-- | Main operator to apply context (XPath or Preprocessor) to a target (PathTree, function, or list).
infixr 7 @>
(@>) :: ContextApplier contextProvider input => contextProvider -> input -> PathTree (ResultClass contextProvider input)
(@>) = applyContext

-- | Operator to set condition
infixr 6 ?>
(?>) :: Condition -> PathTree c -> PathTree c

-- Case 1a: RHS is a Node that likely resulted from `xpath @> tree_without_inner_condition`.
-- It has its own `Just xpath` context and its original condition slot was `Nothing`.
-- We transform `condExpr ?> (Node (Just xpath) Nothing ...)`
-- into `ContextGroup Nothing (Just condExpr) Nothing [StaticTree (original Node structure)]`.
condExpr ?> (Node (Just xpathExpr) Nothing preproc prop children) =
    ContextGroup Nothing (Just condExpr) Nothing [StaticTree (Node (Just xpathExpr) Nothing preproc prop children)]

-- Case 1b: Similar for DynamicNode.
condExpr ?> (DynamicNode (Just xpathExpr) Nothing preproc createTreeFn) =
    ContextGroup Nothing (Just condExpr) Nothing [StaticTree (DynamicNode (Just xpathExpr) Nothing preproc createTreeFn)]

-- Case 2: Fallthrough for all other PathTree structures.
-- This includes:
--   - A Node/DynamicNode where mRelativeXPath is Nothing (e.g., `cond ?> simpleTree`).
--   - A Node/DynamicNode where mCondition was already Just something (e.g., `cond1 ?> (cond2 ?> tree)` or `cond1 ?> (xpath @> (cond2 ?> tree))`).
--   - A ContextGroup itself (e.g., `cond ?> (xpath @> [tree1, tree2])`).
--   - NullTree.
-- Action: Embed condExpr directly into the PathTree, overwriting/setting its condition.
condExpr ?> (Node xpathExpr _ preproc prop children) =
    Node xpathExpr (Just condExpr) preproc prop children
condExpr ?> (DynamicNode xpathExpr _ preproc createTreeFn) =
    DynamicNode xpathExpr (Just condExpr) preproc createTreeFn
condExpr ?> (ContextGroup xpathExpr _ mPreproc childrenGroup) = -- Added mPreproc
    ContextGroup xpathExpr (Just condExpr) mPreproc childrenGroup -- Pass mPreproc
_ ?> NullTree = NullTree

-- | Operator to create a link to a PathTree
--   If the input is Nothing, it returns NullTree.
--   If the input is Just, it creates a link to the PathTree.
maybeLink = maybe NullTree

-- | Operator for property-class pairs with children
infixr 5 ==>
(==>) :: forall domain range src tgt g p. (IsGenerator g, Typeable tgt, ToPathComponent p, PathComponentResult p ~ tgt) =>
         (Property_ domain range src tgt, (ClassRef tgt, g)) ->
         [p] -> -- Now accepts a list of items convertible to PathComponent
         PathTree src
(prop, classElem) ==> childItems = Node Nothing Nothing Nothing (prop, classElem) (Just (toPathComponents childItems))

-- | Helper operator to create property-class pairs
infixr 6 --->
(--->) :: forall domain range src tgt g. IsGenerator g =>
          Property_ domain range src tgt ->
          (ClassRef tgt, g) -> -- Use g
          (Property_ domain range src tgt, (ClassRef tgt, g))
p ---> ce = (p, ce)

-- | Operator for property-class pairs without children
infixr 6 -->
(-->) :: forall domain range src tgt g. (IsGenerator g, Typeable tgt) => -- Added Typeable tgt
         Property_ domain range src tgt -> (ClassRef tgt, g) -> PathTree src
p --> ce = Node Nothing Nothing Nothing (p, ce) Nothing

-- | Mapping construction operator
infixr 4 +>
(+>) :: D c -> [PathTree c] -> Mapping c
(+>) = Mapping

-- | Operator to append children to an existing PathTree Node
infixl 5 ++>
(++>) :: forall src tgt p. (Typeable tgt, ToPathComponent p, PathComponentResult p ~ tgt) => PathTree src -> [p] -> PathTree src
(Node ce cond preproc prop@( _ :: Property_ domain range src nodeTgt, _) mChildren) ++> (newChildItems :: [p]) =
    case eqT @nodeTgt @tgt of
      Just Refl ->
        let newPathComponents = toPathComponents newChildItems -- Convert [p] to [PathComponent tgt]
        in Node ce cond preproc prop (Just (fromMaybe [] mChildren ++ newPathComponents))
      Nothing -> error "Type mismatch: Cannot append children of different types."
tree ++> _ = tree


-- | Conditionable instances
instance Conditionable (PathTree c) where
    withCondition condExpr tree = condExpr ?> tree
