{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

module Task1 where

-- Explicit import of Prelude to hide functions
-- that are not supposed to be used in this assignment
import Prelude hiding (foldl, foldr)

-- * Type definitions

-- | Binary tree
data Tree a = Leaf | Branch a (Tree a) (Tree a)
  deriving Show

-- | Forest (i.e. list of 'Tree's)
type Forest a = [Tree a]

-- | Tree traversal order
data Order = PreOrder | InOrder | PostOrder
  deriving Show

-- * Function definitions

-- | Returns values of given 'Tree' in specified 'Order' with optional leaf value
--
-- Usage example:
--
-- >>> torder PreOrder  (Just '.') (Branch 'A' Leaf (Branch 'B' Leaf Leaf))
-- "A.B.."
-- >>> torder InOrder   (Just '.') (Branch 'A' Leaf (Branch 'B' Leaf Leaf))
-- ".A.B."
-- >>> torder PostOrder (Just '.') (Branch 'A' Leaf (Branch 'B' Leaf Leaf))
-- "...BA"
--
torder :: Order    -- ^ Order of resulting traversal
       -> Maybe a  -- ^ Optional leaf value
       -> Tree a   -- ^ Tree to traverse
       -> [a]      -- ^ List of values in specified order
torder order leaf tree = case order of
                        PreOrder  -> preOrder leaf tree
                        InOrder   -> inOrder leaf tree
                        PostOrder -> postOrder leaf tree

maybeToList :: Maybe a -> [a]
maybeToList (Just a) = [a]
maybeToList Nothing = []

preOrder :: Maybe a -> Tree a -> [a]
preOrder leaf tree = case tree of
  (Branch t l r) -> [t] ++ preOrder leaf l ++ preOrder leaf r
  Leaf -> maybeToList leaf

inOrder :: Maybe a -> Tree a -> [a]
inOrder leaf tree = case tree of
  (Branch t l r) -> inOrder leaf l ++ [t] ++ inOrder leaf r
  Leaf -> maybeToList leaf 

postOrder :: Maybe a -> Tree a -> [a]
postOrder leaf tree  = case tree of
  (Branch t l r) -> postOrder leaf l ++ postOrder leaf r ++ [t]
  Leaf -> maybeToList leaf 
  


-- | Returns values of given 'Forest' separated by optional separator
-- where each 'Tree' is traversed in specified 'Order' with optional leaf value
--
-- Usage example:
--
-- >>> forder PreOrder  (Just '|') (Just '.') [Leaf, Branch 'C' Leaf Leaf, Branch 'A' Leaf (Branch 'B' Leaf Leaf)]
-- ".|C..|A.B.."
-- >>> forder InOrder   (Just '|') (Just '.') [Leaf, Branch 'C' Leaf Leaf, Branch 'A' Leaf (Branch 'B' Leaf Leaf)]
-- ".|.C.|.A.B."
-- >>> forder PostOrder (Just '|') (Just '.') [Leaf, Branch 'C' Leaf Leaf, Branch 'A' Leaf (Branch 'B' Leaf Leaf)]
-- ".|..C|...BA"
--

intercalate :: [a] -> [[a]] -> [a]
intercalate _ [] = []
intercalate _ [l] = l
intercalate s (l:ls) = l ++ s ++ intercalate s ls

forder :: Order     -- ^ Order of tree traversal
       -> Maybe a   -- ^ Optional separator between resulting tree orders
       -> Maybe a   -- ^ Optional leaf value
       -> Forest a  -- ^ List of trees to traverse
       -> [a]       -- ^ List of values in specified tree order
forder o s l f = intercalate (maybeToList s) (map (torder o l) f)

