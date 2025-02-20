{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

module Task2 where

-- Explicit import of Prelude to hide functions
-- that are not supposed to be used in this assignment
import Prelude hiding (compare, foldl, foldr, Ordering(..))

import Task1 (Tree(..))

-- * Type definitions

-- | Ordering enumeration
data Ordering = LT | EQ | GT
  deriving (Show, Eq)

-- | Binary comparison function indicating whether first argument is less, equal or
-- greater than the second one (returning 'LT', 'EQ' or 'GT' respectively)
type Cmp a = a -> a -> Ordering

-- * Function definitions

-- | Binary comparison function induced from `Ord` constraint
--
-- Usage example:
--
-- >>> compare 2 3
-- LT
-- >>> compare 'a' 'a'
-- EQ
-- >>> compare "Haskell" "C++"
-- GT
--
compare :: Ord a => Cmp a
compare a b | a < b = LT
            | a > b = GT
            | otherwise = EQ

-- | Conversion of list to binary search tree
-- using given comparison function
--
-- Usage example:
--
-- >>> listToBST compare [2,3,1]
-- Branch 2 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf)
-- >>> listToBST compare ""
-- Leaf
--

listToBST :: Cmp a -> [a] -> Tree a
listToBST cmp (x:xs) = tinsert cmp x (listToBST cmp xs)
listToBST _ [] = Leaf


-- | Conversion from binary search tree to list
--
-- Resulting list will be sorted
-- if given tree is valid BST with respect
-- to some 'Cmp' comparison.
--
-- Usage example:
--
-- >>> bstToList (Branch 2 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf))
-- [1,2,3]
-- >>> bstToList Leaf
-- []
--
bstToList :: Tree a -> [a]
bstToList tree = case tree of
  (Branch t l r) -> bstToList l ++ [t] ++ bstToList r
  Leaf           -> []

-- | Tests whether given tree is a valid binary search tree
-- with respect to given comparison function
--
-- Usage example:
--
-- >>> isBST compare (Branch 2 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf))
-- True
-- >>> isBST compare (Leaf :: Tree Char)
-- True
-- >>> isBST compare (Branch 5 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf))
-- False

data Range a = Range (Maybe a) (Maybe a)

inRange :: Cmp a -> Range a -> a -> Bool
inRange cmp (Range (Just l) (Just r)) e = cmp l e == LT &&
                                          (cmp e r == LT)
inRange cmp (Range Nothing (Just a)) e = cmp e a == LT
inRange cmp (Range (Just a) Nothing) e = cmp a e == LT
inRange _ _ _= True

isBST :: Cmp a -> Tree a -> Bool
isBST cmp (Branch t l r) = recursiveIsBst cmp l (Range Nothing (Just t)) && recursiveIsBst cmp r (Range (Just t) Nothing)
isBST _ Leaf = True

recursiveIsBst :: Cmp a -> Tree a -> Range a -> Bool
recursiveIsBst cmp (Branch t l r) (Range lr rr) = inRange cmp (Range lr rr) t &&
                                                recursiveIsBst cmp l (Range lr (Just t)) &&
                                                recursiveIsBst cmp r (Range (Just t) rr)
recursiveIsBst _ Leaf _ = True

-- | Searches given binary search tree for
-- given value with respect to given comparison
--
-- Returns found value (might not be the one that was given)
-- wrapped into 'Just' if it was found and 'Nothing' otherwise.
--
-- Usage example:
--
-- >>> tlookup compare 2 (Branch 2 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf))
-- Just 2
-- >>> tlookup compare 'a' Leaf
-- Nothing
-- >>> tlookup (\x y -> compare (x `mod` 3) (y `mod` 3)) 5 (Branch 2 (Branch 0 Leaf Leaf) (Branch 2 Leaf Leaf))
-- Just 2
--
tlookup :: Cmp a -> a -> Tree a -> Maybe a
tlookup cmp e (Branch t l r) = case cmp e t of
  LT -> tlookup cmp e l
  GT -> tlookup cmp e r
  EQ -> Just t
tlookup _ _ Leaf = Nothing

-- | Inserts given value into given binary search tree
-- preserving its BST properties with respect to given comparison
--
-- If the same value with respect to comparison
-- was already present in the 'Tree' then replaces it with given value.
--
-- Usage example:
--
-- >>> tinsert compare 0 (Branch 2 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf))
-- Branch 2 (Branch 1 (Branch 0 Leaf Leaf) Leaf) (Branch 3 Leaf Leaf)
-- >>> tinsert compare 1 (Branch 2 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf))
-- Branch 2 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf)
-- >>> tinsert compare 'a' Leaf
-- Branch 'a' Leaf Leaf
--
tinsert :: Cmp a -> a -> Tree a -> Tree a
tinsert cmp e (Branch t l r) = case cmp e t of
  LT -> Branch t (tinsert cmp e l) r
  GT -> Branch t l (tinsert cmp e r)
  EQ -> Branch e l r
tinsert _ e Leaf = Branch e Leaf Leaf

-- | Deletes given value from given binary search tree
-- preserving its BST properties with respect to given comparison
--
-- Returns updated 'Tree' if the value was present in it;
-- or unchanged 'Tree' otherwise.
--
-- Usage example:
--
-- >>> tdelete compare 1 (Branch 2 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf))
-- Branch 2 Leaf (Branch 3 Leaf Leaf)
-- >>> tdelete compare 'a' Leaf
-- Leaf
--
tdelete :: Cmp a -> a -> Tree a -> Tree a
tdelete cmp e (Branch t l r) = case cmp e t of
  LT -> Branch t (tdelete cmp e l) r
  GT -> Branch t l (tdelete cmp e r)
  EQ -> case (l, r) of
    (Leaf, Leaf) ->  Leaf
    (_, Leaf) -> l
    (Leaf, _) -> r
    _ -> Branch (findMin r) l (tdelete cmp (findMin r) r)
tdelete _ _ Leaf = Leaf

findMin :: Tree a -> a
findMin (Branch t Leaf _) = t
findMin (Branch _ l _) = findMin l
findMin _ = undefined
