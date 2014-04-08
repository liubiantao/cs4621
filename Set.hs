module Set( Set, emptySet, isEmptySet, insert, delete, foldSet ) where

--------------------------------------------------------------------------------
-- I N T E R F A C E  :  P U B L I C
--------------------------------------------------------------------------------

-- Set a : a finite collection of distinct items of type 'a'

--------------------------------------------------------------------------------

-- emptySet : the empty set

emptySet :: Set a

--------------------------------------------------------------------------------

-- isEmptySet s : is set 's' empty ?

isEmptySet :: Set a -> Bool

--------------------------------------------------------------------------------

-- insert x s : the set formed by adding item 'x' to set 's',
--              or 's' itself if 'x' is already a member

insert :: Eq a => a -> Set a -> Set a

--------------------------------------------------------------------------------

-- delete x s : the set formed by removing item 'x' from set 's',
--              or 's' itself if 'x' is not a member

delete :: Eq a => a -> Set a -> Set a

--------------------------------------------------------------------------------

-- foldSet f v s : the result of cumulatively pairwise combining all items
--                 in set 's', using function 'f', having started with item 'v'

foldSet :: ( a -> b -> b ) -> b -> Set a -> b

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- I M P L E M E N T A T I O N  :  P R I V A T E
--------------------------------------------------------------------------------

data Set a = Set [a] deriving (Show, Eq)

--------------------------------------------------------------------------------

emptySet = Set []

--------------------------------------------------------------------------------

isEmptySet (Set []) = True
isEmptySet _ = False

--------------------------------------------------------------------------------

insert x (Set s) 
    | elem x s = Set s
    | otherwise = Set (x:s)

--------------------------------------------------------------------------------

delete x (Set s) = Set (filter (/= x) s)

--delete' :: Eq a => a -> [a] -> [a]
--delete' a xs = [ x | x <- xs, x /= a ]

--------------------------------------------------------------------------------

foldSet f v (Set s) = foldr f v s

-------------------------------------------------------------------------------- 
