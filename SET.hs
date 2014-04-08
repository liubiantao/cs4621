--module SET( module Set, size, listToSet, setToList,
            --(#?), (#<=), (#=), (#+), (#*), (#-) ) where
module SET(module Set, size) where
import Set

--------------------------------------------------------------------------------
-- I N T E R F A C E  :  P U B L I C  :  all exports of module 'Set', plus :
--------------------------------------------------------------------------------

-- size s : the number of items in set 's'

size :: Eq a => Set a -> Int

--------------------------------------------------------------------------------

-- listToSet xs : the set of all distinct items in list 'xs'

listToSet :: Eq a => [ a ] -> Set a

--------------------------------------------------------------------------------

-- setToList s : a list of all items in set 's'

setToList :: Eq a => Set a -> [ a ]

--------------------------------------------------------------------------------

-- x #? s : is 'x' a member of set 's' ?

(#?) :: Eq a => a -> Set a -> Bool

--------------------------------------------------------------------------------

-- s1 #<= s2 : is set 's1' a subset of set 's2' ?

(#<=) :: Eq a => Set a -> Set a -> Bool

--------------------------------------------------------------------------------

-- s1 #= s2 : is set 's1' equal to set 's2' ?

(#=) :: Eq a => Set a -> Set a -> Bool

--------------------------------------------------------------------------------

-- s1 #+ s2 : the union of sets 's1' and 's2'

(#+) :: Eq a => Set a -> Set a -> Set a

--------------------------------------------------------------------------------

-- s1 #* s2 : the intersection of sets 's1' and 's2'

(#*) :: Eq a => Set a -> Set a -> Set a

--------------------------------------------------------------------------------

-- s1 #- s2 : the difference of sets 's1' and 's2'

(#-) :: Eq a => Set a -> Set a -> Set a

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- I M P L E M E N T A T I O N  :  P R I V A T E
--------------------------------------------------------------------------------

size s = foldSet (\_ -> (+1)) 0 s

--------------------------------------------------------------------------------

listToSet xs = listToSet' xs emptySet
listToSet' :: Eq a => [a] -> Set a -> Set a 
listToSet' [] s = s
listToSet' (x:xs) s = listToSet' xs (insert x s)

--------------------------------------------------------------------------------

setToList = foldSet (:) [] 

--------------------------------------------------------------------------------

x #? s 
    | insert x s == s = True
    | otherwise = False

--------------------------------------------------------------------------------

s1 #<= s2 = isSubset (setToList s1) s2 
-- isSubset xs s : is every items of list 'xs' a member of set 's2' ?
isSubset :: Eq a => [a] -> Set a -> Bool
isSubset [] _ = True
isSubset (x:xs) s = (x #? s) && (isSubset xs s)

--------------------------------------------------------------------------------

s1 #= s2 = isSubset (setToList s1) s2 && isSubset (setToList s2) s1

--------------------------------------------------------------------------------

s1 #+ s2 = listToSet ((setToList s1) ++ (setToList s2))

--------------------------------------------------------------------------------

s1 #* s2 = listToSet (intersection (setToList s1) s2)
-- intersection xs s : if a item of list 'xs' belong to set 's',then keep it.
intersection :: Eq a => [a] -> Set a -> [a]
intersection [] _ = []
intersection (x:xs) s
    | (x #? s) = x : (intersection xs s)
    | otherwise = intersection xs s

--------------------------------------------------------------------------------

s1 #- s2 = listToSet (difference (setToList (s1 #* s2)) (setToList (s1 #+ s2)))
difference :: Eq a => [a] -> [a] -> [a]
difference _ [] = []
difference [] [] =
difference (x:xs) (y:ys)
    | x == y = difference xs ys
    | x /= y = difference xs (y:ys)
    | otherwise = x : (difference xs ys)

--------------------------------------------------------------------------------


