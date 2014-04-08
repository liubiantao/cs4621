-- Haitao Yin 112123790
data BST a = EmptyBST | Node (BST a) a (BST a) deriving Show
-- 1. numLeaves bst
-- The number of leaves in the BST 'bst'.
numLeaves :: BST a -> Int
numLeaves EmptyBST = 0
numLeaves (Node EmptyBST _ EmptyBST) = 1
numLeaves (Node l _ r) = numLeaves l + numLeaves r

-- 2. minValue bst
-- The miniumum value stored in the non-empty BST 'bst'
minValue :: BST a -> a 
minValue (Node EmptyBST v _) = v
minValue (Node l v _) = minValue l

-- 3. maxValue bst
-- The maxValue value stored in the non-empty BST 'bst'
maxValue :: BST a -> a
maxValue (Node _ v EmptyBST) = v
maxValue (Node _ v r) = maxValue r

-- 4. height bst
-- The height of the BST 'bst'
height :: BST a -> Int
height EmptyBST = 0
height (Node l _ r) = max (height l) (height r) + 1

-- 5. occurs v bst: Is the value 'v' stored at any node of the BST 'bst'?
occurs :: Ord a => a -> BST a -> Bool
occurs x EmptyBST = False
occurs x (Node l v r) 
    | x == v = True 
    | x < v  = occurs x l 
    | x > v  = occurs x r

-- 6. mirror bst
-- The mirror image of the BST 'bst', 
-- in which all left and right subtrees have been swapped
mirror :: BST a -> BST a
mirror EmptyBST = EmptyBST
mirror (Node l v r) = Node (mirror r) v (mirror l) 

-- 7. treeSort list
-- A copy of the list 'list', with its items arranged into ascending order
treeSort :: Ord a => [a]->[a]
treeSort list = treeToList ( listToTree list )

-- add x to a BST
add :: Ord a => a -> BST a -> BST a
add x EmptyBST = (Node EmptyBST x EmptyBST)
add x (Node l v r)
   | x > v     = Node l v (add x r)
   | otherwise = Node (add x l) v r

-- listToTree xs 
-- build a BST from the items in 'list'
listToTree :: Ord a => [a] -> BST a
listToTree [] = EmptyBST
listToTree (x:xs) = add x (listToTree xs)

-- treeToList tree
-- traverse BST to a list
treeToList :: BST a -> [a]
treeToList EmptyBST = []
treeToList (Node l v r) = treeToList l ++ v : treeToList r
