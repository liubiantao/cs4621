data BST a = EmptyBST | Node (BST a) a (BST a) deriving Show


-- numLeaves bst: The number of leaves in the BST 'bst'
numNodes :: BST a -> Int
numNodes EmptyBST = 0
numNodes (Node EmptyBST _ EmptyBST) = 1
numNodes (Node lSub _ rSub) = numNodes lSub + 0 + numNodes rSub


-- minValue bst: The miniumum value stored in the non-empty BST 'bst'
minValue :: BST a -> a 
minValue (Node EmptyBST v _) = v
minValue (Node lSub _ _) = minValue lSub


-- maxValue bst: The maxValue value stored in the non-empty BST 'bst'
maxValue :: BST a -> a 
maxValue (Node _ v EmptyBST) = v
maxValue (Node _ _ rSub) = maxValue rSub


-- height bst: The height of the BST 'bst'
height :: BST a -> Int
height EmptyBST = 0
height (Node lSub _ rSub) = max (height lSub + 1) (height rSub + 1)


-- occurs v bst: Is the value 'v' stored at any node of the BST 'bst'?
occurs :: Ord a => a -> BST a -> Bool
--occurs v EmptyBST = False
occurs v (Node lSub node rSub)
	| (v == node) = True
	| (v < node) = occurs v lSub
	| (v > node) = occurs v rSub


-- mirror bst: The mirror image of the BST 'bst', in which all left and right subtrees have been swapped
mirror :: BST a -> BST a
mirror EmptyBST = EmptyBST
mirror (Node lSub node rSub) = Node (mirror rSub) node (mirror lSub)


-- treeSort list: A copy of the list 'list', with its items arranged into ascending order
treeSort :: Ord a => [a] -> [a]
treeSort list = traverseBST (generateBST list)
-- traverseBST bst: A helper function for generating the sorted list by traversing BST
traverseBST :: BST a -> [a]
traverseBST EmptyBST = []
traverseBST (Node lSub node rSub) = traverseBST lSub ++ node : traverseBST rSub
-- generateBST list: A helper function for generating a BST from a arbitrary list
generateBST :: Ord a => [a] -> BST a
generateBST [] = EmptyBST
generateBST (x:xs) = Node (generateBST lSub) x (generateBST rSub)
  where 
    lSub = filter (<= x) xs
    rSub = filter (>= x) xs

t0 = Node (Node EmptyBST 5 EmptyBST) 6 (Node EmptyBST 7 EmptyBST)
t1 = Node (Node EmptyBST 4 EmptyBST) 6 EmptyBST
t2 = Node (Node EmptyBST 3 EmptyBST) 6 (Node EmptyBST 7 (Node EmptyBST 8 EmptyBST))
t4 = Node EmptyBST 5 EmptyBST
tl= [4,3,6,2,8]