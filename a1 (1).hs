data BST a = EmptyBST | Node ( BST a ) a ( BST a ) deriving Show
-----------------------------------------------------------------
--leaf v : the single-node BST storing the Value 'v'
leaf :: a -> BST a
leaf v = Node EmptyBST v EmptyBST
-----------------------------------------------------------------
t1 :: BST Int
t1 = Node ( Node ( leaf 1) 2 ( leaf 3) ) 
         4 
           ( Node EmptyBST 5 ( Node ( leaf 6 ) 7 EmptyBST)
         )
-----------------------------------------------------------------
--numLeaves bst:count leaves in the BST 'bst'
numLeaves:: BST a -> Int
numLeaves EmptyBST = 0
numLeaves ( Node EmptyBST _ EmptyBST ) = 1
numLeaves ( Node lSub _ rSub ) = numLeaves lSub + numLeaves rSub
-----------------------------------------------------------------
--minValue bst: find the minimum element in the non-empty BST 'bst' 
minValue :: BST a ->a
minValue (Node EmptyBST root _) = root
minValue (Node lSub root _ ) = minValue (lSub)
----------------------------------------------------------------
--maxValue bst:find the minimum element in the non-empty BST 'bst'
maxValue :: BST a ->a
maxValue (Node _ root EmptyBST) = root
maxValue (Node _ root rSub ) = maxValue (rSub)
-----------------------------------------------------------------
--height bst: the deep of BST 'bst'
height ::(Num a, Ord a) => BST t ->a
height EmptyBST = 0
height (Node lSub root rSub) = 1 + (max (height lSub) (height rSub))
---------------------------------------------------------------------
--occurs v bst: judge whether the v appears in the BST 'bst'
occurs:: Eq a => a -> BST a -> Bool
occurs v EmptyBST = False 
occurs v (Node lSub root rSub) = v==root || occurs v (lSub) ||occurs v (rSub)
-------------------------------------------------------------------------------
--mirror: swap all left and right subtrees
mirror :: BST a => BST a
mirror (Node EmptyBST root EmptyBST) = (Node EmptyBST root EmptyBST)
mirror (Node lSub root rSub) =mirror' (Node (mirror (lSub)) root (mirror' (rSub)))
mirror' :: BST a => BST a
mirror'(Node lSub root rSub) = (Node rSub root lSub)
-------------------------------------------------------------------------------
--insert v bst :the BST formed by inserting the value 'v' into the BST 'bst'
insert :: Ord a => a -> BST a -> BST a
insert v EmptyBST  = (Node EmptyBST v EmptyBST) 
insert v (Node lSub root rSub) = if v < root then 
	                               Node ( insert v lSub) root rSub
	                               else 
	                               Node lSub root (insert v rSub)

--listToTree xs : build a BST from the items in 'list'
listToTree :: Ord a => [a] -> BST a
listToTree [] = EmptyBST
listToTree (x:xs) = insert x (listToTree xs)
--treeToList bst :change the BST to a ascending order
treeToList :: BST a -> [a]
treeToList EmptyBST =[]
treeToList (Node EmptyBST root EmptyBST) = [root]
treeToList (Node lSub root rSub) = treeToList (lSub) ++ root : treeToList (rSub)
--treeSort xs : the items of the list xs is an ascending order
treeSort :: Ord a => [a]->[a]
treeSort xs = treeToList ( listToTree xs )




 



                                        
                                    