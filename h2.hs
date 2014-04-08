import Data.List

data BST = MakeNode BST String BST | Empty deriving Show


add :: String -> BST -> BST
add new Empty = (MakeNode Empty new Empty)
add string tree@(MakeNode left value right)
    | string > value = MakeNode left value (add string right)
    | string < value = MakeNode (add string left) value right
    | otherwise = tree

test = ["alice", "blup", "test", "aa"]