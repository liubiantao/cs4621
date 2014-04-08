splits :: [a] -> [([a], [a])]
splits xs = sp ((length xs)-1) xs
sp 1 xs = [splitAt 1 xs]
sp n xs = (sp (n-1) xs) ++ [splitAt n xs] 

t1 = [1,2,3,4,5]

commonPrefix :: (Eq e) => [e] -> [e] -> [e]
commonPrefix _ [] = []
commonPrefix [] _ = []
commonPrefix (x:xs) (y:ys)
  | x == y    = x : commonPrefix xs ys
  | otherwise = []

cpfx :: (Eq a) => [[a]] -> [a]
cpfx = foldl1 commonPrefix

t2= ["abc","ab", "abcd"] 

editstr [([x:y:z])] a
    | x == "len" = length a