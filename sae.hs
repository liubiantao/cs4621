a = [1..31]
b [] = []
b (x:xs) = mod (x*x) 17 : (b xs)
