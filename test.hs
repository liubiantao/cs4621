module Graph ( Graph, Node, Edge,emptyGraph,nodes,edges,deleteNode,s2n,n2s,insertNode) where

type Edge = ( Node, Node, Float )  -- ( start, finish, weight )


emptyGraph :: Graph

data Graph = G [(Node,[Edge])] deriving Show
data Node = N String deriving (Eq,Show)

emptyGraph = G []

nodes :: Graph -> [ Node ]

nodes (G []) = []
nodes (G ((n, _):ns)) = n:nodes (G ns)

-- edges g : a list of the edges in graph 'g'

edges :: Graph -> [ Edge ]

edges (G []) = []
edges (G ((_, e):ns)) = e ++ edges (G ns)
-- s2n s : a node with label 's'

s2n :: String -> Node
s2n s = N s

-- n2s n : the label of node 'n'

n2s :: Node -> String
n2s (N n) = n


--insertEdge :: Edge -> Graph -> Graph


--insertEdge ((s,f,w)) (G []) = G ((s,((s,f,w):[])):[])
--insertEdge ((s,f,w)) (G ((n,nx):ns)) = if n==s then G ((n,((s,f,w):nx)):ns) else insertEdge ((s,f,w)) (G ns)

-- insertNode n g : the graph formed by inserting node 'n' into graph 'g'

insertNode :: Node -> Graph -> Graph
insertNode n (G g) = G (g ++ [(n, [])])


-- deleteNode n g : the graph formed by deleting node 'n' from graph 'g'

deleteNode :: Node -> Graph -> Graph

--deleteNode dn emptyGraph = emptyGraph
--deleteNode dn (G g) = G (deleteNode' dn g)
deleteNode n (G g) = G [((fst g'),(delEAroundN n (snd g')))| g' <- deleteNode' n g ]
deleteNode' :: Node -> [(Node,[Edge])] -> [(Node,[Edge])]
deleteNode' _ [] = []
deleteNode' dn ((n,t):ns)
    | dn == n = ns
    | otherwise = (n,t):deleteNode' dn ns
matchNInE :: Node -> Edge -> Bool
matchNInE dn (a,b,_) = if (dn == a) || ( dn == b) then True else False
delEAroundN :: Node -> [Edge] -> [Edge]
delEAroundN _ [] = []
delEAroundN dn (e:es) 
    | matchNInE dn e == True = delEAroundN dn es
    | otherwise = e:delEAroundN dn es



------------------------------------------------
insertEdge e (G g) = G (insertEdge' e g) 

insertEdge' :: Edge -> [(Node,[Edge])] -> [(Node,[Edge])]
insertEdge' _ [] = []
insertEdge' ((a,b,c)) ((n,es):g) 
    | a == n = ((n,((a,b,c):es)):g)
    | otherwise = (n,es):insertEdge' (a,b,c) g

--------------------------- ---------------------
--deleteEdge' e ex : delete the edges e from the list of Edge ex
deleteEdge e (G g) = G [((fst g'),(deleteEdge' e (snd g')))| g' <- g]
deleteEdge' :: Edge -> [Edge] -> [Edge]
deleteEdge' _ [] = []
deleteEdge' ((a,b,c)) ((a',b',c'): ns) 
    | a == a' && b == b' && c == c' = deleteEdge' (a,b,c) ns 
    | otherwise = (a',b',c') : (deleteEdge' (a,b,c) ns)


outEdges n (G (g : gs)) = if n == fst g then snd g else outEdges n (G gs)




--------------------------------------
n1= N "A"  
n2= N "B"
n3= N "C"
n4= N "D"
tt= (n1,n3)
ac= (n1,n3,2.0)
ba= (n2,n1,1.0)
bc :: Edge
bc= (n2,n3,3.0)
ad=(n1,n4,3.0)
cd=(n3,n4,1.0)
da=(n4,n1,3.0)
db=(n4,n2,2.0)
g1 = G [( n1,[ac,ad]),(n2,[ba]),(n3,[cd]),(n4,[da,db]) ]
g2 = G [( n1,[ac,ad]),(n2,[ba]),(n3,[cd]),(n4,[]) ]
t1 = n2s (s2n "sdf")