module Graph ( Graph, Node, Edge,
               emptyGraph, nodes, edges, s2n, n2s,
               insertNode, deleteNode, insertEdge, deleteEdge, outEdges ) where

--------------------------------------------------------------------------------
-- I N T E R F A C E  :  P U B L I C
--------------------------------------------------------------------------------

-- Graph : a weighted directed graph

--------------------------------------------------------------------------------

-- Node : a node

--------------------------------------------------------------------------------

-- Edge : an edge

type Edge = ( Node, Node, Float )  -- ( start, finish, weight )

--------------------------------------------------------------------------------

-- emptyGraph : the empty graph

emptyGraph :: Graph

--------------------------------------------------------------------------------

-- nodes g : a list of the nodes in graph 'g'

nodes :: Graph -> [ Node ]

--------------------------------------------------------------------------------

-- edges g : a list of the edges in graph 'g'

edges :: Graph -> [ Edge ]

--------------------------------------------------------------------------------

-- s2n s : a node with label 's'

s2n :: String -> Node

--------------------------------------------------------------------------------

-- n2s n : the label of node 'n'

n2s :: Node -> String

--------------------------------------------------------------------------------

-- insertNode n g : the graph formed by inserting node 'n' into graph 'g'

insertNode :: Node -> Graph -> Graph

--------------------------------------------------------------------------------

-- deleteNode n g : the graph formed by deleting node 'n' from graph 'g'

deleteNode :: Node -> Graph -> Graph

--------------------------------------------------------------------------------

-- insertEdge e g : the graph formed by inserting edge 'e' into graph 'g'

insertEdge :: Edge -> Graph -> Graph

--------------------------------------------------------------------------------

-- deleteEdge e g : the graph formed by deleting edge 'e' from graph 'g'

deleteEdge :: Edge -> Graph -> Graph

--------------------------------------------------------------------------------

-- outEdges n g : a list of the edges starting from node 'n' in graph 'g'

outEdges :: Node -> Graph -> [ Edge ]

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- I M P L E M E N T A T I O N  :  P R I V A T E
--------------------------------------------------------------------------------

data Graph = G [(Node,[Edge])] deriving Show
data Node = N String deriving (Eq,Show)

-------------------------------------------------------------------------------

emptyGraph = G []

-------------------------------------------------------------------------------

nodes (G []) = []
nodes (G ((n, _):ns)) = n:nodes (G ns)

-------------------------------------------------------------------------------

edges (G []) = []
edges (G ((_, e):ns)) = e ++ edges (G ns)

-------------------------------------------------------------------------------

s2n s = N s

-------------------------------------------------------------------------------

n2s (N n) = n

-------------------------------------------------------------------------------

insertNode n (G g) = G (g ++ [(n, [])])

-------------------------------------------------------------------------------

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

-------------------------------------------------------------------------------

insertEdge e (G g) = G (insertEdge' e g) 
insertEdge' :: Edge -> [(Node,[Edge])] -> [(Node,[Edge])]
insertEdge' _ [] = []
insertEdge' ((a,b,c)) ((n,es):g) 
    | a == n = ((n,((a,b,c):es)):g)
    | otherwise = (n,es):insertEdge' (a,b,c) g

-------------------------------------------------------------------------------

deleteEdge e (G g) = G [((fst g'),(deleteEdge' e (snd g')))| g' <- g]
deleteEdge' :: Edge -> [Edge] -> [Edge]
deleteEdge' _ [] = []
deleteEdge' ((a,b,c)) ((a',b',c'): ns) 
    | a == a' && b == b' && c == c' = deleteEdge' (a,b,c) ns 
    | otherwise = (a',b',c') : (deleteEdge' (a,b,c) ns)

-------------------------------------------------------------------------------

outEdges n (G (g : gs)) = if n == fst g then snd g else outEdges n (G gs)

