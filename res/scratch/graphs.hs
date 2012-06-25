import qualified Data.Graph.Inductive as G
import qualified Data.Graph.Inductive.Query.BFS as B
import qualified Data.Graph.Inductive.NodeMap as NM
import Data.Graph.Inductive.Tree

type Graph = Gr Char String

g1 :: (NM.NodeMap Char, Graph) 
g1 = snd . NM.run G.empty $ do
    _ <- NM.insMapNodeM 'a'
    _ <- NM.insMapNodeM 'b'
    _ <- NM.insMapNodeM 'c'
    _ <- NM.insMapNodeM 'd'
    
    -- note, the nodes must already exist
    _ <- NM.insMapEdgeM ('a', 'b', "Edge_A")
    _ <- NM.insMapEdgeM ('b', 'c', "Edge_B")
    _ <- NM.insMapEdgeM ('c', 'a', "Edge_C")
    _ <- NM.insMapEdgeM ('a', 'd', "Edge_D")
    
    return ()

(nm1, graph1) = g1

g2 :: (NM.NodeMap Char, Graph) 
g2 = snd . NM.run G.empty $ do
    _ <- NM.insMapNodeM 'a'
    _ <- NM.insMapNodeM 'b'
    _ <- NM.insMapNodeM 'c'
    _ <- NM.insMapNodeM 'd'
    
    -- note, the nodes must already exist
    _ <- NM.insMapEdgeM ('a', 'b', "Edge_A")
    _ <- NM.insMapEdgeM ('b', 'c', "Edge_B")
    _ <- NM.insMapEdgeM ('c', 'a', "Edge_C")
    _ <- NM.insMapEdgeM ('a', 'd', "Edge_D")
    _ <- NM.insMapEdgeM ('a', 'c', "Edge_E")
    
    return ()

(nm2, graph2) = g2


-- path
path1 = B.lesp (getNodeInt graph1 nm1 'a') (getNodeInt graph1 nm1 'c') graph1
path2 = B.lesp (getNodeInt graph2 nm2 'a') (getNodeInt graph2 nm2 'c') graph2

getEdgesFromPath :: (G.LPath a) -> [a]
getEdgesFromPath (G.LP path) = map snd $ tail path

-- wrapper around the mkNode to lookup a node, assumes node already exists
-- getNode a :: NodeMap a -> a -> Int
getNodeInt' g nm n = if G.gelem intVal g then Just intVal else Nothing
  where
    intVal = fst . NM.mkNode_ nm $ n

getNodeInt g nm n = intVal
  where
    intVal = fst . NM.mkNode_ nm $ n

