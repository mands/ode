import qualified Data.Graph.Inductive as G
import qualified Data.Graph.Inductive.Query.BFS as B
import qualified Data.Graph.Inductive.NodeMap as NM
import Data.Graph.Inductive.Tree

import Utils.Graph as UG

import Control.Conditional
import Control.Applicative

type Graph = Gr Char String
type GM = UG.GraphMap Char String

g3a :: GM
g3a = UG.runGraph_ UG.mkGraphMap $ do
    _ <- NM.insMapNodeM 'a'
    _ <- NM.insMapNodeM 'b'
    _ <- NM.insMapEdgeM ('a', 'b', "Edge_A")
    return ()

g3b :: GM
g3b = UG.runGraph_ g3a $ do
    -- note, the nodes must already exist
    _ <- UG.insertNodeM_ 'b'
    _ <- UG.insertNodeM_ 'a'
    _ <- NM.insMapEdgeM ('b', 'a', "Edge_B")
    return ()


