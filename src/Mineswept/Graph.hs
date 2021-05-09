module Mineswept.Graph
  ( LGraph,
    makeLGraph,
    reachable,
  )
where

import Data.Graph (Graph, Vertex, graphFromEdges)
import Data.Graph qualified as Graph

-- Labelled graph.
data LGraph a = LGraph
  { graph :: Graph,
    labelFromVertex :: Vertex -> (a, [a]),
    vertexFromLabel :: a -> Maybe Vertex
  }

makeLGraph :: (Ord a) => [(a, [a])] -> LGraph a
makeLGraph vs = LGraph {graph, labelFromVertex, vertexFromLabel}
  where
    vs' = (\(k, ns) -> (k, k, ns)) <$> vs
    (graph, labelFromVertex', vertexFromLabel) = graphFromEdges vs'
    labelFromVertex v = let (_, l, ns) = labelFromVertex' v in (l, ns)

reachable :: LGraph a -> a -> Maybe [a]
reachable LGraph {graph, labelFromVertex, vertexFromLabel} label = do
  vertex <- vertexFromLabel label
  let rs = Graph.reachable graph vertex
  pure $ fst . labelFromVertex <$> rs
