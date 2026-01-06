module LoopLinter.Graph where

import qualified Data.Bifunctor
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Graph as G
import GHC.Core
import GHC.Plugins
import LoopLinter.MapBuilder

makeGraph :: CoreExpr -> (Map.Map Id (Either Id Id), Map.Map Id (Set.Set Id))
makeGraph expr = (producerMap, calleeMap)
 where
  recBinds = findPrimaryRecBindings expr
  producerMap = buildProducerMap recBinds
  callees = map (Data.Bifunctor.second collectCallees) recBinds
  calleeMap = Map.fromList callees

breakEdges ::
  (Ord a) =>
  Map.Map a (Either a a) ->
  Map.Map a (Set.Set a) ->
  (a -> Bool) ->
  Map.Map a (Set.Set a)
breakEdges producerMap calleeMap hasRegCheck = updatedMap
 where
  updatedMap = Map.mapWithKey helper calleeMap
  helper k v = 
    let edgeList = Set.toList v
        reg = any hasRegCheck edgeList
    in if reg then Set.empty else v

detectLoops :: (Ord a) => Map.Map a (Set.Set a) -> [[a]]
detectLoops graph = [vs | G.CyclicSCC vs <- sccs]
 where
  adjList = [(n, n, Set.toList neighbors) | (n, neighbors) <- Map.toList graph]
  sccs = G.stronglyConnComp adjList
