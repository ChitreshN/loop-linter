module LoopLinter.Graph where

import qualified Data.Bifunctor
import qualified Data.Map as Map
import qualified Data.Set as Set
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
  Map.Map a Bool -> 
  Map.Map a (Set.Set a)
breakEdges producerMap calleeMap hasReg = updatedMap
 where
  updatedMap = Map.mapWithKey helper calleeMap
  helper k v = edges
    where
      edges = case findProducer producerMap k of
        Nothing -> v
        Just f -> case hasReg Map.!? f of
          Just True -> Set.empty -- remove edges if function has a reg
          Just False -> v
          Nothing -> v

detectLoops :: (Ord a) => Map.Map a (Set.Set a) -> [[a]]
detectLoops graph = filter (not . null) $ snd $ foldl go (Set.empty, []) (Map.keys graph)
 where
  go (visited, cycles) node
    | Set.member node visited = (visited, cycles)
    | otherwise =
        let (visited', newCycles) = findCycles Set.empty [] visited node
         in (visited', cycles ++ newCycles)

  findCycles stack currentPath visited node
    | Set.member node stack =
        -- Found a cycle in the current recursion stack
        let cycle = reverse $ node : takeWhile (/= node) currentPath
         in (visited, [cycle])
    | Set.member node visited = (visited, []) -- Already fully explored
    | otherwise =
        let stack' = Set.insert node stack
            path' = node : currentPath
            neighbors = Map.findWithDefault Set.empty node graph
            (visited'', childCycles) =
              foldl
                ( \(v, cs) n ->
                    let (v', cs') = findCycles stack' path' v n
                     in (v', cs ++ cs')
                )
                (visited, [])
                (Set.toList neighbors)
         in (Set.insert node visited'', childCycles)
