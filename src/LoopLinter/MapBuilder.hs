{-# LANGUAGE LambdaCase #-}
module LoopLinter.MapBuilder where

import qualified Data.Map as Map
import qualified Data.Set as Set
import GHC.Core
import GHC.Plugins
import LoopLinter.Utils

buildProducerMap :: [(Id, CoreExpr)] -> Map.Map Id (Either Id Id)
buildProducerMap = foldl collectSource Map.empty
 where
  collectSource acc (lhs, expr) =
    case stripTicks expr of
      App _ _ ->
        case getAppHead expr of
          Just f -> Map.insert lhs (Right f) acc
          Nothing -> acc -- Should not happen for a valid App
      Case (Var scrut) _ _ _ -> Map.insert lhs (Left scrut) acc
      Var v -> Map.insert lhs (Left v) acc
      _ -> acc

findProducer :: (Ord a) => Map.Map a (Either a a) -> a -> Maybe a
findProducer varSources v = go v Set.empty
 where
  go curr visited
    | Set.member curr visited = Nothing -- Cycle in aliases TODO: this also catches a combinational loop
    | otherwise = case Map.lookup curr varSources of
        Nothing -> Nothing
        Just (Right func) -> Just func
        Just (Left alias) -> go alias (Set.insert curr visited)

collectCallees :: CoreExpr -> Set.Set Id
collectCallees = \case
  Var x -> Set.singleton x
  Lit l -> Set.empty
  App fun arg -> collectArgs fun `Set.union` collectCallees arg
  Lam b body -> collectCallees body
  Let bind body -> collectCalleesFromBind bind `Set.union` collectCallees body
  Case scrut b ty alts ->
    collectCallees scrut
      `Set.union` Set.unions (map (\(Alt _ _ e) -> collectCallees e) alts)
  Cast expr co -> collectCallees expr
  Tick tickish expr -> collectCallees expr
  _ -> Set.empty
 where
  collectArgs :: CoreExpr -> Set.Set Id
  collectArgs (Var x) = Set.empty
  collectArgs (App fun arg) = collectArgs fun `Set.union` collectCallees arg
  collectArgs _ = Set.empty

collectCalleesFromBind :: CoreBind -> Set.Set Id
collectCalleesFromBind = \case
  NonRec b rhs -> collectCallees rhs
  Rec pairs -> Set.unions (map (collectCallees . snd) pairs)

findPrimaryRecBindings :: CoreExpr -> [(Id, CoreExpr)]
findPrimaryRecBindings = \case
  Let (Rec pairs) _ -> pairs
  Let (NonRec _ _) body -> findPrimaryRecBindings body
  Lam _ body -> findPrimaryRecBindings body
  Cast expr _ -> findPrimaryRecBindings expr
  Tick _ expr -> findPrimaryRecBindings expr
  _ -> []
