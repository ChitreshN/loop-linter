{-# LANGUAGE LambdaCase #-}

module LoopLinter.Plugin (plugin) where

import Control.Monad (forM_, when)
import qualified Data.Map as Map
import qualified Data.Set as Set
import GHC.Core
import GHC.Plugins
import LoopLinter.HasReg
import qualified Data.Bifunctor


plugin :: Plugin
plugin =
  defaultPlugin
    { installCoreToDos = install
    , pluginRecompile = purePlugin
    }

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ todos =
  pure (CoreDoPluginPass "Loop Linter (noop)" analyzeModule : todos)

stripTicks :: CoreExpr -> CoreExpr
stripTicks (Tick _ e) = stripTicks e
stripTicks (Cast e _) = stripTicks e
stripTicks e = e

getAppHead :: CoreExpr -> Maybe Id
getAppHead (App f _) = getAppHead f
getAppHead (Var f)   = Just f
getAppHead (Tick _ e) = getAppHead e
getAppHead (Cast e _) = getAppHead e
getAppHead _ = Nothing

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

findProducer :: Map.Map Id (Either Id Id) -> Id -> Maybe Id
findProducer varSources v = go v Set.empty
  where
    go curr visited
      | Set.member curr visited = Nothing -- Cycle in aliases TODO: this also catches a combinational loop
      | otherwise = case Map.lookup curr varSources of
          Nothing -> Nothing
          Just (Right func) -> Just func
          Just (Left alias) -> go alias (Set.insert curr visited)

showEitherId :: Either Id Id -> String
showEitherId (Left i) = "Left " ++ showSDocUnsafe (ppr i)
showEitherId (Right i) = "Right " ++ showSDocUnsafe (ppr i)

collectCallees :: CoreExpr -> Set.Set Id
collectCallees = \case
    Var x           -> Set.singleton x
    Lit l           -> Set.empty
    App fun arg     -> collectArgs fun `Set.union` collectCallees arg
    Lam b body      -> collectCallees body
    Let bind body   -> collectCalleesFromBind bind `Set.union` collectCallees body
    Case scrut b ty alts -> collectCallees scrut `Set.union` Set.unions (map (\(Alt _ _ e) -> collectCallees e) alts)
    Cast expr co    -> collectCallees expr
    Tick tickish expr -> collectCallees expr
    Type ty         -> Set.empty
    Coercion co     -> Set.empty
  where
    collectArgs :: CoreExpr -> Set.Set Id
    collectArgs (Var x) = Set.empty
    collectArgs (App fun arg) = collectArgs fun `Set.union` collectCallees arg
    collectArgs _ = Set.empty

collectCalleesFromBind :: CoreBind -> Set.Set Id
collectCalleesFromBind = \case
    NonRec b rhs -> collectCallees rhs
    Rec pairs    -> Set.unions (map (collectCallees . snd) pairs)

findPrimaryRecBindings :: CoreExpr -> [(Id, CoreExpr)]
findPrimaryRecBindings = \case
  Let (Rec pairs) _ -> pairs
  Let (NonRec _ _) body -> findPrimaryRecBindings body
  Lam _ body -> findPrimaryRecBindings body
  Cast expr _ -> findPrimaryRecBindings expr
  Tick _ expr -> findPrimaryRecBindings expr
  _ -> []

analyzeModule :: ModGuts -> CoreM ModGuts
analyzeModule guts = do
  let binds = mg_binds guts
      moduleNameStr = moduleNameString (moduleName (mg_module guts))

  let topLevelPairs :: [(Id, CoreExpr)]
      topLevelPairs =
        [(b, e) | NonRec b e <- binds]
          ++ [p | Rec ps <- binds, p <- ps]

  let callGraph =
        Map.fromListWith
          Set.union
          [ (caller, collectCallees rhs)
          | (caller, rhs) <- topLevelPairs
          ]

  let directHasReg =
        Map.fromList [(caller, exprHasReg rhs) | (caller, rhs) <- topLevelPairs]

  liftIO $ do
    let targetName = "pentariscAlpha"
    forM_ topLevelPairs $ \(b, rhs) -> do
        let name = occNameString (getOccName b)
        when (name == targetName) $ do
            putStrLn $ "=== Analyzing Loops in " ++ targetName ++ " ==="

            -- 1. Identify Rec bindings
            let recBinds = findPrimaryRecBindings rhs
                producerMap = buildProducerMap recBinds
                callees = map (Data.Bifunctor.second collectCallees) recBinds
                calleeMap = Map.fromList callees

            if null recBinds
              then putStrLn "  No Rec bindings found (loop detection skipped)."
              else do
                putStrLn "--- producerMap ---"
                forM_ (Map.toList producerMap) $ \(lhs, rhsVal) ->
                  putStrLn $ showSDocUnsafe (ppr lhs) ++ " -> " ++ showEitherId rhsVal
                putStrLn "--- calleeMap ---"
                forM_ (Map.toList calleeMap) $ \(lhs, rhsVal) ->
                  putStrLn $ showSDocUnsafe (ppr lhs) ++ " -> " ++ showSDocUnsafe (ppr rhsVal)

  return guts
