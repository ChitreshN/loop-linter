module LoopLinter.Plugin (plugin) where

import Control.Monad (forM_, when)
import qualified Data.Bifunctor
import qualified Data.Map as Map
import qualified Data.Set as Set
import GHC.Core
import GHC.Plugins
import LoopLinter.HasReg
import LoopLinter.Utils
import LoopLinter.MapBuilder
import LoopLinter.Graph
import Data.Maybe (fromMaybe)

plugin :: Plugin
plugin =
  defaultPlugin
    { installCoreToDos = install
    , pluginRecompile = purePlugin
    }

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ todos =
  pure (CoreDoPluginPass "Loop Linter (noop)" analyzeModule : todos)


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

  -- this should load data from files
  let directHasReg =
        Map.fromList [(caller, exprHasReg rhs) | (caller, rhs) <- topLevelPairs]

  liftIO $ do
    let targetName = "pentariscAlpha"
    forM_ topLevelPairs $ \(b, rhs) -> do
      let name = occNameString (getOccName b)
      putStrLn $ "=== Analyzing Loops in " ++ targetName ++ " ==="

        -- 1. Identify Rec bindings
      let recBinds = findPrimaryRecBindings rhs
          (producerMap, calleeMap) = makeGraph rhs
          combiGraph = breakEdges producerMap calleeMap directHasReg
          loops = detectLoops combiGraph

      if null loops
        then putStrLn "  No Loops found (loop detection skipped)."
        else do
          -- putStrLn "--- Producer Map ---"
          -- forM_ (Map.toList producerMap) $ \(lhs, rhsVal) ->
          --   putStrLn $ showSDocUnsafe (ppr lhs) ++ " -> " ++ showEitherId rhsVal
          -- putStrLn "--- Callee Map ---"
          -- forM_ (Map.toList calleeMap) $ \(lhs, rhsVal) ->
          --   putStrLn $ showSDocUnsafe (ppr lhs) ++ " -> " ++ showSDocUnsafe (ppr rhsVal)
          -- putStrLn "--- Combi Graph ---"
          -- forM_ (Map.toList combiGraph) $ \(lhs, rhsVal) ->
          --   putStrLn $ showSDocUnsafe (ppr lhs) ++ " -> " ++ showSDocUnsafe (ppr rhsVal)
          putStrLn "--- Loops ---"
          forM_ loops $ \loop -> do
            putStrLn "  Cycle detected:"
            forM_ loop $ \node -> do
              let name = occNameString (getOccName node)
                  loc = getSrcSpan node
                  locStr = showSDocUnsafe (ppr loc)
                  func = findProducer producerMap node
                  funcName = fmap (occNameString . getOccName) func
              putStrLn $ "    -> " ++ name ++ " (" ++ locStr ++ ")"
              putStrLn $ "      Produced by: " ++ fromMaybe "(unknown)" funcName


  return guts
