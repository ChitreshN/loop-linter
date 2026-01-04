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
import LoopLinter.Registry (loadRegistry, saveRegistry, Registry, idToKey)
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

  -- Compute local register information
  let directHasReg =
        Map.fromList [(caller, exprHasReg rhs) | (caller, rhs) <- topLevelPairs]

  -- Save local registry to disk
  liftIO $ do
    let localRegMap = Map.fromList 
          [ (idToKey k, v) | (k, v) <- Map.toList directHasReg ]
    saveRegistry moduleNameStr localRegMap

  -- Load global registry (including what we just saved, potentially, or from dependencies)
  globalRegistry <- liftIO loadRegistry

  -- Define the check function
  let hasRegCheck :: Id -> Bool
      hasRegCheck i = 
        case Map.lookup i directHasReg of
          Just v -> v
          Nothing -> Map.findWithDefault False (idToKey i) globalRegistry

  liftIO $ do
    forM_ topLevelPairs $ \(b, rhs) -> do
      let name = occNameString (getOccName b)
      putStrLn $ "=== Analyzing Loops in " ++ name ++ " ==="

      -- 1. Identify Rec bindings
      let recBinds = findPrimaryRecBindings rhs
          bindMap = Map.fromList recBinds
          (producerMap, calleeMap) = makeGraph rhs
          combiGraph = breakEdges producerMap calleeMap hasRegCheck
          loops = detectLoops combiGraph

      if null loops
        then putStrLn $ "  No Loops found in " ++ name ++ "."
        else do
          putStrLn "--- Loops ---"
          forM_ loops $ \loop -> do
            putStrLn "  Cycle detected:"
            forM_ loop $ \node -> do
              let name' = occNameString (getOccName node)
                  loc = getSrcSpan node
                  locStr = showSDocUnsafe (ppr loc)
                  func = findProducer producerMap node
                  funcName = fmap (occNameString . getOccName) func
              putStrLn $ "    -> " ++ name' ++ " (" ++ locStr ++ ")"
              putStrLn $ "      Produced by: " ++ fromMaybe "(unknown)" funcName




  return guts
