{-# LANGUAGE LambdaCase #-}
module LoopLinter.Plugin (plugin) where

import Prelude hiding ((<>))
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

  forM_ topLevelPairs $ \(b, rhs) -> do
    -- 1. Find the local recursive bindings for the current top-level function
    let recBinds = findPrimaryRecBindings rhs
    
    -- 2. Create a hasReg map for these local bindings. This is the crucial step.
    --    `localExprHasReg` will check the RHS of each local binding against primitives,
    --    the top-level registry, and the global registry.
    let localHasRegMap = Map.fromList 
          [ (binder, localExprHasReg directHasReg globalRegistry expr) 
          | (binder, expr) <- recBinds 
          ]

    -- 3. Define a final, comprehensive hasReg check for this specific analysis.
    let hasRegCheck :: Id -> Bool
        hasRegCheck i =
          -- Check primitives first (e.g., `register`)
          idHasRegister i
          -- Then check if it's a local binding we just analyzed
          || Map.findWithDefault False i localHasRegMap
          -- Then check if it's a top-level binding from the current module
          || Map.findWithDefault False i directHasReg
          -- Finally, check the global registry for other modules
          || Map.findWithDefault False (idToKey i) globalRegistry

    -- 4. Build and analyze the graph
    let (producerMap, calleeMap) = makeGraph rhs
        combiGraph = breakEdges producerMap calleeMap hasRegCheck
        loops = detectLoops combiGraph

    when (not (null loops)) $ liftIO $ do
      let bindMap = Map.fromList recBinds
          msg = vcat
            [ text "Module:" <+> text moduleNameStr
            , text "Combinational Loop detected in" <+> ppr b
            , nest 2 $ vcat (map (formatLoop bindMap producerMap) loops)
            , text "---------------------------------------"
            ]
      appendFile "loop-linter.log" (showSDocUnsafe msg ++ "\n")

  return guts

formatLoop :: Map.Map Id CoreExpr -> Map.Map Id (Either Id Id) -> [Id] -> SDoc
formatLoop bindMap producerMap loop =
  vcat [ text "Cycle detected:"
       , nest 4 $ vcat (map formatNode loop)
       ]
 where
  formatNode node =
    let loc = getSrcSpan node
        name = occNameString (getOccName node)
        producer = case findProducer producerMap node of
                     Just f -> text " [Produced by:" <+> ppr f <> text "]"
                     Nothing -> empty
    in ppr loc <> colon <+> text name <> producer
