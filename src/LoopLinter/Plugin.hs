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

  -- Define the check function
  let hasRegCheck :: Id -> Bool
      hasRegCheck i = 
        idHasRegister i ||
        case Map.lookup i directHasReg of
          Just v -> v
          Nothing -> Map.findWithDefault False (idToKey i) globalRegistry

  putMsg $ text "Loop Linter running on module" <+> text moduleNameStr

  forM_ topLevelPairs $ \(b, rhs) -> do
    let (producerMap, calleeMap) = makeGraph rhs
        combiGraph = breakEdges producerMap calleeMap hasRegCheck
        loops = detectLoops combiGraph

    when (not (null loops)) $ do
      let recBinds = findPrimaryRecBindings rhs
          bindMap = Map.fromList recBinds
          msg = vcat
            [ text "Combinational Loop detected in" <+> ppr b
            , nest 2 $ vcat (map (formatLoop bindMap producerMap) loops)
            ]
      putMsg msg

  return guts

formatLoop :: Map.Map Id CoreExpr -> Map.Map Id (Either Id Id) -> [Id] -> SDoc
formatLoop bindMap producerMap loop =
  vcat [ text "Cycle detected:"
       , nest 4 $ vcat (map formatNode loop)
       ]
 where
  formatNode node =
    let loc = getSrcSpan node
        producer = case findProducer producerMap node of
                     Just f -> text " [Produced by:" <+> ppr f <> text "]"
                     Nothing -> empty
    in ppr loc <> colon <+> ppr node <> producer