{-# LANGUAGE LambdaCase #-}

module LoopLinter.Plugin (plugin) where

import GHC.Core
import GHC.Plugins
import LoopLinter.HasReg

plugin :: Plugin
plugin =
  defaultPlugin
    { installCoreToDos = install
    , pluginRecompile = purePlugin
    }

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ todos =
  -- run BEFORE simplification so that names are available (no compiler generated names)
  pure (CoreDoPluginPass "Loop Linter (noop)" pass : todos)

pass :: ModGuts -> CoreM ModGuts
pass guts = do
  putMsg $
    text "=== Loop Linter: running on module ==="
      <+> ppr (mg_module guts)
  let binds = mg_binds guts
      moduleN = moduleNameString (moduleName (mg_module guts))

  mapM_ (analyzeBind moduleN) binds

  pure guts

analyzeBind :: String -> CoreBind -> CoreM ()
analyzeBind modName = \case
  NonRec b rhs -> do
    let name = getOccName b
        hasReg = exprHasReg rhs
    liftIO $
      putStrLn $
        unwords
          [ modName ++ "." ++ occNameString name
          , "::"
          , if hasReg then "hasReg" else "noReg"
          ]
  Rec pairs -> do
    mapM_
      ( \(binder, rhs) -> do
          let name = getOccName binder
              hasReg = exprHasReg rhs
          liftIO $
            putStrLn $
              unwords
                [ modName ++ "." ++ occNameString name
                , "::"
                , if hasReg then "hasReg" else "noReg"
                ]
      )
      pairs
