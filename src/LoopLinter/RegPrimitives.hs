module LoopLinter.RegPrimitives(registerPrimitives) where

import GHC.Core
import GHC.Plugins

registerPrimitives :: [(ModuleName, OccName)]
registerPrimitives =
  [ (mkModuleName "Clash.Prelude", mkVarOcc "register")
  , (mkModuleName "Clash.Prelude", mkVarOcc "registerB")
  , (mkModuleName "Clash.Prelude", mkVarOcc "dflipflop")
  , (mkModuleName "Clash.Prelude", mkVarOcc "delay")
  , (mkModuleName "Clash.Prelude", mkVarOcc "delayEn")
  , (mkModuleName "Clash.Prelude", mkVarOcc "delayMaybe")
  , (mkModuleName "Clash.Prelude", mkVarOcc "regMaybe")
  , (mkModuleName "Clash.Prelude", mkVarOcc "regEn")
  , (mkModuleName "Clash.Prelude", mkVarOcc "autoReg")
  , (mkModuleName "Clash.Prelude", mkVarOcc "moore")
  , (mkModuleName "Clash.Prelude", mkVarOcc "mooreB")
  , (mkModuleName "Clash.Prelude", mkVarOcc "mealy")
  , (mkModuleName "Clash.Prelude", mkVarOcc "mealyB")
  , (mkModuleName "Clash.Prelude", mkVarOcc "mealyS")
  , (mkModuleName "Clash.Prelude", mkVarOcc "rom")  -- Memory also introduces delays
  , (mkModuleName "Clash.Prelude", mkVarOcc "blockRam")
  , (mkModuleName "Clash.Signal", mkVarOcc "register")  -- From Clash.Signal
  , (mkModuleName "Clash.Signal", mkVarOcc "delay")
  -- Add more if needed, e.g., from Clash.Explicit.Prelude or Internal
  ]
