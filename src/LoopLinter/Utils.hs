module LoopLinter.Utils where

import GHC.Core
import GHC.Plugins


stripTicks :: CoreExpr -> CoreExpr
stripTicks (Tick _ e) = stripTicks e
stripTicks (Cast e _) = stripTicks e
stripTicks e = e

getAppHead :: CoreExpr -> Maybe Id
getAppHead (App f _) = getAppHead f
getAppHead (Var f) = Just f
getAppHead (Tick _ e) = getAppHead e
getAppHead (Cast e _) = getAppHead e
getAppHead _ = Nothing

showEitherId :: Either Id Id -> String
showEitherId (Left i) = "Left " ++ showSDocUnsafe (ppr i)
showEitherId (Right i) = "Right " ++ showSDocUnsafe (ppr i)
