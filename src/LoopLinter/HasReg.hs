module LoopLinter.HasReg where

import GHC.Core
import GHC.Plugins
import LoopLinter.RegPrimitives

exprHasReg :: CoreExpr -> Bool
exprHasReg expr = go expr
 where
  go (Var v)
    | isId v =
        let name = idName v
         in case nameModule_maybe name of
              Just mod ->
                let occ = getOccName name
                 in any (\(m, o) -> occ == o) registerPrimitives -- Note: we are plainly checking for function names here
                                                                 -- if a function developed internally has the same name as 
                                                                 -- one of the primitives here, it will be false negative
              Nothing -> False
  go (Var _) = False
  go (Lit _) = False
  go (App e1 e2) = go e1 || go e2
  go (Lam _ e) = go e
  go (Let bind body) =
      case bind of
        NonRec _ rhs -> go rhs || go body
        Rec pairs    -> any (go . snd) pairs || go body
  go (Case scrut _ _ alts) =
      go scrut || any (\(Alt _ _ rhs) -> go rhs) alts
  go (Cast e _) = go e
  go (Tick _ e) = go e
  go (Type _) = False
  go (Coercion _) = False
