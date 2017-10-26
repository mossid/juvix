module Juvix.Transpiler.SimplifyExpr (
  simplifyExpr
) where

import           Control.Monad.RWS.Strict
import           Foundation

import qualified Juvix.Michelson.Script   as M
import           Juvix.Transpiler.Utility
import           Juvix.Types
import           Juvix.Utility

{-  Stage 2 : Expression Simplification

    Rewrite the intermediary expression form in ways more conducive to Michelson transformation.

    - Eliminate unnecessary/equivalent function calls, e.g. rewrite (\x → x) y ⇒ y.
    - Minimize requisite stack manipulation to reorder parameters, e.g. rewrite (\x y → f y x) to f (BuiltIn SwapUT)    -}

simplifyExpr ∷ Expr → CompilerM Expr
simplifyExpr expr = do
  let inner e = do
        one ← simplifyExpr' e
        two ← simplifyExpr' one
        if one == two then return two else inner two
  inner expr

simplifyExpr' ∷ Expr → CompilerM Expr
simplifyExpr' expr = do
  let tellReturn ∷ Expr → CompilerM Expr
      tellReturn ret = tell [SimplifiedExpr expr ret] >> return ret
  case expr of

    {- Literal casts. -}
    App (BuiltIn "tezFromInteger") (Lit (LInt i)) → do
      tellReturn (Lit (LTez i))
    App (BuiltIn "intFromInteger") (Lit (LInt i)) → do
      tellReturn (Lit (LInt i))
    App (BuiltIn "natFromInteger") (Lit (LNat i)) → do
      -- check > 0?
      tellReturn (Lit (LNat i))

    {- IO monad (specialized to prevent accidental optimization) -}
    App (BuiltIn "ReturnIO") x        → do
      x ← simplifyExpr' x
      return (ReturnIO x)
    App (App (BuiltIn "SeqIO") x) y   → do
      x ← simplifyExpr' x
      y ← simplifyExpr' y
      return (SeqIO x y)
    App (App (BuiltIn "BindIO") x) y  → do
      x ← simplifyExpr' x
      y ← simplifyExpr' y
      return (BindIO x y)

    Let var bind expr → do
      bind ← simplifyExpr' bind
      expr ← simplifyExpr' expr
      return (Let var bind expr)
    App x y → do
      x ← simplifyExpr' x
      y ← simplifyExpr' y
      return (App x y)

    Lam a (App e (Var b)) | a == b && not (uses a e) → do
      inner ← simplifyExpr' e
      tellReturn inner
    Lam a (Lam b (App (App e (Var c)) (Var d))) | a == d && b == c && not (uses a e) && not (uses b e) → do
      inner ← simplifyExpr' e
      return (App inner (BuiltIn "SwapUT"))

    Lam v e → do
      e ← simplifyExpr' e
      return (Lam v e)

    Case (Var v) _ (M.PairT _ _) [CaseOption _ [Just x, Just y] (App (App (BuiltIn "ConsPairUT") (Var y')) (Var x'))] | x' == x && y' == y → do
      tellReturn (App (App (BuiltIn "ConsPairUT") (App (BuiltIn "CdrUT") (App (BuiltIn "SwapUT") (App (BuiltIn "CarUT") (BuiltIn "DupUT"))))) (Var v))

    Case e b t os → do
      e ← simplifyExpr e
      os ← flip mapM os $ \case
        DefaultCase e → DefaultCase |<< simplifyExpr' e
        CaseOption d b e → CaseOption d b |<< simplifyExpr' e
      let bnw = case b of
                  Just b  → if any (uses b . caseExpr) os then Just b else Nothing
                  Nothing → Nothing
      return (Case e bnw t os)

    BindIO x y → do
      x ← simplifyExpr' x
      y ← simplifyExpr' y
      return (BindIO x y)
    SeqIO x y → do
      x ← simplifyExpr' x
      y ← simplifyExpr' y
      return (SeqIO x y)
    ReturnIO x → do
      x ← simplifyExpr' x
      return (ReturnIO x)

    other                      → return other
