module Juvix.Transpiler.Utility where

import           Data.List            (elemIndex)
import qualified Data.Text            as T
import           Foundation

import qualified Juvix.Michelson      as M
import qualified Juvix.Transpiler.GHC as GHC
import           Juvix.Types

{-  Whether an expression uses the value of a variable. Assumes uniqueness of names.   -}

uses ∷ T.Text → Expr → Bool
uses var expr =
  case expr of
    BuiltIn _     → False
    Lit _         → False
    Var v         → v == var
    Let _ x y     → uses var x || uses var y
    App x y       → uses var x || uses var y
    Lam _ x       → uses var x
    Case s _ _ cs → uses var s || any (uses var . caseExpr) cs
    BindIO x y    → uses var x || uses var y
    SeqIO x y     → uses var x || uses var y
    ReturnIO x    → uses var x

caseExpr ∷ CaseOption → Expr
caseExpr (DefaultCase e)    = e
caseExpr (CaseOption _ _ e) = e

rearrange ∷ Int → M.ExprUT
rearrange 0 = M.NopUT
rearrange 1 = M.SwapUT
rearrange n = M.SeqUT (M.DipUT (rearrange (n - 1))) M.SwapUT

unrearrange ∷ Int → M.ExprUT
unrearrange 0 = M.NopUT
unrearrange 1 = M.SwapUT
unrearrange n = M.SeqUT M.SwapUT (M.DipUT (rearrange (n - 1)))

position ∷ T.Text → StackRep → Maybe Int
position n = elemIndex (BoundVariable n)

dropFirst ∷ T.Text → StackRep → StackRep
dropFirst n (x:xs) = if x == BoundVariable n then xs else x : dropFirst n xs
dropFirst _ []     = []

unForAll ∷ GHC.Type → GHC.Type
unForAll (GHC.ForAllTy _ t) = unForAll t
unForAll ty                 = ty
