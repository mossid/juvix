module Juvix.Reduction where

import qualified CoreSyn           as GHC
import qualified Data.Map.Strict   as M
import           Data.Maybe
import qualified Data.Text         as T
import           Foundation

import           Juvix.PrettyPrint
import           Juvix.Utility

envReduce ∷ M.Map T.Text GHC.CoreExpr → GHC.CoreExpr → GHC.CoreExpr
envReduce env expr
  = case expr of
      GHC.App x y → GHC.App (envReduce env x) (envReduce env y)
      GHC.Lam v e → GHC.Lam v (envReduce env e)
      GHC.Case e v t as → GHC.Case (envReduce env e) v t (fmap (\(x, y, z) → (x, y, envReduce env z)) as)
      e@(GHC.Var v) → fromMaybe e $ envReduce env |<< M.lookup (pprint v) env
      GHC.Cast e t → GHC.Cast (envReduce env e) t
      e → e

letReduce ∷ GHC.CoreExpr → GHC.CoreExpr
letReduce expr =
  let func m e =
        case e of
          GHC.App x y                 → GHC.App (func m x) (func m y)
          GHC.Lam v e                 → GHC.Lam v (func m e)
          GHC.Let (GHC.NonRec v b) e  → func (M.insert v b m) e
          GHC.Case e v t as           → GHC.Case (func m e) v t (fmap (\(x, y, z) → (x, y, func m z)) as)
          e@(GHC.Var v)               → fromMaybe e $ M.lookup v m
          e → e
  in func M.empty expr

betaReduce ∷ GHC.CoreExpr → GHC.CoreExpr
betaReduce expr =
  let func m e =
        case e of
          e@(GHC.Var v)   → fromMaybe e $ M.lookup v m
          GHC.Lam v e     → GHC.Lam v (func m e)
          GHC.Let b e     → GHC.Let b (func m e)
          GHC.App x y     ->
            case func m x of
              GHC.Lam v e → func (M.insert v (func m y) m) e
              x           → GHC.App x (func m y)
          GHC.Case e v t as → GHC.Case (func m e) v t (fmap (\(x, y, z) → (x, y, func m z)) as)
          e               → e
  in func M.empty expr
