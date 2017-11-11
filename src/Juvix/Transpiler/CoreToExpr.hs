module Juvix.Transpiler.CoreToExpr (
  coreToExpr
) where

import           Control.Monad.Except
import           Control.Monad.RWS.Strict
import           Data.List                   (findIndex)
import qualified Data.Map                    as M
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as T
import           Foundation
import qualified Prelude                     as P

import           Juvix.Transpiler.CoreToType
import           Juvix.Transpiler.Encoding
import qualified Juvix.Transpiler.GHC        as GHC
import           Juvix.Transpiler.TypeToType
import           Juvix.Transpiler.Utility
import           Juvix.Types
import           Juvix.Utility

{-  Stage 1: GHC Core → Intermediary Expression

    Transform GHC Core expression into our intermediary stage expression type, executing the following transformations:

    - Eliminating unnecessary type annotations (the input GHC Core expression is assumed to be well-typed, and GHC types are erased at runtime)
    - Rewriting polymorphic calls to the appropriate typeclass instantation (Juvix does not support runtime polymorphism)
    - Miscellaneous GHC builtin replacements
-}

coreToExpr ∷ GHC.CoreExpr → CompilerM Expr
coreToExpr expr = do

  let tellReturn ∷ Expr → CompilerM Expr
      tellReturn ret  = tell [CoreToExpr expr ret] >> return ret
      error ∷ ∀ a . PrettyPrint a ⇒ a → CompilerM Expr
      error v         = throwError (NotYetImplemented (T.concat ["coreToExpr: ", pprint v]))

  case expr of

    {- Variables: replace builtins, replace from env, translate. -}
    GHC.Var v → do
      if GHC.isId v then do
        case nameToTextSimple (GHC.varName v) of
          "()"  → return (Lit LUnit)
          "(,)" → return (BuiltIn "ConsPairUT")
          _     → do
            let name = pprint v
            env ← envExprs |<< ask
            case M.lookup name env of
              Just e  → coreToExpr e
              Nothing → do
                if GHC.isGlobalId v then do
                  case unForAll (GHC.varType v) of
                    GHC.TyConApp tyCon bs | GHC.isAlgTyCon tyCon → do
                      pack ← constructorPack tyCon (GHC.varName v) bs
                      return pack
                      --throwError (NotYetImplemented (T.concat ["packed ADT: ", pprint v, " for ", pprint tyCon, " ⇒ ", pprint pack]))
                    _ → throwError (NotYetImplemented (T.concat ["coreToExpr (var): ", pprint expr, " @ ", pprint (GHC.varType v)]))
                else return (Var name)
        else throwError (NotYetImplemented (pprint v))

    {- Convert literals. -}
    GHC.Lit l ->
      case l of
        GHC.LitInteger i _ → return (Lit (LInt i))
        _                  → error l

    {- Replace builtins. -}
    GHC.App (GHC.App (GHC.App (GHC.Var m) _) _) (GHC.App _ (GHC.Lit (GHC.MachStr s))) | nameToTextSimple (GHC.varName m) == "rewrite" →
      tellReturn (BuiltIn (T.decodeUtf8 s))

    {- TODO: Restructure this. We really just want to inline and optimize away the typeclass instance lookup. Figure out how GHC does this and *copy* it. -}
    {-  i.e. ∀ a . class (A a) ⇒ B a where ...  -}

    {-
    e@(GHC.App (GHC.App (GHC.Var f) (GHC.Type t)) (GHC.App (GHC.App (GHC.Var _) (GHC.Type _)) (GHC.Var d))) → do
      throwError (NotYetImplemented ("caughtMulti: " `T.append` pprint e))
      env ← envExprs |<< ask
      case M.lookup (pprint d) env of
        Nothing → throwError (NotYetImplemented (T.concat ["While attempting to transform.v2 ", pprint e, " could not find typeclass ", pprint d]))
        Just c  → do
          let appR (GHC.Var v)   = return (v, [])
              appR (GHC.App x y) = do
                (v, l) ← appR x
                return (v, l <> [y])
              appR _             = throwError (NotYetImplemented "v2. Unexpected CoreExpr while reducing typeclass")
          (_, funcs') ← appR c
          let inst:_              = filter (\case (GHC.Var _) → True; _ → False) funcs'
          coreToExpr (GHC.App (GHC.App (GHC.Var f) (GHC.Type t)) inst)

    e@(GHC.App (GHC.App (GHC.Var f) (GHC.Type _)) (GHC.Var c)) → do
      error e
      env ← envExprs |<< ask
      case M.lookup (pprint f) env of
        Just (GHC.Lam _ (GHC.Lam _ (GHC.Case _ _ _ [(_, binds, GHC.Var which)]))) → do
          case findIndex ((==) which) binds of
            Nothing    → throwError (NotYetImplemented ("Cannot find index"))
            Just index → do
              case M.lookup (pprint c) env of
                Nothing → throwError (NotYetImplemented ("Cannot find: " `T.append` pprint c))
                Just c → do
                  let appR (GHC.Var v)   = return (v, [])
                      appR (GHC.App x y) = do
                        (v, l) ← appR x
                        return (v, l <> [y])
                      appR _             = throwError (NotYetImplemented "Unexpected CoreExpr while reducing typeclass")
                  (_, funcs) ← appR c
                  tellReturn =<< coreToExpr ((drop 1 funcs) P.!! index)
        Just e@(GHC.Lam _ (GHC.Lam _ (GHC.Lam _ (GHC.App (GHC.App (GHC.App f _) _) _)))) → do
          -- TODO Check for *actual* bind equivalence.
          throwError (NotYetImplemented (T.concat ["found λ: ", pprint f, " // ", pprint e]))
          -- coreToExpr (GHC.App (GHC.App f (GHC.Type t)) (GHC.Var c))
        Just v  → throwError (NotYetImplemented (T.concat ["found invalid: ", pprint v]))
        Nothing → do
          return (BuiltIn "NopUT")
          --throwError (NotYetImplemented (T.concat ["missed: ", pprint f, " // ", pprint e]))
    -}

    {- Discard type application -}
    GHC.App x (GHC.Type _) → do
      coreToExpr x

    {- Discard type application -}
    GHC.App x (GHC.Var v) | GHC.isTyVar v || GHC.isTcTyVar v →
      coreToExpr x

    {- Function application -}
    GHC.App x y → do
      x ← coreToExpr x
      y ← coreToExpr y
      return (App x y)

    {- Translate value lambdas. -}
    GHC.Lam v e | GHC.isId v ->
      Lam (pprint v) |<< coreToExpr e

    {- Discard type lambdas. -}
    GHC.Lam v e | GHC.isTyVar v ->
      coreToExpr e

    {- Not supported. -}
    GHC.Lam v _ → error v

    {- Translate non-recursive let bindings. -}
    GHC.Let (GHC.NonRec v b) e → do
      b ← coreToExpr b
      e ← coreToExpr e
      return (Let (pprint v) b e)

    {- Not supported. -}
    GHC.Let b _ → error b

    {- Typecast or rebind; we don't need to do anything here. -}
    GHC.Case e _ _ [] → coreToExpr e

    {- Cast with irrefutable case and no binds; just evaluate the case. -}
    GHC.Case _ _ _ [(_, [], expr)] →
      coreToExpr expr

    {- Standard case expression. -}
    GHC.Case e b _ c → do
      eT ← coreToType e
      e ← coreToExpr e
      c ← mapM transformAlt c
      let binder = case GHC.occInfo (GHC.idInfo b) of GHC.IAmDead → Nothing; _ → Just (pprint b)
      tellReturn (Case e binder eT c)

    {- Ignore casts. -}
    GHC.Cast e _ → coreToExpr e

    {- Ignore ticks. -}
    GHC.Tick _ e → coreToExpr e

    {- Should be matched elsewhere. -}
    GHC.Type t → error t

    {- Not supported. -}
    GHC.Coercion c → error c

transformAlt ∷ GHC.Alt GHC.Var → CompilerM CaseOption
transformAlt (GHC.DEFAULT, [], expr) = do
  expr ← coreToExpr expr
  return (DefaultCase expr)
transformAlt (GHC.DataAlt dataCon, binds, expr) = do
  dataCon ← transformDataCon dataCon
  expr ← coreToExpr expr
  let modBinds = fmap ((\v → if uses v expr then Just v else Nothing) . pprint) binds
  return (CaseOption dataCon modBinds expr)
transformAlt alt = throwError (NotYetImplemented ("transformAlt: " `T.append` pprint alt))

transformDataCon ∷ GHC.DataCon → CompilerM DataCon
transformDataCon dataCon = do
  repType ←
    case GHC.dataConType dataCon of
      GHC.TyConApp tC bs  → constructorReprType tC (GHC.dataConName dataCon) bs
      _                   → reprType [] dataCon
  return DataCon {
    conTag        = nameToTextSimple (GHC.dataConName dataCon),
    conRepType    = repType,
    conUnique     = GHC.getKey (GHC.getUnique dataCon)
  }
