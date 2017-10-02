module Juvix.Transpiler where

import qualified CoreSyn                  as GHC
import qualified DataCon                  as GHC
import qualified GHC
import qualified Literal                  as GHC
import qualified TyCon                    as GHC
import qualified TyCoRep                  as GHC
import qualified Var                      as GHC

import           Control.Monad.Except
import           Control.Monad.RWS.Strict
import qualified Data.ByteString.Char8    as B
import           Data.Functor.Identity
import qualified Data.Map.Strict          as Map
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as T
import           Foundation
import qualified Prelude                  as P
import           Text.Read                (readMaybe)

import           Juvix.Optimization
import           Juvix.PrettyPrint
import qualified Juvix.Script             as M
import           Juvix.Types
import           Juvix.Utility

{-  Stage 1: GHC Core → Intermediary Expression

    Transform GHC Core expression into our intermediary stage expression type, executing the following transformations:

    - Eliminating unnecessary type annotations (the input GHC Core expression is assumed to be well-typed, and GHC types are erased at runtime)
    - Rewriting polymorphic calls to the appropriate typeclass instantation (Juvix does not support runtime polymorphism)
    - Substituting let-bindings for the bound values in their inner expressions
    - Rewriting case statements to simplified form
    - Rewriting Michelson builtin calls to direct Michelson instructions
    - Replacing GHC builtins (units, lists, either, bool, etc) to Michelson equivalents   -}

coreToExpr ∷ GHC.CoreExpr → CompilerM Expr
coreToExpr expr = do
  let tellReturn ret = tell [CoreToExpr expr ret] >> return ret
  case expr of

      {- Michelson library calls; this could be cleaner, also reliant on naming (don't name any other functions "michelson"). Should also check module. Maybe some way to quasiquote. -}
      GHC.App (GHC.App (GHC.App (GHC.Var m) _) _) (GHC.App _ (GHC.Lit (GHC.MachStr s))) | nameToTextSimple (GHC.varName m) == "michelson" →
        case readMaybe (B.unpack s) of
          Just e  → tellReturn (BuiltIn e)
          Nothing → throwError (NotYetImplemented (T.decodeUtf8 s))
      GHC.App (GHC.App (GHC.App (GHC.Var m) _) _) (GHC.App _ (GHC.Lit (GHC.MachStr s))) | nameToTextSimple (GHC.varName m) == "rewrite" →
        case s of
          "intFromInteger" →
            return (LitCast M.IntT M.IntT)
          "tezFromInteger" →
            return (LitCast M.IntT M.TezT)
          _ → throwError (NotYetImplemented (T.concat ["Rewrite: ", T.decodeUtf8 s]))

      {- GHC builtins. -}
      GHC.Var v | nameToTextSimple (GHC.varName v) == "()"    → return (BuiltIn (M.ConstUT M.UnitUT))
      GHC.Var v | nameToTextSimple (GHC.varName v) == "True"  → return (BuiltIn (M.ConstUT (M.BoolUT True)))
      GHC.Var v | nameToTextSimple (GHC.varName v) == "False" → return (BuiltIn (M.ConstUT (M.BoolUT False)))
      GHC.Var v | nameToTextSimple (typeName (GHC.varType v)) == "Ordering" ->
        (BuiltIn . M.ConstUT . M.IntegerUT) |<<
          case nameToTextSimple (GHC.varName v) of
            "LT" → return 1
            "EQ" → return 2
            "GT" → return 3
            _    → throwError (NotYetImplemented (T.concat ["Ord constructor: ", nameToTextSimple (GHC.varName v)]))

      {- Inline environment-defined expressions. -}
      GHC.Var v | GHC.isId v → do
        let n = pprint v
        env ← ask
        case Map.lookup n env of
          Just e  → coreToExpr e
          Nothing → return $ Var n

      GHC.Lam v e | GHC.isId v →
        Lam (pprint v) |<< coreToExpr e

      {-  Ditch typing information.   -}
      GHC.Lam v e | GHC.isTyVar v →
        coreToExpr e

      {- Native pair constructor. -}
      GHC.App (GHC.App (GHC.Var v) _) _ | nameToTextSimple (GHC.varName v) == "(,)" →
        tellReturn =<< pack

      {-  This is a silly hack; need to figure out how to get CoreModules from package imports with the GHC API.
          The typeclass call inlining algorithm is roughly what we want though.   -}

      {-  i.e. ∀ a . class (A a) ⇒ B a where ...  -}
      e@(GHC.App (GHC.App (GHC.Var f) (GHC.Type t)) (GHC.App (GHC.App (GHC.Var _) (GHC.Type _)) (GHC.Var d))) → do
        env ← ask
        case Map.lookup (pprint d) env of
          Nothing → throwError (NotYetImplemented (T.concat ["While attempting to transform ", pprint e, " could not find typeclass ", pprint d]))
          Just c  → do
            let appR (GHC.Var v)   = return (v, [])
                appR (GHC.App x y) = do
                  (v, l) ← appR x
                  return (v, l P.++ [y])
                appR _             = throwError (NotYetImplemented "Unexpected CoreExpr while reducing typeclass")
            (_, funcs') ← appR c
            let inst:_              = filter (\case (GHC.Var _) → True; _ → False) funcs'
            coreToExpr (GHC.App (GHC.App (GHC.Var f) (GHC.Type t)) inst)

      (GHC.App (GHC.App (GHC.Var f) (GHC.Type t)) (GHC.Var _)) | nameToTextSimple (GHC.varName f) == ">>=" && nameToTextSimple (typeName t) == "IO"     → return (BuiltIn M.NopUT)
      (GHC.App (GHC.App (GHC.Var f) (GHC.Type t)) (GHC.Var _)) | nameToTextSimple (GHC.varName f) == ">>" && nameToTextSimple (typeName t) == "IO"      →  return (BuiltIn M.DropUT)
      (GHC.App (GHC.App (GHC.Var f) (GHC.Type t)) (GHC.Var _)) | nameToTextSimple (GHC.varName f) == "return" && nameToTextSimple (typeName t) == "IO"  → return (BuiltIn M.NopUT)

      e@(GHC.App (GHC.App (GHC.Var f) (GHC.Type t)) (GHC.Var c)) → do
        env ← ask
        case Map.lookup (pprint c) env of
          Just c  → do
            let appR (GHC.Var v)   = return (v, [])
                appR (GHC.App x y) = do
                  (v, l) ← appR x
                  return (v, l P.++ [y])
                appR _             = throwError (NotYetImplemented "Unexpected CoreExpr while reducing typeclass")
            (cls, funcs') ← appR c
            let funcs               = P.foldl (\acc x → case x of (GHC.Var v) → acc P.++ [v]; _ → acc) [] funcs'
                name                = T.concat ["$f", T.drop 2 (nameToTextSimple (GHC.varName cls)), nameToTextSimple (typeName t), "_$c", nameToTextSimple (GHC.varName f)]
            func ← case filter ((==) name . nameToTextSimple . GHC.varName) funcs of [f] → return f; _ → throwError (NotYetImplemented (T.concat ["While attempting to transform ", pprint e, " typeclass function not found: ", name, "; searched: " , T.intercalate ", " (fmap (nameToTextSimple . GHC.varName) funcs)]))
            let Just funcD = Map.lookup (pprint func) env
            coreToExpr funcD
          Nothing → throwError (NotYetImplemented (T.concat ["While attempting to transform ", pprint e, " could not find typeclass ", pprint c]))

      GHC.App x (GHC.Type _) →
        coreToExpr x

      GHC.App x (GHC.Var v) | GHC.isTyVar v || GHC.isTcTyVar v →
        coreToExpr x

      {- Function application; special-case literal casts. -}
      GHC.App x y → do
        x ← coreToExpr x
        case x of
          LitCast M.IntT M.IntT ->
            case y of
              GHC.Lit (GHC.LitInteger i _) →
                tellReturn (BuiltIn (M.ConstUT (M.IntegerUT i)))
              _ → throwError (NotYetImplemented (T.concat ["Literal cast: ", pprint y]))
          LitCast M.IntT M.TezT ->
            case y of
              GHC.Lit (GHC.LitInteger i _) →
                tellReturn (BuiltIn (M.ConstUT (M.TezUT i)))
              _ → throwError (NotYetImplemented (T.concat ["Literal cast: ", pprint y]))
          _ → do
            y ← coreToExpr y
            return (App x y)

      {- Typecast or rebind; we don't need to do anything here. -}
      GHC.Case e _ _ [] → coreToExpr e

      {- Single option; desugar the pattern match. -}
      GHC.Case what _ _ [(GHC.DataAlt dataCon, [a, b], expr)] → do
        what ← coreToExpr what
        unpack ← unpack dataCon
        {- TODO: Only bind vars requested. -}
        {- TODO: Switch to unpack and set variable names on stack; more efficient; we don't need to use a function. -}
        body ← coreToExpr expr
        tellReturn (App (App (Lam (pprint a) (Lam (pprint b) body)) (BuiltIn M.NopUT)) (App unpack what))

      {- Cast with irrefutable case; just evaluate the case. -}
      GHC.Case _ _ _ [(GHC.DataAlt _, [], expr)] →
        coreToExpr expr

      {- Special-case boolean case matches. -}
      GHC.Case what _ _ [(GHC.DataAlt f, _, ifFalse), (GHC.DataAlt t, _, ifTrue)] | nameToTextSimple (GHC.dataConName t) == "True" && nameToTextSimple (GHC.dataConName f) == "False" → do
        what    ← coreToExpr what
        ifTrue  ← coreToExpr ifTrue
        ifFalse ← coreToExpr ifFalse
        tellReturn (If what ifTrue ifFalse)

      {- Sum types -}
      GHC.Case what _ _ [(GHC.DEFAULT, _, ifDefault), (GHC.DataAlt dataCon, _, ifOther)] | GHC.dataConRepArity dataCon == 0 → do
        let tag = GHC.dataConTag dataCon
        what      ← coreToExpr what
        ifDefault ← coreToExpr ifDefault
        ifOther   ← coreToExpr ifOther
        tellReturn (If (App (BuiltIn (M.SeqUT (M.SeqUT (M.ConstUT (M.IntegerUT (fromIntegral tag))) M.SubIntUT) M.EqUT)) what) ifOther ifDefault)

      {- Ditch cast type; we don't care. -}
      GHC.Cast e _ → coreToExpr e

      e → throwError (NotYetImplemented ("coreToExpr: " `T.append` pprint e))

pack ∷ CompilerM Expr
pack = return (BuiltIn M.ConsPairUT)

unpack ∷ GHC.DataCon → CompilerM Expr
unpack con = if
  | nameToTextSimple (GHC.dataConName con) == "(,)" → return (BuiltIn (M.SeqUT (M.SeqUT (M.SeqUT M.DupUT M.CdrUT) M.SwapUT) M.CarUT))
  | otherwise → throwError (NotYetImplemented ("unpack: " `T.append` pprint con))

{-  Stage 2 : Expression Simplification

    Rewrite the intermediary expression form in ways more conducive to Michelson transformation.

    - Eliminate unnecessary/equivalent function calls, e.g. rewrite (\x → x) y ⇒ y.
    - Minimize requisite stack manipulation to reorder parameters, e.g. rewrite (\x y → f y x) to f (BuiltIn SwapUT)    -}

simplifyExpr ∷ Expr → CompilerM Expr
simplifyExpr expr = do
  let tellReturn ret = tell [SimplifiedExpr expr ret] >> return ret
  case expr of
    Lam a (App e (Var b)) | a == b && not (uses a e) → do
      inner ← simplifyExpr e
      tellReturn inner
    Lam a (Lam b (App (App e (Var c)) (Var d))) | a == c && b == d && not (uses a e) && not (uses b e) → do
      e ← simplifyExpr e
      tellReturn =<< simplifyExpr e
    Lam a (Lam b (App (App e (Var c)) (Var d))) | a == d && b == c && not (uses a e) && not (uses b e) → do
      e ← simplifyExpr e
      tellReturn (App e (BuiltIn M.SwapUT))
    Lam v e → simplifyExpr e >>| Lam v
    App x y → do
      x ← simplifyExpr x
      y ← simplifyExpr y
      return $ App x y
    If x y z → do
      x ← simplifyExpr x
      y ← simplifyExpr y
      z ← simplifyExpr z
      return $ If x y z
    e → return e

{-  Whether an expression uses the value of a variable. Assumes uniqueness of names.   -}

uses ∷ T.Text → Expr → Bool
uses var expr =
  case expr of
    BuiltIn _   → False
    Lit _       → False
    LitCast _ _ → False
    Var v       → v == var
    Let _ x y   → uses var x || uses var y
    App x y     → uses var x || uses var y
    Lam _ x     → uses var x
    If x y z    → uses var x || uses var y || uses var z

{-  Stage 3 : Transform simplified intermediary expression to Michelson instruction sequence.

    This requires tracking the state of the stack through the Michelson evaluation path.

    At the moment, this function maintains an invariant relation between the Haskell type of the expression and the type of the Michelson stack.
    Commented on each case statement: ∷ { Haskell Type } ~ { Stack Pre-Evaluation } ⇒ { Stack Post-Evaluation }   -}

exprToMichelson ∷ Expr → CompilerM M.ExprUT
exprToMichelson expr = do
  beg ← get
  let tellReturn ret = do
        end ← get
        tell [ExprToMichelson expr ret beg end]
        return ret
  case expr of

    -- ∀ a . a ~ s ⇒ ∀ s . s
    BuiltIn s@M.FailUT →
      return s

    -- (none) ~ s ⇒ s
    BuiltIn s@M.NopUT →
      return s

    BuiltIn s@M.DropUT → do
      modify (drop 1)
      return s

    -- a → b ~ (a, s) ⇒ (b, s)
    BuiltIn s@M.DefaultAccountUT →
      return s

    -- (none) ~ (a, (b, s)) ⇒ (b, (a, s))
    BuiltIn s@M.SwapUT → do
      modify (\(x:y:xs) → y:x:xs)
      return s

    -- ∷ a ~ s ⇒ (a, s)
    BuiltIn s@(M.ConstUT c) → do
      modify ((:) (Const c))
      return s

    App (BuiltIn s@(M.SeqUT (M.SeqUT (M.ConstUT (M.IntegerUT _)) M.SubIntUT) M.EqUT)) v → do
      v ← exprToMichelson v
      modify ((:) FuncResult . drop 1)
      tellReturn (M.SeqUT v s)

    -- n/a
    BuiltIn s@(M.SeqUT (M.SeqUT (M.SeqUT M.DupUT M.CdrUT) M.SwapUT) M.CarUT) → do
      modify ((:) FuncResult . (:) FuncResult . drop 1)
      return s

    BuiltIn s@M.AmountUT → do
      modify ((:) FuncResult)
      return s

    {- Stack must be empty; this version will only work in a specific case. TODO: Reserve field in storage for this purpose. -}
    BuiltIn s@M.TransferTokensUT → do
      modify (drop 4)
      count ← P.length |<< get
      modify ((:) FuncResult)
      let foldDrop 0 = M.NopUT
          foldDrop n = M.SeqUT M.DropUT (foldDrop (n - 1))
          foldAdd  0 = M.NopUT
          foldAdd  1 = M.SeqUT (M.SeqUT (M.ConstUT M.UnitUT) (M.ConstUT M.UnitUT)) M.ConsPairUT
          foldAdd  n = M.SeqUT (M.ConstUT M.UnitUT) (foldAdd (n - 1))
          clear       = M.DipUT (M.DipUT (M.DipUT (M.DipUT (foldDrop count))))
      tellReturn (M.SeqUT clear (M.SeqUT s (M.SeqUT M.ConsPairUT (M.DipUT (foldAdd count)))))

    App (BuiltIn b) x | b `elem` [M.EqUT, M.LeUT] → do
      xcomp ← exprToMichelson x
      modify ((:) FuncResult . drop 1)
      return (M.SeqUT xcomp b)

    App (BuiltIn b) x | b `elem` [M.SubIntUT, M.SubTezUT] → do
      xcomp ← exprToMichelson x
      modify ((:) FuncResult . drop 2)
      return (M.SeqUT xcomp b)

    -- ∷ a ~ s ⇒ (a, s)
    App (App (BuiltIn b) x) y | b `elem` [M.AddIntIntUT, M.SubIntUT, M.MulIntIntUT, M.ConsPairUT, M.CmpLeUT, M.CmpEqUT] → do
      ycomp ← exprToMichelson y
      xcomp ← exprToMichelson x
      modify ((:) FuncResult . drop 2)
      return (M.SeqUT (M.SeqUT ycomp xcomp) b)

    -- ∷ a ~ s ⇒ (a, s)
    Var v → do
      stack ← get
      case position v stack of
        Nothing → throwError (VariableNotInScope v stack)
        Just i  → do
          put (FuncResult : stack)
          return (M.SeqUT (M.SeqUT (rearrange i) M.DupUT) (M.DipUT (unrearrange i)))

    -- ∷ \a → b ~ (a, s) ⇒ (b, s)
    Lam v e → do
      -- Relabel arguments on the stack when the function is called according to calling convention.
      let seqFold e 1 = M.SeqUT (M.SeqUT e M.SwapUT) M.DropUT
          seqFold e n = M.SeqUT (M.SeqUT (seqFold e (n - 1)) M.SwapUT) M.DropUT
          collect e vs =
            case e of
              Lam v e → collect e (vs <> [v])
              e       → do
                modify ((<>) (fmap BoundVariable vs) . P.drop (P.length vs))
                inner ← exprToMichelson e
                modify (\(x:xs) → x : P.drop (P.length vs) xs)
                tellReturn (seqFold inner (P.length vs))
      collect e [v]

    -- ∷ (\a → b) a ~ s ⇒ (b, s)
    App x y → do
      ycomp ← exprToMichelson y
      xcomp ← exprToMichelson x
      return (M.SeqUT ycomp xcomp)

    -- ∷ a ~ s ⇒ (a, s)
    If x y z → do
      start ← get
      xcomp ← exprToMichelson x
      modify (const start)
      ycomp ← exprToMichelson y
      modify (const start)
      zcomp ← exprToMichelson z
      tellReturn (M.SeqUT xcomp (M.IfUT ycomp zcomp))

    e → throwError (NotYetImplemented ("exprToMichelson: " `T.append` pprint e))

moduleToMichelson ∷ GHC.CoreProgram → GHC.CoreModule → ([CompileLog], Either CompileError M.ExprUT)
moduleToMichelson l m =
  let binds = l `mappend` GHC.cm_binds m
      env   = Map.delete "michelson" $ Map.fromList $ P.concatMap (\case (GHC.NonRec v e) → [(pprint v, e)]; GHC.Rec l → fmap (first pprint) l) binds
      expr  ∷ CompilerM GHC.CoreExpr
      expr  = case filter (\case (GHC.NonRec v _) | nameToTextSimple (GHC.varName v) == "main" → True; _ → False) binds of
                GHC.NonRec _ e:_ → tell [FrontendToCore e] >> return e
                _                → throwError MainFunctionNotFound
      (res, _, logs) = runRWS (runExceptT (optimize =<< exprToMichelson =<< simplifyExpr =<< coreToExpr =<< expr)) env [FuncResult] in
  (logs, res)

exprToType ∷ GHC.CoreExpr → Except CompileError M.Type
exprToType (GHC.Lam v e) = do
  v ← varToType v
  e ← exprToType e
  return $ M.LamT v e
exprToType (GHC.Var v)   = varToType v
exprToType (GHC.App (GHC.App (GHC.App (GHC.App (GHC.Var v) _) _) x) y) | nameToTextSimple (GHC.varName v) == "(,)" = do
  x ← exprToType x
  y ← exprToType y
  return $ M.PairT x y
exprToType (GHC.Case _ _ _ ((_, _, e):_)) = exprToType e

{-  Deal with: functions applied to types. Will need environment access.
    The below is a temporary implentation; ignore it. -}
exprToType (GHC.App (GHC.Var v) (GHC.Type t)) | nameToTextSimple (GHC.varName v) == "id" = do
  r ← tyCoToType t
  return (M.LamT r r)
exprToType (GHC.App (GHC.App (GHC.Var v) _) _) | nameToTextSimple (GHC.varName v) == "+" =
  return M.IntT
exprToType (GHC.App (GHC.Var v) _) | nameToTextSimple (GHC.varName v) == "fail" =
  return (M.PairT M.UnitT M.UnitT)
exprToType (GHC.App (GHC.App (GHC.App (GHC.App (GHC.App (GHC.App (GHC.Var v) _) _) _) _) _) _) | nameToTextSimple (GHC.varName v) == ">>" =
  return (M.PairT M.UnitT M.UnitT)
exprToType (GHC.App (GHC.App (GHC.Cast (GHC.Var v) _) _) _) | nameToTextSimple (GHC.varName v) == "$fAddIntIntInt" =
  return M.IntT
exprToType (GHC.App (GHC.App (GHC.Cast (GHC.Var v) _) _) _) | nameToTextSimple (GHC.varName v) == "$fMulIntIntInt" =
  return M.IntT
exprToType (GHC.App (GHC.App (GHC.App (GHC.Var _) (GHC.Type _)) (GHC.Var _)) _) =
  return M.IntT
exprToType (GHC.App (GHC.Var v) _) | nameToTextSimple (GHC.varName v) == ">" =
  return M.BoolT

exprToType e             = throwError $ NotYetImplemented $ "exprToType: " `T.append `pprint e

varToType ∷ GHC.Var → Except CompileError M.Type
varToType = tyCoToType . GHC.varType

tyCoToType ∷ GHC.Type → Except CompileError M.Type
tyCoToType ty =
  case ty of
    GHC.TyVarTy v → varToType v
    GHC.TyConApp c [] | GHC.isAlgTyCon c ->
      case nameToTextSimple (GHC.tyConName c) of
        "String" → return M.StringT
        "()"     → return M.UnitT
        "Int"    → return M.IntT
        "Key"    → return M.KeyT
        "Tez"    → return M.TezT
        name     → throwError $ NotYetImplemented $ "TyCon: " `T.append` name
    GHC.TyConApp c [a, b] | GHC.isAlgTyCon c && nameToTextSimple (GHC.tyConName c) == "(,)" → do
      a ← tyCoToType a
      b ← tyCoToType b
      return $ M.PairT a b
    t → throwError $ NotYetImplemented $ "tyCoToType: " `T.append` pprint t

typeOf ∷ GHC.CoreModule → T.Text → Either CompileError M.Type
typeOf (GHC.CoreModule _ _ binds _) str = do
  expr ← case filter (\case (GHC.NonRec v _) | nameToTextSimple (GHC.varName v) == str → True; _ → False) binds of
            GHC.NonRec _ e:_ → return e
            _                → throwError MainFunctionNotFound
  runIdentity $ runExceptT $ exprToType expr
