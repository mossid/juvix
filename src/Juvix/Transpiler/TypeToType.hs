module Juvix.Transpiler.TypeToType where

import           Control.Monad.Except
import           Control.Monad.RWS.Strict
import qualified Data.Text                as T
import           Foundation
import qualified Prelude                  as P

import qualified Juvix.Michelson          as M
import qualified Juvix.Transpiler.GHC     as GHC
import           Juvix.Transpiler.Utility
import           Juvix.Types
import           Juvix.Utility

typeToType ∷ GHC.Type → CompilerM M.Type
typeToType ty = do
  case ty of
    GHC.TyVarTy v → typeToType (GHC.varType v)
    GHC.TyConApp c [x, y] | GHC.isAlgTyCon c && nameToTextSimple (GHC.tyConName c) == "(,)" → do
      x ← typeToType x
      y ← typeToType y
      return (M.PairT x y)
    GHC.TyConApp c [x] | nameToTextSimple (GHC.tyConName c) == "IO" → do
      typeToType x
    GHC.TyConApp c bs | GHC.isAlgTyCon c ->
      case nameToTextSimple (GHC.tyConName c) of
        "()"      → return M.UnitT
        "String"  → return M.StringT
        "Int"     → return M.IntT
        "Tez"     → return M.TezT
        "Key"     → return M.KeyT
        "Hash"    → return M.HashT
        _         → do
          let constructors = GHC.visibleDataCons (GHC.algTyConRhs c)
          reprType ← algReprType constructors bs
          return reprType
    GHC.ForAllTy (GHC.Anon x) y → do
      x ← typeToType x
      y ← typeToType y
      return (M.LamT x y)
    GHC.TyConApp c [] | GHC.isTypeSynonymTyCon c ->
      case GHC.synTyConRhs_maybe c of
        Just t  → typeToType t
        Nothing → throwError (NotYetImplemented (T.concat ["typeToType (syn): ", pprint ty]))
    GHC.TyConApp _ [GHC.TyVarTy v] → typeToType (GHC.varType v) -- Double-check correctness here.
    ty → throwError (NotYetImplemented (T.concat ["typeToType (unmatched): ", pprint ty, " expanded: ", pprint (GHC.coreView ty)]))

constructorReprType ∷ GHC.TyCon → GHC.Name → [GHC.Type] → CompilerM M.Type
constructorReprType tyCon name binds = do
  let constructors = GHC.visibleDataCons (GHC.algTyConRhs tyCon)
  case filter ((==) (nameToTextSimple name) . nameToTextSimple . GHC.dataConName) constructors of
    [dataCon] → reprType binds dataCon `catchError` (\_ → throwError (NotYetImplemented ("constructorReprType: " `T.append` pprint tyCon)))
    _         → throwError (NotYetImplemented ("constructorReprType (data constructor not found): " `T.append` pprint name))

constructorPack ∷ GHC.TyCon → GHC.Name → [GHC.Type] → CompilerM Expr
constructorPack tyCon name binds = do
  let constructors = GHC.visibleDataCons (GHC.algTyConRhs tyCon)
  case filter ((==) (nameToTextSimple name) . nameToTextSimple . GHC.dataConName . snd) (P.zip [0..] constructors) of
    [(index, dataCon)]    → do
      let either = packEither index (P.length constructors)
      reprType ← reprType binds dataCon `catchError` (\_ → throwError (NotYetImplemented (T.concat ["constructorPack: ", pprint tyCon, ", ", pprint binds])))
      case reprType of
        M.UnitT → return (BuiltIn (T.pack (P.show (M.SeqUT (M.ConstUT M.UnitUT) either))))
        _ → return (BuiltIn (T.pack (P.show either)))
    _                     → throwError (NotYetImplemented ("constructorPack (data constructor not found): " `T.append` pprint name))

seqRepeat ∷ M.ExprUT → Int → M.ExprUT
seqRepeat expr 0 = M.NopUT
seqRepeat expr 1 = expr
seqRepeat expr n = M.SeqUT (seqRepeat expr (n - 1)) expr

packEither ∷ Int → Int → M.ExprUT
packEither position count =
  let rights  = seqRepeat M.RightUT position
      lefts   = seqRepeat M.LeftUT (min 1 (count - position - 1))
  in M.SeqUT lefts rights

algReprType ∷ [GHC.DataCon] → [GHC.Type] → CompilerM M.Type
algReprType []  _     = return M.UnitT
algReprType tys binds = nestedEither =<< mapM (reprType binds) tys

reprType ∷ [GHC.Type] → GHC.DataCon → CompilerM M.Type
reprType binds dataCon = do
  let repType   = GHC.dataConRepType dataCon
  unarrowed ← unarrow binds repType
  case P.take (P.length unarrowed - 1) unarrowed of
    [] → return M.UnitT
    l  → do
            l ← mapM typeToType l `catchError` (\_ → throwError (NotYetImplemented ("reprType: " `T.append` pprint dataCon `T.append` " @ " `T.append` pprint l)))
            nestedPair l

nestedPair ∷ [M.Type] → CompilerM M.Type
nestedPair = nested M.PairT

nestedEither ∷ [M.Type] → CompilerM M.Type
nestedEither = nested M.EitherT

nested ∷ (M.Type → M.Type → M.Type) → [M.Type] → CompilerM M.Type
nested func =
  \case
    []      → throwError (NotYetImplemented "nested: empty type list")
    [ty]    → return ty
    (t:ts)  → func t |<< nested func ts

unarrow ∷ [GHC.Type] → GHC.Type → CompilerM [GHC.Type]
unarrow binds ty =
  case ty of
    GHC.ForAllTy (GHC.Anon x) y → do
      x ← unarrow binds x
      y ← unarrow binds y
      return (x <> y)
    GHC.ForAllTy (GHC.Named v _) y ->
      case binds of
        b:bs  → do
          unarrow bs (typeApply v b y)
        [] → return [ty]
    x                           → return [x]
