module Juvix.Transpiler.TypeToType where

import           Control.Monad.Except
import           Control.Monad.RWS.Strict
import qualified Data.Text                as T
import           Foundation
import qualified Prelude                  as P

import qualified Juvix.Michelson          as M
import qualified Juvix.Transpiler.GHC     as GHC
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
    _ → throwError (NotYetImplemented (T.concat ["typeToType (unmatched): ", pprint ty]))

constructorReprType ∷ GHC.TyCon → GHC.Name → CompilerM M.Type
constructorReprType tyCon name = do
  let constructors = GHC.visibleDataCons (GHC.algTyConRhs tyCon)
  case filter ((==) (nameToTextSimple name) . nameToTextSimple . GHC.dataConName) constructors of
    [dataCon] → reprType [] dataCon
    _         → throwError (NotYetImplemented ("constructorReprType (data constructor not found): " `T.append` pprint name))

algReprType ∷ [GHC.DataCon] → [GHC.Type] → CompilerM M.Type
algReprType []  _     = return M.UnitT
algReprType tys binds = nestedEither =<< mapM (reprType binds) tys

reprType ∷ [GHC.Type] → GHC.DataCon → CompilerM M.Type
reprType _ dataCon = do
  let repType   = GHC.dataConRepType dataCon
      unarrowed = unarrow repType
  case P.take (P.length unarrowed - 1) unarrowed of
    [] → return M.UnitT
    l  → do
            l ← mapM typeToType l
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

unarrow ∷ GHC.Type → [GHC.Type]
unarrow ty =
  case ty of
    GHC.ForAllTy (GHC.Anon x) y → unarrow x <> unarrow y
    x                           → [x]
