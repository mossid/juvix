module Juvix.Transpiler where

import           Control.Monad.Except
import           Control.Monad.RWS.Strict
import qualified Data.Map                         as M
import qualified Data.Text                        as T
import           Data.Typeable
import           Foundation
import qualified Prelude                          as P

import qualified Juvix.Michelson                  as M hiding (Map)
import           Juvix.Transpiler.CoreToExpr
import           Juvix.Transpiler.CoreToType
import           Juvix.Transpiler.ExprToMichelson
import qualified Juvix.Transpiler.GHC             as GHC
import           Juvix.Transpiler.SimplifyExpr
import           Juvix.Types
import           Juvix.Utility

moduleToMichelson ∷ GHC.CoreProgram → GHC.CoreModule → ([CompileLog], Either CompileError (M.SomeExpr, M.Type, M.Type, M.Type))
moduleToMichelson l m =
  let binds ∷ GHC.CoreProgram
      binds   = l `mappend` GHC.cm_binds m
      env ∷ M.Map T.Text GHC.CoreExpr
      env     = M.fromList (P.concatMap (\case (GHC.NonRec v e) → [(pprint v, e)]; GHC.Rec l → fmap (first pprint) l) binds)
      computation ∷ CompilerM (M.SomeExpr, M.Type, M.Type, M.Type)
      computation = do
        core ← case filter (\case (GHC.NonRec v _) | nameToTextSimple (GHC.varName v) == "main" → True; _ → False) binds of
                  GHC.NonRec _ e:_ → tell [FrontendToCore e] >> return e
                  _                → throwError MainFunctionNotFound
        expr ← coreToExpr core
        exprType ← coreToType core
        case exprType of
          M.LamT start@(M.PairT paramTy startStorageTy) end@(M.PairT retTy endStorageTy) | startStorageTy == endStorageTy → do
            case (M.liftType paramTy, M.liftType startStorageTy, M.liftType retTy, M.liftType endStorageTy) of
              (M.SomeType (_ ∷ paramTyLifted), M.SomeType (_ ∷ startStorageTyLifted), M.SomeType (_ ∷ retTyLifted), M.SomeType (_ ∷ endStorageTyLifted)) → do
                simplified ← simplifyExpr expr
                michelson ← exprToMichelson simplified
                (M.SomeExpr (expr ∷ M.Expr (M.Stack a) (M.Stack b)), _) ← M.liftUntyped michelson (M.typeToStack start)
                case (eqT ∷ Maybe (a :~: (M.Pair paramTyLifted startStorageTyLifted, ())), eqT ∷ Maybe (b :~: (M.Pair retTyLifted endStorageTyLifted, ()))) of
                  (Just Refl, Just Refl) → do
                    optimized ← M.optimize expr
                    return (M.SomeExpr optimized, paramTy, retTy, startStorageTy)
                  _ → do
                    throwError (NotYetImplemented (T.concat ["Cannot unify start/end stack types: start ", pprint start, " / end ", pprint end, " with compilation output type; this is a bug in Juvix and should be reported"]))
          _ → throwError (NotYetImplemented (T.concat ["Invalid type for main function - must be ∷ (param, storage) → (ret, storage) but was instead ", pprint exprType]))
      (res, _, logs) = runRWS (runExceptT computation) (Env env) [FuncResult]
  in (logs, res)
