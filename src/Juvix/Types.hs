module Juvix.Types where

import           Control.Monad.Except
import           Control.Monad.RWS.Strict
import qualified Data.Map.Strict          as Map
import qualified Data.Text                as T
import           Foundation

import qualified CoreSyn                  as GHC
import qualified Juvix.Michelson.Script   as M

data Literal
  = LUnit
  | LBool Bool
  | LKey  T.Text
  | LInt  Integer
  | LTez  Integer
  | LNat  Integer
  | LStr  T.Text
  | LPair Literal Literal

  deriving (Show, Eq)

data DataCon
  = DataCon {
    conTag     ∷ T.Text,
    conRepType ∷ M.Type,
    conUnique  ∷ Int
  }

  deriving (Show, Eq)

data CaseOption
  = DefaultCase {
    defaultExpr       ∷ Expr
  }
  | CaseOption {
    optionConstructor ∷ DataCon,
    optionBinds       ∷ [Maybe T.Text],
    optionExpr        ∷ Expr
  }

  deriving (Show, Eq)

data Expr
  = BuiltIn T.Text
  | Lit Literal
  | Var T.Text
  | Let T.Text Expr Expr
  | App Expr Expr
  | Lam T.Text Expr
  | Case Expr (Maybe T.Text) M.Type [CaseOption]
  | BindIO    Expr Expr
  | SeqIO     Expr Expr
  | ReturnIO  Expr

  deriving (Show, Eq)

data CompileError =

  InvalidType |
  InvariantError |
  MainFunctionNotFound |
  VariableNotInScope T.Text StackRep |
  NotYetImplemented T.Text

data CompileLog
  = FrontendToCore  GHC.CoreExpr
  | CoreToExpr      GHC.CoreExpr Expr
  | SimplifiedExpr  Expr Expr
  | ExprToMichelson Expr M.ExprUT StackRep StackRep
  | Optimized       M.SomeExpr M.SomeExpr

type StackRep = [StackObject]

data StackObject =

  BoundVariable T.Text |
  Const M.ConstUT |
  FuncResult

  deriving (Eq, Show)

newtype Env = Env {
  envExprs ∷ Map.Map T.Text GHC.CoreExpr
}

type CompilerM a = ExceptT CompileError (RWS Env [CompileLog] StackRep) a
