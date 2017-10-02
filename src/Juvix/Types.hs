module Juvix.Types where

import           Control.Monad.Except
import           Control.Monad.RWS.Strict
import qualified Data.Map.Strict          as Map
import qualified Data.Text                as T
import           Foundation

import qualified CoreSyn                  as GHC
import           Juvix.Script             (ExprUT, Type)

data Literal
  = LBool Bool
  | LInt  Integer
  | LStr  T.Text

data Expr
  = BuiltIn ExprUT
  -- | Lit Literal
  | Lit ExprUT
  | Var T.Text
  | Let T.Text Expr Expr
  | App Expr Expr
  | Lam T.Text Expr
  | If Expr Expr Expr
  | LitCast Type Type

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
  | ExprToMichelson Expr ExprUT StackRep StackRep
  | Optimized       ExprUT ExprUT

type StackRep = [StackObject]

data StackObject =

  BoundVariable T.Text |
  Const ExprUT |
  FuncResult

  deriving (Eq, Show)

type CompilerM a = ExceptT CompileError (RWS (Map.Map T.Text GHC.CoreExpr) [CompileLog] StackRep) a
