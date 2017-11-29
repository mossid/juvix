module Juvix.Transpiler.CoreToType (
  coreToType
) where

import           Control.Monad.Except
import qualified Data.Text                   as T
import           Foundation

import qualified Juvix.Michelson             as M
import qualified Juvix.Transpiler.GHC        as GHC
import           Juvix.Transpiler.TypeToType
import           Juvix.Types
import           Juvix.Utility

coreToType ∷ GHC.CoreExpr → CompilerM M.Type
coreToType = typeToType . GHC.exprType
