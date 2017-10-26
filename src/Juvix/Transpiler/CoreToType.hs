module Juvix.Transpiler.CoreToType (
  coreToType
) where

import           Foundation

import qualified Juvix.Michelson             as M
import qualified Juvix.Transpiler.GHC        as GHC
import           Juvix.Transpiler.TypeToType
import           Juvix.Types

coreToType ∷ GHC.CoreExpr → CompilerM M.Type
coreToType = typeToType . GHC.exprType
