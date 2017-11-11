module Types where

import           Foundation
import qualified Prelude         as P

import qualified Juvix.Michelson as M

data TranspilationTestCase
  = TranspilationTestCase {
    testName    ∷ P.String,
    testHaskell ∷ P.String,
    testInputs  ∷ [(M.SomeType, M.Tez, M.Timestamp, M.SomeType)]
  }
