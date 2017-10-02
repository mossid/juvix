module Juvix (
  compileToTz,
  pprint
) where

import           Control.Monad
import qualified Data.Text         as T
import qualified Data.Text.IO      as T
import           Foundation
import qualified GHC
import qualified Prelude           as P

import           Juvix.Emit
import           Juvix.GHC
import           Juvix.PrettyPrint
import           Juvix.Script
import           Juvix.Transpiler
import           Juvix.Types
import           Juvix.Utility

compileToTz ∷ P.FilePath → Bool → IO (Either CompileError T.Text)
compileToTz fn log = do
  lib ← compileModule "Juvix/Lib.hs"
  aux ← P.concat |<< mapM compilePackage ["Data.Ord"]
  mod ← compileModule fn
  let mainType          = typeOf mod "main"
      (logs, compiled)  = moduleToMichelson (GHC.cm_binds lib P.++ aux) mod
  let code              = compiled
  when log $ P.mapM_ (T.putStrLn . pprint) logs
  return $ do
    code ← code
    (LamT (PairT paramTy storageTy) (PairT retTy _)) ← mainType
    return $ T.unlines [
      T.concat ["parameter ", emitType paramTy, ";"],
      T.concat ["return ", emitType retTy, ";"],
      T.concat ["storage ", emitType storageTy, ";"],
      T.concat ["code ", emitFinal code, ";"]
      ]
