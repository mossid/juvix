module Juvix (
  compileToTz,
  pprint
) where

import           Control.Monad
import qualified Data.Text        as T
import qualified Data.Text.IO     as T
import           Foundation
import qualified GHC
import qualified Prelude          as P

import           Juvix.GHC
import qualified Juvix.Michelson  as M
import           Juvix.Transpiler
import           Juvix.Types
import           Juvix.Utility

compileToTz ∷ P.FilePath → Bool → IO (Either CompileError T.Text)
compileToTz fn log = do
  lib ← compileModule "Juvix/Lib.hs"
  mod ← compileModule fn
  let (logs, compiled) = moduleToMichelson (GHC.cm_binds lib) mod
  when log (P.mapM_ (T.putStrLn . pprint) logs)
  return (do
    (M.SomeExpr code, paramTy, retTy, storageTy) ← compiled
    return $ T.unlines [
      T.concat ["parameter ", M.emitType paramTy, ";"],
      T.concat ["return ", M.emitType retTy, ";"],
      T.concat ["storage ", M.emitType storageTy, ";"],
      T.concat ["code ", M.emitFinal code, ";"]
      ])
