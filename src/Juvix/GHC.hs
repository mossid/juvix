module Juvix.GHC where

import qualified Control.Exception      as E
import qualified CoreSyn                as GHC
import qualified DynFlags
import           Foundation
import qualified GHC
import qualified GHC.LanguageExtensions as LangExt
import qualified GHC.Paths              as Paths
-- import qualified HscTypes               as GHC
import           Paths_juvix
import           Prelude                (FilePath)
import qualified Prelude                as P
import           System.Posix.Files

runGHC ∷ GHC.Ghc a → IO a
runGHC x = do
  {-  This is a silly way to fake a package import. To be reimplemented correctly as a Haskell package in the future.   -}
  libf ← getDataFileName ("lib/Juvix/Lib.hs" ∷ FilePath)
  let root = P.take (P.length libf - P.length ("Lib.hs" ∷ FilePath)) libf
  E.bracket
    (createSymbolicLink root "Juvix")
    (\_ → removeLink "Juvix")
    (\_ → GHC.runGhc (Just Paths.libdir) $ do
      oldFlags ← DynFlags.getDynFlags
      let newFlags      = DynFlags.updOptLevel 0 $ oldFlags {
                            GHC.ghcLink = DynFlags.NoLink,
                            GHC.hscTarget = DynFlags.HscNothing
                          }
          setFlags      = []
          unsetFlags    = [DynFlags.Opt_WarnMissingMethods]
          newFlags'     = foldr (flip DynFlags.wopt_set) (foldr (flip DynFlags.wopt_unset) newFlags unsetFlags) setFlags
          {-  Juvix should support all GHC optimizations, which don't change the semantics of Core.
              However, GHC's underlying lazy STG evaluation model is quite different than our Michelson output, so a lot of GHC's optimizations don't help us.
              Some things like inlining and specialization are still quite useful.
              Worthy of more research / tweaking.   -}
          setGFlags     = [DynFlags.Opt_FunToThunk, DynFlags.Opt_SpecConstr, DynFlags.Opt_ExposeAllUnfoldings]
          {-  Full laziness is useless since our evaluation isn't lazy (at the moment).   -}
          unsetGFlags   = [DynFlags.Opt_FullLaziness, DynFlags.Opt_IgnoreInterfacePragmas, DynFlags.Opt_OmitInterfacePragmas]
          newFlags''    = foldr (flip DynFlags.gopt_set) (foldr (flip DynFlags.gopt_unset) newFlags' unsetGFlags) setGFlags
          setOptions    = [LangExt.UnicodeSyntax]
          unsetOptions  = [LangExt.ImplicitPrelude]
          finalFlags    = foldr (flip DynFlags.xopt_set) (foldr (flip DynFlags.xopt_unset) newFlags'' unsetOptions) setOptions
      _ ← GHC.setSessionDynFlags finalFlags
      x)

compileModule ∷ FilePath → IO GHC.CoreModule
compileModule = runGHC . GHC.compileToCoreSimplified

compilePackage ∷ P.String → IO [GHC.CoreBind]
compilePackage _ = return []

{- Need to figure out how to compile package module to GHC Core. This doesn't work. -}
{-
compilePackage name =
  runGHC $ do
    target ← GHC.guessTarget "Juvix/Lib.hs" Nothing
    GHC.setTargets [target]
    _ ← GHC.load GHC.LoadAllTargets
    ext ← GHC.packageDbModules False
    GHC.setContext [GHC.IIDecl (GHC.simpleImportDecl (GHC.mkModuleName name))]
    s ← GHC.getModSummary (GHC.mkModuleName name)
    p ← GHC.parseModule s
    t ← GHC.typecheckModule p
    d ← GHC.desugarModule t
    let c = GHC.coreModule d
    return (GHC.mg_binds c)
-}
