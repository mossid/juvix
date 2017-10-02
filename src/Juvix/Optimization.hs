module Juvix.Optimization (
  optimize,
  optimizeNoLogs
) where

import           Control.Monad.Writer
import           Foundation

import           Juvix.Script
import           Juvix.Types          (CompileLog (..))
import           Juvix.Utility

{-  Exported for testing convenience.   -}

optimizeNoLogs ∷ ExprUT → ExprUT
optimizeNoLogs = fst . runWriter . optimize

{-  This is a simple optimization strategy which replaces sequences of Michelson instructions with equivalent sequences of fewer instructions.
    Runs optimization passes until no further optimizations are found.
    At the moment nontrivial programs are unlikely to compile to the smallest equivalent Michelson instruction sequence, but little time has been spent on optimization so far - a high degree should be possible; the Haskell typesystem provides very strong guarantees.
    A more interesting / potentially more effective strategy might be to search the space of equivalent Michelson programs, which at small program sizes using bounded heuristic search should be computationally feasible
    - then choose the one with the fewest instructions (or based on some other preference function, depending on how Tezos ends up pricing contract execution).   -}

optimize ∷ (MonadWriter [CompileLog] m) ⇒ ExprUT → m ExprUT
optimize expr = do
  let tellReturn ret = tell [Optimized expr ret] >> return ret
      inner e = do
        one ← optimize' e
        two ← optimize' one
        if one == two then tellReturn two else inner two
  inner expr

{-  Note: this function should be typed as Expr a b → Expr a b, with the equivalence constraint enforced by the compiler.
    At the moment that is not done because the transpiler outputs untyped Michelson.  -}

optimize' ∷ (MonadWriter [CompileLog] m) ⇒ ExprUT → m ExprUT
optimize' expr =
  case expr of
    (DipUT NopUT)                                                         → return NopUT
    (DipUT e)                                                             → optimize' e >>| DipUT
    (SeqUT FailUT _)                                                      → return FailUT
    (SeqUT DupUT SwapUT)                                                  → return DupUT
    (SeqUT DupUT DropUT)                                                  → return NopUT
    (SeqUT (SeqUT (SeqUT (SeqUT x CdrUT) SwapUT) CarUT) (SeqUT SwapUT y)) → optimize' x >>= \x → optimize' y >>= \y → return (SeqUT (SeqUT (SeqUT (SeqUT x CarUT) SwapUT) CdrUT) y)
    (SeqUT (SeqUT (SeqUT e SwapUT) DropUT) DropUT)                        → optimize' e >>= (\e → return (SeqUT (SeqUT e DropUT) DropUT))
    (SeqUT (SeqUT (SeqUT e DupUT) SwapUT) DropUT)                         → optimize' e
    (SeqUT (SeqUT e@(ConstUT _) SwapUT) DropUT)                           → return (SeqUT DropUT e)
    (SeqUT SwapUT SwapUT)                                                 → return NopUT
    (SeqUT NopUT e)                                                       → optimize' e
    (SeqUT e NopUT)                                                       → optimize' e
    (SeqUT x y)                                                           → optimize' x >>= \x → optimize' y >>= \y → return (SeqUT x y)
    (IfUT x y)                                                            → optimize' x >>= \x → optimize' y >>= \y → return (IfUT x y)
    e                                                                     → return e
