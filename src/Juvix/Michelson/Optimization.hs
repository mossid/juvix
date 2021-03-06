module Juvix.Michelson.Optimization (
  optimize,
  optimizeNoLogs
) where

import           Control.Monad.Writer
import           Foundation

import           Juvix.Michelson.Script
import           Juvix.Types            (CompileLog (..))
import           Juvix.Utility

{-  Exported for testing convenience.   -}

optimizeNoLogs ∷ (Eq a, Eq b, Typeable a, Typeable b) ⇒ Expr (Stack a) (Stack b) → Expr (Stack a) (Stack b)
optimizeNoLogs = fst . runWriter . optimize

{-  This is a simple optimization strategy which replaces sequences of Michelson instructions with equivalent sequences of fewer instructions.
    Runs optimization passes until no further optimizations are found, up to a maximum number of passes.
    At the moment nontrivial programs are unlikely to compile to the smallest equivalent Michelson instruction sequence, but little time has been spent on optimization so far - a high degree should be possible; the Haskell typesystem provides very strong guarantees.
    A more interesting / potentially more effective strategy might be to search the space of equivalent Michelson programs, which at small program sizes using bounded heuristic search should be computationally feasible
      - then choose the one with the fewest instructions (or based on some other preference function, depending on how Tezos ends up pricing contract execution).
    This optimization function is typed in the Expr GADT, so it cannot produce invalid output Michelson. However, the typesystem does not enforce computation correctness. -}

optimize ∷ (Eq a, Eq b, Typeable a, Typeable b, MonadWriter [CompileLog] m) ⇒ Expr (Stack a) (Stack b) → m (Expr (Stack a) (Stack b))
optimize expr = do
  let tellReturn ret = tell [Optimized (SomeExpr expr) (SomeExpr ret)] >> return ret
      inner e = do
        one ← optimize' e
        two ← optimize' one
        if one == two then tellReturn two else inner two
  inner expr

optimize' ∷ (Eq a, Eq b, Typeable a, Typeable b, MonadWriter [CompileLog] m) ⇒ Expr (Stack a) (Stack b) → m (Expr (Stack a) (Stack b))
optimize' expr =
  case expr of

    (IfLeft x y)    → optimize' x >>= \x → optimize' y >>= \y → return (IfLeft x y)

    (Dip e)         → Dip |<< optimize' e

    (Seq (Seq Dup (Dip e)) Drop) → optimize' e

    {- Possibly could move these elsewhere... -}
    (Seq (Seq (Const v) (Seq (Seq (Dip Dup) Swap) ConsPair)) (Seq Swap Drop)) → return (Seq (Dip (Const v)) ConsPair)

    (Seq (Seq (Seq e Dup) Swap) Drop) → optimize' e

    {- This is stupid. Should left-force Seq constructors in another phase. -}
    (Seq (Seq Dup Swap) Drop) → return Nop
    (Seq Dup (Seq Swap Drop)) → return Nop
    (Seq Dup (Dip Drop))      → return Nop

    (Seq (Seq Swap Dup) (Dip Swap)) → return (Seq (Dip Dup) Swap)

    (Seq Dup Swap)  → return Dup
    (Seq Dup Drop)  → return Nop
    (Seq Swap Swap) → return Nop

    (Seq e Nop)     → optimize' e
    (Seq Nop e)     → optimize' e

    (Seq x y)       → optimize' x >>= \x → optimize' y >>= \y → return (Seq x y)

    (If x y)        → optimize' x >>= \x → optimize' y >>= \y → return (If x y)

    expr            → return expr
