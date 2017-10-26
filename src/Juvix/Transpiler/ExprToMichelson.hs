module Juvix.Transpiler.ExprToMichelson (
  exprToMichelson
) where

import           Control.Monad.Except
import           Control.Monad.RWS.Strict
import qualified Data.Text                 as T
import           Foundation
import qualified Prelude                   as P
import           Safe                      (readMay)

import qualified Juvix.Michelson           as M
import           Juvix.Transpiler.Encoding
import           Juvix.Transpiler.Utility
import           Juvix.Types
import           Juvix.Utility

{-  Stage 3 : Transform simplified intermediary expression to Michelson instruction sequence.

    This requires tracking the state of the stack through the Michelson evaluation path.

    At the moment, this function maintains an invariant forward mapping between the Haskell type of the expression and the type of the Michelson stack.
    Commented on each case statement: ∷ { Haskell Type } ~ { Stack Pre-Evaluation } ⇒ { Stack Post-Evaluation }   -}

exprToMichelson ∷ Expr → CompilerM M.ExprUT
exprToMichelson expr = do
  beg ← get
  let tellReturn ∷ M.ExprUT → CompilerM M.ExprUT
      tellReturn ret = do
        end ← get
        tell [ExprToMichelson expr ret beg end]
        return ret
  case expr of

    -- (special)
    -- TODO Deal with compile-time known stack type.
    BuiltIn s | readMay (T.unpack s) == Just M.TransferTokensUT → do
      modify (drop 4)
      count ← P.length |<< get
      modify ((:) FuncResult)
      let foldDrop 0 = M.NopUT
          foldDrop n = M.SeqUT M.DropUT (foldDrop (n - 1))
          foldAdd  0 = M.NopUT
          foldAdd  1 = M.SeqUT (M.SeqUT (M.ConstUT M.UnitUT) (M.ConstUT M.UnitUT)) M.ConsPairUT
          foldAdd  n = M.SeqUT (M.ConstUT M.UnitUT) (foldAdd (n - 1))
          clear       = M.DipUT (M.DipUT (M.DipUT (M.DipUT (foldDrop count))))
      tellReturn (M.SeqUT clear (M.SeqUT M.TransferTokensUT (M.SeqUT M.ConsPairUT (M.DipUT (foldAdd count)))))

    -- (various)
    BuiltIn b ->
      case readMay (T.unpack b) of
        Just e  → do
          func ← genFunc e
          modify func
          return e
        Nothing → throwError (NotYetImplemented ("exprToMichelson: BuiltIn: " `T.append` b))

    -- ∷ a ~ s ⇒ (a, s)
    Lit l → do
      modify ((:) FuncResult)
      return (litToExpr l)

    -- ∷ a ~ s ⇒ (a, s)
    Var v → do
      stack ← get
      case position v stack of
        Nothing → throwError (VariableNotInScope v stack)
        Just i  → do
          put (FuncResult : stack)
          let before  = if i == 0 then M.NopUT else rearrange i
              after   = if i == 0 then M.NopUT else M.DipUT (unrearrange i)
          return (M.SeqUT (M.SeqUT before M.DupUT) after)

    -- ∷ \a → b ~ (a, s) ⇒ (b, s)
    Lam v e → do
      -- TODO: Check if arguments used and drop unused ones before function call.
      -- Relabel arguments on the stack when the function is called according to calling convention.
      let seqFold e 1 = M.SeqUT (M.SeqUT e M.SwapUT) M.DropUT
          seqFold e n = M.SeqUT (M.SeqUT (seqFold e (n - 1)) M.SwapUT) M.DropUT
          collect e vs =
            case e of
              Lam v e → collect e (vs <> [v])
              e       → do
                modify ((<>) (fmap BoundVariable vs) . P.drop (P.length vs))
                inner ← exprToMichelson e
                modify (\(x:xs) → x : P.drop (P.length vs) xs)
                tellReturn (seqFold inner (P.length vs))
      collect e [v]

    -- ∷ (\a → b) a ~ s ⇒ (b, s)
    App x y → do
      ycomp ← exprToMichelson y
      xcomp ← exprToMichelson x
      return (M.SeqUT ycomp xcomp)

    -- Should be eliminated prior to this stage.
    Let _ _ _ → throwError (NotYetImplemented ("exprToMichelson: " `T.append` pprint expr))

    -- ∷ IO a >>= (\a → IO b) ~ s ⇒ (b, s)
    BindIO x y → exprToMichelson (App y x)

    -- ∷ IO a >> IO b ~ s ⇒ (b, s)
    SeqIO x y → do
      x ← exprToMichelson x
      modify (drop 1)
      y ← exprToMichelson y
      return (M.SeqUT (M.SeqUT x M.DropUT) y)

    -- ∷ IO a ~ s ⇒ (a, s)
    ReturnIO e → exprToMichelson e

    -- ∷ a ~ s ⇒ (a, s)
    Case scrutinee maybeBinder scrutineeType alternatives → do
      -- Special-case bool, either, maybe
      -- Evaluate binder, bind as necessary
      -- Generate switch on native repr (never constructor tag except for e.g. data Ord = A | B | C)
      -- Unpack necessary variables in fixed pattern according to desired bindings
      -- (later: lazy unpack? or remove in optimization stage any unused bindings?)
      -- Rewrite case alternatives with more than two alternatives to nested version?

      {- Evaluate scrutinee. -}
      scrutinee ← exprToMichelson scrutinee

      {- Bind scrutinee. -}
      (binderDup, binderDupM, binderDrop, binderDropM) ← do
        case maybeBinder of
          Just binder → do
            modify ((:) (BoundVariable binder) . drop 1)
            return (M.DupUT, modify ((:) FuncResult), M.SeqUT M.SwapUT M.DropUT, modify (drop 1))
          Nothing     → do
            return (M.NopUT, return (), M.NopUT, return ())

      case alternatives of
        [CaseOption conA bindsA caseA, CaseOption conB bindsB caseB] → do
          switch ← genSwitch scrutineeType

          -- TODO: Constructor ordering?
          {-
          if conTag conA == fst && conTag conB == snd then do
            return (switch caseA caseB)
          else if conTag conA == snd && conTag conB == fst then do
            return (switch (M.SeqUT unpackB caseB) (M.SeqUT unpackA caseA))
          else throwError (NotYetImplemented (T.concat ["exprToMichelson (tag mismatch): ", conTag conA, " / ", conTag conB, " /= ", fst, " / ", snd, " in ", pprint expr]))
          -}

          switchCase ← do
            binderDupM
            now ← get
            unpackA ← unpack (conRepType conA) bindsA
            caseA ← exprToMichelson caseA
            unpackDropA ← unpackDrop bindsA
            modify (const now)
            unpackB ← unpack (conRepType conB) bindsB
            caseB ← exprToMichelson caseB
            unpackDropB ← unpackDrop bindsB
            let a = M.SeqUT (M.SeqUT unpackA caseA) unpackDropA
                b = M.SeqUT (M.SeqUT unpackB caseB) unpackDropB
            return (switch a b)

          binderDropM
          return (M.SeqUT (M.SeqUT scrutinee binderDup) (M.SeqUT switchCase binderDrop))

        (CaseOption _ binds expr):[] → do
          binderDupM
          unpack ← unpack scrutineeType binds
          expr ← exprToMichelson expr
          unpackDrop ← unpackDrop binds
          binderDropM
          return (M.SeqUT (M.SeqUT (M.SeqUT scrutinee binderDup) (M.SeqUT (M.SeqUT unpack expr) unpackDrop)) binderDrop)

        (DefaultCase expr):[] → do
          {- This should probably be optimized away. -}
          expr ← exprToMichelson expr
          binderDropM
          return (M.SeqUT scrutinee (M.SeqUT expr binderDrop))

        _ → do
          throwError (NotYetImplemented ("exprToMichelson: " `T.append` pprint expr))

litToExpr ∷ Literal → M.ExprUT
litToExpr LUnit     = M.ConstUT M.UnitUT
litToExpr (LInt l)  = M.ConstUT (M.IntegerUT l)
litToExpr (LTez t)  = M.ConstUT (M.TezUT t)
litToExpr (LBool b) = M.ConstUT (M.BoolUT b)
litToExpr _         = undefined -- TODO

genSwitch ∷ M.Type → CompilerM (M.ExprUT → M.ExprUT → M.ExprUT)
genSwitch M.BoolT           = return M.IfUT
genSwitch (M.EitherT _ _)   = return M.IfLeftUT
genSwitch (M.OptionT _)     = return M.IfNoneUT
genSwitch (M.ListT _)       = return M.IfConsUT
genSwitch ty                = throwError (NotYetImplemented ("genSwitch: " `T.append` pprint ty))

{-
orderSwitch ∷ M.Type → CompilerM (T.Text, T.Text)
orderSwitch M.BoolT         = return ("True", "False")
orderSwitch (M.EitherT _ _) = return ("Left", "Right")
orderSwitch (M.OptionT _)   = return ("None", "Some")
orderSwitch (M.ListT _)     = return ("Cons", "Nil")
orderSwitch ty              = throwError (NotYetImplemented ("orderSwitch: " `T.append` pprint ty))
-}

genFunc ∷ M.ExprUT → CompilerM (StackRep → StackRep)
genFunc expr = if

  | expr `elem` [M.FailUT, M.NopUT] → return id

  | expr `elem` [M.DropUT] → return (drop 1)
  | expr `elem` [M.DupUT] → return (\(x:xs) → x:x:xs)
  | expr `elem` [M.SwapUT] → return (\(x:y:xs) → y:x:xs)

  | expr `elem` [M.AmountUT] → return ((:) FuncResult)

  | expr `elem` [M.DefaultAccountUT, M.CarUT, M.CdrUT, M.EqUT, M.LeUT] → return ((:) FuncResult . drop 1)

  | expr `elem` [M.AddIntIntUT, M.AddTezUT, M.SubIntUT, M.SubTezUT, M.MulIntIntUT, M.ConsPairUT] → return ((:) FuncResult . drop 2)

  | otherwise →
      case expr of

        M.SeqUT x y → do
          x ← genFunc x
          y ← genFunc y
          return (y . x)

        M.ConstUT c → return ((:) (Const c))

        _           → throwError (NotYetImplemented (T.concat ["genFunc: ", pprint expr]))

{-
    {- Stack must be empty; this version will only work in a specific case. TODO: Reserve field in storage for this purpose. -}
    App (BuiltIn b) x | b `elem` [M.EqUT, M.LeUT] → do
-}
