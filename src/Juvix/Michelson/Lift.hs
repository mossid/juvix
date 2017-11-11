module Juvix.Michelson.Lift where

import           Control.Monad.Except
import qualified Data.Text              as T
import           Data.Typeable
import           Foundation             hiding (Either (..))
import qualified Foundation             as F (Either (..))

import           Juvix.Michelson.Script
import           Juvix.Types            (CompileError (..))
import           Juvix.Utility

{-  Lift an untyped Michelson instruction sequence, with an initial stack type, into a typed (GADT) Michelson instruction sequence and return stack type in a typesafe manner.
    The type of the instruction sequence is *not* known until runtime (hence the heavy existential / scoped type variable use).
    If typechecking fails, return useful information about why and where.
    See [https://stackoverflow.com/questions/38024458/type-juggling-with-existentials-at-runtime] for some context, although this is substantially more intricate.
    This would be far nicer with dependent types.    -}

-- Do we need to also pass an expected return type? e.g. Either
-- Intermediary lift stage for type annotation of future instructions?
-- We do *know* the stack state from exprToMichelson, we could keep it through this stage.
-- https://github.com/tezos/tezos/blob/master/src/proto/alpha/script_ir_translator.ml

liftUntyped ∷ (MonadError CompileError m) ⇒ ExprUT → SomeStack → m (SomeExpr, SomeStack)
liftUntyped expr stk@(SomeStack (stack ∷ Stack stkTy)) = do

  let cannotUnify ∷ (MonadError CompileError m) ⇒ m (SomeExpr, SomeStack)
      cannotUnify = throwError (NotYetImplemented "Cannot unify types")

  case expr of

    DropUT ->
      case stack of
        Empty                                 → cannotUnify
        ((Item _ (rest ∷ Stack b)) ∷ Stack a) → return (SomeExpr (Drop ∷ Expr (Stack a) (Stack b)), SomeStack rest)

    DupUT ->
      case stack of
        Empty                                 → cannotUnify
        (Item (item ∷ a) (_ ∷ Stack b))    → return (SomeExpr (Dup ∷ Expr (Stack (a, b)) (Stack (a, (a, b)))), SomeStack (Item item stack))

    SwapUT ->
      case stack of
        (Item (x ∷ a) (Item (y ∷ b) (rest ∷ Stack c)))    → return (SomeExpr (Swap ∷ Expr (Stack (a, (b, c))) (Stack (b, (a, c)))), SomeStack (Item y (Item x rest)))
        _                                                 → cannotUnify

    ConstUT c ->
      case c of
        UnitUT      → return (SomeExpr (Const () ∷ Expr (Stack stkTy) (Stack ((), stkTy))), SomeStack (Item () stack))
        StringUT s  → return (SomeExpr (Const s ∷ Expr (Stack stkTy) (Stack (T.Text, stkTy))), SomeStack (Item s stack))
        BoolUT b    → return (SomeExpr (Const b ∷ Expr (Stack stkTy) (Stack (Bool, stkTy))), SomeStack (Item b stack))
        TezUT t     → return (SomeExpr (Const (Tez t) ∷ Expr (Stack stkTy) (Stack (Tez, stkTy))), SomeStack (Item (Tez t) stack))
        IntegerUT i → return (SomeExpr (Const i ∷ Expr (Stack stkTy) (Stack (Integer, stkTy))), SomeStack (Item i stack))

    ConsPairUT ->
      case stack of
        (Item (x ∷ a) (Item (y ∷ b) (rest ∷ Stack c))) → return (SomeExpr (ConsPair ∷ Expr (Stack (a, (b, c))) (Stack (Pair a b, c))), SomeStack (Item (Pair x y) rest))
        _                                              → cannotUnify

    CarUT ->
      case stack of
        (Item (pair ∷ a) (rest ∷ Stack b)) →
          case extractPair pair of
            Just (SomeType (x ∷ xT), SomeType (_ ∷ yT)) → return (SomeExpr (Car ∷ Expr (Stack (Pair xT yT, b)) (Stack (xT, b))), SomeStack (Item x rest))
            _ → cannotUnify
        _ → cannotUnify

    CdrUT ->
      case stack of
        (Item (pair ∷ a) (rest ∷ Stack b)) →
          case extractPair pair of
            Just (SomeType (_ ∷ xT), SomeType (y ∷ yT)) → return (SomeExpr (Cdr ∷ Expr (Stack (Pair xT yT, b)) (Stack (yT, b))), SomeStack (Item y rest))
            _ → cannotUnify
        _ → cannotUnify

    IfLeftUT xUT yUT ->
      case stack of
        (Item (union ∷ a) (rest ∷ Stack b)) ->
          case extractUnion union of
            Just (SomeType (xV ∷ xT), SomeType (yV ∷ yT)) → do
              (SomeExpr (x ∷ Expr (Stack xS) (Stack xF)), xEnd) ← liftUntyped xUT (SomeStack (Item xV rest))
              (SomeExpr (y ∷ Expr (Stack yS) (Stack yF)), _)    ← liftUntyped yUT (SomeStack (Item yV rest))
              case (eqT ∷ Maybe (xF :~: yF), eqT ∷ Maybe (xS :~: (xT, b)), eqT ∷ Maybe (yS :~: (yT, b))) of
                (Just Refl, Just Refl, Just Refl) → return (SomeExpr (IfLeft x y ∷ Expr (Stack (Union xT yT, b)) (Stack xF)), xEnd)
                _ → throwError (NotYetImplemented (T.concat ["liftUntyped - cannot unify: ", pprint expr]))
            _ → cannotUnify
        _ → cannotUnify

    LeftUT ->
      case stack of
        (Item (head ∷ a) (rest ∷ Stack b)) ->
          return (SomeExpr (Left ∷ Expr (Stack (a, b)) (Stack (Union a (), b))), SomeStack (Item (Union (F.Left head ∷ F.Either a ())) rest)) -- TODO

    RightUT ->
      case stack of
        (Item (head ∷ a) (rest ∷ Stack b)) ->
          return (SomeExpr (Right ∷ Expr (Stack (a, b)) (Stack (Union () a, b))), SomeStack (Item (Union (F.Right head ∷ F.Either () a)) rest)) -- TODO

    AddIntIntUT ->
      case stack of
        (Item (_ ∷ a) (Item (_ ∷ b) (rest ∷ Stack c))) ->
          case (eqT ∷ Maybe (a :~: Integer), eqT ∷ Maybe (b :~: Integer)) of
            (Just Refl, Just Refl) → return (SomeExpr (AddIntInt ∷ Expr (Stack (Integer, (Integer, c))) (Stack (Integer, c))), SomeStack (Item (undefined ∷ Integer) rest))
            _ → cannotUnify
        _ → cannotUnify

    MulIntIntUT ->
      case stack of
        (Item (_ ∷ a) (Item (_ ∷ b) (rest ∷ Stack c))) ->
          case (eqT ∷ Maybe (a :~: Integer), eqT ∷ Maybe (b :~: Integer)) of
            (Just Refl, Just Refl) → return (SomeExpr (MulIntInt ∷ Expr (Stack (Integer, (Integer, c))) (Stack (Integer, c))), SomeStack (Item (undefined ∷ Integer) rest))
            _ → throwError (NotYetImplemented (T.concat ["liftUntyped - cannot unify stack type: ", pprint expr]))
        _ → throwError (NotYetImplemented (T.concat ["liftUntyped - cannot unify stack form: ", pprint expr]))

    AddTezUT ->
      case stack of
        (Item (_ ∷ a) (Item (_ ∷ b) (rest ∷ Stack c))) ->
          case (eqT ∷ Maybe (a :~: Tez), eqT ∷ Maybe (b :~: Tez)) of
            (Just Refl, Just Refl) → return (SomeExpr (AddTez ∷ Expr (Stack (Tez, (Tez, c))) (Stack (Tez, c))), SomeStack (Item (undefined ∷ Tez) rest))
            _ → cannotUnify
        _ → cannotUnify

    DefaultAccountUT ->
      case stack of
        (Item (_ ∷ a) (rest ∷ Stack b)) ->
          case eqT ∷ Maybe (a :~: Hash) of
            Just Refl → return (SomeExpr (DefaultAccount ∷ Expr (Stack (Hash, b)) (Stack (Contract () (), b))), SomeStack (Item (undefined ∷ Contract () ()) rest))
            _ → cannotUnify
        _ → cannotUnify

    TransferTokensUT ->
      case stack of
        (Item (param ∷ a) (Item (tez ∷ b) (Item (contract ∷ c) (Item (storage ∷ d) Empty)))) ->
          case (extractParam contract, extractResult contract) of
            (Just (SomeType (_ ∷ paramType)), Just (SomeType (_ ∷ resultType))) ->
              case (eqT ∷ Maybe (b :~: Tez), eqT ∷ Maybe (a :~: paramType)) of
                (Just Refl, Just Refl) → return (SomeExpr (TransferTokens ∷ Expr (Stack (a, (Tez, (Contract paramType resultType, (d, ()))))) (Stack (resultType, (d, ())))), SomeStack (Item (undefined ∷ resultType) (Item storage Empty)))

    SeqUT xUT yUT → do
      (SomeExpr (x ∷ Expr (Stack xS) (Stack xF)), xEnd) ← liftUntyped xUT stk
      (SomeExpr (y ∷ Expr (Stack yS) (Stack yF)), yEnd) ← liftUntyped yUT xEnd
      case eqT ∷ Maybe (xF :~: yS) of
        Just Refl → return (SomeExpr (Seq x y ∷ Expr (Stack xS) (Stack yF)), yEnd)
        Nothing   → throwError (NotYetImplemented (T.concat ["liftUntyped - cannot unify seq: ", pprint xUT, " ", pprint yUT]))

    IfUT xUT yUT → do
      case stack of
        (Item (_ ∷ a) (rest ∷ Stack b)) ->
          case eqT ∷ Maybe (a :~: Bool) of
            Just Refl → do
              let start = SomeStack rest
              (SomeExpr (x ∷ Expr (Stack xS) (Stack xF)), SomeStack (xEnd ∷ Stack xEndT)) ← liftUntyped xUT start
              (SomeExpr (y ∷ Expr (Stack yS) (Stack yF)), SomeStack (_ ∷ Stack yEndT)) ← liftUntyped yUT start
              case (eqT ∷ Maybe (xS :~: yS), eqT ∷ Maybe (xF :~: yF), eqT ∷ Maybe (xEndT :~: yEndT)) of
                (Just Refl, Just Refl, Just Refl) → do
                  return (SomeExpr (If x y), SomeStack xEnd)
            _ → cannotUnify
        _ → cannotUnify

    DipUT xUT ->
      case stack of
        Empty                                       → cannotUnify
        ((Item (val ∷ a) (rest ∷ Stack b)) ∷ Stack c) → do
          (SomeExpr (x ∷ Expr (Stack xS) (Stack xF)), SomeStack xEnd) ← liftUntyped xUT (SomeStack rest)
          case eqT ∷ Maybe (xS :~: b) of
            Just Refl → return (SomeExpr (Dip x ∷ Expr (Stack c) (Stack (a, xF))), SomeStack (Item val xEnd))
            Nothing   → throwError (NotYetImplemented (T.concat ["liftUntyped - cannot unify dip: ", pprint xUT]))

    LambdaUT xUT → do
      (SomeExpr (x ∷ Expr (Stack xS) (Stack xF)), _) ← liftUntyped xUT (SomeStack Empty) {- TODO: Magic stack type. -}
      return (SomeExpr (Lambda (LambdaW x) ∷ Expr (Stack stkTy) (Stack (Lambda xS xF, stkTy))), SomeStack (Item (undefined ∷ Lambda xS xF) stack))

    FailUT  → return (SomeExpr (Fail ∷ Expr (Stack stkTy) (Stack ())), stk)

    NopUT   → return (SomeExpr (Nop ∷ Expr (Stack stkTy) (Stack stkTy)), stk)

    CompareUT ->
      case stack of
        (Item (_ ∷ a) (Item (_ ∷ b) (rest ∷ Stack c))) ->
          case eqT ∷ Maybe (a :~: b) of
            Just Refl → return (SomeExpr (Compare ∷ Expr (Stack (a, (a, c))) (Stack (Int, c))), SomeStack (Item (undefined ∷ Integer) rest))
            _ → cannotUnify
        _ → cannotUnify

    NowUT       → return (SomeExpr (Now ∷ Expr (Stack stkTy) (Stack (Timestamp, stkTy))), SomeStack (Item (undefined ∷ Timestamp) stack))
    BalanceUT   → return (SomeExpr (Balance ∷ Expr (Stack stkTy) (Stack (Tez, stkTy))), SomeStack (Item (undefined ∷ Tez) stack))
    AmountUT    → return (SomeExpr (Amount ∷ Expr (Stack stkTy) (Stack (Tez, stkTy))), SomeStack (Item (undefined ∷ Tez) stack))

    expr → throwError (NotYetImplemented ("liftUntyped: " `T.append` pprint expr))

{-  Lift a Michelson type into a Haskell type existential.  -}

liftType ∷ Type → SomeType
liftType UnitT          = SomeType (undefined ∷ ())
liftType KeyT           = SomeType (undefined ∷ Key)
liftType HashT          = SomeType (undefined ∷ Hash)
liftType IntT           = SomeType (undefined ∷ Integer)
liftType TezT           = SomeType (undefined ∷ Tez)
liftType BoolT          = SomeType (undefined ∷ Bool)
liftType StringT        = SomeType (undefined ∷ T.Text)
liftType (EitherT a b)  =
  case (liftType a, liftType b) of
    (SomeType (_ ∷ a), SomeType (_ ∷ b)) → SomeType (undefined ∷ Union a b)
liftType (OptionT a) =
  case liftType a of
    SomeType (_ ∷ a) → SomeType (undefined ∷ Option a)
liftType (ListT a) =
  case liftType a of
    SomeType (_ ∷ a) → SomeType (undefined ∷ List a)
liftType (PairT a b) =
  case (liftType a, liftType b) of
    (SomeType (_ ∷ x), SomeType (_ ∷ y)) → SomeType (undefined ∷ Pair x y)
liftType (LamT _ _)  = SomeType (undefined ∷ ())

{-  Utility functions.  -}

typeToStack ∷ Type → SomeStack
typeToStack ty =
  case liftType ty of
    SomeType v → mkStack v

mkStack ∷ ∀ a . (Eq a, Typeable a, Extractable a) ⇒ a → SomeStack
mkStack val = SomeStack (Item val Empty ∷ Stack (a, ()))
