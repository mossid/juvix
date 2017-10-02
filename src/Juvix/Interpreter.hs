module Juvix.Interpreter where

import           Control.Monad.Except
import qualified Data.Either          as Either
import           Data.Foldable
import qualified Data.Set             as Set
import           Foundation           hiding (Left, Right)

import           Juvix.Script

{-  This is a more-or-less direct port of the Tezos client alpha protocol Michelson interpreter (https://github.com/tezos/tezos/blob/master/src/proto/alpha/script_interpreter.ml).
    Incomplete; in the future potentially to be used in a REPL, for compile-time constant reduction, automated testing, etc.  -}

interpret ∷ ∀ a b m . (Monad m, MonadError InterpretError m) ⇒ OriginationNonce → Int → Contract → Contract → Tez → Context → Lambda a b → a → m (b, Int, Context, OriginationNonce)
interpret origination quota orig source amount context (Lambda code) arg =

  let step ∷ ∀ a b . OriginationNonce → Int → Context → Descr a b → Stack a → m (Stack b, Int, Context, OriginationNonce)
      step origination quota context (Descr _ _ _ instr) stack =
        if quota <= 0 then throwError QuotaExceeded
        else case (instr, stack) of

          {-  Stack Operations  -}
          (Drop, Item _ rest)           → return (rest, quota - 1, context, origination)
          (Dup,  Item x rest)           → return (Item x (Item x rest), quota - 1, context, origination)
          (Swap, Item x (Item y rest))  → return (Item y (Item x rest), quota - 1, context, origination)
          (Const x, rest)               → return (Item x rest, quota - 1, context, origination)

          {-  Options   -}
          (ConsSome, Item x rest)                   → return (Item (Option $ Just x) rest, quota - 1, context, origination)
          (ConsNone, rest)                          → return (Item (Option Nothing) rest, quota - 1, context, origination)
          (IfNone t _, Item (Option Nothing) rest)  → step origination quota context t rest
          (IfNone _ f, Item (Option (Just x)) rest) → step origination quota context f (Item x rest)

          {-  Pairs   -}
          (ConsPair, Item x (Item y rest))          → return (Item (Pair (x, y)) rest, quota - 1, context, origination)
          (Car, Item (Pair (x, _)) rest)            → return (Item x rest, quota - 1, context, origination)
          (Cdr, Item (Pair (_, y)) rest)            → return (Item y rest, quota - 1, context, origination)

          {-  Unions  -}
          (Left, Item x rest)                               → return (Item (Union $ Either.Left x) rest, quota - 1, context, origination)
          (Right, Item y rest)                              → return (Item (Union $ Either.Right y) rest, quota - 1, context, origination)
          (IfLeft t _, Item (Union (Either.Left x)) rest)   → step origination quota context t (Item x rest)
          (IfLeft _ f, Item (Union (Either.Right y)) rest)  → step origination quota context f (Item y rest)

          {-  Lists   -}
          (ConsList, Item x (Item (List y) rest))             → return (Item (List $ x : y) rest, quota - 1, context, origination)
          (Nil, rest)                                         → return (Item (List []) rest, quota - 1, context, origination)
          (IfCons _ f, Item (List []) rest)                   → step origination quota context f rest
          (IfCons t _, Item (List (x:y)) rest)                → step origination quota context t (Item x (Item (List y) rest))
          (ListMap, Item (Lambda fun) (Item (List lst) rest)) → do
            (rlst, quota, context, origination) ← foldrM
              (\arg (acc, quota, context, origination) → do
                (ret, quota', context', origination') ← interpret origination quota orig source amount context (Lambda fun) arg
                return (ret : acc, quota', context', origination'))
              ([], quota, context, origination) lst
            return (Item (List rlst) rest, quota, context, origination)
          (ListReduce, Item (Lambda fun) (Item (List lst) (Item init rest))) → do
            (ret, quota, context, origination) ← foldlM
              (\(acc, quota, context, origination) arg → interpret origination quota orig source amount context (Lambda fun) (arg, acc))
              (init, quota, context, origination) lst
            return (Item ret rest, quota, context, origination)

          {-  Sets  -}
          (EmptySet, rest)                                  → return (Item (Set Set.empty) rest, quota - 1, context, origination)
          (SetMap, Item (Lambda fun) (Item (Set set) rest)) → do
            (ret, quota, context, origination) ← foldlM
              (\(acc, quota, context, origination) arg → do
                (ret, quota', context', origination') ← interpret origination quota orig source amount context (Lambda fun) arg
                return (Set.insert ret acc, quota', context', origination'))
              (Set.empty, quota, context, origination) (Set.toList set)
            return (Item (Set ret) rest, quota, context, origination)

          {-  Maps  -}

          _                             → undefined

      stack ∷ Stack (a, ())
      stack = Item arg Empty in

  step origination quota context code stack >>= \case
    (Item ret Empty, quota', context', origination') → return (ret, quota', context', origination')
