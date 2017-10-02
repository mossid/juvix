module Juvix.Script where

import qualified Data.Either     as Either
import qualified Data.Map.Strict as Map
import qualified Data.Maybe      as Maybe
import qualified Data.Set        as Set
import qualified Data.Text       as Text
import           Foundation
import qualified Prelude         as P

{-  Interpreter Types   -}

type Tez = Integer

data Contract =

  Default () |
  Originated ()

data OriginationNonce = OriginationNonce {
  hash  ∷ (),
  index ∷ Int32
}

data Storage = Storage

type Context = Storage

data InterpretError =

  QuotaExceeded |
  UndefinedBehaviour

  deriving (Show, Eq)

data Type
  = UnitT
  | KeyT
  | IntT
  | TezT
  | BoolT
  | StringT
  | PairT Type Type
  | LamT Type Type

  deriving (Show, Eq)

{-  Expr (untyped)  -}

data ExprUT

  {- Constants -}

  = UnitUT
  | StringUT Text.Text
  | BoolUT Bool
  | TezUT Integer
  | IntegerUT Integer

  {- Stack Operations -}

  | DropUT
  | DupUT
  | SwapUT
  | ConstUT ExprUT

  {- Pairs -}

  | ConsPairUT
  | CarUT
  | CdrUT

  {- Options -}

  | ConsSomeUT
  | ConsNoneUT
  | IfNoneUT ExprUT ExprUT

  {- Unions -}

  | LeftUT
  | RightUT
  | IfLeftUT ExprUT ExprUT

  {- Lists -}

  | ConsListUT
  | NilUT
  | IfConsUT ExprUT ExprUT
  | ListMapUT
  | ListReduceUT

  {- Sets -}

  | EmptySetUT
  | SetMapUT
  | SetReduceUT
  | SetMemUT
  | SetUpdateUT
  | SetSizeUT

  {- Maps -}

  | EmptyMapUT
  | MapMapUT
  | MapReduceUT
  | MapMemUT
  | MapGetUT
  | MapUpdateUT
  | MapSizeUT

  {- String Operations -}

  | ConcatUT

  {- Timestamp Operations -}

  | AddSecondsToTimestampUT
  | AddTimestampToSecondsUT

  {- Currency Operations -}

  | AddTezUT
  | SubTezUT
  | MulTezNatUT
  | MulNatTezUT
  | EdivTezNatUT
  | EdivTezUT

  {- Boolean Operations -}

  | OrUT
  | AndUT
  | XorUT
  | NotUT

  {- Integer Operations -}

  | NegNatUT
  | NegIntUT
  | AbsNatUT
  | AbsIntUT
  | IntNatUT
  | AddIntIntUT
  | AddIntNatUT
  | AddNatIntUT
  | AddNatNatUT
  | SubIntUT
  | MulIntIntUT
  | MulIntNatUT
  | MulNatIntUT
  | MulNatNatUT
  | EdivIntIntUT
  | EdivIntNatUT
  | EdivNatIntUT
  | EdivNatNatUT
  | LslNatUT
  | LsrNatUT
  | OrNatUT
  | AndNatUT
  | XorNatUT
  | NotNatUT
  | NotIntUT

  {- Control -}

  | SeqUT ExprUT ExprUT
  | IfUT ExprUT ExprUT
  | LoopUT ExprUT
  | DipUT ExprUT
  | ExecUT
  | LambdaUT ExprUT
  | FailUT
  | NopUT

  {- Comparision -}

  | CompareUT
  | EqUT
  | NeqUT
  | LtUT
  | GtUT
  | LeUT
  | GeUT

  | CmpLtUT
  | CmpLeUT
  | CmpEqUT
  | CmpGeUT
  | CmpGtUT

  {- Protocol -}

  | ManagerUT
  | TransferTokensUT
  | CreateAccountUT
  | DefaultAccountUT
  | CreateContractUT
  | NowUT
  | BalanceUT
  | CheckSignatureUT
  | HUT
  | StepsToQuotaUT
  | SourceUT
  | AmountUT

  deriving (P.Read, Show, Eq)

newtype Pair a b    = Pair    { unPair ∷ (a, b) }

newtype Map k v     = Map     { unMap ∷ Map.Map k v }

newtype Set a       = Set     { unSet ∷ Set.Set a }

newtype Option a    = Option  { unOption ∷ Maybe.Maybe a }

newtype Union a b   = Union   { unUnion ∷ Either.Either a b }

newtype List a      = List    { unList ∷ [a] }

newtype Lambda a b  = Lambda  { unLambda ∷ Descr (a, ()) (b, ()) }

data Script a b c = Script {
  code        ∷ Lambda (Pair (Pair Tez a) c) (Pair b c),
  argType     ∷ a,
  retType     ∷ b,
  storage     ∷ c,
  storageType ∷ c
}

data Stack a where

  Item      ∷ a → Stack b → Stack (a, b)
  Empty     ∷ Stack ()

{-  Expr (GADT)   -}

data Expr a b where

  {- Stack Operations -}

  Drop      ∷ Expr (Stack (a, b)) (Stack b)
  Dup       ∷ Expr (Stack (a, b)) (Stack (a, (a, b)))
  Swap      ∷ Expr (Stack (a, (b, c))) (Stack (b, (a, c)))
  Const     ∷ a → Expr (Stack b) (Stack (a, b))

  {- Pairs -}

  ConsPair  ∷ Expr (Stack (a, (b, c))) (Stack (Pair a b, c))
  Car       ∷ Expr (Stack (Pair a b, c)) (Stack (a, c))
  Cdr       ∷ Expr (Stack (Pair a b, c)) (Stack (b, c))

  {- Options -}

  ConsSome  ∷ Expr (Stack (a, b)) (Stack (Option a, b))
  ConsNone  ∷ Expr (Stack b) (Stack (Option a, b))
  IfNone    ∷ Descr b c → Descr (a, b) c → Expr (Stack (Option a, b)) (Stack c)

  {- Unions -}

  Left      ∷ Expr (Stack (a, b)) (Stack (Union a c, b))
  Right     ∷ Expr (Stack (a, b)) (Stack (Union c a, b))
  IfLeft    ∷ Descr (a, c) d → Descr (b, c) d → Expr (Stack (Union a b, c)) (Stack d)

  {- Lists -}

  ConsList    ∷ Expr (Stack (a, (List a, b))) (Stack (List a, b))
  Nil         ∷ Expr (Stack a) (Stack (List b, a))
  IfCons      ∷ Descr (a, (List a, b)) c → Descr b c → Expr (Stack (List a, b)) (Stack c)
  ListMap     ∷ Expr (Stack (Lambda a b, (List a, c))) (Stack (List b, c))
  ListReduce  ∷ Expr (Stack (Lambda (a, b) b, (List a, (b, c)))) (Stack (b, c))

  {- Sets -}

  EmptySet    ∷ (Ord a) ⇒ Expr (Stack b) (Stack (Set a, b))
  SetMap      ∷ (Ord a, Ord b) ⇒ Expr (Stack (Lambda a b, (Set a, c))) (Stack (Set b, c))
  SetReduce   ∷ (Ord a) ⇒ Expr (Stack (Lambda (a, b) b, (Set a, (b, c)))) (Stack (b, c))
  SetMem      ∷ (Ord a) ⇒ Expr (Stack (a, (Set a, b))) (Stack (Bool, b))
  SetUpdate   ∷ (Ord a) ⇒ Expr (Stack (a, (Bool, (Set a, b)))) (Stack (Set a, b))

  {- Maps -}

  {- String Operations -}

  {- Timestamp Operations -}

  {- Currency Operations -}

  {- Boolean Operations -}

  {- Integer Operations -}

  {- Control -}

  Seq ∷ Descr a b → Descr b c → Expr (Stack a) (Stack c)
  Exec ∷ Expr (Stack (a, (Lambda a b, c))) (Stack (b, c))
  LambdaE ∷ Lambda a b → Expr (Stack c) (Stack (Lambda a b, c))
  Fail ∷ Expr (Stack a) (Stack b)
  Nop ∷ Expr (Stack a) (Stack a)

  {- Comparision -}

  {- Comparators -}

  {- Casts -}

  {- Protocol -}

data Descr a b = Descr {
  loc   ∷ Int,
  bef   ∷ Stack a,
  aft   ∷ Stack b,
  instr ∷ Expr (Stack a) (Stack b)
}
