module Juvix.Michelson.Script where

import qualified Data.Map.Strict as Map
import qualified Data.Maybe      as Maybe
import qualified Data.Set        as Set
import qualified Data.Text       as Text
import           Data.Typeable
import           Foundation      hiding (Left, Right)
import qualified Foundation      as F (Either (..))
import qualified Prelude         as P

{-  Expr Type     -}

data Type
  = UnitT
  | KeyT
  | HashT
  | IntT
  | TezT
  | BoolT
  | StringT
  | EitherT Type Type
  | OptionT Type
  | ListT Type
  | PairT Type Type
  | LamT Type Type

  deriving (Show, Eq)

{-  Expr (untyped)  -}

data ConstUT
  = UnitUT
  | StringUT Text.Text
  | BoolUT Bool
  | TezUT Integer
  | IntegerUT Integer

  deriving (P.Read, Show, Eq)

data ExprUT

  {- Stack Operations -}

  = DropUT
  | DupUT
  | SwapUT
  | ConstUT ConstUT

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

  {- Protocol -}

  | ManagerUT
  | TransferTokensUT
  | CreateAccountUT
  | DefaultAccountUT
  | CreateContractUT
  | NowUT
  | BalanceUT
  | CheckSignatureUT
  | HashKeyUT
  | HUT
  | StepsToQuotaUT
  | SourceUT
  | AmountUT

  deriving (P.Read, Show, Eq)

{-  Interpreter Types   -}

data Contract a b =

  Default () |
  Originated ()

  deriving (Eq)

instance (Extractable a, Extractable b) ⇒ Extractable (Contract a b)

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

newtype Tez         = Tez       { unTez ∷ Integer }                   deriving (Eq)

newtype Key         = Key       { unKey ∷ Text.Text }                 deriving (Eq)

newtype Hash        = Hash      { unHash ∷ Text.Text }                deriving (Eq)

newtype Signature   = Signature { unSignature ∷ Text.Text }           deriving (Eq)

data Pair a b       where Pair ∷ (Typeable a, Typeable b, Eq a, Eq b) ⇒ a → b → Pair a b

instance (Eq a, Eq b) ⇒ Eq (Pair a b) where
  Pair a b == Pair c d = a == c && b == d

newtype Map k v     = Map       { unMap ∷ Map.Map k v }               deriving (Eq)

newtype Set a       = Set       { unSet ∷ Set.Set a }                 deriving (Eq)

newtype Option a    = Option    { unOption ∷ Maybe.Maybe a }          deriving (Eq)

data Union a b      where Union ∷ (Typeable a, Typeable b, Eq a, Eq b) ⇒ Either a b → Union a b

instance (Eq a, Eq b) ⇒ Eq (Union a b) where
  Union (F.Left x)  == Union (F.Left y)   = x == y
  Union (F.Right x) == Union (F.Right y)  = x == y
  _               == _                = False

newtype List a      = List      { unList ∷ [a] }                      deriving (Eq)

data Lambda a b     where LambdaW ∷ (Typeable a, Typeable b) ⇒ Descr a b → Lambda a b

instance (Eq a, Eq b) ⇒ Eq (Lambda a b) where
  LambdaW x == LambdaW y = x == y

newtype Timestamp   = Timestamp { unTimestamp ∷ Integer }             deriving (Eq)

newtype Nat         = Nat       { unNat ∷ Integer }                   deriving (Eq)

type Descr a b      = Expr (Stack a) (Stack b)

data SomeExpr where
  SomeExpr  ∷ ∀ a b . (Typeable a, Typeable b, Eq a, Eq b) ⇒ Expr (Stack a) (Stack b) → SomeExpr

data SomeStack where
  SomeStack ∷ ∀ a . (Typeable a, Eq a) ⇒ Stack a → SomeStack

data SomeType where
  SomeType  ∷ ∀ a . (Extractable a, Typeable a, Eq a) ⇒ a → SomeType

{-  Stack (GADT)  -}

data Stack a where

  Item      ∷ ∀ a b . (Eq a, Eq b, Typeable a, Typeable b, Extractable a) ⇒ a → Stack b → Stack (a, b)
  Empty     ∷ Stack ()

  deriving (Typeable)

class Extractable a where
  extractPair ∷ a → Maybe (SomeType, SomeType)
  extractPair _ = Nothing

  extractUnion ∷ a → Maybe (SomeType, SomeType)
  extractUnion _ = Nothing

instance (Extractable a) ⇒ Extractable (Option a)

instance (Extractable a) ⇒ Extractable (List a)

instance (Extractable a, Extractable b, Typeable a, Typeable b, Eq a, Eq b) ⇒ Extractable (Pair a b) where
  extractPair (_ ∷ Pair x y) = return (SomeType (undefined ∷ x), SomeType (undefined ∷ y))

instance (Extractable a, Extractable b, Typeable a, Typeable b, Eq a, Eq b) ⇒ Extractable (Union a b) where
  extractUnion (_ ∷ Union a b) = return (SomeType (undefined ∷ a), SomeType (undefined ∷ b))

instance Extractable Text.Text
instance Extractable Tez
instance Extractable Key
instance Extractable Hash
instance Extractable Timestamp
instance Extractable Integer
instance Extractable ()
instance Extractable Bool

instance Extractable (Lambda a b)

instance Eq a ⇒ Eq (Stack a) where
  Empty     == Empty    = True
  Item a b  == Item c d = a == c && b == d

{-  Expr (GADT)   -}

data Expr a b where

  {- Stack Operations -}

  Drop      ∷ ∀ a b . (Typeable a, Typeable b, Extractable a) ⇒ Expr (Stack (a, b)) (Stack b)
  Dup       ∷ ∀ a b . (Typeable a, Typeable b, Extractable a) ⇒ Expr (Stack (a, b)) (Stack (a, (a, b)))
  Swap      ∷ ∀ a b c . (Typeable a, Typeable b, Typeable c, Extractable a, Extractable b) ⇒ Expr (Stack (a, (b, c))) (Stack (b, (a, c)))
  Const     ∷ ∀ a b . (Extractable a, Typeable a, Typeable b, Eq a, Eq b) ⇒ a → Expr (Stack b) (Stack (a, b))

  {- Pairs -}

  ConsPair  ∷ ∀ a b c . (Typeable a, Typeable b, Typeable c, Eq a, Eq b, Eq c, Extractable a, Extractable b) ⇒ Expr (Stack (a, (b, c))) (Stack (Pair a b, c))
  Car       ∷ ∀ a b c . Expr (Stack (Pair a b, c)) (Stack (a, c))
  Cdr       ∷ ∀ a b c . Expr (Stack (Pair a b, c)) (Stack (b, c))

  {- Options -}

  ConsSome  ∷ Expr (Stack (a, b)) (Stack (Option a, b))
  ConsNone  ∷ Expr (Stack b) (Stack (Option a, b))
  IfNone    ∷ (Typeable a, Typeable b, Typeable c, Eq a, Eq b, Eq c) ⇒ Descr b c → Descr (a, b) c → Expr (Stack (Option a, b)) (Stack c)

  {- Unions -}

  Left      ∷ Expr (Stack (a, b)) (Stack (Union a c, b))
  Right     ∷ Expr (Stack (a, b)) (Stack (Union c a, b))
  IfLeft    ∷ ∀ a b c d . (Typeable a, Typeable b, Typeable c, Typeable d, Eq a, Eq b, Eq c, Eq d) ⇒ Descr (a, c) d → Descr (b, c) d → Expr (Stack (Union a b, c)) (Stack d)

  {- Lists -}

  ConsList    ∷ Expr (Stack (a, (List a, b))) (Stack (List a, b))
  Nil         ∷ Expr (Stack a) (Stack (List b, a))
  IfCons      ∷ ∀ a b c . (Typeable a, Typeable b, Typeable c, Eq a, Eq b, Eq c) ⇒ Descr (a, (List a, b)) c → Descr b c → Expr (Stack (List a, b)) (Stack c)
  ListMap     ∷ Expr (Stack (Lambda a b, (List a, c))) (Stack (List b, c))
  ListReduce  ∷ Expr (Stack (Lambda (a, b) b, (List a, (b, c)))) (Stack (b, c))

  {- Sets -}

  EmptySet    ∷ (Ord a) ⇒ Expr (Stack b) (Stack (Set a, b))
  SetMap      ∷ (Ord a, Ord b) ⇒ Expr (Stack (Lambda a b, (Set a, c))) (Stack (Set b, c))
  SetReduce   ∷ (Ord a) ⇒ Expr (Stack (Lambda (a, b) b, (Set a, (b, c)))) (Stack (b, c))
  SetMem      ∷ (Ord a) ⇒ Expr (Stack (a, (Set a, b))) (Stack (Bool, b))
  SetUpdate   ∷ (Ord a) ⇒ Expr (Stack (a, (Bool, (Set a, b)))) (Stack (Set a, b))
  SetSize     ∷ (Ord a) ⇒ Expr (Stack (Set a, b)) (Stack (Integer, b))

  {- Maps -}

  EmptyMap    ∷ (Ord k) ⇒ Expr (Stack b) (Stack (Map k v, b))
  MapMap      ∷ (Ord k) ⇒ Expr (Stack (Lambda (k, v) r, (Map k v, b))) (Stack (Map k v, b))
  MapReduce   ∷ (Ord k) ⇒ Expr (Stack (Lambda ((k, v), a) a, (Map k v, b))) (Stack (a, b))
  MapMem      ∷ (Ord k) ⇒ Expr (Stack (a, (Map k v, b))) (Stack (Bool, b))
  MapGet      ∷ (Ord k) ⇒ Expr (Stack (k, (Map k v, b))) (Stack (Option v, b))
  MapUpdate   ∷ (Ord k) ⇒ Expr (Stack (k, (Option v, (Map k v, b)))) (Stack (Map k v, b))
  MapSize     ∷ (Ord k) ⇒ Expr (Stack (Map k v, b)) (Stack (Integer, b))

  {- String Operations -}

  Concat      ∷ Expr (Stack (String, (String, b))) (Stack (String, b))

  {- Timestamp Operations -}

  AddSecondsToTimestamp ∷ Expr (Stack (Nat, (Timestamp, b))) (Stack (Timestamp, b))
  AddTimestampToSeconds ∷ Expr (Stack (Timestamp, (Nat, b))) (Stack (Timestamp, b))

  {- Currency Operations -}

  AddTez      ∷ Expr (Stack (Tez, (Tez, a))) (Stack (Tez, a))
  SubTez      ∷ Expr (Stack (Tez, (Tez, a))) (Stack (Tez, a))
  MulTezNat   ∷ Expr (Stack (Tez, (Nat, a))) (Stack (Tez, a))
  MulNatTez   ∷ Expr (Stack (Nat, (Tez, a))) (Stack (Tez, a))
  EdivTezNat  ∷ Expr (Stack (Tez, (Nat, a))) (Stack (Pair Tez Tez, a))
  EdivTez     ∷ Expr (Stack (Tez, (Tez, a))) (Stack (Pair Nat Tez, a))

  {- Boolean Operations -}

  Or          ∷ Expr (Stack (Bool, (Bool, a))) (Stack (Bool, a))
  And         ∷ Expr (Stack (Bool, (Bool, a))) (Stack (Bool, a))
  Xor         ∷ Expr (Stack (Bool, (Bool, a))) (Stack (Bool, a))
  Not         ∷ Expr (Stack (Bool, a)) (Stack (Bool, a))

  {- Integer Operations -}

  NegNat      ∷ Expr (Stack (Nat, a)) (Stack (Integer, a))
  NegInt      ∷ Expr (Stack (Integer, a)) (Stack (Integer, a))
  AbsInt      ∷ Expr (Stack (Integer, a)) (Stack (Nat, a))
  IntNat      ∷ Expr (Stack (Integer, a)) (Stack (Nat, a))
  AddIntInt   ∷ Expr (Stack (Integer, (Integer, a))) (Stack (Integer, a))
  AddIntNat   ∷ Expr (Stack (Integer, (Nat, a))) (Stack (Integer, a))
  AddNatInt   ∷ Expr (Stack (Nat, (Integer, a))) (Stack (Integer, a))
  AddNatNat   ∷ Expr (Stack (Nat, (Nat, a))) (Stack (Nat, a))
  SubInt      ∷ Expr (Stack (Integer, (Integer, a))) (Stack (Integer, a))
  MulIntInt   ∷ Expr (Stack (Integer, (Integer, a))) (Stack (Integer, a))
  MulIntNat   ∷ Expr (Stack (Integer, (Nat, a))) (Stack (Integer, a))
  MulNatInt   ∷ Expr (Stack (Nat, (Integer, a))) (Stack (Integer, a))
  MulNatNat   ∷ Expr (Stack (Nat, (Nat, a))) (Stack (Nat, a))
  EdivIntInt  ∷ Expr (Stack (Integer, (Integer, a))) (Stack (Option (Pair Integer Nat), a))
  EdivIntNat  ∷ Expr (Stack (Integer, (Nat, a))) (Stack (Option (Pair Integer Nat), a))
  EdivNatInt  ∷ Expr (Stack (Nat, (Integer, a))) (Stack (Option (Pair Integer Nat), a))
  EdivNatNat  ∷ Expr (Stack (Nat, (Nat, a))) (Stack (Option (Pair Nat Nat), a))
  LslNat      ∷ Expr (Stack (Nat, (Nat, a))) (Stack (Nat, a))
  LsrNat      ∷ Expr (Stack (Nat, (Nat, a))) (Stack (Nat, a))
  OrNat       ∷ Expr (Stack (Nat, (Nat, a))) (Stack (Nat, a))
  AndNat      ∷ Expr (Stack (Nat, (Nat, a))) (Stack (Nat, a))
  XorNat      ∷ Expr (Stack (Nat, (Nat, a))) (Stack (Nat, a))
  NotNat      ∷ Expr (Stack (Nat, a)) (Stack (Integer, a))
  NotInt      ∷ Expr (Stack (Integer, a)) (Stack (Integer, a))

  {- Control -}

  Seq     ∷ (Typeable a, Typeable b, Typeable c, Eq a, Eq b, Eq c) ⇒ Descr a b → Descr b c → Expr (Stack a) (Stack c)
  If      ∷ (Typeable a, Typeable b, Eq a, Eq b) ⇒ Descr a b → Descr a b → Expr (Stack (Bool, a)) (Stack b)
  Loop    ∷ Descr a (Bool, a) → Expr (Stack (Bool, a)) (Stack a)
  Dip     ∷ ∀ a b c . (Typeable a, Typeable b, Typeable c, Eq a, Eq b, Eq c) ⇒ Descr b c → Expr (Stack (a, b)) (Stack (a, c))
  Exec    ∷ Expr (Stack (a, (Lambda a b, c))) (Stack (b, c))
  Lambda  ∷ ∀ a b c . (Typeable a, Typeable b, Eq a, Eq b) ⇒ Lambda a b → Expr (Stack c) (Stack (Lambda a b, c))
  Fail    ∷ ∀ a b . (Typeable a, Typeable b) ⇒ Expr (Stack a) (Stack b)
  Nop     ∷ Expr (Stack a) (Stack a)

  {- Comparision -}

  Compare ∷ Expr (Stack (a, (a, b))) (Stack (Int, b))

  {- Comparators -}

  Eq    ∷ Expr (Stack (Integer, a)) (Stack (Bool, a))
  Neq   ∷ Expr (Stack (Integer, a)) (Stack (Bool, a))
  Lt    ∷ Expr (Stack (Integer, a)) (Stack (Bool, a))
  Gt    ∷ Expr (Stack (Integer, a)) (Stack (Bool, a))
  Le    ∷ Expr (Stack (Integer, a)) (Stack (Bool, a))
  Ge    ∷ Expr (Stack (Integer, a)) (Stack (Bool, a))

  {- Protocol -}

  Manager         ∷ Expr (Stack (Contract a b, c)) (Stack (Hash, c))
  TransferTokens  ∷ Expr (Stack (a, (Tez, (Contract a b, (c, ()))))) (Stack (b, (c, ())))
  CreateAccount   ∷ Expr (Stack (Hash, (Option Hash, (Bool, (Tez, a))))) (Stack (Contract () (), a))
  DefaultAccount  ∷ Expr (Stack (Hash, a)) (Stack (Contract () (), a))
  CreateContract  ∷ Expr (Stack (Hash, (Option Hash, (Bool, (Bool, (Tez, (Lambda (a, b) (c, b), (b, d)))))))) (Stack (Contract a c, d))
  Now             ∷ Expr (Stack a) (Stack (Timestamp, a))
  Balance         ∷ Expr (Stack a) (Stack (Tez, a))
  CheckSignature  ∷ Expr (Stack (Key, (Signature, a))) (Stack (Bool, a))
  HashKey         ∷ Expr (Stack (Key, a)) (Stack (Hash, a))
  H               ∷ Expr (Stack (a, b)) (Stack (String, b))
  StepsToQuota    ∷ Expr (Stack a) (Stack (Nat, a))
  Source          ∷ Expr (Stack a) (Stack (Contract b c, a))
  Amount          ∷ Expr (Stack a) (Stack (Tez, a))

  deriving (Typeable)

instance (Eq a, Eq b) ⇒ Eq (Expr a b) where

  Drop        == Drop     = True
  Dup         == Dup      = True
  Swap        == Swap     = True
  Const a     == Const b  = a == b

  ConsPair    == ConsPair = True
  Car         == Car      = True
  Cdr         == Cdr      = True

  ConsSome    == ConsSome   = True
  ConsNone    == ConsNone   = True
  IfNone a b  == IfNone c d = a == c && b == d

  Left        == Left       = True
  Right       == Right      = True
  IfLeft a b  == IfLeft c d = a == c && b == d

  ConsList    == ConsList   = True
  Nil         == Nil        = True
  IfCons a b  == IfCons c d = a == c && b == d
  ListMap     == ListMap    = True
  ListReduce  == ListReduce = True

  EmptySet    == EmptySet   = True
  SetMap      == SetMap     = True
  SetReduce   == SetReduce  = True
  SetMem      == SetMem     = True
  SetUpdate   == SetUpdate  = True
  SetSize     == SetSize    = True

  EmptyMap    == EmptyMap   = True
  MapMap      == MapMap     = True
  MapReduce   == MapReduce  = True
  MapMem      == MapMem     = True
  MapGet      == MapGet     = True
  MapUpdate   == MapUpdate  = True
  MapSize     == MapSize    = True

  Concat      == Concat     = True

  AddSecondsToTimestamp == AddSecondsToTimestamp = True
  AddTimestampToSeconds == AddTimestampToSeconds = True

  AddTez      == AddTez     = True
  SubTez      == SubTez     = True
  MulTezNat   == MulTezNat  = True
  MulNatTez   == MulNatTez  = True
  EdivTezNat  == EdivTezNat = True
  EdivTez     == EdivTez    = True

  Or          == Or         = True
  And         == And        = True
  Xor         == Xor        = True
  Not         == Not        = True

  NegNat      == NegNat     = True
  NegInt      == NegInt     = True
  AbsInt      == AbsInt     = True
  IntNat      == IntNat     = True
  AddIntInt   == AddIntInt  = True
  AddIntNat   == AddIntNat  = True

  SubInt      == SubInt     = True
  MulIntInt   == MulIntInt  = True
  MulIntNat   == MulIntNat  = True
  MulNatInt   == MulNatInt  = True
  MulNatNat   == MulNatNat  = True
  EdivIntInt  == EdivIntInt = True
  EdivIntNat  == EdivIntNat = True
  EdivNatInt  == EdivNatInt = True
  EdivNatNat  == EdivNatNat = True
  LslNat      == LslNat     = True
  LsrNat      == LsrNat     = True
  OrNat       == OrNat      = True
  AndNat      == AndNat     = True
  XorNat      == XorNat     = True
  NotNat      == NotNat     = True
  NotInt      == NotInt     = True

  Seq (a ∷ Expr (Stack q) (Stack xA)) b == Seq (c ∷ Expr (Stack w) (Stack yA)) d =
    case eqT ∷ Maybe (xA :~: yA) of
      Just Refl → a == c && b == d
      Nothing   → False
  If a b      == If c d     = a == c && b == d
  Loop a      == Loop b     = a == b
  Dip a       == Dip b      = a == b
  Exec        == Exec       = True
  Lambda a    == Lambda b   = a == b
  Fail        == Fail       = True
  Nop         == Nop        = True

  Compare     == Compare    = True

  Eq          == Eq         = True
  Neq         == Neq        = True
  Lt          == Lt         = True
  Gt          == Gt         = True
  Le          == Le         = True
  Ge          == Ge         = True

  Manager         == Manager          = True
  TransferTokens  == TransferTokens   = True
  CreateAccount   == CreateAccount    = True
  DefaultAccount  == DefaultAccount   = True
  CreateContract  == CreateContract   = True
  Now             == Now              = True
  Balance         == Balance          = True
  CheckSignature  == CheckSignature   = True
  HashKey         == HashKey          = True
  H               == H                = True
  StepsToQuota    == StepsToQuota     = True
  Source          == Source           = True
  Amount          == Amount           = True

  _ == _ = False
