{-# LANGUAGE EmptyDataDecls         #-}
{-# LANGUAGE ExplicitForAll         #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MagicHash              #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE UnicodeSyntax          #-}

{-  Prelude-equivalent for Juvix-compiled programs.
    This file is mostly a lie; these functions are replaced by the transpiler with Michelson builtins.
    The intent of this library is to be "batteries-included" - so additional common functionality should be added as necessary.
    Subject to API instability until Michelson is finalized.  -}

module Juvix.Lib (

  {- Datatypes -}

  List,
  Set,
  Map,
  Signature,
  Key,
  Contract,
  Account,
  Either(..),
  Maybe,
  Pair,
  Unit,
  Tez,
  String,
  Int,
  Nat,
  Bool(..),

  {- General -}

  IO,
  Applicative,
  Functor,
  Monad,
  Monoid,
  pure,
  (<*>),
  fmap,
  (>>=),
  return,
  mappend,
  mempty,
  (<>),

  {- Boolean Operations -}

  (&&),
  (||),
  not,
  xor,

  {- Comparision -}

  Ord,
  compare,
  max,
  min,
  (<),
  (<=),
  (>),
  (>=),

  {- Equality -}

  Eq,
  (==),
  (/=),

  {- Arithmetic -}

  Mul,
  Add,
  Div,
  Sub,
  Negate,
  (+),
  (-),
  (*),
  (/),
  negate,
  abs,

  {- Michelson Calls -}

  manager,
  defaultAccount,
  source,
  self,
  transferTokens,
  now,
  amount,
  balance,
  hash,
  stepsToQuota,
  fail,

  {- Misc -}

  either,
  maybe,
  fromMaybe,
  emptySet,
  setMember,
  setUpdate,
  setSize,
  emptyMap,
  mapGet,
  mapMember,
  mapUpdate,
  mapSize

) where

import           Data.Either (Either (..), either)
import           Data.Eq     (Eq, (/=), (==))
import           Data.Maybe  (Maybe (..), fromMaybe, maybe)
import           Data.Monoid (Monoid, mappend, mempty, (<>))
import           Data.Ord    (Ord, compare, max, min, (<), (<=), (>), (>=))
import           Data.String (IsString, fromString)
import           GHC.Base    (Applicative, Functor, Monad, fmap, pure, return,
                              (<*>), (>>=))
import qualified GHC.Base    (fail)
import           GHC.Num     (Num, fromInteger)
import           GHC.Tuple   hiding (Unit)
import           GHC.Types   (Bool (..), IO)

class Mul a b c | a b → c where
  (*) ∷ a → b → c

class Add a b c | a b → c where
  (+) ∷ a → b → c

class Sub a b c | a b → c where
  (-) ∷ a → b → c

class Div a b c | a b → c where
  (/) ∷ a → b → c

class Negate a where
  negate  ∷ a → a
  abs     ∷ a → a

type List a = [a]

{-  May make sense to encapsulate common functionality between maps/sets in typeclasses. -}

data Set a

emptySet ∷ ∀ a . Set a
emptySet = michelson "EmptySetUT"

setMember ∷ ∀ a . a → Set a → Bool
setMember = michelson "SetMemUT"

setUpdate ∷ ∀ a . a → Bool → Set a → Set a
setUpdate = michelson "SetUpdateUT"

setSize ∷ ∀ a . Set a → Int
setSize = michelson "SetSizeUT"

data Map k v

emptyMap ∷ ∀ k v . (Ord k) ⇒ Map k v
emptyMap = michelson "EmptyMapUT"

mapGet ∷ ∀ k v . (Ord k) ⇒ k → Map k v → Maybe v
mapGet = michelson "MapGetUT"

mapMember ∷ ∀ k v . (Ord k) ⇒ k → Map k v → Bool
mapMember = michelson "MapMemberUT"

mapUpdate ∷ ∀ k v . (Ord k) ⇒ k → Maybe v → Map k v → Bool
mapUpdate = michelson "MapUpdateUT"

mapSize ∷ ∀ k v . (Ord k) ⇒ Map k v → Int
mapSize = michelson "MapSizeUT"

data Signature

checkSignature ∷ Key → (Signature, String) → Bool
checkSignature = michelson "CheckSignatureUT"

data Key

data Contract a b

type Account = Contract () ()

manager ∷ ∀ a b . Contract a b → Key
manager = michelson "ManagerUT"

defaultAccount ∷ Key → Account
defaultAccount = michelson "DefaultAccountUT"

source ∷ ∀ a b . Contract a b
source = michelson "SourceUT"

self ∷ ∀ a b . Contract a b
self = michelson "SelfUT"

{-  c *must* be the type of the contract's storage - at the moment this is not enfored by the Haskell typesystem  -}

transferTokens ∷ ∀ a b c . a → Tez → Contract a b → c → IO (b, c)
transferTokens = michelson "TransferTokensUT"

type Pair a b = (a, b)

type Unit = ()

unit ∷ Unit
unit = ()

data Timestamp

now ∷ Timestamp
now = michelson "NowUT"

data Nat

data Tez

amount ∷ Tez
amount = michelson "AmountUT"

balance ∷ Tez
balance = michelson "BalanceUT"

(&&) ∷ Bool → Bool → Bool
(&&) = michelson "AndUT"

(||) ∷ Bool → Bool → Bool
(||) = michelson "OrUT"

xor ∷ Bool → Bool → Bool
xor = michelson "XorUT"

not ∷ Bool → Bool
not = michelson "NotUT"

data String

hash ∷ a → String
hash = michelson "HUT"

data Int

{-  This is *not* a side effect - however, it makes no sense without specified evaluation order, so it's in the IO monad.
    Not sure if this is useful with Juvix anyways; if the quota is static the transpiler can analyze it.  -}

stepsToQuota ∷ IO Int
stepsToQuota = michelson "StepsToQuotaUT"

{-  Typeclass Instances   -}

{-  Equality      -}

instance Eq Tez where
  x == y = (michelson "CmpEqUT") x y

instance Eq Int where
  x == y = (michelson "CmpEqUT") x y

instance Eq Nat where
  x == y = (michelson "CmpEqUT") x y

{-  Comparision   -}

instance Ord Tez where
  x <= y = (michelson "CmpLeUT") x y

instance Ord Int where
  x <= y = (michelson "CmpLeUT") x y

instance Ord Nat where
  x <= y = (michelson "CmpLeUT") x y

{-  Arithmetic    -}

instance Add Int Int Int where
  (+) = michelson "AddIntIntUT"

instance Sub Int Int Int where
  (-) = michelson "SubIntUT"

instance Mul Int Int Int where
  (*) = michelson "MulIntIntUT"

instance Div Int Int Int where
  (/) = michelson "EdivIntIntUT"

instance Negate Int where
  negate  = michelson "NegIntUT"
  abs     = michelson "AbsIntUT"

instance Negate Nat where
  negate  = michelson "NegNatUT"
  abs     = michelson "AbsNatUT"

instance Add Nat Nat Nat where
  (+) = michelson "AddNatNatUT"

instance Sub Nat Nat Nat where
  (-) = michelson "SubNatNatUT"

instance Mul Nat Nat Nat where
  (*) = michelson "MulNatNatUT"

instance Div Nat Nat Nat where
  (/) = michelson "EdivNatNatUT"

instance Add Tez Tez Tez where
  (+) = michelson "AddTezUT"

instance Sub Tez Tez Tez where
  (-) = michelson "SubTezUT"

instance Add Int Nat Int where
  (+) = michelson "AddIntNatUT"

instance Add Nat Int Int where
  (+) = michelson "AddNatIntUT"

instance Mul Int Nat Int where
  (*) = michelson "MulIntNatUT"

instance Mul Nat Int Int where
  (*) = michelson "MulNatIntUT"

{-  Strings   -}

instance Monoid String where
  mempty  = fromString ""
  mappend = michelson "ConcatUT"

{-  For string literal overloading.   -}

instance IsString String where
  fromString = rewrite "stringFromString"

{-  Never exposed; we need these for numeric literal overloading.   -}

instance Num Tez where
  fromInteger = rewrite "tezFromInteger"
  {-# NOINLINE fromInteger #-}

instance Num Int where
  fromInteger = rewrite "intFromInteger"
  {-# NOINLINE fromInteger #-}

instance Num Nat where
  fromInteger = rewrite "natFromInteger"
  {-# NOINLINE fromInteger #-}

fail ∷ IO a
fail = michelson "FailUT"

{-  Michelson library calls, rewritten by Juvix.  -}

michelson ∷ ∀ a b . a → b
michelson op = let x = x in x
{-# NOINLINE michelson #-}

{-  Auxiliary rewrites.   -}

rewrite ∷ ∀ a b . a → b
rewrite label = let x = x in x
{-# NOINLINE rewrite #-}
