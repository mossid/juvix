Examples
========

A few may be a bit contrived, but they should cover useful language features. Suggestions welcome!

## Basics

### Identity

All the following are equivalent.

```haskell
module Identity where

import Juvix.Lib

main :: (String, ()) -> (String, ())
main x = x
```

```haskell
module Identity where

import Juvix.Lib

main :: (String, ()) -> (String, ())
main (x, ()) = (x, ())
```

```haskell
module Identity where

import Juvix.Lib

main :: (String, ()) -> (String, ())
main (param, storage) = (param, storage)
```

```haskell
module Identity where

import Juvix.Lib

main :: (String, ()) -> (String, ())
main = \x -> x
```

```haskell
module Identity where

import Juvix.Lib

main :: (String, ()) -> (String, ())
main = func

func =
  let x = 3
      y = 4
      z = \x -> \y -> y x
  in \a -> a
```

To run in a shell:

```bash
./juvix.sh run identity.hs 'Unit' '"param"'
```

### Arithmetic

Numeric operators are overloaded using typeclasses.

```haskell
module Add where

import Juvix.Lib

main :: ((Int, Tez), ()) -> ((Int, Tez), ())
main ((x, y), ()) = ((x + x, 2 * y), ())
```

```bash
./juvix.sh run add.hs 'Unit' '(Pair 3 4)'
```

### Calling Contracts

Side effects are typed in the IO monad, meaning that Juvix prevents you from accidentally calling a contract inside of a pure function, and any pure functions you import from other libraries or copy blindly from StackOverflow can't cause side effects.

(restrictions on stack empty)

Thus the following will work just fine:

```haskell
module Call where

import Juvix.Lib

main :: (Key, ()) -> IO ((), ())
main (key, ()) = do
  transferTokens () amount (defaultAccount key)
  return ((), ())
```

```bash
./juvix.sh run call.hs 'Unit' '""'
``` 

While this will fail to compile:

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Bad where

import Juvix.Lib

maliciousFunction :: Int -> Int
maliciousFunction x = do
  transferTokens () amount (defaultAccount "xyz")
  return (x + x)

main :: (Int, ()) -> (Int, ())
main (x, ()) = (maliciousFunction x, ())
```

```bash
./juvix.sh run bad.hs 'Unit' '2'
```

### Datatypes

Juvix supports standard Haskell algebraic datatypes and record syntax.

```haskell
module DatatypeDemo where

import Juvix.Lib

data ASumType
  = One
  | Two
  | Three

data AProductType
  = AProductType Int ASumType

data ASumProductType
  = ProdOne Int Tez
  | ProdTwo Tez Int

data ARecordType
  = ARecordType {
    aRecordOne    :: ASumType,
    aRecordTwo    :: AProductType,
    aRecordThree  :: ASumProductType
  }

main :: (ARecordType, ()) -> (ARecordType, ())
main (param, ()) =
  let result = ARecordType {
                aRecordOne = aRecordOne param,
                aRecordTwo = AProductType 3 One,
                aRecordThree = ProdTwo 0 42
                }
  in (result, ())
```

```bash
./juvix.sh run datatypedemo.hs 'Unit' '""'
```

Datatypes can be polymorphic.

```haskell
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ExplicitForAll #-}

module Either3 where

import Juvix.Lib

data MyEither3 a b c
  = MyOne a
  | MyTwo b
  | MyThree c

myEitherFunction :: forall a b c . (a -> c) -> (c -> b) -> (b -> a) -> MyEither3 a b c -> MyEither3 a b c
myEitherFunction oneToThree threeToTwo twoToOne =
  \case
    MyOne x   -> MyThree (oneToThree x)
    MyTwo y   -> MyOne (twoToOne y)
    MyThree z -> MyTwo (threeToTwo z)

main :: (MyEither3 Int Tez Bool, ()) -> (MyEither3 Int Tez Bool, ())
main (param, ()) =
  let result = myEitherFunction (const True) (const 1) (const 1) param 
  in (result, ())
```

```bash
./juvix.sh run either3.hs 'Unit' '""'
```

## Demonstration Contracts

### Account System

```haskell
module Accounts where

import Juvix.Lib

data Action
  = Deposit   Key
  | Withdraw  Key Tez (Signed Tez)

main :: (Action, Map Key Tez) -> IO ((), Map Key Tez)
main (action, state) = do
  let previousBalance = fromMaybe 0 (mapGet key state)
  case action of
    Deposit key -> do
      let newBalance      = previousBalance + amount
          newState        = mapUpdate key (Just newBalance) state
      return ((), newState)
    Withdraw key tez signature -> do
      let newBalance = previousBalance - tez
      if newBalance >= 0 && verify key signature then do
        let newState = mapUpdate key (Just newBalance) state
        transferTokens () tez (defaultAccount key)
        return ((), newState)
```
