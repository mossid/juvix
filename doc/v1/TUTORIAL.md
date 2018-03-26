Juvix Tutorial
==============

*N.B. This tutorial is intended for a prospective developer without prior Haskell experience. If you've already programmed in Haskell (or Objective Caml / Standard ML, which are a bit different in syntax but pretty close in semantics), picking up Juvix should be trivial. You cannot import existing Haskell libraries - Juvix does not support GHC primops - instead you need to use `Juvix.Lib`, which provides Michelson calls and supported GHC builtins exposed as a Prelude-esque interface. Otherwise you can just write standard Haskell.*

Juvix is a Haskell-to-Michelson transpiler, which means that it takes Haskell code and converts it into Michelson instructions. If Juvix does its job effectively, the programmer should never need to work with or worry about Michelson, although you can easily examine or modify the output instruction sequence if you like.

[Haskell](https://haskell.org) is a high-level, purely functional programming language. This tutorial will only cover language aspects relevant to Juvix which you are likely to use in writing smart contracts. For a general language tutorial, check out [Learn You a Haskell](http://learnyouahaskell.com/). For a detailed overview / reference manual, I highly recommend [What I Wish I Knew When Learning Haskell](http://dev.stephendiehl.com/hask/).

This tutorial will take you through the basics of writing a smart contract in Haskell, transpiling it to Michelson with Juvix, deploying it to the Tezos alphanet - then writing and deploying a second smart contract which can interact with the first. You'll need a Linux computer (or a cloud compute instance with shell access).

(todo: write tutorial)
