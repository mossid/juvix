Architecture
============

Transpilation Stages
--------------------

Juvix uses the GHC frontend lexer, parser, and simplifier to transform Haskell source into [GHC Core](https://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/CoreSynType), a concise, explicitly-typed lambda calculus ([System FC](https://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/FC)). GHC Core is then converted into an untyped intermediary expression type where Michelson call placeholders are replaced with explicit representations. This intermediary type is simplified and then converted into Michelson instructions. The resulting Michelson code (currently represented untyped) is run through a few simple optimizations, then written out to a program which can be parsed by the Tezos client.

Juvix will maintain the Haskell frontend syntax, typesystem, and semantics, but this particular transpilation architecture could change depending on optimization requirements and desired features.

Typesystem / Type Inference
---------------------------

By virtue of using the GHC frontend, Juvix supports full Hindley-Milner type inference and all the features of the Haskell typesystem, including any GHC-supported extensions.

Algebraic Datatypes
-------------------

Juvix supports Haskell's [algebraic datatype](https://wiki.haskell.org/Algebraic_data_type) syntax and semantics. Types isomorphic to boolean (`data T = A | B`) are encoded as booleans, types isomorphic to either (`data T = A B | C D`) are encoded as either, types isomorphic to option (`data T = A B | C`) are encoded as options, and more complex types are encoded as nested pairs or alternatives as appropriate.

Typeclasses
-----------

Juvix supports Haskell typeclasses. However, Juvix does *not* currently support runtime ad-hoc polymorphism - all ad-hoc polymorphic function calls (e.g. +) must be resolvable to a specific typed instantation at compile-time. GHC requires this anyway unless you use specific (and rather risque) language extensions, i.e. `-XOverlappingInstances`.
