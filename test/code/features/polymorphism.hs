module Polymorphism where

import           Juvix.Lib

fst ∷ (a, b) → a
fst (x, _) = x

snd ∷ (a, b) → b
snd (_, y) = y

main ∷ ((String, String), (Int, String)) → (String, (Int, String))
main (param, storage) = (fst param, (fst storage, snd storage))
