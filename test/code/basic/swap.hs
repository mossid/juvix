module Swap where

import           Juvix.Lib

main ∷ (String, String) → (String, String)
main (param, storage) = (storage, param)
