module Identity where

import           Juvix.Lib

main ∷ (String, ()) → (String, ())
main = id

id ∷ a → a
id x = x
