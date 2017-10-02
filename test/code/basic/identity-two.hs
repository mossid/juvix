module Identity where

import           Juvix.Lib

-- This identity example is *not* fully optimized by Juvix (yet).

main ∷ (String, ()) -> (String, ())
main (str, ()) = (str, ())
