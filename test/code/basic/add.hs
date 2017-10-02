module Add where

import           Juvix.Lib

main ∷ (Int, ()) -> (Int, ())
main (x, ()) = (x + x, ())
