module AtLeast where

import           Juvix.Lib

main ∷ ((), Tez) → IO ((), Tez)
main ((), minimum) = if amount > minimum then return ((), minimum) else fail
