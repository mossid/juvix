module Send where

import           Juvix.Lib

main ∷ (Key, ()) → IO ((), ())
main (key, ()) = do
  transferTokens () 1 (defaultAccount key) ()
  return ((), ())
