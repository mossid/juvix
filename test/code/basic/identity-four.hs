module Identity where

import           Juvix.Lib

main ∷ (String, ()) → (String, ())
main = func

func =
  let x :: Int
      x = 3
      y :: Int
      y = 4
      z = \x → \y → y x
  in \a → a
