module Case where

import           Juvix.Lib

data Action
  = One Int
  | Two Tez

type Result = Int

main ∷ (Action, ()) → (Result, ())
main (action, ()) =
  case action of
    Two _ → (0, ())
    One i → (i, ())
