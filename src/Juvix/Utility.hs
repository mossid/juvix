module Juvix.Utility where

import           Data.List    (elemIndex)
import qualified Data.Text    as T
import           Foundation

import           Juvix.Script
import           Juvix.Types

(|<<) ∷ Functor f ⇒ (a → b) → f a → f b
(|<<) = fmap

(>>|) ∷ Functor f ⇒ f a → (a → b) → f b
(>>|) = flip fmap

rearrange ∷ Int → ExprUT
rearrange 0 = NopUT
rearrange 1 = SwapUT
rearrange n = SeqUT (DipUT (rearrange (n - 1))) SwapUT

unrearrange ∷ Int → ExprUT
unrearrange 0 = NopUT
unrearrange 1 = SwapUT
unrearrange n = SeqUT SwapUT (DipUT (rearrange (n - 1)))

position ∷ T.Text → StackRep → Maybe Int
position n = elemIndex (BoundVariable n)

dropFirst ∷ T.Text → StackRep → StackRep
dropFirst n (x:xs) = if x == BoundVariable n then xs else x : dropFirst n xs
dropFirst _ []     = []
