module Juvix.Utility (
  module Juvix.Utility.PrettyPrint,
  (|<<),
  (>>|)
) where

import           Foundation

import           Juvix.Utility.PrettyPrint

(|<<) ∷ Functor f ⇒ (a → b) → f a → f b
(|<<) = fmap

(>>|) ∷ Functor f ⇒ f a → (a → b) → f b
(>>|) = flip fmap
