module Juvix.Utility.Types where

import qualified Data.Text as T

class PrettyPrint a where
  pprint ∷ a → T.Text
