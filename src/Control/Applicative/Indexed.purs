module Control.Applicative.Indexed
  ( class IxApplicative
  , ipure
  , iwhen
  , iunless
  , module Control.Apply.Indexed
  ) where

import Prelude (Unit, unit)
import Control.Apply.Indexed
import Control.ValidTransition (class ValidTransition)

class IxApply m ⇐ IxApplicative m where
  ipure ∷ ∀ a x. ValidTransition x x ⇒ a → m x x a

iwhen ∷ ∀ m x. ValidTransition x x ⇒ IxApplicative m ⇒ Boolean → m x x Unit → m x x Unit
iwhen true m = m
iwhen false _ = ipure unit

iunless ∷ ∀ m x. ValidTransition x x ⇒ IxApplicative m ⇒ Boolean → m x x Unit → m x x Unit
iunless false m = m
iunless true _ = ipure unit
