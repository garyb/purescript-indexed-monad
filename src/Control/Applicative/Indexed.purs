module Control.Applicative.Indexed
  ( class IxApplicative
  , ipure
  , iwhen
  , iunless
  , module Control.Apply.Indexed
  ) where

import Prelude (Unit, unit)
import Control.Apply.Indexed

class IxApplicative :: forall k. (k -> k -> Type -> Type) -> Constraint
class IxApply m ⇐ IxApplicative m where
  ipure ∷ ∀ a x y. a → m x y a

iwhen ∷ ∀ m x y. IxApplicative m ⇒ Boolean → m x y Unit → m x y Unit
iwhen true m = m
iwhen false _ = ipure unit

iunless ∷ ∀ m x y. IxApplicative m ⇒ Boolean → m x y Unit → m x y Unit
iunless false m = m
iunless true _ = ipure unit
