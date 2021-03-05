module Control.Apply.Indexed
  ( class IxApply
  , iapply
  , iapplyFirst, (<*:)
  , iapplySecond, (:*>)
  , module Data.Functor.Indexed
  ) where

import Prelude (const, identity)
import Data.Functor.Indexed

class IxApply ∷ ∀ ix. (ix → ix → Type → Type) → Constraint
class IxFunctor m ⇐ IxApply m where
  iapply ∷ ∀ a b x y z. m x y (a → b) → m y z a → m x z b

iapplyFirst ∷ ∀ m a b x y z. IxApply m ⇒ m x y a → m y z b → m x z a
iapplyFirst a b = const `imap` a `iapply` b

infixl 4 iapplyFirst as <*:

iapplySecond ∷ ∀ m a b x y z. IxApply m => m x y a -> m y z b -> m x z b
iapplySecond a b = const identity `imap` a `iapply` b

infixl 4 iapplySecond as :*>
