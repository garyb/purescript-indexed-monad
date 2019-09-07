module Control.Apply.Indexed
  ( class IxApply
  , iapply
  , iapplyFirst, (<*:)
  , iapplySecond, (:*>)
  , module Data.Functor.Indexed
  ) where

import Data.Functor.Indexed
import Control.ValidTransition (class ValidTransition)
import Prelude (const, identity)

class IxFunctor m ⇐ IxApply m where
  iapply ∷ ∀ a b x y z. ValidTransition x y ⇒ ValidTransition y z ⇒ m x y (a → b) → m y z a → m x z b

iapplyFirst ∷ ∀ m a b x y z. ValidTransition x y ⇒ ValidTransition y z ⇒ IxApply m ⇒ m x y a → m y z b → m x z a
iapplyFirst a b = const `imap` a `iapply` b

infixl 4 iapplyFirst as <*:

iapplySecond ∷ ∀ m a b x y z. ValidTransition x y ⇒ ValidTransition y z ⇒ IxApply m => m x y a -> m y z b -> m x z b
iapplySecond a b = const identity `imap` a `iapply` b

infixl 4 iapplySecond as :*>
