module Control.Monad.Indexed
  ( class IxMonad
  , iap
  , iwhenM
  , iunlessM
  , module Control.Applicative.Indexed
  , module Control.Apply.Indexed
  , module Control.Bind.Indexed
  , module Data.Functor.Indexed
  ) where

import Prelude

import Control.Applicative.Indexed (class IxApplicative, ipure, iwhen, iunless)
import Control.Apply.Indexed (class IxApply, iapply, iapplyFirst, iapplySecond, (:*>), (<*:))
import Control.Bind.Indexed (class IxBind, ibind, (:>>=), ibindFlipped, (=<<:), composeiKleisli, (:>=>), composeiKleisliFlipped, (<=<:))
import Data.Functor.Indexed (class IxFunctor, imap, ivoid, ivoidLeft, ivoidRight, (:$>), (<$:))

class IxMonad ∷ ∀ ix. (ix → ix → Type → Type) → Constraint
class (IxApplicative m, IxBind m) ⇐ IxMonad m

iap ∷ ∀ m a b x y z. IxMonad m ⇒ m x y (a → b) → m y z a → m x z b
iap f a = do
  f' ← f
  a' ← a
  ipure (f' a')
  where
    bind = ibind

iwhenM ∷ ∀ m x y. IxMonad m ⇒ m x y Boolean → m y y Unit → m x y Unit
iwhenM mb m = mb :>>= \b → iwhen b m

iunlessM ∷ ∀ m x y. IxMonad m ⇒ m x y Boolean → m y y Unit → m x y Unit
iunlessM mb m = mb :>>= \b → iunless b m
