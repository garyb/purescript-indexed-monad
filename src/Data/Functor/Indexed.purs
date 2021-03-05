module Data.Functor.Indexed where

import Prelude

class IxFunctor ∷ ∀ ix. (ix → ix → Type → Type) → Constraint
class IxFunctor f where
  imap ∷ ∀ a b x y. (a → b) → f x y a → f x y b

ivoid ∷ ∀ f a x y. IxFunctor f ⇒ f x y a → f x y Unit
ivoid = imap (const unit)

ivoidRight ∷ ∀ f a b x y. IxFunctor f ⇒ a → f x y b → f x y a
ivoidRight x = imap (const x)

infixl 4 ivoidRight as <$:

ivoidLeft ∷ ∀ f a b x y. IxFunctor f ⇒ f x y a → b → f x y b
ivoidLeft f x = imap (const x) f

infixl 4 ivoidLeft as :$>
