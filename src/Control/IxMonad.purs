module Control.IxMonad where

import Prelude

class IxMonad m where
  ipure ∷ ∀ a x. a → m x x a
  ibind ∷ ∀ a b x y z. m x y a → (a → m y z b) → m x z b

infixl 1 ibind as :>>=

ibindFlipped ∷ ∀ m a b x y z. IxMonad m ⇒ (a → m y z b) → m x y a → m x z b
ibindFlipped = flip ibind

infixr 1 ibindFlipped as =<<:

ijoin ∷ forall m z y x a. IxMonad m => m x y (m y z a) -> m x z a
ijoin m = m :>>= id

composeiKleisli ∷ ∀ m a b c x y z. IxMonad m ⇒ (a → m x y b) → (b → m y z c) → a → m x z c
composeiKleisli f g a = f a :>>= g

infixr 1 composeiKleisli as :>=>

composeiKleisliFlipped ∷ ∀ m a b c x y z. IxMonad m ⇒ (b → m y z c) → (a → m x y b) → a → m x z c
composeiKleisliFlipped f g a = f =<<: g a

infixr 1 composeiKleisliFlipped as <=<:
