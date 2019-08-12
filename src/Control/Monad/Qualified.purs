-- | Use this module to rebind `do`/`ado` notation via Qualified Do/Ado.
-- |
-- | For example, here's how one would rebind do notation.
-- | ```purescript
-- | import Control.Monad.Indexed.Qualified as Ix
-- |
-- | f = Ix.do -- remaps do notation to use `ibind` and `idiscard`
-- |   x <- foo
-- |   bar
-- |   I.pure y
-- | ```
-- |
-- | For example, here's how one would rebind ado notation.
-- | ```purescript
-- | import Control.Monad.Indexed.Qualified as Ix
-- |
-- | g = Ix.ado -- remaps do notation to use `iapply` and `imap`
-- |  x <- foo
-- |  y <- bar
-- |  in x + y
-- | ```
module Control.Monad.Indexed.Qualified where

import Data.Functor.Indexed (class IxFunctor, imap)
import Control.Apply.Indexed (class IxApply, iapply)
import Control.Applicative.Indexed (class IxApplicative, ipure)
import Control.Bind.Indexed (class IxBind, ibind, class IxDiscard, idiscard)
import Control.Monad.Indexed (class IxMonad)

map ∷ ∀ f a b x y. IxFunctor f => (a → b) → f x y a → f x y b
map = imap

apply ∷ ∀ m a b x y z. IxApply m => m x y (a → b) → m y z a → m x z b
apply = iapply

pure ∷ ∀ m a x. IxApplicative m => a → m x x a
pure = ipure

bind ∷ ∀ m a b x y z. IxMonad m => m x y a → (a → m y z b) → m x z b
bind = ibind

discard ∷ ∀ m a b x y z. IxBind m => IxDiscard a => m x y a → (a → m y z b) → m x z b
discard = idiscard
