module Control.IxMonad.Aff where

import Prelude

import Control.IxMonad (class IxMonad)
import Control.Monad.Aff (Aff)

import Data.Newtype (class Newtype)

newtype IxAff eff i o a = IxAff (Aff eff a)

derive instance newtypeIxAff ∷ Newtype (IxAff eff i o a) _

runIxAff ∷ forall eff i o a. IxAff eff i o a → Aff eff a
runIxAff (IxAff ea) = ea

instance ixMonadIxAff ∷ IxMonad (IxAff eff) where
  ipure = IxAff <<< pure
  ibind (IxAff ma) f = IxAff $ runIxAff <<< f =<< ma
