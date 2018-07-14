module Control.IxMonad.Aff where

import Prelude

import Control.IxMonad (class IxMonad)
import Effect.Aff (Aff)

import Data.Newtype (class Newtype)

newtype IxAff i o a = IxAff (Aff a)

derive instance newtypeIxAff ∷ Newtype (IxAff i o a) _

runIxAff ∷ forall i o a. IxAff i o a → Aff a
runIxAff (IxAff ea) = ea

instance ixMonadIxAff ∷ IxMonad IxAff where
  ipure = IxAff <<< pure
  ibind (IxAff ma) f = IxAff $ runIxAff <<< f =<< ma
