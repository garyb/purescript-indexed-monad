module Control.IxMonad.Eff where

import Prelude

import Control.IxMonad (class IxMonad)
import Effect (Effect)

import Data.Newtype (class Newtype)

newtype IxEff i o a = IxEff (Effect a)

derive instance newtypeIxEff ∷ Newtype (IxEff i o a) _

runIxEff ∷ forall i o a. IxEff i o a → Effect a
runIxEff (IxEff ea) = ea

instance ixMonadIxEff ∷ IxMonad IxEff where
  ipure = IxEff <<< pure
  ibind (IxEff ma) f = IxEff $ runIxEff <<< f =<< ma
