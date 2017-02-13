module Control.IxMonad.Eff where

import Prelude

import Control.IxMonad (class IxMonad)
import Control.Monad.Eff (Eff)

import Data.Newtype (class Newtype)

newtype IxEff eff i o a = IxEff (Eff eff a)

derive instance newtypeIxEff ∷ Newtype (IxEff eff i o a) _

runIxEff ∷ forall eff i o a. IxEff eff i o a → Eff eff a
runIxEff (IxEff ea) = ea

instance ixMonadIxEff ∷ IxMonad (IxEff eff) where
  ipure = IxEff <<< pure
  ibind (IxEff ma) f = IxEff $ runIxEff <<< f =<< ma
