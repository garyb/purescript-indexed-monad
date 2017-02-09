module Control.IxMonad.Eff.Class where

import Prelude

import Control.IxMonad (class IxMonad)
import Control.IxMonad.Eff (IxEff)

class IxMonad m ⇐ IxMonadEff eff m | m → eff where
  iliftEff ∷ ∀ i o a. IxEff eff i o a → m i o a

instance ixMonadEffEff :: IxMonadEff eff (IxEff eff) where
  iliftEff = id
