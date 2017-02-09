module Control.IxMonad.Aff.Class where

import Prelude

import Control.IxMonad (class IxMonad)
import Control.IxMonad.Aff (IxAff)

class IxMonad m ⇐ IxMonadAff eff m | m → eff where
  iliftAff ∷ ∀ i o a. IxAff eff i o a → m i o a

instance ixMonadAffAff :: IxMonadAff eff (IxAff eff) where
  iliftAff = id
