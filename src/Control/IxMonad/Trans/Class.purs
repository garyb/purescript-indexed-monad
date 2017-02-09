module Control.IxMonad.Trans.Class where

import Control.IxMonad (class IxMonad)

class IxMonadTrans t where
  ilift ∷ forall m i o a. IxMonad m ⇒ m i o a → t m i o a
