module Control.ValidTransition where

-- | A type class for defining a valid state transition in
-- | `IxFunctor`, `IxApply`, `IxApplicative`, `IxBind`, and `IxMonad`
class ValidTransition x y
