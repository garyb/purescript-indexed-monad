module Data.Indexed.IxNaturalTransformation where

type IxNaturalTransformation m n = forall x y a. m x y a -> n x y a

infixr 4 type IxNaturalTransformation as ~~>
