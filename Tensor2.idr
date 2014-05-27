module Tensor2

data Tensor2 : (dim : Nat) -> Vect dim Nat -> Type -> Type where
  Scalar : a -> Tensor2 Z Nil a
  ConsT : Tensor2 dim ds a ->
          Tensor2 (S dim) (d::ds) a ->
          Tensor2 (S dim) ((S d)::ds) a
  LiftT : Tensor2 dim ds a -> Tensor2 (S dim) (1::ds) a

