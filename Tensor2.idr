data Tensor : (dim : Nat) -> Vect dim Nat -> Type -> Type where
  Scalar : a -> Tensor 0 Nil a
  Vector : 
  ConsT : (Tensor dim ds a) ->
          (Tensor (S dim) (d::ds) a) ->
          (Tensor (s dim) ((S d)::ds) a)
