
Inductive myeq (A:Type) (x:A) : A -> Prop :=
   myeq_refl : x = x :>A

where "x = y :> A" := (@myeq A x y) : type_scope.

Notation "x = y" := (x = y :>_) : type_scope.
