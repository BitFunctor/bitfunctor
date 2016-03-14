Require Import EqSample.

Print EqSample.myeq_refl.

Print Implicit myeq_refl.

Lemma fooeq: forall (X: Type) (x:X), x = x.
Proof.
 apply myeq_refl.
Qed.