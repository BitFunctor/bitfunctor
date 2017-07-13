Variable l: nat.

Example exnat: 5 = S 4.
Proof.
 reflexivity.
Qed.

Inductive I1 := 
| I11: I1
| I12: I1.

Module M0.

Module M1.

Inductive I1 := 
| I11: I1
| I12: I1.

Variable a b: I1.
Variable fI1neg: I1 -> I1.
Variable fI1and: I1 -> I1 -> I1.
Variable fI1or: I1 -> I1 -> I1.

Axiom aI1: forall (a b: I1), fI1neg (fI1or a b) = fI1and (fI1neg a) (fI1neg b).

Lemma eqI1: a = a.
Proof.
  reflexivity.
Qed.

Lemma lemfI1: fI1neg (fI1or a b) = fI1and (fI1neg a) (fI1neg b).
Proof.
 apply aI1.
Qed.


Lemma eqI11: forall (x: I1), x=x.
Proof.
 reflexivity.
Qed.


Lemma eqI12: forall (x: I1), x=x.
Proof.
 apply eqI11.
Qed.


Lemma eqI2: forall (x: I1), x=x.
Proof.
apply eqI11.
Qed.

Module M11.

Let i1 := I11.

Inductive mylist {X} := 
| mynil: mylist
| mycons: X -> mylist -> mylist.

Lemma eqL1: forall X (x:X) l, mycons x l = mycons x l.
Proof.
reflexivity.
Qed.  

Lemma eqI3: forall (x: I1), x=x.
Proof.
apply eqI11.
Qed.

End M11.

End M1.

Module M2.

Lemma eqI4: forall (x: M1.I1), x=x.
Proof.
apply M1.M11.eqI3.
Qed.

End M2.

Variable l: @M1.M11.mylist nat.


End M0.

Variable ll: @M0.M1.M11.mylist nat.