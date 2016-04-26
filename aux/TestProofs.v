Definition eq1 (n: nat) := n + 1 = 2.

Check eq1.

Lemma lem_eq1 : forall n, n = 1 -> n + 1 = 2.
Proof.
 intros. subst. auto.
Qed.

Check lem_eq1.
Check (eq1 1). 

Definition sol_eq1 := 1.

Lemma solve_eq1: eq1 sol_eq1.
Proof.
 
 intros.
 unfold eq1. unfold sol_eq1. auto.
Qed.

Print solve_eq1.  

Check solve_eq1.



Definition eq2 (m n:nat) := (n = m*m) /\ (n < 10).

Check eq2.


