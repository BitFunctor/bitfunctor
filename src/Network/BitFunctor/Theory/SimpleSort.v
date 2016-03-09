(* name: (type, nat) *)
(* uses: "" *)

(*Inductive nat :=  
| O: nat 
| S: nat -> nat.*)

(* name: (type, natlist) *)
(* uses: (type, nat) *)


Require Import List.

(*
Definition natge := 
fix natge (a b : nat) {struct a} : bool :=
  let (n, n0) := (a, b) in
  match n with
  | 0 => match n0 with
         | 0 => true
         | S _ => false
         end
  | S a' => match n0 with
            | 0 => true
            | S b' => natge a' b'
            end
  end.
*)


Require Import Nats.
  
Module M11.

Definition bnt := blt_nat_trans.

Module M12.

Definition R145 := nat.

End M12.

Section S21.

Definition natlist := list nat.

End S21. 

End M11.

Definition natlist := M11.natlist.

(*Inductive natlist := 
| nil: natlist
| cons: nat -> natlist -> natlist.*)

Lemma foo: forall (n:M11.M12.R145), n=n.
Proof.
intros. reflexivity.
Qed.
(*
Inductive bool :=
| true: bool
| false: bool.*)

(*Fixpoint natgt (a b: nat) : bool :=
match (a, b) with
| (O, O) =>  false
| (S _, O) =>  true
| (O, S _) => false
| (S a', S b') => natgt a' b'
end.

Fixpoint natge (a b: nat) : bool :=
match (a, b) with
| (O, O) =>  true
| (S _, O) =>  true
| (O, S _) => false
| (S a', S b') => natge a' b'
end.*)

Fixpoint insert (n:nat) (ms : natlist) {struct ms} : natlist :=
match ms with
| nil => cons n nil
| cons m ms' => match (natge n m) with
                | false => cons n ms             
                | true => cons m (insert n ms')
                end
end.

(* name: simplesort *)
(* uses: (type, nat) *)
Fixpoint sort (ms : natlist) : natlist :=
match ms with
| nil => nil
| cons m ms' => insert m (sort ms')
end.


Notation "x :: l" := (cons x l) (at level 60, right associativity).
Notation "[]" := nil.
Notation "[ x , .. , y ]" := (cons x .. (cons y nil) ..).

(*Eval compute in sort (7::2::3::1::nil).*)

Definition head (a: nat) (l: natlist) : nat :=
match l with
| nil => a
| cons x _ => x
end.

Inductive sorted : natlist -> Prop :=
| sortnil: sorted nil
| sortcons: forall a m, sorted m -> (natge (head a m) a = true) -> sorted (cons a m).

Lemma eqge: forall n, natge n n = true.
Proof.
 intros. induction n.
 auto. simpl. auto.
Qed.

Lemma headinsert: forall (a n:nat) (l: natlist),
                  sorted l -> head a (insert n l) = n \/ head a (insert n l) = (head a l).
Proof.
 intros. induction l.
 simpl. left. auto.
 simpl. remember (natge n a0) as b. destruct b; auto.
Qed.

Lemma natgeswap : forall (a b: nat), natge a b = false -> natge b a = true.
Proof.
 intros. generalize dependent a. induction b.
 intros. destruct a. auto. inversion H.
 intros. destruct a. auto. simpl.
 apply IHb. inversion H. auto.
Qed.


Lemma insert_sorted: forall (n: nat) (l: natlist),
                     sorted l -> sorted (insert n l).
Proof.
 intros. induction l.
 constructor.  constructor.
 simpl. apply eqge.
 simpl. remember (natge n a) as b.
 destruct b. constructor.
 inversion H. auto. inversion H.
 assert (head a (insert n l) = n \/ head a (insert n l) = (head a l)).
 apply headinsert. auto.
 inversion H4. rewrite H5. rewrite Heqb. auto.
 rewrite H5. auto.
 constructor. auto. simpl. apply natgeswap.
 auto.
Qed.
  
Theorem sort_sorted: forall (l: natlist),
                     sorted (sort l).
Proof.
 intros. induction l.
 constructor. simpl. 
 apply insert_sorted. auto.
Qed.






 



