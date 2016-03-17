Fixpoint beq_nat (n m : nat) : bool :=
  match n with
  | O => match m with
         | O => true
         | S m' => false
         end
  | S n' => match m with
            | O => false
            | S m' => beq_nat n' m'
            end
  end.

Inductive lt_nat : nat -> nat -> Prop :=
| lt0 : forall (n:nat), lt_nat 0 (S n)
| lt1 : forall (n m:nat), lt_nat n m -> lt_nat (S n) (S m).

Lemma dec0_0 : forall (n : nat), n - 0 = n.
Proof.
  intros. destruct n. reflexivity. reflexivity.
Qed. 

Lemma remove_S : forall (m n : nat), m = n -> S m = S n.
Proof.
 intros. rewrite -> H. reflexivity.
Qed.

Lemma between : forall (m n : nat), lt_nat m n -> 0 = n - S m -> n = S m.
Proof.
 intros. generalize dependent m. induction n. intros.
 inversion H. intros. inversion H0.  destruct m.
 rewrite -> dec0_0 in H2. rewrite -> H2. reflexivity. apply remove_S.
 apply IHn. inversion H. apply H4. apply H2.
Qed.

Lemma posdec_gt: forall (m n k: nat), n - m = S k -> lt_nat m n.
Proof.
 intros. generalize dependent k. generalize dependent m.
 induction n. intros. inversion H. intros.
 destruct m. apply lt0. apply lt1. simpl in H. apply IHn with (k:=k).
 apply H.
Qed.

Lemma decSS: forall (n m:nat), S n - S m = n - m.
Proof.
 intros. generalize dependent m. induction n. intros.
 simpl. reflexivity. intros. simpl. destruct m. reflexivity.
 reflexivity.
Qed.

Lemma decS : forall (n:nat), S n - n = 1.
Proof.
 intros. induction n. reflexivity. rewrite decSS with (n:=S n) (m:=n). apply IHn.
Qed.

Lemma posdec_S: forall (m n k: nat), n - S m = S k -> n - m = S (n - S m).
Proof.
 intros. generalize dependent k. generalize dependent m.
 induction n. intros. inversion H. intros. simpl. destruct m.
 rewrite -> dec0_0. reflexivity. apply IHn with (k:=k).
 inversion H. reflexivity.
Qed.

Theorem lt_nat_trans : forall (m n k : nat), lt_nat m n -> lt_nat n k -> lt_nat (S m) k.
Proof.
 intros. generalize dependent m.  generalize dependent n. induction k.
 intros. inversion H0. intros. destruct m. apply lt1. destruct n. inversion H.
 inversion H0. destruct n. apply H3. destruct k. inversion H3. apply lt0.
 destruct n. inversion H. 
 inversion H. inversion H0. apply lt1. apply IHk with (n:=n).
 apply H6. apply H3.
Qed.

Lemma lt_S : forall (n : nat), lt_nat n (S n).
Proof.
 intros. induction n. apply lt0. apply lt1. apply IHn.
Qed.

Lemma lt_SS : forall (n : nat), lt_nat n (S (S (n))).
Proof.
 intros. induction n. apply lt0. apply lt1. apply IHn.
Qed.


Theorem plus_0_r : forall n:nat, n + 0 = n.
Proof.
  intros n. induction n. reflexivity.
  simpl. rewrite -> IHn. reflexivity.  
Qed.

Theorem plus_n_Sm : forall n m : nat, 
  S (n + m) = n + (S m).
Proof. 
  intros n m. induction n.
  simpl.   reflexivity.
  simpl. rewrite -> IHn. reflexivity.  
Qed.

Theorem plus_comm : forall n m : nat, n + m = m + n.
Proof.
    intros. induction n. simpl.
    rewrite -> plus_0_r. reflexivity.
    simpl. rewrite -> IHn. rewrite <- plus_n_Sm. reflexivity.  
Qed.


Theorem plus_assoc : forall n m p : nat,
  n + (m + p) = (n + m) + p.
Proof.
  intros n m p. induction n. 
  reflexivity. simpl. rewrite -> IHn. reflexivity.   
Qed.

Theorem plus_swap : forall n m p : nat, 
  n + (m + p) = m + (n + p).
Proof.
  intros n m p.
  rewrite -> plus_comm.  
  assert (H: p + n = n + p).
  rewrite -> plus_comm. reflexivity.
  rewrite <- H. rewrite <- plus_assoc.
  reflexivity.  
Qed.

Fixpoint fact (n:nat) : nat := 
match n with
| 0 => 1
| S m => n * (fact m)
end.

Fixpoint ble_nat (n m : nat) : bool :=
  match n with
  | O => true
  | S n' =>
      match m with
      | O => false
      | S m' => ble_nat n' m'
      end
  end.

Fixpoint blt_nat (n m : nat) : bool :=
  match n with
  | O => 
    match m with
      | O => false
      | S _ => true
      end 
  | S n' =>
      match m with
      | O => false
      | S m' => blt_nat n' m'
      end
  end.

Lemma ble_nat_refl: forall (m:nat), ble_nat m m = true.
Proof.
 intros. induction m. reflexivity. simpl. apply IHm.
Qed.

Lemma beq_nat_refl: forall (m:nat), beq_nat m m = true.
Proof.
 intros. induction m. reflexivity. simpl. apply IHm.
Qed.

Lemma blt_nat_notrefl: forall (m:nat), blt_nat m m = false.
Proof.
 intros. induction m. reflexivity. simpl. apply IHm.
Qed.

Lemma blt_nat_plus1: forall (m:nat), blt_nat m (m + 1) = true.
Proof.
  intros. induction m. reflexivity. simpl. apply IHm.
Qed.

Lemma blt_nat_0 : forall (n:nat), blt_nat 0 (S n) = true.
Proof.
 intros. destruct n. reflexivity. reflexivity.
Qed.

Theorem blt_nat_trans: forall (m n k:nat), 
      blt_nat m n = true -> blt_nat n k = true -> blt_nat m k = true.
Proof. 
 intros. generalize dependent m. generalize dependent n. induction k.
 intros. destruct n. inversion H0. inversion H0. intros.
 destruct m. apply blt_nat_0. destruct n. inversion H.
 simpl. simpl in H0. simpl in H. apply IHk with (n:=n).
 apply H0. apply H.
Qed.

Lemma blt_nat_plus: forall (m n:nat), blt_nat m (m + S n) = true.
Proof.
 intros. generalize dependent m. induction n. intros. 
 apply blt_nat_plus1. intros. 
 apply blt_nat_trans with (m:=m) (n:=m + S n) (k:=m + S (S n)).
 apply IHn. replace (m + S (S n)) with ((m + (S n)) + 1).
 apply blt_nat_plus1. replace (S (S n)) with (S (S n + 0)).
 rewrite -> plus_n_Sm. rewrite -> plus_assoc. reflexivity.
 rewrite -> plus_0_r. reflexivity.
Qed.


Theorem lt_nat_trans' : forall (m n k : nat), lt_nat m n -> lt_nat n k -> lt_nat m k.
Proof.
 intros. generalize dependent m.  generalize dependent n. induction k.
 intros. inversion H0. intros. destruct m. apply lt0. destruct n. inversion H.
 inversion H0. inversion H.  apply lt1. apply IHk with (n:=n).
 apply H3. apply H6.
Qed.

Lemma blt_nat_plus' : forall (m n k : nat), ble_nat (m + S k) n = true -> blt_nat m n = true.
Proof.
  intros. generalize dependent k. generalize dependent m.
  induction n. intros. destruct m. inversion H. inversion H.
  intros. destruct m. reflexivity. simpl. apply IHn with (k:=k).
  inversion H. reflexivity.
Qed.

Fixpoint pow (m n : nat) : nat :=
match n with
| 0 => 1
| S n' => m*(pow m n')
end.
           
Lemma mult_1 : forall (n:nat), 1*n = n.
Proof.
 intros. simpl. rewrite -> plus_0_r. reflexivity.
Qed.

Lemma mult_1_1 : forall (n:nat), n*1 = n.
Proof.
 intros. induction n.  reflexivity. simpl. rewrite ->IHn. reflexivity.
Qed.

Lemma minus_n_n : forall (n:nat), n - n =0.
Proof.
 intros. induction n. reflexivity. simpl. apply IHn.
Qed.

Lemma minus_n_0 : forall (n:nat), n - 0 = n.
Proof.
 intros. destruct n. reflexivity. reflexivity.
Qed.

Theorem mult_0_l : forall n:nat, 0 * n = 0.
Proof.
  intros n. reflexivity.  Qed.

Theorem mult_0_r : forall n:nat, n * 0 = 0.
Proof.
  intros n. induction n as [| n'].
  reflexivity. simpl. rewrite -> IHn'. reflexivity. 
Qed.

Lemma mult_m_Sn: forall m n : nat,
   m * n + m = m * S n.
Proof.
   intros m n.
   induction m.
   reflexivity. simpl.
   rewrite <- IHm. 
   assert (H2: n + m * n + S m = S (n + m * n + m)).
   rewrite -> plus_n_Sm. reflexivity.
   rewrite -> H2. 
   rewrite -> plus_assoc. reflexivity. Qed.

Theorem mult_plus_distr_r : forall n m p : nat,
  (n + m) * p = (n * p) + (m * p).
Proof.
  intros m n p.
  induction p.
  rewrite -> mult_0_r. rewrite -> mult_0_r. rewrite -> mult_0_r. reflexivity.
  rewrite <- mult_m_Sn. rewrite <- mult_m_Sn. rewrite <- mult_m_Sn. rewrite -> IHp.
  rewrite <- plus_assoc. rewrite <- plus_assoc. 
  assert(H1: n * p + (m + n) =  m + (n * p + n)).
  rewrite -> plus_assoc. rewrite -> plus_assoc.
  assert (H2: n * p + m = m + n * p).
  rewrite -> plus_comm. reflexivity.
  rewrite -> H2. reflexivity.
  rewrite -> H1. reflexivity.
Qed. 

Theorem mult_comm : forall m n : nat,
 m * n = n * m.
Proof.
 intros n m. induction n.
 simpl. rewrite -> mult_0_r. reflexivity.
 simpl. rewrite -> IHn. rewrite -> plus_comm. rewrite -> mult_m_Sn. 
 reflexivity.
Qed. 

Theorem mult_assoc : forall n m p : nat,
  n * (m * p) = (n * m) * p.
Proof.
  intros m n p.
  induction n. simpl. rewrite -> mult_0_r. reflexivity.
  simpl.
  assert (H: p + n * p = p * n + p).
  rewrite -> mult_comm. rewrite -> plus_comm. reflexivity.
  rewrite <- mult_m_Sn.
  rewrite -> mult_plus_distr_r.
  rewrite -> mult_comm.
  rewrite -> mult_plus_distr_r.
  rewrite <- IHn.
  rewrite -> mult_comm. rewrite -> plus_comm.
  rewrite -> mult_comm. 
  reflexivity.
Qed.

Fixpoint bge_nat (n m : nat) : bool :=
match m with
| 0 => true
| S m' => match n with
          | 0 => false
          | S n' => bge_nat n' m'
          end
end. 


Lemma andb_1: forall (a b : bool), andb a b = true -> a = true.
Proof.
 intros. destruct a. reflexivity. inversion H.
Qed.

Lemma andb_2: forall (a b : bool), andb a b = true -> b = true.
Proof.
 intros. destruct b. reflexivity. destruct a. inversion H.
 inversion H.
Qed.

Lemma bge_beq: forall (n m : nat), bge_nat n m = true -> beq_nat (S n) m = false.
Proof.
 intros n. induction n. intros. inversion H. destruct m. reflexivity.
 inversion H1. intros. remember (S n) as n'. simpl. destruct m.
 reflexivity. apply IHn. rewrite -> Heqn' in H. inversion H.
 reflexivity.
Qed. 

Lemma bge_Sn : forall (n m : nat), bge_nat n (S m) = true -> bge_nat n m = true.
Proof.
 intros n. induction n. intros. inversion H.
 intros. destruct m. reflexivity. simpl. apply IHn.
 inversion H. reflexivity.
Qed.

Lemma bge_nat_refl : forall (n:nat), bge_nat n n = true.
Proof.
  intros. induction n. reflexivity. simpl. apply IHn.
Qed.

Lemma beq_nat_comm : forall (n m :nat), beq_nat n m = beq_nat m n.
Proof.
 intros n. induction n. intros. induction m. reflexivity.
 simpl. reflexivity. intros. destruct m. simpl. reflexivity.
 simpl. apply IHn.
Qed.

Theorem bleS : forall (n m : nat), ble_nat n m = true -> ble_nat n (S m) = true.
Proof.
  intros n m. generalize dependent n. induction m. intros n H.
  destruct n. reflexivity. inversion H. intros n H.
  destruct n. reflexivity. simpl. apply IHm. apply H.
Qed.

Lemma blt_S : forall (m n : nat), blt_nat (S m) (S n) = blt_nat m n.
Proof.
 intros. simpl. reflexivity.
Qed.

Lemma blt_nat_trans'' : forall (m n k : nat), blt_nat m n = true -> blt_nat n k = true-> blt_nat (S m) k = true.
Proof.
 intros. generalize dependent m. generalize dependent n.
 induction k. intros. destruct n. inversion H0. inversion H0.
 intros. destruct m. destruct k. destruct n. inversion H.
 inversion H0. destruct n. inversion H2. inversion H2.
 reflexivity. destruct n. inversion H.  inversion H.
 inversion H0. rewrite -> blt_S. rewrite -> H2. apply IHk with (n:=n).
 apply H3. apply H2.
Qed.

Lemma blt_nat_trans''' : forall (m n k : nat), blt_nat m n = true -> blt_nat n (S k) = true-> blt_nat m k = true.
Proof.
  intros. generalize dependent m. generalize dependent n.
 induction k. intros. destruct n. destruct m. inversion H.
 inversion H. inversion H0. destruct n. inversion H2.
 inversion H2. intros. destruct m. reflexivity. destruct n. inversion H.
 rewrite -> blt_S. apply IHk with (n:=n).
 apply H0. apply H.
Qed.


Lemma ble_nat_plus1' : forall (m k:nat), ble_nat m k = true -> ble_nat m (S k) = true.
Proof.
 intros. generalize dependent m. induction k. intros. destruct m.
 reflexivity. inversion H. intros. destruct m. reflexivity.
 simpl. apply IHk. inversion H. reflexivity.
Qed.

Theorem ble_nat_trans: forall (m n k:nat), 
      ble_nat m n = true -> ble_nat n k = true -> ble_nat m k = true.
Proof. 
 intros. generalize dependent m. generalize dependent n. induction k.
 intros. destruct n. destruct m. reflexivity. inversion H. inversion H0.
 intros. destruct m. reflexivity. simpl. destruct n. inversion H.
 apply IHk with (n:=n). apply H0. apply H.
Qed.

Lemma ble_lt : forall (m n : nat), ble_nat m n = true -> blt_nat m (S n) = true.
Proof.
 intros. generalize dependent m.  induction n. intros.
 destruct m. reflexivity.
 inversion H. intros. destruct m. reflexivity.
 simpl. apply IHn. apply H.
Qed.

Lemma beq_nat_eq : forall (m n: nat), beq_nat m n = true -> m = n.
Proof.
 intros. generalize dependent n. induction m.
 intros. destruct n. reflexivity. inversion H.
 intros. destruct n. inversion H. assert (m=n).
 apply IHm. inversion H. reflexivity. rewrite -> H0. reflexivity.
Qed.

Lemma beq_nat_trans : forall (m n k :nat), beq_nat m n = true -> beq_nat n k = true -> beq_nat m k = true.
Proof.
 intros. generalize dependent m. generalize dependent n. induction k.
 intros. replace m with n. apply H0. symmetry. apply beq_nat_eq. apply H.
 intros. destruct m. replace (S k) with n. apply H. symmetry.
 apply beq_nat_eq. rewrite -> beq_nat_comm. apply H0.
 simpl. destruct n. inversion H0. apply IHk with (n:=n).
 inversion H0. reflexivity. inversion H. reflexivity.
Qed. 

Lemma blt_ble: forall (n m:nat), blt_nat m n = true -> ble_nat m n = true.
Proof.
 intros. generalize dependent m. induction n.
 intros. destruct m. inversion H. inversion H.
 intros. destruct m. reflexivity. simpl.
 apply IHn. inversion H. reflexivity.
Qed.

Lemma not_blt_ble: forall (m n:nat), blt_nat m n = false -> ble_nat n m = true.
Proof.
 intros. generalize dependent n. induction m. intros.
 destruct n. reflexivity. inversion H. intros.
 destruct n. reflexivity. simpl. apply IHm.
 simpl in H. apply H.
Qed.

Lemma ble_not_blt: forall (m n:nat), ble_nat m n = true -> blt_nat n m = false.
Proof. 
 intros. generalize dependent n. induction m.
 intros. destruct n. reflexivity. reflexivity.
 intros. destruct n. inversion H. simpl.
 apply IHm. simpl in H. apply H.
Qed.

Lemma not_ble_blt: forall (m n:nat), ble_nat m n = false -> blt_nat n m = true.
Proof.
 intros. generalize dependent n. induction m.
 intros. destruct n. inversion H.
 inversion H. intros.
 destruct n. reflexivity. simpl. apply IHm.
 simpl in H. apply H.
Qed.

Lemma ble_blt_trans: forall (m n k:nat), ble_nat m n = true -> blt_nat n k = true -> blt_nat m k = true.
Proof.
 intros. generalize dependent m. generalize dependent n. induction k.
 intros. destruct m. destruct n. inversion H0. inversion H0.
 destruct n. inversion H0. inversion H0. intros.
 destruct m. reflexivity. destruct n.
 inversion H. simpl. apply IHk with (n:=n).
 simpl in H0. apply H0. simpl in H. apply H.
Qed.

Lemma ble_blt_trans2: forall (m n k:nat), blt_nat m n = true -> ble_nat n k = true -> blt_nat m k = true.
Proof.
 intros. generalize dependent m. generalize dependent n. induction k.
 intros. destruct m. destruct n. inversion H.
 inversion H0. destruct n. inversion H. inversion H0.
 intros. destruct m. reflexivity. destruct n.
 inversion H. simpl. apply IHk with (n:=n).
 simpl in H0. apply H0. simpl in H. apply H.
Qed.
