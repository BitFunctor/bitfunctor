Require Export Lists.List.


Notation "x :: l" := (cons x l) (at level 60, right associativity).
Notation "[]" := nil.
Notation "[ x , .. , y ]" := (cons x .. (cons y nil) ..).
Notation "x ++ y" := (app x y) 
                     (right associativity, at level 60).

Print None.

Variable X: Type.

Inductive Ordering :=
| GT: Ordering
| LT: Ordering
| EQ: Ordering
| NC: Ordering.


Variable ord: X -> X -> Ordering.

(* a > b > c *)

Definition transord (x: Ordering) : Ordering :=
match  x with
| GT => LT
| LT => GT
| EQ => EQ
| NC => NC
end.

Definition eqOrd (o1 o2: Ordering) : bool :=
match o1, o2 with
| GT, GT => true
| LT, LT => true
| EQ, EQ => true
| NC, NC => true
| _, _ => false
end.

Lemma eqOrdeq : forall a b Or, eqOrd (ord a b) Or = true <-> ord a b = Or.
Proof.
 intros. split. intros. 
 unfold eqOrd in H. destruct (ord a b); destruct Or; try inversion H; auto.
 intros. rewrite H. destruct Or; auto.
Qed. 

Axiom ord_correct_comm: forall (a b: X), ord a b = transord (ord b a).

Axiom ord_correct_refl_full: forall (x y: X), ord x y = EQ <-> x = y.


(* this axiom is not enough for no-cycles *)
Axiom ord_correct_transGT: forall (a b c: X) , ord a b = GT ->
                                               ord b c = GT ->
                                               ord c a <> GT .

Inductive downSeq: list X -> Prop :=
| downSeqNil: downSeq nil
| downSeq1: forall x, downSeq (cons x nil)
| downSeqCons: forall x y l, ord x y = GT -> downSeq (cons y l) -> downSeq (cons x (cons y l)). 

Axiom ord_correct_nocyclesGT: forall x y l, downSeq (x::(l++[y])) -> ord x y <> LT /\ ord x y <> EQ. 


Lemma ord_correct_transLT: forall (a b c: X), 
                                             ord a b = LT ->
                                             ord b c = LT ->
                                             ord c a <> LT.
Proof.
 intros. assert (ord b a = GT). rewrite ord_correct_comm.
 rewrite H. auto. assert (ord c b = GT). rewrite ord_correct_comm.
 rewrite H0. auto. assert (ord a c <> GT). apply ord_correct_transGT with (b:=b).
 auto. auto. unfold not. intros. rewrite ord_correct_comm in H4.
 unfold not in H3. destruct (ord a c); simpl in H4;
 try inversion H4. apply H3. auto.
Qed.
  
Definition head (a: X) (l: list X) : X :=
match l with
| nil => a
| cons x _ => x
end.

Print Forall.

Inductive partsorted: list X -> Prop :=
| sortnil: partsorted nil
| sortcons: forall a m, Forall (fun x => ord a x <> GT) m -> partsorted m -> partsorted (cons a m).

Variable x0:X.

Lemma len0nil: forall (l:list X), length l = 0 -> l = nil.
Proof.
 intros. destruct l. auto.
 inversion H.
Qed.

Print nth.

Definition lesslists (l: list X) := map (fun x => (x, filter (fun y => eqOrd (ord x y) GT) l)) l.
Check lesslists. 

Print find.

(*need to remove x'' from ll - otherwise we cannot get decreasing ll and induction will be hard *)
Fixpoint minroute (n: nat) (x: X) (ll: list (X * list X)) {struct n} :=
match n with
| O => None
| S n' =>  let mx := find (fun yl => eqOrd (ord x (fst yl)) EQ) ll in
           match mx with
             | None => None
             | Some (x', xl') => 
                    match xl' with
                    | nil => Some x'
                    | cons x'' _ => minroute n' x'' ll
                    end
           end
end.
   
Definition minelem (x: X) (l: list X) :=
match l with
| nil => None
| cons x _ => minroute (length l) x (lesslists l)
end.

Inductive isSome{A} : option A -> Prop :=
| issome: forall a, isSome (Some a).


About Lists.find_some.

Axiom find_some: forall (A: Type) l (x:A) f, find f l = Some x -> In x l /\ f x = true.
Axiom find_some_inv: forall (A: Type) x l f, In x l /\ f x = true -> (exists (y:A), find f l = Some y). 
Axiom find_none: forall (A: Type) l f, find f l = None -> forall (x:A), In x l -> f x = false.
(* prove it later *) 

Lemma downSeqSnoc: forall l x y, downSeq (l ++ [x]) -> ord x y = GT -> downSeq (l ++ [x] ++ [y]).
Proof.
 intros. induction l. constructor. auto. constructor.
 inversion H.
 destruct l; inversion H3.
 replace ((a :: l) ++ [x] ++ [y]) with (a::y0::l0 ++ [y]).
 rewrite <- H2 in IHl. replace (l++[x]++[y]) with (y0::l0++[y]) in IHl.
 constructor. auto. apply IHl. auto. rewrite <- app_ass.
 rewrite <- H2. auto.  simpl. replace (y0 :: l0 ++ [y]) with (l++[x]++[y]).
 auto. rewrite <- app_ass. rewrite <- H2. auto.
Qed.
 
Lemma minexists_help2 : forall n lx l x1 x, downSeq (x::(lx++[x1])) -> 
                minroute n x1 (lesslists l) = minroute n x1 (lesslists (x :: l)).
Proof.
 intros. generalize dependent lx. generalize dependent x1. induction n; intros.
 simpl. auto. remember (x::l) as l'.
 simpl. remember (find (fun yl : X * list X => eqOrd (ord x1 (fst yl)) EQ) (lesslists l)) as fl.
 replace (find (fun yl : X * list X => eqOrd (ord x1 (fst yl)) EQ) (lesslists l')) with fl.
 destruct fl. destruct p. destruct l0. auto. apply IHn with (lx:=lx ++ [x1]).
 rewrite app_ass. replace (x :: lx ++ [x1] ++ [x3]) with ((x :: lx) ++ [x1] ++ [x3]).
 apply downSeqSnoc. simpl. apply H.
 symmetry in Heqfl. apply find_some in Heqfl. inversion Heqfl.
 simpl in H1. unfold lesslists in H0. apply in_map_iff in H0.
 inversion H0. inversion H2. inversion H3. 
 assert (In x3 (filter (fun y : X => eqOrd (ord x2 y) GT) l)).
 rewrite H7. constructor. auto. apply filter_In in H5.
 inversion H5. assert (x1 = x2). apply ord_correct_refl_full.
 apply eqOrdeq. auto. apply eqOrdeq in H9. rewrite <- H10 in H9. auto.
 simpl. auto. auto.
 rewrite Heql'.
Lemma minexists_help3: forall x1 l lx x, downSeq (x::(lx++[x1])) ->
 (find (fun yl : X * list X => eqOrd (ord x1 (fst yl)) EQ) (lesslists l) = 
 find (fun yl : X * list X => eqOrd (ord x1 (fst yl)) EQ) (lesslists (x :: l)) ).
Proof.
 intros. 
 remember (find (fun yl : X * list X => eqOrd (ord x1 (fst yl)) EQ) (lesslists (x :: l))) as fl'.
 remember (find (fun yl : X * list X => eqOrd (ord x1 (fst yl)) EQ) (lesslists l)) as fl.
 destruct fl; destruct fl';
 symmetry in Heqfl; symmetry in Heqfl'; 
 try apply find_some in Heqfl; try apply find_none in Heqfl;
 try apply find_some in Heqfl'; try apply find_none in Heqfl';
 try inversion Heqfl; inversion Heqfl'. 
 
 unfold lesslists in H0. apply in_map_iff in H0.
 inversion H0. inversion H4.
 rewrite <- H5 in H1.
 simpl in H1. apply eqOrdeq in H1. 
 apply ord_correct_refl_full in H1.
 
 unfold lesslists in H2. apply in_map_iff in H2.
 inversion H2. inversion H7.
 rewrite <- H8 in H3.
 simpl in H3. apply eqOrdeq in H3. 
 apply ord_correct_refl_full in H3.

 replace x3 with x2 in H8. simpl in H8.
 replace (eqOrd (ord x2 x) GT) with false in H8.
 rewrite <- H8. rewrite <- H5.
 auto. rewrite <- H1.
 apply ord_correct_nocyclesGT in H.
 inversion H. rewrite ord_correct_comm.
 destruct (ord x x1). auto.
 unfold not in H10. assert False.
 apply H10. auto. inversion H12.
 unfold not in H11. assert False.
 apply H11. auto. inversion H12.
 auto. rewrite <- H3. rewrite <- H1. auto.

 replace (eqOrd (ord x1 x) EQ) with false.
 replace (eqOrd (ord x1 x) EQ) with false in H3.
 assert (exists p0, find  (fun yl : X * list X => eqOrd (ord x1 (fst yl)) EQ)
           (lesslists (x :: l)) = Some p0 ). 
 apply find_some_inv with (x:=p).
 split. simpl. replace (eqOrd (ord x x) GT) with false.
 right. apply in_map_iff.
 exists (fst p). replace (eqOrd (ord (fst p) x) GT) with false.
 split. unfold lesslists in H0. 
 apply in_map_iff in H0. inversion H0. inversion H2.
 replace (fst p) with x2. auto. inversion H4. auto.
 unfold lesslists in H0. apply in_map_iff in H0.
 inversion H0. inversion H2. replace (fst p) with x2.
 auto. rewrite <- H4. auto.
 replace (fst p) with x1.
 apply ord_correct_nocyclesGT in H. inversion H.
 rewrite ord_correct_comm. destruct (ord x x1); simpl.
 auto. unfold not in H2. assert False. apply H2.
 auto. inversion H5. auto. auto. 
 inversion Heqfl. apply eqOrdeq in H4.
 apply ord_correct_refl_full. auto.
 replace (ord x x ) with EQ. auto. symmetry.
 apply ord_correct_refl_full.
 auto. auto. inversion H2.  
 rewrite Heqfl' in H4. inversion H4. 

 apply ord_correct_nocyclesGT in H.  inversion H.
 rewrite ord_correct_comm. destruct (ord x x1); simpl.
 auto. auto. unfold not in H4. assert False. apply H4.
 auto. inversion H5. auto.   

 apply ord_correct_nocyclesGT in H.  inversion H.
 rewrite ord_correct_comm. destruct (ord x x1); simpl.
 auto. auto. unfold not in H4. assert False. apply H4.
 auto. inversion H5. auto. 

 unfold lesslists in H0. apply in_map_iff in H0.
 inversion H0. inversion H3.
 destruct p. inversion H4. simpl in H2.
 apply eqOrdeq in H2. apply ord_correct_refl_full in H2.
 rewrite <- H2 in H8. replace (eqOrd (ord x1 x) GT) with false in H8.
 inversion H5. apply ord_correct_nocyclesGT in H.
 inversion H. unfold not in H10. 
 assert False. apply H10. apply ord_correct_refl_full.
 rewrite H6. rewrite H2. auto. inversion H11.
 assert (In (x2, l0) (lesslists l)).
 unfold lesslists. apply in_map_iff. exists x2.
 split. rewrite H7. rewrite <- H2.
 rewrite H8. auto. auto.
 assert (eqOrd (ord x1 x2) EQ = true).
 apply eqOrdeq. apply ord_correct_refl_full.
 rewrite H2. auto.
 assert (In (x2,l0) (lesslists l) /\  eqOrd (ord x1 x2) EQ = true).
 auto. assert (exists p0, find (fun yl : X * list X => eqOrd (ord x1 (fst yl)) EQ) (lesslists l) = Some p0).
 apply find_some_inv with (x:=(x2,l0)). split.
 auto. auto. inversion H12. rewrite H1 in H13. inversion H13.
 apply ord_correct_nocyclesGT in H. rewrite ord_correct_comm.
 inversion H. destruct (ord x x1); simpl.
 auto. unfold not in H6. assert False.
 apply H6. auto. inversion H10. auto. auto. 
 rewrite H1. auto.
Qed. 
 rewrite Heqfl.  apply minexists_help3 with (lx:=lx).
 auto.
Qed.
 
 

Theorem minexists: forall (l: list X) (x:X), 
    length l > 0 -> In x l -> isSome (minroute (length l) x (lesslists l)).
Proof.
 intros l.
 induction l; intros. inversion H0. remember (lesslists (a::l)) as all.
 simpl. remember (find (fun yl : X * list X => eqOrd (ord x (fst yl)) EQ) all) as fl.
 destruct fl. destruct p. destruct l0. constructor.
 rewrite Heqall. rewrite <- minexists_help2 with (lx:=[]).
 apply IHl. 
 
 rewrite Heqall in Heqfl.  destruct l.
 simpl in Heqfl. destruct (eqOrd (ord x a) EQ).
 replace (ord a a) with EQ in Heqfl.
 inversion Heqfl. symmetry. apply ord_correct_refl_full.
 auto. inversion Heqfl. simpl. intuition.
 rewrite Heqall in Heqfl. symmetry in Heqfl.
 apply find_some in Heqfl.
 inversion Heqfl. simpl in H2.
 (* x = x1, x1 > x2 *)
 (* 1. x = a -> x1 = a -> a > x2 -> a <> x2 *)
 (* 2. In x l -> x1 = x > x2 -> *)
 

 
  
  
 
 


Fixpoint insert (a:X) (ms : list X) {struct ms} : list X:=
match ms with
| nil => cons a nil
| cons m ms' => match (ord a m) with
                | Some false => cons a ms             
                | true => cons m (insert n ms')
                end
end.


Fixpoint partsort (ms : list X) : list X:=
match ms with
| nil => nil
| cons m ms' => insert m (partsort ms')
end.



  
