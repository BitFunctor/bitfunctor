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

Print remove.

Lemma eq_decX : forall x y : X, {x = y} + {x <> y}.
Proof.
 intros. remember (ord x y) as o. destruct o.
 right. unfold not. intros. apply ord_correct_refl_full in H.
 rewrite <- Heqo in H. inversion H.
 right. unfold not. intros. apply ord_correct_refl_full in H.
 rewrite <- Heqo in H. inversion H.
 left. apply ord_correct_refl_full. auto.
 right. unfold not. intros. apply ord_correct_refl_full in H.
 rewrite <- Heqo in H. inversion H.
Qed.

Print remove.

Fixpoint removeXL (x:X) (ll: list (X*list X)) :=
 match ll with
  | [] => []
  | (y,yl) :: tl => if eq_decX x y then removeXL x tl else (y,yl) :: removeXL x tl
  end.

Definition removeXL' x'' ll := (filter (fun xxl => negb (eqOrd (ord (fst xxl) x'') EQ)) (map (fun xxl => (fst xxl, filter (fun x => negb (eqOrd (ord x x'') EQ)) (snd xxl))) ll)).

(*need to remove x'' from ll - otherwise we cannot get decreasing ll and induction will be hard *)
(*use filter - not remove !*)
Fixpoint minroute (n: nat) (x: X) (ll: list (X * list X)) {struct n} :=
match n with
| O => None
| S n' =>  let mx := find (fun yl => eqOrd (ord x (fst yl)) EQ) ll in
           match mx with
             | None => None
             | Some (x', xl') => 
                    match xl' with
                    | nil => Some x'
                    | cons x'' _ => minroute n' x'' (removeXL' x' ll)                                    
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
Axiom find_none: forall (A: Type) l f, find f l = None -> (forall (x:A), In x l -> f x = false).
Axiom find_some_filter: forall (A:Type) (x y:A) l f g, f x = false -> find g l = Some y -> f y = false -> find g (filter f l) = Some y.
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

Require Export omega.Omega.

Theorem list_ind''': forall (X:Type) (P: list X -> Prop), 
    P []  -> (forall (l2: list X), (forall (l1:list X),  length l1 < length l2 -> P l1) -> P l2) ->
    forall l : list X, P l.
Proof.
  intros. apply H0. induction l. intros. inversion H1.

  (* destruct (length l1) in H3.
  inversion H3. inversion H3.*)
  intros. apply H0.
  intros. apply IHl. simpl in H1. 
  omega. 
Qed.


Lemma mapmap: forall X A B (f: A -> B) (g: X -> A) l, map f (map g l) = map (fun x => f (g x)) l.
Proof.
 intros. induction l.
 auto. simpl. rewrite IHl.
 auto.
Qed.

Lemma funeq: forall X Y x y (f: X -> Y), x = y -> f x = f y.
Proof.
 intros. subst. auto.
Qed.

Lemma funeq': forall X Y Z x y z (f: X -> Y -> Z), x = y -> f x z = f y z.
Proof.
 intros. subst. auto.
Qed.

Lemma eqCons1: forall A (x:A) lx y ly, x=y -> lx=ly -> x::lx = y::ly.
Proof.
 intros. subst. auto.
Qed.

Lemma eqProd1: forall A B (x1 x2: A) (y1 y2: B), x1 = x2 -> y1 = y2 -> (x1,y1) = (x2,y2).
Proof.
 intros. subst. auto.
Qed.

Lemma filterComm: forall A f g (l: list A),
 filter f (filter g l) = filter g (filter f l).
Proof.
 intros. induction l.
 auto. simpl. remember (f a) as fa. remember (g a) as ga.
 destruct fa; destruct ga;
 simpl; try rewrite <- Heqfa; try rewrite <- Heqga; rewrite IHl; auto.
Qed. 

Lemma mapfilter1: forall A B f f' (g:A->B) l, (forall x, f (g x) = f' x) ->  
                      filter f (map g l) = map g (filter f' l).
Proof.
 intros. induction l. auto.
 simpl. rewrite H. remember (f' a) as f'a. destruct f'a.
 simpl. rewrite IHl. auto.
 auto.
Qed.


Require Export FunctionalExtensionality.

Theorem minexists: forall (l: list X) (x:X) (m:nat), 
         In x l -> m >= length l -> isSome (minroute m x (lesslists l)).
Proof.
 intros. generalize dependent x. generalize dependent m. 
 apply list_ind''' with (l:=l); intros.
 inversion H. remember (length l2) as n. destruct n.
 symmetry in Heqn. apply len0nil in Heqn.
 rewrite Heqn in H1. inversion H1. destruct m. inversion H0.
 simpl. remember (find (fun yl : X * list X => eqOrd (ord x (fst yl)) EQ) (lesslists l2)) as fl.
 destruct fl. destruct p. destruct l0.
 constructor. 
Lemma minexists_help1: forall x l, removeXL' x (lesslists l) =  
                                  lesslists (filter (fun y => negb (eqOrd (ord y x) EQ)) l).
Proof.
 intros. generalize dependent x. induction l; intros.
 auto. simpl. unfold lesslists.
 simpl. replace (ord a a) with EQ.
 simpl. unfold removeXL'. simpl.
 remember (eqOrd (ord a x) EQ) as bxa.
 destruct bxa; simpl. rewrite mapmap.
 simpl. unfold removeXL' in IHl. unfold lesslists in IHl.
 rewrite <- IHl.
 rewrite mapmap. simpl. apply funeq. apply funeq'.
 extensionality x'. destruct (eqOrd (ord x' a) GT).
 simpl. rewrite <- Heqbxa. simpl. auto. auto.

 replace (ord a a) with EQ. simpl.
 apply eqCons1. apply eqProd1.
 auto. apply filterComm. rewrite mapmap.
 simpl. rewrite mapfilter1 with (f' := fun y : X => negb (eqOrd (ord y x) EQ)).
 apply funeq'. extensionality x'.
 apply eqProd1. auto. destruct (eqOrd (ord x' a) GT).
 simpl. rewrite <- Heqbxa. simpl. rewrite filterComm.
 auto. rewrite filterComm. auto. simpl. auto. symmetry.
 apply ord_correct_refl_full. auto. symmetry.
 apply ord_correct_refl_full. auto.
Qed.

 rewrite minexists_help1.
 apply H.

Lemma filterlen: forall A f (l: list A), length (filter f l) <= length l.
Proof.
 intros. induction l. auto.
 simpl. destruct (f a). simpl. omega.
 omega.
Qed.

Lemma minexists_help2: forall x l, In x l -> length (filter (fun y : X => negb (eqOrd (ord y x) EQ)) l) < (length l).
Proof.
 intros. induction l. inversion H.
 inversion H. simpl. rewrite H0.
 replace (ord x x) with EQ. simpl. 
 assert (length (filter (fun y : X => negb (eqOrd (ord y x) EQ)) l) <= length l).
 apply filterlen. omega. symmetry.
 apply ord_correct_refl_full. auto.
 simpl. destruct (eqOrd (ord a x) EQ).
 simpl. assert (length (filter (fun y : X => negb (eqOrd (ord y x) EQ)) l) <= length l).
 apply filterlen. omega. simpl.
 assert (length (filter (fun y : X => negb (eqOrd (ord y x) EQ)) l) < length l).
 apply IHl. auto. omega.
Qed.

 rewrite Heqn. apply minexists_help2. 
 symmetry in Heqfl. apply find_some in Heqfl.
 inversion Heqfl. unfold lesslists in H2.
 apply in_map_iff in H2. inversion H2.
 inversion H4. inversion H5. rewrite <- H8. auto.
 assert (length (filter (fun y : X => negb (eqOrd (ord y x1) EQ)) l2) < length l2).
 apply minexists_help2. symmetry in Heqfl. apply find_some in Heqfl.
 inversion Heqfl. unfold lesslists in H2.
 apply in_map_iff in H2. inversion H2.
 inversion H4. inversion H5. rewrite <- H8. auto.
 omega. symmetry in Heqfl. apply find_some in Heqfl.
 inversion Heqfl. unfold lesslists in H2.
 apply in_map_iff in H2. inversion H2.
 inversion H4. inversion H5. 
 apply filter_In. split.
 assert (In x2 (filter (fun y : X => eqOrd (ord x1 y) GT) l2)).
 rewrite  H9. constructor. auto.
 apply filter_In in H7. inversion H7.
 auto. assert (ord x1 x2 = GT).
 assert (In x2 (filter (fun y : X => eqOrd (ord x1 y) GT) l2)).
 rewrite  H9. constructor. auto.
 apply filter_In in H7. inversion H7.
 apply eqOrdeq. auto. rewrite ord_correct_comm.
 rewrite H7. auto. symmetry in Heqfl. Check find_none.
 assert (exists y, find (fun yl : X * list X => eqOrd (ord x (fst yl)) EQ)
          (lesslists l2)  = Some y).
 apply find_some_inv with (x:=(x, filter (fun y => eqOrd (ord x y) GT) l2)).
 split. unfold lesslists.
 apply in_map_iff. exists x.
 split. auto. auto. simpl.
 apply eqOrdeq. apply ord_correct_refl_full.
 auto.
 inversion H2. rewrite H3 in Heqfl. inversion Heqfl.
Qed.  