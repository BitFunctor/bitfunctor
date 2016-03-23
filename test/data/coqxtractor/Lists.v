Require Export Nats.
Require Export Lists.List.
Require Export omega.Omega.

Notation "x :: l" := (cons x l) (at level 60, right associativity).
Notation "[]" := nil.
Notation "[ x , .. , y ]" := (cons x .. (cons y nil) ..).
Notation "x ++ y" := (app x y) 
                     (right associativity, at level 60).

Definition ttt := [1,2].

Definition fst3 {X Y Z: Type} (p : X * Y * Z) : X := 
  match p with (x,_,_) => x end.

Definition snd3 {X Y Z: Type} (p : X * Y * Z) : Y := 
  match p with (_,y,_) => y end.

Definition thrd3 {X Y Z: Type} (p : X * Y * Z) : Z := 
  match p with (_,_,z) => z end.

Theorem list_ind': forall (X:Type) (P: list X -> Prop), 
    (forall (l0 : list X), length l0 = 0 -> P l0)  -> (forall (l1 : list X) (l2 : list X), ble_nat (length l1) (length l2) = true -> (P l1 -> P l2)) ->
    forall l : list X, P l.
Proof.
  intros X. intros P. intros H0. intros H1. intros l. induction l. apply H0. reflexivity.
  apply H1 with (l2:=a::l) (l1:=l). simpl. apply bleS. apply ble_nat_refl.
  apply IHl.
Qed.

Theorem list_ind'': forall (X:Type) (P: list X -> Prop), 
    P []  -> (forall (l2: list X), (forall (l1:list X), length l2 = S(length l1) -> P l1) -> P l2) ->
    forall l : list X, P l.
Proof.
  intros. apply H0. induction l. intros. inversion H1. intros. apply H0.
  intros. apply IHl. rewrite <- H2. inversion H1.
  reflexivity.
Qed.

Theorem list_ind''': forall (X:Type) (P: list X -> Prop), 
    P []  -> (forall (l2: list X), (forall (l1:list X), blt_nat (length l1) (length l2)= true -> P l1) -> P l2) ->
    forall l : list X, P l.
Proof.
  intros. apply H0. induction l. intros. inversion H1. destruct (length l1) in H3.
  inversion H3. inversion H3.
  intros. apply H0.
  intros. apply IHl. 
  apply blt_nat_trans''' with (n:=length l1). apply H2. apply H1.
Qed.

Fixpoint index {X:Type} (n:nat) (x0:X) (l:list X) : X := 
match l with
| nil => x0
| cons x s => match n with
         | 0 => x
         | S m => index m x0 s
         end
end.


Fixpoint series (n:nat) : list nat := 
match n with
| 0 => []
| S n' => [n'] ++ (series n')
end.

Fixpoint const_list (n x : nat) : list nat :=
match n with
| 0 => []
| S n' => x::(const_list n' x)
end.


Example ser5 :  (series 5) = [4,3,2,1,0].
Proof. simpl. reflexivity. Qed.

 
Lemma nil_series_0: forall (n:nat), series n = [] -> n = 0.
Proof.
 intros. destruct n. reflexivity. inversion H.
Qed.

Fixpoint snoc {X:Type} (l:list X) (v:X) : list X:= 
  match l with
  | nil    => [v]
  | h :: t => h :: (snoc t v)
  end.


Lemma len0nil (X:Type) : forall (l:list X), length l = 0 -> l = @nil X.
Proof.
 intros l H. destruct l. reflexivity. inversion H. 
Qed.

Lemma lensnoc (X:Type) : forall (l:list X) (v:X), length (snoc l v) = S (length l).
Proof.
  intros. induction l. reflexivity. simpl. rewrite -> IHl. reflexivity.
Qed.

Lemma snoc_app (X:Type): forall (l:list X) (v:X), snoc l v = l ++ [v].
Proof.
  intros. induction l. reflexivity. simpl. rewrite -> IHl. reflexivity.
Qed.

Lemma length_series : forall (n:nat), length (series n) = n.
Proof.
 intros. induction n. reflexivity. simpl. rewrite -> IHn. reflexivity.
Qed.

Lemma nth_series: forall (n:nat), index 0 0 (series (S n)) = n.
Proof.
 intros. reflexivity.
Qed.

Lemma lengthth_series: forall (n:nat), index n 0 (series (S n)) = 0.
Proof.
  intros. induction n. reflexivity.
  simpl. apply IHn.
Qed.

Lemma index1 : forall (m n:nat), lt_nat m n -> 
    index (n - S m) 0 (n :: series n) = index (n - m) 0 (S n :: n :: series n).
Proof.
 intros. remember (n - S m) as k. destruct k. assert (n = S m).
 apply Nats.between. apply H. apply Heqk. rewrite -> H0. simpl.
 destruct m. simpl. reflexivity. rewrite -> decS. simpl. reflexivity.
 assert (n - m = S (S k)). rewrite -> Heqk. destruct n. simpl. inversion Heqk.
 simpl. destruct m. rewrite -> dec0_0. reflexivity. apply posdec_S with (k:=k).
 inversion Heqk. reflexivity. rewrite -> H0.
 simpl. reflexivity.
Qed.
 
Lemma nth_series_1: forall (n:nat), index n 0 (S n :: n :: series n) = 1.
Proof.
  intros. induction n. reflexivity.
  simpl. apply IHn.
Qed.

Lemma mth_series: forall (m n:nat), lt_nat m (S n) -> index (n-m) 0 (series (S n)) = m.
Proof.
 intros. generalize dependent m. induction n.
 intros. inversion H. simpl. reflexivity. destruct m. 
 simpl. rewrite -> H1. reflexivity. inversion H2.
 intros. simpl. destruct m. simpl. apply lengthth_series.
 remember (n-m) as k. destruct k. simpl. apply Nats.between with (m:=m) (n:=S n).
 inversion H. apply H2. rewrite -> Heqk. reflexivity.
 rewrite -> Heqk.
 replace  (index (n - m) 0 (S n :: n :: series n)) with (index (n - S m) 0 (n :: series n)).
 apply IHn. apply lt1. apply posdec_gt with (k:=k). rewrite -> Heqk. reflexivity.
 apply index1. apply posdec_gt with (k:=k). rewrite -> Heqk. reflexivity.
Qed.

Lemma lt_series : forall (m n: nat), lt_nat (index m 0 (series (S n))) (S n).
Proof.
 intros. generalize dependent m. induction n.
 intros. destruct m. simpl. apply lt0.
 simpl. destruct m. simpl. apply lt0. simpl. apply lt0.
 intros. simpl. destruct m. simpl. apply lt1. apply Nats.lt_S.
 simpl. remember (index m 0 (n :: series n)) as ind.
 assert (lt_nat ind (S n)). rewrite -> Heqind.
 apply IHn. destruct ind. apply lt0. apply lt_nat_trans with (n:=n).
 inversion H. apply H2. apply lt_SS.
Qed.

Fixpoint fold {X Y:Type} (f: X->Y->Y) (l:list X) (b:Y) : Y :=
  match l with
  | nil => b
  | h :: t => f h (fold f t b)
  end.

(*Fixpoint map {X Y: Type} (f : X -> Y) (l : list X) : list Y := 
match l with
| [] => []
| x::s => (f x) :: (map f s)
end.*)

Definition sum_flist {X: Type} (f : X -> nat) (l : list X) :=
  fold plus (map f l) 0.

Lemma sum_mult_flist : forall (f : nat->nat) (x:nat) (l:list nat),
   x*(sum_flist f l) = sum_flist (fun z => x*(f z)) l.
Proof.
  intros. induction l. unfold sum_flist. simpl. apply mult_0_r.
  unfold sum_flist. simpl. rewrite -> mult_comm. rewrite -> mult_plus_distr_r.
  rewrite -> mult_comm. unfold sum_flist in IHl. rewrite -> mult_comm in IHl.
 rewrite -> IHl. reflexivity.
Qed.

Fixpoint rev {X:Type} (l:list X) : list X := 
  match l with
  | []      => []
  | h :: t => (rev t) ++ [h]
  end.
 
Lemma map_last : forall (f:nat->nat) (x:nat) (l:list nat), 
                 map f (l++[x]) = (map f l) ++ [f x].
Proof.
 intros. induction l. simpl. reflexivity.
 simpl. rewrite -> IHl. reflexivity.
Qed.

Lemma fold_last : forall (l:list nat) (x:nat),
              fold plus (l++[x]) 0 = fold plus l 0 + x.
Proof.
  intros. induction l. simpl. apply plus_0_r.
  simpl. rewrite -> IHl. rewrite -> plus_assoc. reflexivity.
Qed.

Lemma sum_flist_last : forall (f : nat->nat) (l:list nat) (x:nat),
    sum_flist f (l++[x]) = sum_flist f l + (f x).
Proof.
  intros. unfold sum_flist. rewrite -> map_last.
  rewrite -> fold_last. reflexivity.
Qed.

Lemma sum_flist_comm : forall (f : nat->nat) (l:list nat),
              sum_flist f l = sum_flist f (rev l).
Proof.
 intros. induction l. unfold sum_flist. simpl. reflexivity.
 unfold sum_flist. simpl. rewrite -> map_last. rewrite -> fold_last.
 unfold sum_flist in IHl. rewrite -> IHl. apply plus_comm.
Qed.

Lemma ser_S : forall (n:nat), series (S n) = map (plus 1) (series n) ++ [0].
Proof.
 intros. induction n. simpl. reflexivity.
 simpl. simpl in IHn. rewrite -> IHn. reflexivity.
Qed.

Fixpoint bge_nat_list (n:nat) (l:list nat) : bool :=
match l with
| [] => true
| a::s => andb (bge_nat n a) (bge_nat_list n s)
end.  

Lemma bge_Sn_list : forall (n:nat) (l:list nat), bge_nat_list n l=true -> bge_nat_list (S n) l = true.
Proof.
 intros. induction l. reflexivity. simpl. rewrite -> IHl.
 destruct a. reflexivity. remember (bge_nat n a) as b.
 assert (b=true).  remember (b=true) as bb.
 inversion H. apply andb_1 in H1. rewrite -> Heqbb. rewrite -> Heqb.
 apply bge_Sn. apply H1. rewrite -> H0. reflexivity.
 remember (bge_nat_list n l = true) as bb.
 inversion H. apply andb_2 in H1. rewrite -> Heqbb.
 apply H1.
Qed.
  

Lemma bge_n_ser : forall (n:nat), bge_nat_list n (series (S n)) = true.
Proof.
  intros. induction n. simpl. reflexivity.
  replace (series (S (S n))) with (S n :: series (S n)).
  remember ((series (S n))) as ser1. simpl. rewrite -> bge_Sn_list.
  rewrite -> bge_nat_refl. reflexivity. apply IHn.
  reflexivity.
Qed.

Lemma plus4 : forall (a b c d:nat), (a+b)+(c+d) = (a+c)+(b+d).
Proof.
 intros. rewrite -> plus_assoc. rewrite -> plus_assoc. 
 replace (a+b+c) with (a+(b+c)). replace (a+c+b) with (a+(c+b)).
 replace (b+c) with (c+b). reflexivity. apply plus_comm.
 apply plus_assoc. apply plus_assoc.
Qed.

Lemma sumflist_plus: forall (f1 f2 : nat->nat) (l:list nat), 
  sum_flist f1 l + sum_flist f2 l = sum_flist (fun x=> f1 x + f2 x) l.
Proof.
 intros. induction l. unfold sum_flist. simpl. reflexivity.
 unfold sum_flist. simpl. rewrite -> plus4. unfold sum_flist in IHl.
 rewrite -> IHl. reflexivity.
Qed.


Lemma app_ass (X:Type) : forall l1 l2 l3 : list X, 
  (l1 ++ l2) ++ l3 = l1 ++ (l2 ++ l3).   
Proof.
  intros l1 l2 l3. induction l1 as [| n l1'].
  reflexivity. simpl. rewrite -> IHl1'. reflexivity.
Qed.

Lemma app_nil_end (X:Type) : forall l : list X, 
  l ++ [] = l.   
Proof.
  intros l. induction l. reflexivity. replace (a :: l) with ([a] ++ l).
  rewrite -> app_ass. rewrite -> IHl. reflexivity. simpl. reflexivity.
Qed.

 
Lemma snoc_cons : forall (x:nat) (l1 l2 : list nat), (l1++[x])++l2 = l1 ++ (x::l2).
Proof.
 intros. induction l1. reflexivity.
 simpl. rewrite -> IHl1. reflexivity.
Qed.

Lemma cons_app : forall (x:nat) (l1 l2 : list nat), x::(l1++l2) = (x::l1) ++ l2.
Proof.
 intros. generalize dependent x. induction l1. reflexivity. 
 simpl. intros. rewrite -> IHl1 with (x:=a).
 simpl. reflexivity.
Qed.

Fixpoint collect_some {X: Type} (l: list (option X)) : option X :=
match l with
| [] => None
| x::xs => match x with
           | None => collect_some xs
           | Some a => Some a
           end
end. 

Fixpoint flat_list {X:Type} (l: list (list X)) : list X :=
match l with 
| [] => []
| lx::ls => lx ++ (flat_list ls)
end.

(*returns the list of the largest element of the given list*)
Fixpoint max_list {X: Type} (ord: X -> nat) (l: list X) : list X :=
match l with
| [] => []
| x::xs => let y := (max_list ord xs) in
                   match y with 
                   | [] => [x] 
                   | yh::yt => if blt_nat (ord x) (ord yh) then y else
                               if beq_nat (ord x) (ord yh) then x::y else [x]                                      
                   end
end. 

Definition measure {X:Type} (pow:X->nat) (l: list X) := fold_left plus (map pow l) 0.

Inductive contains {X: Type}: X -> list X -> Prop :=
| contains1: forall x l l', contains x (l++x::l'). 

Lemma contains_cons: forall (X:Type) (x:X) a l, contains x l -> contains x (a :: l).
Proof.
  intros. inversion H.
  replace (a :: l0 ++ x :: l') with ((a::l0)++x::l').
  constructor. auto.
Qed.


(*some useful lemmas*)
Lemma contains_ornot1: forall (X:Type) (a x: X) l,  x<>a -> 
                                        contains x (a::l) -> contains x l.
Proof.
  intros. inversion H0.
  destruct l0. inversion H3.
  unfold not in H. apply H in H4. inversion H4.
  inversion H3. constructor.
Qed.

Lemma contains_ornot2: forall (X:Type) (a x: X) l, 
                                        contains x (a::l) -> contains x l \/ (x=a).
Proof.
  intros. inversion H.
  destruct l0. inversion H2. right. auto.
  inversion H2. left. constructor.
Qed.

Lemma contains_app: forall (X:Type) (x:X) l l' , contains x (l++l') -> 
                                           contains x l \/ contains x l'.
Proof.
 intros. induction l. right. auto.
 simpl in H. apply contains_ornot2 in H. inversion H.
 apply IHl in H0. inversion H0.
 left. apply contains_cons. auto.
 right. auto. left. rewrite H0.
 replace (a::l) with ([]++a::l).
 constructor. auto.
Qed.

Lemma app_contains: forall (X:Type) (x:X) l l' , 
                     contains x l \/ contains x l' -> contains x (l++l').
Proof.
 intros. inversion H. clear H. generalize dependent l.
 induction l'. intros. replace (l++[]) with l. auto. 
 symmetry. apply app_nil_end. intros. replace (l ++ a::l') with ((l++[a])++l').
 apply IHl'. inversion H0. replace ((l0 ++ x :: l'0) ++ [a]) with
 (l0 ++ x :: (l'0 ++ [a])). constructor. simpl.
 rewrite -> app_ass. auto. 
 rewrite -> app_ass. auto. 
 clear H. inversion H0. 
 replace (l ++ l0 ++ x :: l'0) with ((l ++ l0) ++ x :: l'0).
 constructor. rewrite app_ass. auto.
Qed.
 
Lemma maxlist_in: forall (X:Type) f l (x:X), contains x (max_list f l) -> contains x l.
Proof.
  intros. induction l. inversion H.
  destruct l; inversion H2. 
  simpl in H. remember (max_list f l) as ml.
  destruct ml. inversion H. 
  destruct l0; inversion H2.
  replace (a::l) with ([]++a::l). constructor. auto.
  destruct l0; inversion H4.
  destruct (blt_nat (f a) (f x0)). 
  apply IHl in H. apply contains_cons. auto.
  destruct (beq_nat (f a) (f x0)).
  apply contains_ornot2 in H.
  inversion H. apply IHl in H0.
  apply contains_cons. auto.
  rewrite H0. replace (a::l) with ([]++a::l). constructor.
  auto. inversion H.
  destruct l0; inversion H2.
  replace (a::l) with ([]++a::l). constructor. auto.
  destruct l0; inversion H4.
Qed.

Lemma cons_cont: forall (X:Type) (a x: X) l ll, contains (a :: l) (map (cons x) ll) -> contains l ll.
Proof.
 intros. induction ll. inversion H. 
 destruct l0; inversion H2. simpl in H.
 apply contains_ornot2 in H. inversion H.
 apply contains_cons. auto.
 inversion H0. replace (a0::ll) with ([]++a0::ll).
 constructor. auto.
Qed.

Lemma cont_cons: forall (X:Type) (x: X) l ll, contains l ll -> contains (x :: l) (map (cons x) ll).
Proof.
 intros. induction ll. inversion H.
 destruct l0; inversion H2.
 simpl. apply contains_ornot2 in H.
 inversion H.  apply contains_cons. auto.
 rewrite H0. replace  ((x :: a) :: map (cons x) ll) with ([]++(x :: a) :: map (cons x) ll).
 constructor. auto.
Qed.

Lemma foo: forall X (a:X) l (P:X->list X->Prop), (exists x:X, contains x l /\ P x l) ->
                       (exists x, contains x (a::l) /\ P x l).
Proof.
 intros. inversion H. exists x.
 inversion H0. split. apply contains_cons. auto. auto.
Qed.

Lemma cons_flat: forall (X Y:Type) (f:X->list Y) (y:  Y) (lx: list X), contains y (flat_list (map f lx)) -> 
                                    exists x, contains x lx /\ contains y (f x).
Proof.
 intros. induction lx.
 inversion H. destruct l; inversion H2.
 simpl in H. apply contains_app in H. 
 inversion H. exists a. split. replace (a::lx) with ([]++a::lx).
 constructor. auto. auto.
 apply IHlx in H0. inversion H0.
 inversion H1. exists x.
 split. apply  contains_cons. auto. auto.
Qed.

Lemma map_app: forall (X Y:Type) (f:X->Y) (l' l'':list X), map f (l'++l'') = (map f l') ++ (map f l'').
Proof.
 intros. generalize dependent l''. induction l'.
 intros. auto.  intros.
 simpl. rewrite IHl'. auto.
Qed.

Inductive non_empty_list {X:Type} : list X -> Prop :=
| nel: forall x l, non_empty_list (x::l).


Inductive sublist {X: Type} : list X -> list X -> Prop :=
| sublist0: forall (l' l'': list X), sublist l' (l'++l'').


Lemma sublist_cons: forall (X:Type) x (l l':list X),
                    sublist l l' -> sublist (x::l) (x::l').
Proof.
 intros. inversion H. replace (x :: l ++ l'') with ((x :: l) ++ l'').
 constructor. auto.
Qed.

Lemma fold_plus1: forall (a:nat) (l:list nat), 
                  fold_left plus l a = a + fold_left plus l 0.
Proof.
 intros. generalize dependent a. induction l. intros. auto.
 intros. simpl. rewrite IHl. remember (fold_left plus l 0) as q. rewrite IHl.
 omega.
Qed.

Lemma ble_nat_plus: forall (m n k: nat), ble_nat (k+m) (k+n) = ble_nat m n.
Proof with omega.
  intros. generalize dependent m. generalize dependent n.
  induction k. intros. auto.
  intros. simpl. auto.
Qed.
 

Lemma sublist_less: forall (X: Type) (l l': list X) p,
      sublist l l' -> ble_nat (measure p l) (measure p l') = true.
Proof.
 intros. generalize dependent l'. induction l.
 intros. inversion H. auto.
 intros. inversion H. simpl. unfold measure.
 simpl. rewrite fold_plus1. 
 remember (fold_left plus (map p l) 0) as q. rewrite fold_plus1.
 rewrite Heqq. rewrite ble_nat_plus.
 unfold measure in IHl. apply IHl.
 constructor.
Qed.
 
Lemma max_list_not_empty: forall (X:Type) (l:list X) p, non_empty_list l -> 
                                  non_empty_list (max_list p l).
Proof.
 intros. induction l. inversion H.
 simpl. remember (max_list p l) as ml.
 destruct ml. constructor.
 destruct (blt_nat (p a) (p x)).  constructor.
 destruct (beq_nat (p a) (p x)). constructor.
 constructor.
Qed.

Lemma max_list_equal: forall (X:Type) (l: list X) (x x1:X)  p,
                      contains x (max_list p l) -> contains x1 (max_list p l) -> p x = p x1.
Proof.
 intros X l. induction l.
 intros. inversion H. destruct l; inversion H3.
 intros. simpl in H. simpl in H0.
 remember (max_list p l) as ml.
 destruct ml. inversion H. destruct l0; inversion H3.
 inversion H0. destruct l0; inversion H7. auto.
 destruct l0; inversion H9. destruct l0; inversion H5.
 remember (blt_nat (p a) (p x0)) as bltax0.
 destruct bltax0. apply IHl. rewrite <- Heqml. auto.
 rewrite <- Heqml. auto.  
 remember (beq_nat (p a) (p x0)) as beqax0.
 destruct beqax0. apply contains_ornot2 in H0.
 apply contains_ornot2 in H. inversion H.
 inversion H0. apply IHl. rewrite <- Heqml. auto.
 rewrite <- Heqml. auto. assert (p x0 = p x).
 apply IHl. rewrite <- Heqml. replace (x0::ml) with ([]++x0::ml).
 constructor. auto. rewrite <- Heqml. auto.
 assert (p a = p x0). apply beq_nat_eq.
 auto. rewrite H2. rewrite <- H3. auto.
 inversion H0. assert (p x0 = p x1).
 apply IHl; rewrite <- Heqml; try replace (x0::ml) with ([]++x0::ml);
 try constructor; auto. assert (p a = p x0). apply beq_nat_eq.
 auto. rewrite H1. rewrite <- H3. auto. rewrite H1. rewrite H2. auto.
 inversion H; inversion H0. destruct l0; inversion H3. destruct l1; inversion H5. auto.
 destruct l1; inversion H9; auto. destruct l0; inversion H7; auto.
Qed. 
  
Lemma notbltbeq_blt: forall (m n: nat), blt_nat m n = false -> beq_nat m n = false ->
                                   blt_nat n m = true.
Proof.
 intros. generalize dependent n. induction m.
 intros. destruct n. inversion H0. inversion H.
 intros. destruct n. auto. simpl. 
 apply IHm. auto. auto.
Qed. 

Lemma empty_or_not: forall (X:Type) l, 
                    l = @nil X \/ non_empty_list l.
Proof.
 intros. destruct l. left. auto.
 right. constructor.
Qed.

Lemma id_map: forall (X:Type) l (f:X->X), 
                      (forall x, contains x l -> f x = x) -> map f l = l.
Proof.
 intros. induction l. auto. simpl. rewrite IHl.
 rewrite H. auto. replace (a::l) with ([]++a::l).
 constructor. auto. intros. apply H.
 apply contains_cons. auto.
Qed.

Lemma eq_map: forall (X Y:Type) l (ff gg: X->Y), 
                      (forall x, contains x l -> ff x = gg x) -> map ff l = map gg l.
Proof.
 intros.  induction l. auto. simpl. rewrite IHl.
 rewrite H. auto. replace (a::l) with ([]++a::l).
 constructor. auto. intros. apply H.
 apply contains_cons. auto.
Qed.
 
Lemma map_ass: forall (X Y Z:Type) l (ff:X->Y) (gg:Y->Z),
               map gg (map ff l) = map (fun x => gg (ff x)) l.
Proof.
 intros. induction l. auto.
 simpl. rewrite IHl. auto.
Qed.

Lemma summore_than_one: forall (X:Type) f (x:X) l, 
                        contains x l -> fold_left plus (map f l) 0 >= f x.
Proof.
 intros. induction l.
 inversion H. destruct l; inversion H2.
 simpl. rewrite fold_plus1. apply contains_ornot2 in H.
 inversion H. assert (fold_left plus (map f l) 0 >= f x).
 apply IHl. auto. omega. rewrite H0. omega.
Qed.

Lemma map_contains: forall (X Y: Type) (y:Y) (l: list X) f, contains y (map f l) -> exists x:X, contains x l /\ y = f x.
Proof.
 intros. induction l.
 inversion H. destruct l; inversion H2.
 simpl in H. apply contains_ornot2 in H.
 inversion H. apply IHl in H0.
 inversion H0. exists x. split.
 inversion H1. apply contains_cons. auto. inversion H1. auto.
 exists a. split. replace (a::l) with ([]++a::l).
 constructor. auto. auto.
Qed.

Lemma contains_map: forall (X Y: Type) (x:X) (l: list X) (f:X->Y),  contains x l -> contains (f x) (map f l).
Proof.
 intros. induction l. inversion H. destruct l; inversion H2.
 simpl. apply contains_ornot2 in H. inversion H.
 apply IHl in H0. apply contains_cons. auto.
 rewrite H0. replace (f a:: map f l) with ([]++(f a)::(map f l)).
 constructor. auto.
Qed.
  
Lemma fold_left_n: forall l' l'' n m, fold_left plus l' m = fold_left plus l'' m ->
      fold_left plus l' n = fold_left plus l'' n.
Proof.
 intros. generalize dependent m. generalize dependent n.  generalize dependent l''. 
 induction l'. intros. simpl in H. simpl. rewrite fold_plus1. rewrite fold_plus1 in H.  
 omega.
 intros. simpl. simpl in H. rewrite fold_plus1. rewrite fold_plus1 in H.
 symmetry. rewrite fold_plus1. symmetry in H. rewrite fold_plus1 in H.
 omega.
Qed. 


















