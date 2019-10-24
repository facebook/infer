(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* A simple, concrete memory model where the heap is an array of bytes. This is
 * how llair views memory, and how we give semantics to llvm. Although LLVM's
 * real memory model is still a subject of research, this is mostly to do with
 * enabling the right collection of compiler optimisations. Since we only want to
 * think about LLVM semantics after optimisation, the concrete model is likely to
 * be sufficient. *)

open HolKernel boolLib bossLib Parse;
open arithmeticTheory listTheory rich_listTheory pairTheory;
open logrootTheory numposrepTheory wordsTheory pred_setTheory finite_mapTheory;
open settingsTheory miscTheory;

new_theory "memory_model";

numLib.prefer_num ();

(* Heap addresses *)
Datatype:
  addr = A num
End

(* Values that fit in registers *)
Datatype:
  reg_v =
  | FlatV 'a
  | AggV (reg_v list)
End

(* An interval of allocated memory.
 * Args: whether it's freeable, the starting address (inclusive), and the ending address
 * (exclusive)
 *)
Datatype:
  interval = Interval bool num num
End

Datatype:
  heap = <| memory : addr |-> 'a # word8;
            allocations : interval set;
            valid_addresses : addr set|>
End

Definition erase_tags_def:
  erase_tags h =
    <| memory := (λ(t,b). ((), b)) o_f h.memory;
       allocations := h.allocations;
       valid_addresses := h.valid_addresses |>
End

(* shapes statically describe the shape of a value in a register. Flat shapes
 * take a given number of bytes, Array shapes are replicated a certain number of
 * times
 *)
Datatype:
  shape =
  | Flat num 'a
  | Array shape num
  | Tuple (shape list)
End

(* Does a value have a given shape. The function argument answers the question
 * for flat values/shapes.
 * We use mutual recursion instead of list_rel to work around a HOL bug. *)
Definition value_shape_def:
  (value_shape f (Flat n t) (FlatV x) ⇔ f n t x) ∧
  (value_shape f (Array s n) (AggV vs) ⇔
    every (value_shape f s) vs ∧ length vs = n) ∧
  (value_shape f (Tuple ss) (AggV vs) ⇔
    value_shapes f ss vs) ∧
  (value_shape _ _ _ ⇔ F) ∧
  (value_shapes f [] [] ⇔ T) ∧
  (value_shapes f (s::ss) (v::vs) ⇔
    value_shape f s v ∧ value_shapes f ss vs) ∧
  (value_shapes _ _ _ ⇔ F)
Termination
  WF_REL_TAC `measure (\x. case x of | INL (_, x, _) => shape_size (\x.0) x | INR (_, y, _) => shape1_size (\x.0) y)`
End

Theorem value_shapes_list_rel:
  ∀f ss vs. value_shapes f ss vs ⇔ list_rel (value_shape f) ss vs
Proof
  Induct_on `ss` >> Cases_on `vs` >> rw [value_shape_def]
QED

Theorem value_shape_cases:
  ∀f s v.
    value_shape f s v ⇔
      (∃n t x. s = Flat n t ∧ v = FlatV x ∧ f n t x) ∨
      (∃s2 n vs. s = Array s2 n ∧ v = AggV vs ∧
        every (value_shape f s2) vs ∧ length vs = n) ∨
      (∃ss vs. s = Tuple ss ∧ v = AggV vs ∧ value_shapes f ss vs)
Proof
  rw [] >>
  Cases_on `s` >> Cases_on `v` >> rw [value_shape_def] >> metis_tac []
QED

Definition sizeof_def:
  (sizeof (Flat n t) = n) ∧
  (sizeof (Array s n) = n * sizeof s) ∧
  (sizeof (Tuple ss) = sum (map sizeof ss))
Termination
  WF_REL_TAC `measure (shape_size (\x.0))` >> rw [] >>
  Induct_on `ss` >> rw [definition "shape_size_def"] >>
  res_tac >> decide_tac
End

Definition interval_to_set_def:
  interval_to_set (Interval _ start stop) =
    { n | start ≤ n ∧ n < stop }
End

Definition interval_ok_def:
  interval_ok (Interval b i1 i2) valid_addresses ⇔
    i1 ≤ i2 ∧ image A (interval_to_set (Interval b i1 i2)) ⊆ valid_addresses
End

Definition interval_freeable_def:
  interval_freeable (Interval b _ _) ⇔ b
End

Definition is_allocated_def:
  is_allocated b1 h ⇔
    interval_ok b1 h.valid_addresses ∧
    ∃b2. b2 ∈ h.allocations ∧ (interval_freeable b1 ⇔ interval_freeable b2) ∧
         interval_to_set b1 ⊆ interval_to_set b2
End

Definition is_free_def:
  is_free b1 h ⇔
    interval_ok b1 h.valid_addresses ∧
    ∀b2. b2 ∈ h.allocations ⇒ interval_to_set b1 ∩ interval_to_set b2 = ∅
End

(* The allocations are of intervals that don't overlap *)
Definition allocations_ok_def:
  allocations_ok h ⇔
    ∀i1 i2.
      i1 ∈ h.allocations ∧ i2 ∈ h.allocations
      ⇒
      interval_ok i1 h.valid_addresses ∧ interval_ok i2 h.valid_addresses ∧
      (interval_to_set i1 ∩ interval_to_set i2 ≠ ∅ ⇒ i1 = i2)
End

(* The heap maps exactly the address in the allocations *)
Definition heap_ok_def:
  heap_ok h ⇔
    allocations_ok h ∧
    ∀n. flookup h.memory (A n) ≠ None ⇔ ∃i. i ∈ h.allocations ∧ n ∈ interval_to_set i
End

Definition get_bytes_def:
  get_bytes h (Interval _ start stop) =
    map
      (λoff.
        case flookup h.memory (A (start + off)) of
        | Some w => w)
      (count_list (stop - start))
End

Definition set_bytes_def:
  (set_bytes p [] n h = h) ∧
  (set_bytes p (b::bs) n h =
    set_bytes p bs (Suc n) (h with memory := h.memory |+ (A n, (p, b))))
End

(* Allocate a free chunk of memory, and write non-deterministic bytes into it *)
Inductive allocate:
  ∀h size tag p bytes b.
   b = Interval T p (p + size) ∧
   is_free b h ∧
   length bytes = size
   ⇒
   allocate h size tag (p, set_bytes tag bytes p
                           <| memory := h.memory;
                              allocations := { b } ∪ h.allocations;
                              valid_addresses := h.valid_addresses |>)
End

Definition deallocate_def:
  deallocate addrs h =
    let to_remove = { Interval T n stop | A n ∈ set addrs ∧ Interval T n stop ∈ h.allocations } in
      <| memory := fdiff h.memory (image A (bigunion (image interval_to_set to_remove)));
         allocations := h.allocations DIFF to_remove;
         valid_addresses := h.valid_addresses |>
End


(* Read len bytes from the list of bytes, and convert it into a number,
 * little-endian encoding *)
Definition le_read_num_def:
  le_read_num len (bs : word8 list) =
    if length bs < len then
      (l2n 256 (map w2n bs), [])
    else
      (l2n 256 (map w2n (take len bs)), drop len bs)
End

(* Return len bytes that are the little-endian encoding of the argument number,
 * assuming that it fits*)
Definition le_write_num_def:
  le_write_num len n =
    let (l : word8 list) = map n2w (n2l 256 n) in
      take len (l ++ replicate (len - length l) 0w)
End

(* Read a from a list of bytes bs to build a register value described by a
 * shape. The function f is applied to the word read from a flat list of bytes
 * in little-endian order. *)
Definition bytes_to_value_def:
  (bytes_to_value f (Flat n t) bs = (FlatV o f n t ## I) (le_read_num n bs)) ∧
  (bytes_to_value f (Array s n) bs = (AggV ## I) (read_array f n s bs)) ∧
  (bytes_to_value f (Tuple ts) bs = (AggV ## I) (read_str f ts bs)) ∧
  (read_array f 0 s bs = ([], bs)) ∧
  (read_array f (Suc n) s bs =
    let (v, bs) = bytes_to_value f s bs in
    let (rest, bs) = read_array f n s bs in
      (v::rest, bs)) ∧
  (read_str f [] bs = ([], bs)) ∧
  (read_str f (s::ss) bs =
    let (v, bs) = bytes_to_value f s bs in
    let (rest, bs) = read_str f ss bs in
      (v::rest, bs))
Termination
  WF_REL_TAC `measure (λx. case x of
                           | INL (_, t, bs) => shape_size (\x.0) t
                           | INR (INL (_, n, t, bs)) => n + shape_size (\x.0) t
                           | INR (INR (_, ts, bs)) => shape1_size (\x.0) ts)`
End

(* Convert the given value to a list of bytes, the function gives the size, and
 * a machine word for a flat value *)
Definition value_to_bytes_def:
  (value_to_bytes f (FlatV x) =
    let (size, word) = f x in
      le_write_num size word) ∧
  (value_to_bytes f (AggV vs) = flat (map (value_to_bytes f) vs))
Termination
  WF_REL_TAC `measure (reg_v_size (\x. 0) o snd)` >>
  Induct >> rw [definition "reg_v_size_def"] >>
  TRY (first_x_assum drule) >>
  decide_tac
End

(* ----- Theorems about converting between values and byte lists ----- *)

Theorem le_write_num_length:
  ∀l x. length (le_write_num l w) = l
Proof
  rw [le_write_num_def]
QED

Theorem v2b_size_lem:
  (∀(f:num->'a->'b->bool) s v. (∀n t x. f n t x ⇒ fst (g x) = n) ∧ value_shape f s v ⇒ length (value_to_bytes g v) = sizeof s) ∧
  (∀(f:num->'a->'b->bool) ss vs. (∀n t x. f n t x ⇒ fst (g x) = n) ∧ value_shapes f ss vs ⇒ sum (map (length o value_to_bytes g) vs) = sum (map sizeof ss))
Proof
  ho_match_mp_tac value_shape_ind >>
  rw [value_to_bytes_def, sizeof_def, value_shape_def]
  >- (
    pairarg_tac >> simp [] >>
    metis_tac [FST, le_write_num_length])
  >- (
    simp [LENGTH_FLAT, MAP_MAP_o, combinTheory.o_DEF] >>
    Induct_on `vs` >> rw [ADD1] >> fs [LEFT_ADD_DISTRIB] >>
    metis_tac [])
  >- (fs [LENGTH_FLAT, ETA_THM, MAP_MAP_o, combinTheory.o_DEF] >> metis_tac [])
  >- metis_tac [ADD_COMM]
QED

Theorem v2b_size:
  ∀f s v g. (∀n t x. f n t x ⇒ fst (g x) = n) ∧ value_shape f s v ⇒ length (value_to_bytes g v) = sizeof s
Proof
  metis_tac [v2b_size_lem]
QED

Theorem b2v_size_lem:
  (∀(f:num->'c ->num -> 'b) s bs. sizeof s ≤ length bs ⇒
    ∃v. bytes_to_value f s bs = (v, drop (sizeof s) bs)) ∧
  (∀(f:num->'c ->num -> 'b) n s bs. n * sizeof s ≤ length bs ⇒
    ∃vs. read_array f n s bs = (vs, drop (n * sizeof s) bs)) ∧
  (∀(f:num->'c ->num -> 'b) ss bs. sum (map sizeof ss) ≤ length bs ⇒
    ∃vs. read_str f ss bs = (vs, drop (sum (map sizeof ss)) bs))
Proof
  ho_match_mp_tac bytes_to_value_ind >>
  rw [sizeof_def, bytes_to_value_def, le_read_num_def] >>
  fs []
  >- (simp [PAIR_MAP] >> metis_tac [SND])
  >- (
    pairarg_tac >> rw [] >> pairarg_tac >> rw [] >>
    fs [ADD1] >> rw [] >> fs [DROP_DROP_T, LEFT_ADD_DISTRIB])
  >- fs [DROP_DROP_T, LEFT_ADD_DISTRIB]
QED

Theorem b2v_size:
  ∀f s bs. sizeof s ≤ length bs ⇒
    ∃v. bytes_to_value f s bs = (v, drop (sizeof s) bs)
Proof
  metis_tac [b2v_size_lem]
QED

Theorem b2v_smaller:
  ∀f s bs. sizeof s ≤ length bs ⇒
    length (snd (bytes_to_value f s bs)) = length bs - sizeof s
Proof
  rw [] >> imp_res_tac b2v_size >>
  Cases_on `bytes_to_value f s bs` >> fs [] >>
  first_x_assum (qspec_then `f` mp_tac) >> rw []
QED

Theorem b2v_append_lem:
  (∀(f:num->'c->num -> 'b) s bs. sizeof s ≤ length bs ⇒
    bytes_to_value f s (bs ++ bs') = (I ## (λx. x ++ bs')) (bytes_to_value f s bs)) ∧
  (∀(f:num->'c->num -> 'b) n s bs. n * sizeof s ≤ length bs ⇒
    ∃vs. read_array f n s (bs ++ bs') = (I ## (λx. x ++ bs')) (read_array f n s bs)) ∧
  (∀(f:num->'c-> num -> 'b) ss bs. sum (map sizeof ss) ≤ length bs ⇒
    ∃vs. read_str f ss (bs ++ bs') = (I ## (λx. x ++ bs')) (read_str f ss bs))
Proof
  ho_match_mp_tac bytes_to_value_ind >>
  rw [sizeof_def, bytes_to_value_def, le_read_num_def] >>
  fs [TAKE_APPEND, DROP_APPEND,
      DECIDE ``!x y. x ≤ y ⇒ x - y = 0n``, ETA_THM]
  >- (simp [PAIR_MAP] >> metis_tac [SND])
  >- (simp [PAIR_MAP] >> metis_tac [SND])
  >- (
    rpt (pairarg_tac >> simp []) >> fs [ADD1] >>
    BasicProvers.VAR_EQ_TAC >> fs [LEFT_ADD_DISTRIB] >>
    first_x_assum irule >>
    `sizeof s ≤ length bs` by decide_tac >>
    qspec_then `f` drule b2v_smaller >>
    disch_then (qspec_then `f` mp_tac) >>
    rw [])
  >- (
    rpt (pairarg_tac >> simp []) >> fs [ADD1] >>
    BasicProvers.VAR_EQ_TAC >> fs [LEFT_ADD_DISTRIB] >>
    first_x_assum irule >>
    `sizeof s ≤ length bs` by decide_tac >>
    qspec_then `f` drule b2v_smaller >>
    disch_then (qspec_then `f` mp_tac) >>
    rw [])
QED

Theorem b2v_append:
  ∀f s bs bs'. sizeof s ≤ length bs ⇒
    bytes_to_value f s (bs ++ bs') = (I ## (λx. x ++ bs')) (bytes_to_value f s bs)
Proof
  metis_tac [b2v_append_lem]
QED

Theorem le_read_write:
  ∀size n  bs.
    n < 256 ** size ⇒ le_read_num size (le_write_num size n ⧺ bs) = (n, bs)
Proof
  rw [le_read_num_def, le_write_num_length]
  >- (
    `take size (le_write_num size n ⧺ bs) = le_write_num size n`
    by metis_tac [le_write_num_length, TAKE_LENGTH_APPEND] >>
    simp [le_write_num_def, w2l_def, l2w_def] >>
    fs [l2n_padding, TAKE_APPEND, take_replicate] >>
    simp [MAP_TAKE, MAP_MAP_o, combinTheory.o_DEF, mod_n2l] >>
    rename1 `n2l 256 m` >>
    Cases_on `size = 0` >> fs [] >>
    `length (n2l 256 m) ≤ size`
    by (
      rw [LENGTH_n2l] >>
      `256 = 2 ** 8` by EVAL_TAC >>
      ASM_REWRITE_TAC [] >> simp [log_change_base_power, GSYM LESS_EQ] >>
      rw [] >> fs [bitTheory.LOG2_def, dimword_def] >>
      `8 * (log 2 m DIV 8) ≤ log 2 m` by metis_tac [mul_div_bound, EVAL ``8 ≠ 0n``] >>
      `LOG 2 m ≤ LOG 2 (256 ** size)` by simp [LOG_LE_MONO, LESS_IMP_LESS_OR_EQ] >>
      pop_assum mp_tac >>
      `256 = 2 ** 8` by EVAL_TAC >>
      ASM_REWRITE_TAC [EXP_MUL] >> simp [log_base_power] >>
      Cases_on `log 2 m DIV 8 = size` >> rw [] >>
      CCONTR_TAC >> fs [] >>
      `log 2 m = (8 * (log 2 m DIV 8))` by intLib.COOPER_TAC >> fs [] >>
      `2 ** log 2 m = 2 ** (8 * (log 2 m DIV 8))` by rw [] >>
      fs [EXP_EXP_MULT] >>
      `2 ** log 2 m ≤ m` by rw [exp_log_bound] >>
      decide_tac) >>
    simp [mod_n2l, l2n_n2l, TAKE_LENGTH_TOO_LONG])
  >- metis_tac [le_write_num_length, DROP_LENGTH_APPEND]
QED

Theorem le_write_read:
  ∀size n bs bs'.
    size ≤ length bs ∧
    le_read_num size bs = (n, bs')
    ⇒
    le_write_num size n ++ bs' = bs
Proof
  rw [le_read_num_def] >>
  qmatch_goalsub_abbrev_tac `l2n _ l` >>
  `le_write_num size (l2n 256 l) = take size bs` suffices_by metis_tac [TAKE_DROP] >>
  simp [le_write_num_def, w2l_l2w] >>
  `l2n 256 l < 256 ** size`
  by (
    `size ≤ length bs` by decide_tac >>
    metis_tac [l2n_lt, LENGTH_TAKE, LENGTH_MAP, EVAL ``0n < 256``]) >>
  `every ($> 256) l`
  by (
    simp [EVERY_MAP, Abbr `l`] >> irule EVERY_TAKE >> simp [] >>
    rpt (pop_assum kall_tac) >>
    Induct_on `bs` >> rw [] >>
    Cases_on `h` >> fs []) >>
  rw [n2l_l2n]
  >- (
    rw [TAKE_def, take_replicate] >>
    Cases_on `size` >> fs [] >>
    rfs [l2n_0] >> unabbrev_all_tac >> fs [EVERY_MAP] >>
    ONCE_REWRITE_TAC [GSYM REPLICATE] >>
    qmatch_goalsub_abbrev_tac `take size _` >>
    qpat_assum `¬(_ < _)` mp_tac >>
    qpat_assum `every (\x. 0 = w2n x) _` mp_tac >>
    rpt (pop_assum kall_tac) >>
    qid_spec_tac `bs` >>
    Induct_on `size` >> rw [] >>
    Cases_on `bs` >> rw [] >> fs [] >>
    Cases_on `h` >> fs [] >>
    first_x_assum irule >> rw [] >>
    irule MONO_EVERY >>
    qexists_tac `(λx. 0 = w2n x)` >> rw []) >>
  fs [MAP_TAKE, MAP_MAP_o, combinTheory.o_DEF] >>
  `exists (\y. 0 ≠ y) l`
  by (
    fs [l2n_eq_0, combinTheory.o_DEF] >> fs [EXISTS_MEM, EVERY_MEM] >>
    qexists_tac `x` >> rfs [MOD_MOD, GREATER_DEF]) >>
  simp [LOG_l2n_dropWhile] >>
  `length (dropWhile ($= 0) (reverse l)) ≠ 0`
  by (
    Cases_on `l` >> fs [dropWhile_eq_nil, combinTheory.o_DEF, EXISTS_REVERSE]) >>
  `0 < length (dropWhile ($= 0) (reverse l))` by decide_tac >>
  fs [SUC_PRE] >>
  `map n2w l = take size bs`
  by (simp [Abbr `l`, MAP_TAKE, MAP_MAP_o, combinTheory.o_DEF, n2w_w2n]) >>
  simp [TAKE_TAKE_MIN] >>
  `length l = size` by simp [Abbr `l`] >>
  `length (dropWhile ($= 0) (reverse l)) ≤ size`
  by metis_tac [LESS_EQ_TRANS, LENGTH_dropWhile_LESS_EQ, LENGTH_REVERSE] >>
  rw [MIN_DEF] >> fs []
  >- (
    simp [TAKE_APPEND, TAKE_TAKE_MIN, MIN_DEF, take_replicate] >>
    `replicate (length l − length (dropWhile ($= 0) (reverse l))) 0w =
     take (length l − (length (dropWhile ($= 0) (reverse l)))) (drop (length (dropWhile ($= 0) (reverse l))) bs)`
    suffices_by (rw [] >> irule take_drop_partition >> simp []) >>
    rw [LIST_EQ_REWRITE, EL_REPLICATE, EL_TAKE, EL_DROP] >>
   `length (dropWhile ($= 0) (reverse l)) =
     length (dropWhile (λx. 0 = w2n x) (reverse (take (length l) bs)))`
    by (
      `reverse l = reverse (take (length l) (map w2n bs))` by metis_tac [] >>
      ONCE_ASM_REWRITE_TAC [] >>
      qpat_x_assum `Abbrev (l = _)` kall_tac >>
      simp [GSYM MAP_TAKE, GSYM MAP_REVERSE, dropWhile_map, combinTheory.o_DEF]) >>
    fs [] >>
    `x + length (dropWhile (λx. 0 = w2n x) (reverse (take (length l) bs))) < length l` by decide_tac >>
    drule (SIMP_RULE std_ss [LET_THM] dropWhile_rev_take) >>
    rw [] >>
    REWRITE_TAC [GSYM w2n_11, word_0_n2w] >>
    simp [])
  >- rw [TAKE_APPEND, TAKE_TAKE]
QED

Theorem b2v_v2b_lem:
  (∀f s v.
    value_shape f s v ⇒
    (∀x t n. f n t x ⇒ fst (h x) = n ∧ g n t (snd (h x)) = x ∧ snd (h x) < 256 ** n) ⇒
    ∀bs.
      bytes_to_value g s (value_to_bytes h v ++ bs) = (v, bs)) ∧
  (∀f ss vs.
    value_shapes f ss vs ⇒
    (∀x t n. f n t x ⇒ fst (h x) = n ∧ g n t (snd (h x)) = x ∧ snd (h x) < 256 ** n) ⇒
    ∀bs.
      read_str g ss (flat (map (value_to_bytes h) vs) ++ bs) = (vs, bs))
Proof
  ho_match_mp_tac value_shape_ind >> rw [value_shape_def] >>
  fs [bytes_to_value_def, value_to_bytes_def] >> rw []
  >- (
    pairarg_tac >> fs [] >>
    first_x_assum drule >> simp [] >> rw [] >>
    simp [le_read_write])
  >- (
    qmatch_abbrev_tac `_ x = _` >>
    `x = (vs, bs)` suffices_by (simp [PAIR_MAP] >> metis_tac [PAIR_EQ, FST, SND]) >>
    unabbrev_all_tac >>
    qid_spec_tac `bs` >> Induct_on `vs` >> simp [bytes_to_value_def] >>
    rw [] >> fs [definition "reg_v_size_def"] >>
    pairarg_tac >> fs [] >>
    pairarg_tac >> fs [] >>
    rename1 `value_shape _ s v1` >>
    qpat_x_assum `_ ⇒ !bs. P bs` mp_tac >> impl_tac >> simp [] >>
    metis_tac [APPEND_ASSOC, PAIR_EQ])
  >- (
    qmatch_abbrev_tac `_ x = _` >>
    `x = (vs, bs)` suffices_by (simp [PAIR_MAP] >> metis_tac [PAIR_EQ, FST, SND]) >>
    metis_tac [])
  >- (
    pairarg_tac >> fs [] >>
    pairarg_tac >> fs [] >>
    metis_tac [APPEND_ASSOC, PAIR_EQ])
QED

Theorem b2v_v2b:
  ∀f g h s v bs.
    value_shape f s v ⇒
    (∀x n t. f n t x ⇒ fst (h x) = n ∧ g n t (snd (h x)) = x ∧ snd (h x) < 256 ** n) ⇒
      bytes_to_value g s (value_to_bytes h v ++ bs) = (v, bs)
Proof
  metis_tac [b2v_v2b_lem]
QED

Theorem flookup_set_bytes:
  ∀tag bytes n h n'.
    flookup ((set_bytes tag bytes n h).memory) (A n') =
      if n ≤ n' ∧ n' < n + length bytes then
        Some (tag, el (n' - n) bytes)
      else
        flookup h.memory (A n')
Proof
  Induct_on `bytes` >> rw [set_bytes_def, EL_CONS, PRE_SUB1] >>
  fs [ADD1, FLOOKUP_UPDATE] >>
  `n = n'` by decide_tac >>
  rw []
QED

Theorem set_bytes_unchanged:
  ∀t bs p h. (set_bytes t bs p h).valid_addresses = h.valid_addresses ∧
             (set_bytes t bs p h).allocations = h.allocations
Proof
  Induct_on `bs` >> rw [set_bytes_def]
QED

Theorem allocate_unchanged:
  ∀h1 v1 t h2 v2.
    allocate h1 v1 t (v2, h2)
    ⇒
    h1.valid_addresses = h2.valid_addresses ∧
    h1.allocations ⊆ h2.allocations
Proof
  rw [allocate_cases] >> rw [set_bytes_unchanged]
QED

Theorem allocate_heap_ok:
  ∀h1 v1 t v2 h2. heap_ok h1 ∧ allocate h1 v1 t (v2,h2) ⇒ heap_ok h2
Proof
  rw [allocate_cases, heap_ok_def]
  >- (
    fs [allocations_ok_def] >> rpt gen_tac >> disch_tac >> fs [is_free_def] >> rw [] >>
    fs [set_bytes_unchanged] >> metis_tac [INTER_COMM])
  >- (
    rw [flookup_set_bytes, set_bytes_unchanged]
    >- rw [RIGHT_AND_OVER_OR, EXISTS_OR_THM, interval_to_set_def] >>
    eq_tac >> rw [] >> fs [interval_to_set_def] >>
    metis_tac [])
QED

Theorem set_bytes_heap_ok:
  ∀h tag bytes n b.
    heap_ok h ∧ is_allocated (Interval b n (n + length bytes)) h
    ⇒
    heap_ok (set_bytes tag bytes n h)
Proof
  rw [heap_ok_def]
  >- (fs [allocations_ok_def] >> rw [set_bytes_unchanged] >> metis_tac [])
  >- (
    fs [flookup_set_bytes, set_bytes_unchanged] >> rw [] >>
    fs [is_allocated_def, interval_to_set_def, SUBSET_DEF] >>
    metis_tac [LESS_EQ_REFL, DECIDE ``!x y. x < x + SUC y``])
QED
Theorem erase_tags_set_bytes:
  ∀p v l h. erase_tags (set_bytes p v l h) = set_bytes () v l (erase_tags h)
Proof
  Induct_on `v` >> rw [set_bytes_def] >>
  irule (METIS_PROVE [] ``x = y ⇒ f a b c x = f a b c y``) >>
  rw [erase_tags_def]
QED

Theorem erase_tags_unit_id[simp]:
  ∀h. erase_tags h = h
Proof
  rw [erase_tags_def, theorem "heap_component_equality", fmap_eq_flookup, FLOOKUP_o_f] >>
  CASE_TAC >> rw [] >>
  Cases_on `x'` >> rw []
QED

Theorem is_allocated_suc:
  n ≤ n' ⇒ is_allocated (Interval b n (Suc n')) h ⇒ is_allocated (Interval b n n') h
Proof
  rw [is_allocated_def, interval_ok_def, interval_to_set_def, SUBSET_DEF,
      interval_freeable_def]
  >- (first_x_assum irule >> rw [])
  >- (qexists_tac `b2` >> rw [])
QED

Theorem get_bytes_erase_tags:
  ∀h i. heap_ok h ∧ is_allocated i h ⇒ map snd (get_bytes (erase_tags h) i) = map snd (get_bytes h i)
Proof
  Cases_on `i` >> rw [get_bytes_def, MAP_MAP_o, combinTheory.o_DEF, erase_tags_def] >>
  Induct_on `n0 - n` >> rw [erase_tags_def, FLOOKUP_o_f]
  >- (`n0 - n = 0` by decide_tac >> rw [COUNT_LIST_def]) >>
  Cases_on `n0` >> fs [] >>
  `Suc n' - n = Suc (n' - n)` by decide_tac >>
  asm_simp_tac std_ss [COUNT_LIST_SNOC] >>
  `v = n' - n` by decide_tac >> fs [] >>
  first_x_assum (qspecl_then [`n'`, `n`] mp_tac) >> rw []
  >- (
    fs [LIST_EQ_REWRITE, EL_MAP, FLOOKUP_o_f] >> rw [] >>
    `n ≤ n'` by decide_tac >>
    drule is_allocated_suc >> disch_then drule >> rw []) >>
  BasicProvers.EVERY_CASE_TAC >> rw []
  >- (
    fs [heap_ok_def] >> rfs [is_allocated_def] >>
    first_x_assum (qspec_then `n'` mp_tac) >> rw [] >>
    pop_assum (qspec_then `b2` mp_tac) >> rw [] >>
    fs [interval_to_set_def, SUBSET_DEF] >>
    first_x_assum (qspec_then `n'` mp_tac) >> rw []) >>
  pairarg_tac >> rw []
QED

Theorem is_allocated_erase_tags[simp]:
  ∀i h. is_allocated i (erase_tags h) ⇔ is_allocated i h
Proof
  rw [is_allocated_def, erase_tags_def]
QED

export_theory ();
