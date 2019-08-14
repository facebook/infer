(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* A mini-LLVM model, focussing on the semantics of the parts of the IR that
 * are relevant for the LLVM -> LLAIR translation, especially exceptions. *)

open HolKernel boolLib bossLib Parse;
open settingsTheory;

new_theory "llvm";

numLib.prefer_num ();

(* ----- Abstract syntax ----- *)

(* Only support 1, 8, 32, and 64 bit words for now *)
Datatype `
  size = W1 | W8 | W32 | W64`;

Datatype `
  ty =
  | FunT ty (ty list)
  | IntT size
  | PtrT ty
  | ArrT num ty
  | StrT (ty list)`;

Datatype `
  label = Lab string`;

Datatype `
  loc_var = Loc string`;

Datatype `
  glob_var = GlobName string`;

Datatype `
  fun_name = Fn string`;

Datatype `
  const =
  | IntC size int
  | StrC ((ty # const) list)
  | ArrC ((ty # const) list)
  | GepC ty const (ty # const) ((ty # const) list)
  | GlobalC glob_var
  | UndefC`;

Datatype `
  arg = Constant const | Variable loc_var`;

type_abbrev ("targ", ``:ty # arg``);

Datatype `
  cond = Eq | Ult | Slt`;

Datatype `
  instr =
  (* Terminators *)
  | Ret targ
  | Br arg label label
  | Invoke loc_var ty arg (targ list) label label
  | Unreachable
  (* Non-terminators *)
  | Sub loc_var bool bool ty arg arg
  | Extractvalue loc_var targ (const list)
  | Insertvalue loc_var targ targ (const list)
  | Alloca loc_var ty targ
  | Load loc_var ty targ
  | Store targ targ
  | Gep loc_var targ (targ list)
  | Ptrtoint loc_var targ ty
  | Inttoptr loc_var targ ty
  | Icmp cond ty arg arg
  | Call loc_var ty fun_name (targ list)
  (* C++ runtime functions *)
  | Cxa_allocate_exn loc_var arg
  | Cxa_throw arg arg arg
  | Cxa_begin_catch loc_var arg
  | Cxa_end_catch
  | Cxa_get_exception_ptr loc_var arg`;

Datatype `
  phi = Phi loc_var ty (label option |-> arg)`;

Datatype `
  clause = Catch targ`;

Datatype `
  landingpad = Landingpad ty bool (clause list)`;

Datatype `
  blockHeader =
  | Entry
  | Head (phi list) (landingpad option)`;

Datatype `
  block = <| h : blockHeader; body : instr list |>`;

Datatype `
  def =
    <| r : ty;
       params : loc_var list;
       (* None -> entry block, and Some name -> non-entry block *)
       blocks : label option |-> block |>`;

type_abbrev ("prog", ``:fun_name |-> def``);

Definition terminator_def:
  (terminator (Ret _) ⇔ T) ∧
  (terminator (Br _ _ _) ⇔ T) ∧
  (terminator (Invoke _ _ _ _ _ _) ⇔ T) ∧
  (terminator Unreachable ⇔ T) ∧
  (terminator _ ⇔ F)
End

(* ----- Semantic states ----- *)

Datatype `
  addr = A num`;

Datatype `
  v =
  | W1V word1
  | W8V word8
  | W32V word32
  | W64V word64
  | AggV (v list)
  | PtrV word64
  | UndefV`;

Datatype `
  pv = <| poison : bool; value : v |>`;

Datatype `
  pc = <| f : fun_name; b : label option; i : num |>`;

Datatype `
  frame = <| ret : pc; saved_locals : loc_var |-> pv; result_var : loc_var; stack_allocs : addr list |>`;

Datatype `
  state =
    <| ip : pc;
       (* Keep the size of the global with its memory address *)
       globals : glob_var |-> (num # word64);
       locals : loc_var |-> pv;
       stack : frame list;
       (* The set of allocated ranges. The bool indicates whether the range is
        * free-able or not *)
       allocations : (bool # num # num) set;
       (* A byte addressed heap, with a poison tag *)
       heap : addr |-> bool # word8 |>`;

(* ----- Things about types ----- *)

(* How many bytes a value of the given type occupies *)
Definition sizeof_def:
  (sizeof (IntT W1) = 1) ∧
  (sizeof (IntT W8) = 1) ∧
  (sizeof (IntT W32) = 4) ∧
  (sizeof (IntT W64) = 8) ∧
  (sizeof (PtrT _) = 8) ∧
  (sizeof (ArrT n t) = n * sizeof t) ∧
  (sizeof (StrT ts) = sum (map sizeof ts))
Termination
  WF_REL_TAC `measure ty_size` >> simp [] >>
  Induct >> rw [definition "ty_size_def"] >> simp [] >>
  first_x_assum drule >> decide_tac
End

Definition first_class_type_def:
  (first_class_type (IntT _) ⇔ T) ∧
  (first_class_type (PtrT _) ⇔ T) ∧
  (first_class_type (ArrT _ t) ⇔ first_class_type t) ∧
  (first_class_type (StrT ts) ⇔ every first_class_type ts) ∧
  (first_class_type _ ⇔ F)
Termination
  WF_REL_TAC `measure ty_size` >>
  rw [] >>
  Induct_on `ts` >> rw [definition "ty_size_def"] >>
  res_tac >> decide_tac
End

Definition indices_ok_def:
  (indices_ok _ [] ⇔ T) ∧
  (indices_ok (ArrT n t) (i::indices) ⇔
    i < n ∧ indices_ok t indices) ∧
  (indices_ok (StrT ts) (i::indices) ⇔
    i < length ts ∧ indices_ok (el i ts) indices) ∧
  (indices_ok _ _ ⇔ F)
End

Inductive value_type:
  (value_type (IntT W1) (W1V w1)) ∧
  (value_type (IntT W8) (W8V w8)) ∧
  (value_type (IntT W32) (W32V w32)) ∧
  (value_type (IntT W64) (W64V w64)) ∧
  (value_type (PtrT _) (PtrV w64)) ∧
  (every (value_type t) vs ∧ length vs = n ∧ first_class_type t
   ⇒
   value_type (ArrT n t) (AggV vs)) ∧
  (list_rel value_type ts vs
   ⇒
   value_type (StrT ts) (AggV vs))
End

(* ----- Semantic transitions ----- *)

Definition w64_cast_def:
  (w64_cast w (IntT W1) = Some (W1V (w2w w))) ∧
  (w64_cast w (IntT W8) = Some (W8V (w2w w))) ∧
  (w64_cast w (IntT W32) = Some (W32V (w2w w))) ∧
  (w64_cast w (IntT W64) = Some (W64V w)) ∧
  (w64_cast _ _ = None)
End

Definition cast_w64_def:
  (cast_w64 (W1V w) = Some (w2w w)) ∧
  (cast_w64 (W8V w) = Some (w2w w)) ∧
  (cast_w64 (W32V w) = Some (w2w w)) ∧
  (cast_w64 (W64V w) = Some w) ∧
  (cast_w64 _ = None)
End

Definition cast_num_def:
  cast_num v = option_map w2n (cast_w64 v)
End

Definition bool_to_v_def:
  bool_to_v b = if b then W1V 1w else W1V 0w
End

(* Calculate the offset given by a list of indices *)
Definition get_offset_def:
  (get_offset _ [] = Some 0) ∧
  (get_offset (ArrT _ t) (i::is) =
    case get_offset t is of
    | None => None
    | Some off => Some (i * sizeof t + off)) ∧
  (get_offset (StrT ts) (i::is) =
    if i < length ts then
      case get_offset (el i ts) is of
      | None => None
      | Some off => Some (sum (map sizeof (take i ts)) + off)
    else
      None) ∧
  (get_offset _ _ = Some 0)
End

Definition eval_const_def:
  (eval_const g (IntC W1 i) = W1V (i2w i)) ∧
  (eval_const g (IntC W8 i) = W8V (i2w i)) ∧
  (eval_const g (IntC W32 i) = W32V (i2w i)) ∧
  (eval_const g (IntC W64 i) = W64V (i2w i)) ∧
  (eval_const g (StrC tconsts) = AggV (map (eval_const g) (map snd tconsts))) ∧
  (eval_const g (ArrC tconsts) = AggV (map (eval_const g) (map snd tconsts))) ∧
  (eval_const g (GepC ty ptr (t, idx) indices) =
    case (eval_const g ptr, cast_num (eval_const g idx)) of
    | (PtrV w, Some n) =>
      let ns = map (λ(t,ci). case cast_num (eval_const g ci) of None => 0 | Some n => n) indices in
        (case get_offset ty ns of
         | None => UndefV
         | Some off => PtrV (n2w (w2n w + sizeof ty * n + off)))
    | _ => UndefV) ∧
  (eval_const g (GlobalC var) =
    case flookup g var of
    | None => PtrV 0w
    | Some (s,w) => PtrV w) ∧
  (eval_const g UndefC = UndefV)
Termination
  WF_REL_TAC `measure (const_size o snd)` >> rw [listTheory.MEM_MAP] >>
  TRY
    (TRY (PairCases_on `y`) >> simp [] >>
    Induct_on `tconsts` >> rw [] >> rw [definition "const_size_def"] >>
    res_tac >> fs [] >> NO_TAC) >>
  Induct_on `indices` >> rw [] >> rw [definition "const_size_def"] >>
  fs []
End

Definition eval_def:
  (eval s (Variable v) =
    case flookup s.locals v of
    | None => <| poison := F; value := W1V 0w |>
    | Some v => v) ∧
  (eval s (Constant c) = <| poison := F; value := eval_const s.globals c |>)
End

Definition v2n_def:
  (v2n (W1V b) = Some (if T then 1 else 0)) ∧
  (v2n (W8V w8) = Some (w2n w8)) ∧
  (v2n (W32V w32) = Some (w2n w32)) ∧
  (v2n (W64V w64) = Some (w2n w64)) ∧
  (v2n _ = None)
End

Definition interval_to_set_def:
  interval_to_set (_, start,stop) =
    { n | start ≤ n ∧ n < stop }
End

Definition interval_ok_def:
  interval_ok ((_:bool), i1, i2) ⇔
    i1 ≤ i2 ∧ i2 < 2 ** 64
End

Definition is_allocated_def:
  is_allocated b1 allocs ⇔
    interval_ok b1 ∧
    ∃b2. b2 ∈ allocs ∧ fst b1 = fst b2 ∧ interval_to_set b1 ⊆ interval_to_set b2
End

Definition is_free_def:
  is_free b1 allocs ⇔
    interval_ok b1 ∧
    ∀b2. b2 ∈ allocs ⇒ interval_to_set b1 ∩ interval_to_set b2 = ∅
End

Definition get_bytes_def:
  get_bytes h (_, start, stop) =
    map
      (λoff.
        case flookup h (A (start + off)) of
        | None => (F, 0w)
        | Some w => w)
      (count_list (stop - start))
End

Definition set_bytes_def:
  (set_bytes p [] n h = h) ∧
  (set_bytes p (b::bs) n h =
    set_bytes p bs (Suc n) (h |+ (A n, (p, b))))
End

(* Allocate a free chunk of memory, and write non-deterministic bytes into it *)
Inductive allocate:
  v2n v.value = Some m ∧
   b = (T, w2n w, w2n w + m * len) ∧
   is_free b s.allocations ∧
   length bytes = m * len
   ⇒
   allocate s v len
     (<| poison := v.poison; value := PtrV w |>,
         s with <| allocations := { b } ∪ s.allocations;
                   heap := set_bytes v.poison bytes (w2n w) s.heap |>)
End

Definition deallocate_def:
  deallocate addrs allocs h =
    let to_remove = { (T, n, stop) | A n ∈ set addrs ∧ (T, n, stop) ∈ allocs } in
      (allocs DIFF to_remove, fdiff h (image A (bigunion (image interval_to_set to_remove))))
End

(* Read len bytes from the list of bytes, and convert it into a word value,
 * little-endian encoding *)
Definition le_read_w_def:
  le_read_w len (bs : word8 list) =
    if length bs < len then
      (l2w 256 (map w2n bs), [])
    else
      (l2w 256 (map w2n (take len bs)), drop len bs)
End

(* Return len bytes that are the little-endian encoding of the argument word *)
Definition le_write_w_def:
  le_write_w len w =
    let (l : word8 list) = map n2w (w2l 256 w) in
      take len (l ++ replicate (len - length l) 0w)
End

Definition bytes_to_value_def:
  (bytes_to_value (IntT W1) (b::bs) = (W1V (w2w b), bs)) ∧
  (bytes_to_value (IntT W8) (b::bs) = (W8V b, bs)) ∧
  (bytes_to_value (IntT W32) bs = (W32V ## I) (le_read_w 4 bs)) ∧
  (bytes_to_value (IntT W64) bs = (W64V ## I) (le_read_w 8 bs)) ∧
  (bytes_to_value (PtrT _) bs = (PtrV ## I) (le_read_w 8 bs)) ∧
  (bytes_to_value (ArrT n t) bs = (AggV ## I) (read_array n t bs)) ∧
  (bytes_to_value (StrT ts) bs = (AggV ## I) (read_str ts bs)) ∧
  (read_array 0 t bs = ([], bs)) ∧
  (read_array (Suc n) t bs =
    let (v, bs) = bytes_to_value t bs in
    let (rest, bs) = read_array n t bs in
      (v::rest, bs)) ∧
  (read_str [] bs = ([], bs)) ∧
  (read_str (t::ts) bs =
    let (v, bs) = bytes_to_value t bs in
    let (rest, bs) = read_str ts bs in
      (v::rest, bs))
Termination
  WF_REL_TAC `measure (λx. case x of
                           | INL (t, bs) => ty_size t
                           | INR (INL (n, t, bs)) => n + ty_size t
                           | INR (INR (ts, bs)) => ty1_size ts)`
End

Definition value_to_bytes_def:
  (value_to_bytes (W1V w) = [w2w w]) ∧
  (value_to_bytes (W8V w) = [w]) ∧
  (value_to_bytes (W32V w) = le_write_w 4 w) ∧
  (value_to_bytes (W64V w) = le_write_w 8 w) ∧
  (value_to_bytes (PtrV n) = le_write_w 8 n) ∧
  (value_to_bytes (AggV vs) = flat (map value_to_bytes vs))
Termination
  WF_REL_TAC `measure v_size` >>
  Induct >> rw [definition "v_size_def"] >>
  TRY (first_x_assum drule) >>
  decide_tac
End

Definition do_sub_def:
  do_sub (nuw:bool) (nsw:bool) (v1:pv) (v2:pv) =
    let (diff, u_overflow, s_overflow) =
      case (v1.value, v2.value) of
      | (W1V w1, W1V w2) => (W1V ## I) (add_with_carry (w1, ¬w2, T))
      | (W8V w1, W8V w2) => (W8V ## I) (add_with_carry (w1, ¬w2, T))
      | (W32V w1, W32V w2) => (W32V ## I) (add_with_carry (w1, ¬w2, T))
      | (W64V w1, W64V w2) => (W64V ## I) (add_with_carry (w1, ¬w2, T))
    in
    let p = ((nuw ∧ u_overflow) ∨ (nsw ∧ s_overflow) ∨ v1.poison ∨ v2.poison) in
      <| poison := p; value := diff |>
End

Definition get_comp_def:
  (get_comp Eq = $=) ∧
  (get_comp Slt = $<) ∧
  (get_comp Ult = $<+)
End

Definition do_icmp_def:
  do_icmp c v1 v2 =
    <| poison := (v1.poison ∨ v2.poison);
       value := bool_to_v (
         case (v1.value, v2.value) of
         | (W1V w1, W1V w2) => (get_comp c) w1 w2
         | (W8V w1, W8V w2) => (get_comp c) w1 w2
         | (W32V w1, W32V w2) => (get_comp c) w1 w2
         | (W64V w1, W64V w2) => (get_comp c) w1 w2
         | (PtrV w1, PtrV w2) => (get_comp c) w1 w2) |>
End

Definition do_phi_def:
  do_phi from_l s (Phi id _ entries) =
    option_map (λarg. (id, eval s arg)) (flookup entries from_l)
End

Definition extract_value_def:
  (extract_value v [] = Some v) ∧
  (extract_value (AggV vs) (i::indices) =
    if i < length vs then
      extract_value (el i vs) indices
    else
      None) ∧
  (extract_value _ _ = None)
End

Definition insert_value_def:
  (insert_value _ v [] = Some v) ∧
  (insert_value (AggV vs) v (i::indices) =
    if i < length vs then
      case insert_value (el i vs) v indices of
      | None => None
      | Some v => Some (AggV (list_update v i vs))
    else
      None) ∧
  (insert_value _ _ _ = None)
End

Definition update_result_def:
  update_result x v s = s with locals := s.locals |+ (x, v)
End

Definition inc_pc_def:
  inc_pc s = s with ip := (s.ip with i := s.ip.i + 1)
End

(* NB, the semantics tracks the poison values, but not much thought has been put
 * into getting it exactly right, so we don't have much confidence that it is
 * exactly right. We also are currently ignoring the undefined value. *)
Inductive step_instr:

  (s.stack = fr::st ∧
   deallocate fr.stack_allocs s.allocations s.heap = (new_allocs, new_h)
   ⇒
   step_instr prog s
     (Ret (t, a))
     (update_result fr.result_var (eval s a)
       <| ip := fr.ret;
          globals := s.globals;
          locals := fr.saved_locals;
          stack := st;
          allocations := new_allocs;
          heap := new_h |>)) ∧

(* Do the phi assignments in parallel. The manual says "For the purposes of the
 * SSA form, the use of each incoming value is deemed to occur on the edge from
 * the corresponding predecessor block to the current block (but after any
 * definition of an 'invoke' instruction's return value on the same edge)".
 * So treat these two as equivalent
 * %r1 = phi [0, %l]
 * %r2 = phi [%r1, %l]
 * and
 * %r2 = phi [%r1, %l]
 * %r1 = phi [0, %l]
 *)
  (eval s a = <| poison := p; value := W1V tf |> ∧
   l = Some (if tf = 1w then l1 else l2) ∧
   flookup prog s.ip.f = Some d ∧
   flookup d.blocks l = Some <| h := Head phis None; body := b |> ∧
   map (do_phi l s) phis = map Some updates
   ⇒
   step_instr prog s
     (Br a l1 l2)
     (s with <| ip := <| f := s.ip.f; b := l; i := 0 |>;
                locals := s.locals |++ updates |>)) ∧

  (* TODO *)
  (step_instr prog s (Invoke r t a args l1 l2) s) ∧

  (step_instr prog s
     (Sub r nuw nsw t a1 a2)
     (inc_pc (update_result r (do_sub nuw nsw (eval s a1) (eval s a2)) s))) ∧

  (eval s a = v ∧
   map (λci. cast_num (eval_const s.globals ci)) const_indices = map Some ns ∧
   extract_value v.value ns = Some result
   ⇒
   step_instr prog s
     (Extractvalue r (t, a) const_indices)
     (inc_pc (update_result r
                <| poison := v.poison; value := result |> s))) ∧

  (eval s a1 = v1 ∧
   eval s a2 = v2 ∧
   map (λci. cast_num (eval_const s.globals ci)) const_indices = map Some ns ∧
   insert_value v1.value v2.value ns = Some result
   ⇒
   step_instr prog s
     (Insertvalue r (t1, a1) (t2, a2) const_indices)
     (inc_pc (update_result r
                <| poison := (v1.poison ∨ v2.poison); value := result |> s))) ∧

  (allocate s (eval s a1) (sizeof t) (v2, s2)
   ⇒
   step_instr prog s
     (Alloca r t (t1, a1))
     (inc_pc (update_result r v2 s2))) ∧

  (eval s a1 = <| poison := p1; value := PtrV w |> ∧
   interval = (freeable, w2n w, w2n w + sizeof t) ∧
   is_allocated interval s.allocations ∧
   pbytes = get_bytes s.heap interval
   ⇒
   step_instr prog s
     (Load r t (t1, a1))
     (inc_pc (update_result r <| poison := (T ∈ set (map fst pbytes));
                                 value := fst (bytes_to_value t (map snd pbytes)) |>
                            s))) ∧

  (eval s a2 = <| poison := p2; value := PtrV w |> ∧
   interval = (freeable, w2n w, w2n w + sizeof t) ∧
   is_allocated interval s.allocations ∧
   bytes = value_to_bytes (eval s a1).value ∧
   length bytes = sizeof t
   ⇒
   step_instr prog s
     (Store (t1, a1) (t2, a2))
     (inc_pc (s with heap := set_bytes p2 bytes (w2n w) s.heap))) ∧

  (map (eval s o snd) tindices = i1::indices ∧
   (eval s a1).value = PtrV w1 ∧
   cast_num i1.value = Some n ∧
   map (λx. cast_num x.value) indices = map Some ns ∧
   get_offset t1 ns = Some off
   ⇒
   step_instr prog s
    (Gep r ((PtrT t1), a1) tindices)
    (inc_pc (update_result r
               <| poison := (v1.poison ∨ i1.poison ∨ exists (λv. v.poison) indices);
                  value := PtrV (n2w (w2n w1 + sizeof t1 * n + off)) |>
               s))) ∧

  (eval s a1 = v1 ∧
   v1.value = PtrV w ∧
   w64_cast w t = Some int_v
   ⇒
   step_instr prog s
    (Ptrtoint r (t1, a1) t)
    (inc_pc (update_result r <| poison := v1.poison; value := int_v |> s))) ∧

  (eval s a1 = v1 ∧
   cast_w64 v1.value = Some w
   ⇒
   step_instr prog s
    (Inttoptr r (t1, a1) t)
    (inc_pc (update_result r <| poison := v1.poison; value := PtrV w |> s))) ∧

  (step_instr prog s
    (Icmp c t a1 a2)
    (inc_pc (update_result r (do_icmp c (eval s a1) (eval s a2)) s))) ∧

  (flookup prog fname = Some d
   ⇒
   step_instr prog s
     (Call r t fname targs)
     <| ip := <| f := fname; b := None; i := 0 |>;
        locals := alist_to_fmap (zip (d.params, map (eval s o snd) targs));
        globals := s.globals;
        allocations:= s.allocations;
        stack :=
          <| ret := s.ip with i := s.ip.i + 1;
             saved_locals := s.locals;
             result_var := r;
             stack_allocs := [] |> :: s.stack;
        heap := s.heap |>) ∧

  (* TODO *)
  (step_instr prog s (Cxa_allocate_exn r a) s) ∧
  (* TODO *)
  (step_instr prog s (Cxa_throw a1 a2 a3) s) ∧
  (* TODO *)
  (step_instr prog s (Cxa_begin_catch r a) s) ∧
  (* TODO *)
  (step_instr prog s (Cxa_end_catch) s) ∧
  (* TODO *)
  (step_instr prog s (Cxa_get_exception_ptr r a) s)
End

Inductive next_instr:
  flookup p s.ip.f = Some d ∧
  flookup d.blocks s.ip.b = Some b ∧
  s.ip.i < length b.body
  ⇒
  next_instr p s (el s.ip.i b.body)
End

Inductive step:
  next_instr p s i ∧
  step_instr p s i s'
  ⇒
  step p s s'
End

(* ----- Invariants on state ----- *)

(* The allocations are of intervals that don't overlap *)
Definition allocations_ok_def:
  allocations_ok s ⇔
    ∀i1 i2.
      i1 ∈ s.allocations ∧ i2 ∈ s.allocations
      ⇒
      interval_ok i1 ∧ interval_ok i2 ∧
      (interval_to_set i1 ∩ interval_to_set i2 ≠ ∅ ⇒ i1 = i2)
End

(* The heap maps exactly the address in the allocations *)
Definition heap_ok_def:
  heap_ok s ⇔
    ∀n. flookup s.heap (A n) ≠ None ⇔ ∃i. i ∈ s.allocations ∧ n ∈ interval_to_set i
End

(* All global variables are allocated in non-freeable memory *)
Definition globals_ok_def:
  globals_ok s ⇔
    ∀g n w.
      flookup s.globals g = Some (n, w)
      ⇒
      is_allocated (F, w2n w, w2n w + n) s.allocations
End

(* Instruction pointer points to an instruction *)
Definition ip_ok_def:
  ip_ok p ip ⇔
    ∃dec block. flookup p ip.f = Some dec ∧ flookup dec.blocks ip.b = Some block ∧ ip.i < length block.body
End

Definition prog_ok_def:
  prog_ok p ⇔
    ((* All blocks end with terminators *)
     ∀fname dec bname block.
       flookup p fname = Some dec ∧
       flookup dec.blocks bname = Some block
       ⇒
       block.body ≠ [] ∧ terminator (last block.body)) ∧
    ((* All functions have an entry block *)
     ∀fname dec.
       flookup p fname = Some dec ⇒ ∃block. flookup dec.blocks None = Some block) ∧
     (* There is a main function *)
     ∃dec. flookup p (Fn "main") = Some dec
End

(* All call frames have a good return address, and the stack allocations of the
 * frame are all in freeable memory *)
Definition frame_ok_def:
  frame_ok p s f ⇔
    ip_ok p f.ret ∧
    every (λn. ∃start stop. n = A start ∧ (T, start, stop) ∈ s.allocations) f.stack_allocs
End

(* The frames are all of, and no two stack allocations have the same address *)
Definition stack_ok_def:
  stack_ok p s ⇔
    every (frame_ok p s) s.stack ∧
    all_distinct (flat (map (λf. f.stack_allocs) s.stack))
End

Definition state_invariant_def:
  state_invariant p s ⇔
    ip_ok p s.ip ∧ allocations_ok s ∧ heap_ok s ∧ globals_ok s ∧ stack_ok p s
End

(* ----- Initial state ----- *)

(* The initial state contains allocations for the initialised global variables *)
Definition is_init_state_def:
  is_init_state s (global_init : glob_var |-> ty # v) ⇔
    s.ip.f = Fn "main" ∧
    s.ip.b = None ∧
    s.ip.i = 0 ∧
    s.locals = fempty ∧
    s.stack = [] ∧
    allocations_ok s ∧
    globals_ok s ∧
    heap_ok s ∧
    fdom s.globals = fdom global_init ∧
    (* The initial allocations for globals are not freeable *)
    s.allocations ⊆ { (F, start, stop) | T } ∧
    (* The heap starts with the initial values of the globals written to their
     * addresses *)
    ∀g w t v n.
      flookup s.globals g = Some (n, w) ∧ flookup global_init g = Some (t,v) ⇒
      ∃bytes.
        get_bytes s.heap (F, w2n w, w2n w + sizeof t) = map (λb. (F,b)) bytes ∧
        bytes_to_value t bytes = (v, [])
End

export_theory();
