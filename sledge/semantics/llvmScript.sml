(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* A mini-LLVM model, focussing on the semantics of the parts of the IR that
 * are relevant for the LLVM -> LLAIR translation, especially exceptions. *)

open HolKernel boolLib bossLib Parse;
open settingsTheory memory_modelTheory;

new_theory "llvm";

numLib.prefer_num ();

(* ----- Abstract syntax ----- *)

(* Only support 1, 8, 32, and 64 bit words for now *)
Datatype:
  size = W1 | W8 | W32 | W64
End

Datatype:
  ty =
  | FunT ty (ty list)
  | IntT size
  | PtrT ty
  | ArrT num ty
  | StrT (ty list)
End

Datatype:
  label = Lab string
End

Datatype:
  reg = Reg string
End

Datatype:
  glob_var = Glob_var string
End

Datatype:
  fun_name = Fn string
End

Datatype:
  const =
  | IntC size int
  | StrC ((ty # const) list)
  | ArrC ((ty # const) list)
  | GepC ty const (ty # const) ((ty # const) list)
  | GlobalC glob_var
  | UndefC
End

Datatype:
  arg = Constant const | Variable reg
End

Type targ = ``:ty # arg``

Datatype:
  cond = Eq | Ult | Slt
End

Datatype:
  instr =
  (* Terminators *)
  | Ret targ
  | Br arg label label
  | Invoke reg ty arg (targ list) label label
  | Unreachable
  (* Non-terminators *)
  | Sub reg bool bool ty arg arg
  | Extractvalue reg targ (const list)
  | Insertvalue reg targ targ (const list)
  | Alloca reg ty targ
  | Load reg ty targ
  | Store targ targ
  | Gep reg ty targ (targ list)
  | Ptrtoint reg targ ty
  | Inttoptr reg targ ty
  | Icmp reg cond ty arg arg
  | Call reg ty fun_name (targ list)
  (* C++ runtime functions *)
  | Cxa_allocate_exn reg arg
  | Cxa_throw arg arg arg
  | Cxa_begin_catch reg arg
  | Cxa_end_catch
  | Cxa_get_exception_ptr reg arg
End

Datatype:
  phi = Phi reg ty ((label option, arg) alist)
End

Datatype:
  clause = Catch targ
End

Datatype:
  landingpad = Landingpad ty bool (clause list)
End

Datatype:
  blockHeader =
  | Entry
  | Head (phi list) (landingpad option)
End

Datatype:
  block = <| h : blockHeader; body : instr list |>
End

Datatype:
  def =
    <| r : ty;
       params : (ty # reg) list;
       (* None -> entry block, and Some name -> non-entry block *)
       blocks : (label option, block) alist |>
End

Type prog = ``:(fun_name, def) alist``

Definition terminator_def:
  (terminator (Ret _) ⇔ T) ∧
  (terminator (Br _ _ _) ⇔ T) ∧
  (terminator (Invoke _ _ _ _ _ _) ⇔ T) ∧
  (terminator Unreachable ⇔ T) ∧
  (terminator _ ⇔ F)
End

(* ----- Semantic states ----- *)

Datatype:
  flat_v =
  | W1V word1
  | W8V word8
  | W32V word32
  | W64V word64
  | PtrV word64
  | UndefV
End

Type v = ``:flat_v reg_v``

Datatype:
  pv = <| poison : bool; value : v |>
End

Datatype:
  pc = <| f : fun_name; b : label option; i : num |>
End

Datatype:
  frame = <| ret : pc; saved_locals : reg |-> pv; result_var : reg; stack_allocs : addr list |>
End

Datatype:
  state =
    <| ip : pc;
       (* Keep the size of the global with its memory address *)
       globals : glob_var |-> (num # word64);
       locals : reg |-> pv;
       stack : frame list;
       heap : bool heap |>
End

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
  (value_type (IntT W1) (FlatV (W1V w1))) ∧
  (value_type (IntT W8) (FlatV (W8V w8))) ∧
  (value_type (IntT W32) (FlatV (W32V w32))) ∧
  (value_type (IntT W64) (FlatV (W64V w64))) ∧
  (value_type (PtrT _) (FlatV (PtrV w64))) ∧
  (every (value_type t) vs ∧ length vs = n ∧ first_class_type t
   ⇒
   value_type (ArrT n t) (AggV vs)) ∧
  (list_rel value_type ts vs
   ⇒
   value_type (StrT ts) (AggV vs))
End

Definition extract_type_def:
  (extract_type t [] = Some t) ∧
  (extract_type (ArrT n t) (i::idx) =
    if i < n then
      extract_type t idx
    else
      None) ∧
  (extract_type (StrT ts) (i::idx) =
    if i < length ts then
      extract_type (el i ts) idx
    else
      None) ∧
  (extract_type _ _ = None)
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

(* ----- Semantic transitions ----- *)

Definition w64_cast_def:
  (w64_cast w (IntT W1) = Some (FlatV (W1V (w2w w)))) ∧
  (w64_cast w (IntT W8) = Some (FlatV (W8V (w2w w)))) ∧
  (w64_cast w (IntT W32) = Some (FlatV (W32V (w2w w)))) ∧
  (w64_cast w (IntT W64) = Some (FlatV (W64V w))) ∧
  (w64_cast _ _ = None)
End

Definition cast_w64_def:
  (cast_w64 (FlatV (W1V w)) = Some (w2w w)) ∧
  (cast_w64 (FlatV (W8V w)) = Some (w2w w)) ∧
  (cast_w64 (FlatV (W32V w)) = Some (w2w w)) ∧
  (cast_w64 (FlatV (W64V w)) = Some w) ∧
  (cast_w64 _ = None)
End

Definition cast_num_def:
  cast_num v = option_map w2n (cast_w64 v)
End

Definition bool_to_v_def:
  bool_to_v b = if b then FlatV (W1V 1w) else FlatV (W1V 0w)
End

Definition eval_const_def:
  (eval_const g (IntC W1 i) = FlatV (W1V (i2w i))) ∧
  (eval_const g (IntC W8 i) = FlatV (W8V (i2w i))) ∧
  (eval_const g (IntC W32 i) = FlatV (W32V (i2w i))) ∧
  (eval_const g (IntC W64 i) = FlatV (W64V (i2w i))) ∧
  (eval_const g (StrC tconsts) = AggV (map (eval_const g) (map snd tconsts))) ∧
  (eval_const g (ArrC tconsts) = AggV (map (eval_const g) (map snd tconsts))) ∧
  (eval_const g (GepC ty ptr (t, idx) indices) =
    case (eval_const g ptr, cast_num (eval_const g idx)) of
    | (FlatV (PtrV w), Some n) =>
      let ns = map (λ(t,ci). case cast_num (eval_const g ci) of None => 0 | Some n => n) indices in
        (case get_offset ty ns of
         | None => FlatV UndefV
         | Some off => FlatV (PtrV (n2w (w2n w + sizeof ty * n + off))))
    | _ => FlatV UndefV) ∧
  (eval_const g (GlobalC var) =
    case flookup g var of
    | None => FlatV (PtrV 0w)
    | Some (s,w) => FlatV (PtrV w)) ∧
  (eval_const g UndefC = FlatV UndefV)
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
    | None => <| poison := F; value := FlatV (W1V 0w) |>
    | Some v => v) ∧
  (eval s (Constant c) = <| poison := F; value := eval_const s.globals c |>)
End

Definition v2n_def:
  (v2n (FlatV (W1V b)) = Some (if T then 1 else 0)) ∧
  (v2n (FlatV (W8V w8)) = Some (w2n w8)) ∧
  (v2n (FlatV (W32V w32)) = Some (w2n w32)) ∧
  (v2n (FlatV (W64V w64)) = Some (w2n w64)) ∧
  (v2n _ = None)
End

Definition type_to_shape_def:
  (type_to_shape (IntT s) = Flat (sizeof (IntT s)) (IntT s)) ∧
  (type_to_shape (PtrT t) = Flat (sizeof (PtrT t)) (PtrT t)) ∧
  (type_to_shape (ArrT n t) = Array (type_to_shape t) n) ∧
  (type_to_shape (StrT ts) = Tuple (map type_to_shape ts))
Termination
  WF_REL_TAC `measure ty_size` >> rw [] >>
  Induct_on `ts` >> rw [definition "ty_size_def"] >>
  res_tac >> simp []
End

Definition convert_value_def:
  (convert_value (IntT W1) w = W1V (w2w w)) ∧
  (convert_value (IntT W8) w = W8V (w2w w)) ∧
  (convert_value (IntT W32) w = W32V (w2w w)) ∧
  (convert_value (IntT W64) w = W64V w) ∧
  (convert_value (PtrT _) w = PtrV w)
End

Definition bytes_to_llvm_value_def:
  bytes_to_llvm_value t bs =
    (bytes_to_value (λn t w. convert_value t w) (type_to_shape t) bs)
End

Definition unconvert_value_def:
  (unconvert_value (W1V w) = (1, w2w w)) ∧
  (unconvert_value (W8V w) = (1, w2w w)) ∧
  (unconvert_value (W32V w) = (4, w2w w)) ∧
  (unconvert_value (W64V w) = (8, w)) ∧
  (unconvert_value (PtrV w) = (8, w))
End

Definition llvm_value_to_bytes_def:
  llvm_value_to_bytes v =
    value_to_bytes unconvert_value v
End

Definition do_sub_def:
  do_sub (nuw:bool) (nsw:bool) (v1:pv) (v2:pv) =
    let (diff, u_overflow, s_overflow) =
      case (v1.value, v2.value) of
      | (FlatV (W1V w1), FlatV (W1V w2)) => (FlatV o W1V ## I) (add_with_carry (w1, ¬w2, T))
      | (FlatV (W8V w1), FlatV (W8V w2)) => (FlatV o W8V ## I) (add_with_carry (w1, ¬w2, T))
      | (FlatV (W32V w1), FlatV (W32V w2)) => (FlatV o W32V ## I) (add_with_carry (w1, ¬w2, T))
      | (FlatV (W64V w1), FlatV (W64V w2)) => (FlatV o W64V ## I) (add_with_carry (w1, ¬w2, T))
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
         | (FlatV (W1V w1), FlatV (W1V w2)) => (get_comp c) w1 w2
         | (FlatV (W8V w1), FlatV (W8V w2)) => (get_comp c) w1 w2
         | (FlatV (W32V w1), FlatV (W32V w2)) => (get_comp c) w1 w2
         | (FlatV (W64V w1), FlatV (W64V w2)) => (get_comp c) w1 w2
         | (FlatV (PtrV w1), FlatV (PtrV w2)) => (get_comp c) w1 w2) |>
End

Definition do_phi_def:
  do_phi from_l s (Phi id _ entries) =
    option_map (λarg. (id, eval s arg)) (alookup entries from_l)
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
   deallocate fr.stack_allocs s.heap = new_h
   ⇒
   step_instr prog s
     (Ret (t, a))
     (update_result fr.result_var (eval s a)
       <| ip := fr.ret;
          globals := s.globals;
          locals := fr.saved_locals;
          stack := st;
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
  (eval s a = <| poison := p; value := FlatV (W1V tf) |> ∧
   l = Some (if tf = 1w then l1 else l2) ∧
   alookup prog s.ip.f = Some d ∧
   alookup d.blocks l = Some <| h := Head phis None; body := b |> ∧
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

  (eval s a1 = v ∧
   v2n v.value = Some n ∧
   allocate s.heap (n * sizeof t) v.poison (v2, new_h)
   ⇒
   step_instr prog s
     (Alloca r t (t1, a1))
     (inc_pc (update_result r v2 (s with heap := new_h)))) ∧

  (eval s a1 = <| poison := p1; value := FlatV (PtrV w) |> ∧
   interval = Interval freeable (w2n w) (w2n w + sizeof t) ∧
   is_allocated interval s.heap ∧
   pbytes = get_bytes s.heap interval
   ⇒
   step_instr prog s
     (Load r t (t1, a1))
     (inc_pc (update_result r <| poison := (T ∈ set (map fst pbytes));
                                 value := fst (bytes_to_llvm_value t (map snd pbytes)) |>
                            s))) ∧

  (eval s a2 = <| poison := p2; value := FlatV (PtrV w) |> ∧
   interval = Interval freeable (w2n w) (w2n w + sizeof t) ∧
   is_allocated interval s.heap ∧
   bytes = llvm_value_to_bytes (eval s a1).value ∧
   length bytes = sizeof t
   ⇒
   step_instr prog s
     (Store (t1, a1) (t2, a2))
     (inc_pc (s with heap := set_bytes p2 bytes (w2n w) s.heap))) ∧

  (map (eval s o snd) tindices = i1::indices ∧
   (eval s a1).value = FlatV (PtrV w1) ∧
   cast_num i1.value = Some n ∧
   map (λx. cast_num x.value) indices = map Some ns ∧
   get_offset t1 ns = Some off
   ⇒
   step_instr prog s
    (Gep r t ((PtrT t1), a1) tindices)
    (inc_pc (update_result r
               <| poison := (v1.poison ∨ i1.poison ∨ exists (λv. v.poison) indices);
                  value := FlatV (PtrV (n2w (w2n w1 + sizeof t1 * n + off))) |>
               s))) ∧

  (eval s a1 = v1 ∧
   v1.value = FlatV (PtrV w) ∧
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
    (inc_pc (update_result r <| poison := v1.poison; value := FlatV (PtrV w) |> s))) ∧

  (step_instr prog s
    (Icmp r c t a1 a2)
    (inc_pc (update_result r (do_icmp c (eval s a1) (eval s a2)) s))) ∧

  (alookup prog fname = Some d
   ⇒
   step_instr prog s
     (Call r t fname targs)
     <| ip := <| f := fname; b := None; i := 0 |>;
        locals := alist_to_fmap (zip (map snd d.params, map (eval s o snd) targs));
        globals := s.globals;
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
  alookup p s.ip.f = Some d ∧
  alookup d.blocks s.ip.b = Some b ∧
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

(* All global variables are allocated in non-freeable memory *)
Definition globals_ok_def:
  globals_ok s ⇔
    ∀g n w.
      flookup s.globals g = Some (n, w)
      ⇒
      is_allocated (Interval F (w2n w) (w2n w + n)) s.heap
End

(* Instruction pointer points to an instruction *)
Definition ip_ok_def:
  ip_ok p ip ⇔
    ∃dec block. alookup p ip.f = Some dec ∧ alookup dec.blocks ip.b = Some block ∧ ip.i < length block.body
End

Definition prog_ok_def:
  prog_ok p ⇔
    ((* All blocks end with terminators *)
     ∀fname dec bname block.
       alookup p fname = Some dec ∧
       alookup dec.blocks bname = Some block
       ⇒
       block.body ≠ [] ∧ terminator (last block.body)) ∧
    ((* All functions have an entry block *)
     ∀fname dec.
       alookup p fname = Some dec ⇒ ∃block. alookup dec.blocks None = Some block) ∧
     (* There is a main function *)
     ∃dec. alookup p (Fn "main") = Some dec
End

(* All call frames have a good return address, and the stack allocations of the
 * frame are all in freeable memory *)
Definition frame_ok_def:
  frame_ok p s f ⇔
    ip_ok p f.ret ∧
    every (λn. ∃start stop. n = A start ∧ Interval T start stop ∈ s.heap.allocations) f.stack_allocs
End

(* The frames are all of, and no two stack allocations have the same address *)
Definition stack_ok_def:
  stack_ok p s ⇔
    every (frame_ok p s) s.stack ∧
    all_distinct (flat (map (λf. f.stack_allocs) s.stack))
End

Definition state_invariant_def:
  state_invariant p s ⇔
    ip_ok p s.ip ∧ heap_ok s.heap ∧ globals_ok s ∧ stack_ok p s
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
    globals_ok s ∧
    heap_ok s.heap ∧
    fdom s.globals = fdom global_init ∧
    s.heap.valid_addresses =  { A n | n < dimword (:64) } ∧
    (* The initial allocations for globals are not freeable *)
    s.heap.allocations ⊆ { Interval F start stop | T } ∧
    (* The heap starts with the initial values of the globals written to their
     * addresses *)
    ∀g w t v n.
      flookup s.globals g = Some (n, w) ∧ flookup global_init g = Some (t,v) ⇒
      ∃bytes.
        get_bytes s.heap (Interval F (w2n w) (w2n w + sizeof t)) = map (λb. (F,b)) bytes ∧
        bytes_to_llvm_value t bytes = (v, [])
End

export_theory();
