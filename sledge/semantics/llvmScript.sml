(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* A mini-LLVM model, focussing on the semantics of the parts of the IR that
 * are relevant for the LLVM -> LLAIR translation, especially exceptions. *)

open HolKernel boolLib bossLib Parse;
open llistTheory pathTheory;
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
  phi = Phi reg ty ((label option, arg) alist)
End

Datatype:
  cast_op = Trunc | Zext | Sext | Ptrtoint | Inttoptr
End

(*
 * The Exit instruction below models a system/libc call to exit the program. The
 * semantics needs some way to tell the difference between normally terminated
 * programs and stuck states, and this lets it do that. From a C++ perspective,
 * a program could call this directly, in which case it's good to model, or it
 * might simply return from main and then the code in libc that called main will
 * call exit. However, adding special handling for main in the semantics will
 * cruft things up a bit, and it's not very satisfying, because it's not really
 * an LLVM concept.
 *)

Datatype:
  instr =
  (* Terminators *)
  | Ret targ
  | Br arg label label
  | Invoke reg ty arg (targ list) label label
  | Unreachable
  | Exit arg
  (* Non-terminators *)
  | Sub reg bool bool ty arg arg
  | Extractvalue reg targ (const list)
  | Insertvalue reg targ targ (const list)
  | Alloca reg ty targ
  | Load reg ty targ
  | Store targ targ
  | Gep reg ty targ (targ list)
  | Cast reg cast_op targ ty
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
  (terminator (Exit _) ⇔ T) ∧
  (terminator (Cxa_throw _ _ _) ⇔ T) ∧
  (terminator _ ⇔ F)
End

(* ----- Semantic states ----- *)

Definition pointer_size_def:
  pointer_size = 8
End

Datatype:
  flat_v =
  | W1V word1
  | W8V word8
  | W32V word32
  | W64V word64
  (* LLVM guarantees that 64 bits is enough to hold a pointer *)
  | PtrV word64
  | UndefV
End

Type v = ``:flat_v reg_v``

Datatype:
  pv = <| poison : bool; value : v |>
End

(* Instruction pointer into a block. Phi_ip indicates to do the phi instruction,
 * coming from the given label. Offset points to a normal (non-phi) instruction.
 * *)
Datatype:
  bip =
  | Phi_ip (label option)
  | Offset num
End

Datatype:
  pc = <| f : fun_name; b : label option; i : bip |>
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
       heap : bool heap;
       status : trace_type |>
End

(* ----- Things about types ----- *)

(* Given a number n that fits into pointer_size number of bytes, create the
 * pointer value. Since the pointer is represented as a 64-bit word,
 * pointer_size must be 8 or less, which LLVM guarantees *)
Definition mk_ptr_def:
  mk_ptr n =
    if n < 256 ** pointer_size then Some (FlatV (PtrV (n2w n))) else None
End

(* How many bytes a value of the given type occupies *)
Definition sizeof_def:
  (sizeof (IntT W1) = 1) ∧
  (sizeof (IntT W8) = 1) ∧
  (sizeof (IntT W32) = 4) ∧
  (sizeof (IntT W64) = 8) ∧
  (sizeof (PtrT _) = pointer_size) ∧
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

(* Are the indices all in bounds? *)
Definition indices_ok_def:
  (indices_ok _ [] ⇔ T) ∧
  (indices_ok (ArrT n t) (i::indices) ⇔
    i < n ∧ indices_ok t indices) ∧
  (indices_ok (StrT ts) (i::indices) ⇔
    i < length ts ∧ indices_ok (el i ts) indices) ∧
  (indices_ok _ _ ⇔ F)
End

(* Which values have which types *)
Inductive value_type:
  (∀w1. value_type (IntT W1) (FlatV (W1V w1))) ∧
  (∀w8. value_type (IntT W8) (FlatV (W8V w8))) ∧
  (∀w32. value_type (IntT W32) (FlatV (W32V w32))) ∧
  (∀w64. value_type (IntT W64) (FlatV (W64V w64))) ∧
  (∀t ptr. value_type (PtrT t) (FlatV (PtrV ptr))) ∧
  (∀t vs n.
   every (value_type t) vs ∧ length vs = n ∧ first_class_type t
   ⇒
   value_type (ArrT n t) (AggV vs)) ∧
  (∀ts vs.
   list_rel value_type ts vs
   ⇒
   value_type (StrT ts) (AggV vs))
End

(* Get the component of a type referred by the indices *)
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

(* Put a 64-bit word into a smaller value, truncating if necessary *)
Definition w64_cast_def:
  (w64_cast w (IntT W1) = Some (FlatV (W1V (w2w w)))) ∧
  (w64_cast w (IntT W8) = Some (FlatV (W8V (w2w w)))) ∧
  (w64_cast w (IntT W32) = Some (FlatV (W32V (w2w w)))) ∧
  (w64_cast w (IntT W64) = Some (FlatV (W64V w))) ∧
  (w64_cast _ _ = None)
End

Definition bool_to_v_def:
  bool_to_v b = if b then FlatV (W1V 1w) else FlatV (W1V 0w)
End

(* Convert a word value into an integer, interpreting the word in 2's complement *)
Definition signed_v_to_int_def:
  (signed_v_to_int (FlatV (W1V w)) = Some (w2i w)) ∧
  (signed_v_to_int (FlatV (W8V w)) = Some (w2i w)) ∧
  (signed_v_to_int (FlatV (W32V w)) = Some (w2i w)) ∧
  (signed_v_to_int (FlatV (W64V w)) = Some (w2i w)) ∧
  (signed_v_to_int _ = None)
End

(* Convert a non-negative word value (interpreted as 2's complement) into a natural number *)
Definition signed_v_to_num_def:
  signed_v_to_num v =
    option_join
      (option_map (\i. if i < 0 then None else Some (Num i)) (signed_v_to_int v))
End

(* Convert a word value into a natural number, interpreting the word as unsigned *)
Definition unsigned_v_to_num_def:
  (unsigned_v_to_num (FlatV (W1V w)) = Some (w2n w)) ∧
  (unsigned_v_to_num (FlatV (W8V w)) = Some (w2n w)) ∧
  (unsigned_v_to_num (FlatV (W32V w)) = Some (w2n w)) ∧
  (unsigned_v_to_num (FlatV (W64V w)) = Some (w2n w)) ∧
  (unsigned_v_to_num _ = None)
End

Definition eval_const_def:
  (eval_const g (IntC W1 i) = FlatV (W1V (i2w i))) ∧
  (eval_const g (IntC W8 i) = FlatV (W8V (i2w i))) ∧
  (eval_const g (IntC W32 i) = FlatV (W32V (i2w i))) ∧
  (eval_const g (IntC W64 i) = FlatV (W64V (i2w i))) ∧
  (eval_const g (StrC tconsts) = AggV (map (eval_const g) (map snd tconsts))) ∧
  (eval_const g (ArrC tconsts) = AggV (map (eval_const g) (map snd tconsts))) ∧
  (eval_const g (GepC ty ptr (t, idx) indices) =
    case (eval_const g ptr, signed_v_to_num (eval_const g idx)) of
    | (FlatV (PtrV ptr), Some n) =>
      let ns = map (λ(t,ci). case signed_v_to_num (eval_const g ci) of None => 0 | Some n => n) indices in
        (case get_offset ty ns of
         | None => FlatV UndefV
         | Some off => FlatV (PtrV (n2w ((w2n ptr) + (sizeof ty) * n + off))))
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
  (eval s (Variable v) = flookup s.locals v) ∧
  (eval s (Constant c) = Some <| poison := F; value := eval_const s.globals c |>)
End

(* BEGIN Functions to interface to the generic memory model *)
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
  (convert_value (IntT W1) n = W1V (if n = 0 then 0w else 1w)) ∧
  (convert_value (IntT W8) n = W8V (n2w n)) ∧
  (convert_value (IntT W32) n = W32V (n2w n)) ∧
  (convert_value (IntT W64) n = W64V (n2w n)) ∧
  (convert_value (PtrT _) n = PtrV (n2w n))
End

Definition bytes_to_llvm_value_def:
  bytes_to_llvm_value t bs =
    (bytes_to_value (λn t w. convert_value t w) (type_to_shape t) bs)
End

Definition unconvert_value_def:
  (unconvert_value (W1V w) = (1, w2n w)) ∧
  (unconvert_value (W8V w) = (1, w2n w)) ∧
  (unconvert_value (W32V w) = (4, w2n w)) ∧
  (unconvert_value (W64V w) = (8, w2n w)) ∧
  (unconvert_value (PtrV w) = (pointer_size, w2n w))
End

Definition llvm_value_to_bytes_def:
  llvm_value_to_bytes v =
    value_to_bytes unconvert_value v
End

(* END Functions to interface to the generic memory model *)

Definition do_sub_def:
  do_sub (nuw:bool) (nsw:bool) (v1:pv) (v2:pv) t =
    let diff =
      case (v1.value, v2.value, t) of
      | (FlatV (W1V w1), FlatV (W1V w2), IntT W1) =>
        Some ((FlatV o W1V ## I) (add_with_carry (w1, ¬w2, T)))
      | (FlatV (W8V w1), FlatV (W8V w2), IntT W8) =>
        Some ((FlatV o W8V ## I) (add_with_carry (w1, ¬w2, T)))
      | (FlatV (W32V w1), FlatV (W32V w2), IntT W32) =>
        Some ((FlatV o W32V ## I) (add_with_carry (w1, ¬w2, T)))
      | (FlatV (W64V w1), FlatV (W64V w2), IntT W64) =>
        Some ((FlatV o W64V ## I) (add_with_carry (w1, ¬w2, T)))
      | _ => None
    in
      option_map
        (\(diff, u_overflow, s_overflow).
           let p = ((nuw ∧ u_overflow) ∨ (nsw ∧ s_overflow) ∨ v1.poison ∨ v2.poison) in
             <| poison := p; value := diff |>)
        diff
End

Definition get_comp_def:
  (get_comp Eq = $=) ∧
  (get_comp Slt = $<) ∧
  (get_comp Ult = $<+)
End

Definition do_cast_def:
  (do_cast Trunc v t =
    option_join (option_map (λw. w64_cast (n2w w) t) (unsigned_v_to_num v))) ∧
  (do_cast Zext v t =
    option_join (option_map (λw. w64_cast (n2w w) t) (unsigned_v_to_num v))) ∧
  (do_cast Sext v t =
    option_join (option_map (λi. w64_cast (i2w i) t) (signed_v_to_int v))) ∧
  (do_cast Ptrtoint v t =
    case v of
    | FlatV (PtrV w) => w64_cast w t
    | _ => None) ∧
  (do_cast Inttoptr v t =
    option_join (option_map mk_ptr (unsigned_v_to_num v)))
End

(*
  EVAL ``do_cast Trunc (FlatV (W32V 4294967295w)) (IntT W8) = Some (FlatV (W8V 255w))``
  EVAL ``do_cast Trunc (FlatV (W32V 511w)) (IntT W8) = Some (FlatV (W8V 255w))``
  EVAL ``do_cast Trunc (FlatV (W32V 255w)) (IntT W8) = Some (FlatV (W8V 255w))``
  EVAL ``do_cast Trunc (FlatV (W32V 4294967166w)) (IntT W8) = Some (FlatV (W8V 126w))``
  EVAL ``do_cast Trunc (FlatV (W32V 257w)) (IntT W8) = Some (FlatV (W8V 1w))``
  EVAL ``do_cast Zext (FlatV (W8V 127w)) (IntT W32) = Some (FlatV (W32V 127w))``
  EVAL ``do_cast Zext (FlatV (W8V 129w)) (IntT W32) = Some (FlatV (W32V 129w))``
  EVAL ``do_cast Sext (FlatV (W8V 127w)) (IntT W32) = Some (FlatV (W32V 127w))``
  EVAL ``do_cast Sext (FlatV (W8V 129w)) (IntT W32) = Some (FlatV (W32V (n2w (2 ** 32 - 1 - 255 + 129))))``
  *)


Definition do_icmp_def:
  do_icmp c v1 v2 =
    option_map (\b. <| poison := (v1.poison ∨ v2.poison); value := bool_to_v b |>)
      (case (v1.value, v2.value) of
       | (FlatV (W1V w1), FlatV (W1V w2)) => Some ((get_comp c) w1 w2)
       | (FlatV (W8V w1), FlatV (W8V w2)) => Some ((get_comp c) w1 w2)
       | (FlatV (W32V w1), FlatV (W32V w2)) => Some ((get_comp c) w1 w2)
       | (FlatV (W64V w1), FlatV (W64V w2)) => Some ((get_comp c) w1 w2)
       | (FlatV (PtrV w1), FlatV (PtrV w2)) => Some ((get_comp c) w1 w2)
       | _ => None)
End

Definition do_phi_def:
  do_phi from_l s (Phi id _ entries) =
    option_join (option_map (λarg. option_map (\v. (id, v)) (eval s arg)) (alookup entries from_l))
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

Definition inc_bip_def:
  (inc_bip (Phi_ip _) = Offset 0) ∧
  (inc_bip (Offset i) = Offset (i + 1))
End

Definition inc_pc_def:
  inc_pc s = s with ip := (s.ip with i := inc_bip s.ip.i)
End

Inductive get_obs:
  (∀s w bytes x n. flookup s.globals x = Some (n, w) ⇒ get_obs s w bytes (W x bytes)) ∧
  (∀s w bytes. (∀n. (n, w) ∉ FRANGE s.globals) ⇒ get_obs s w bytes Tau)
End

(* NB, the semantics tracks the poison values, but not much thought has been put
 * into getting it exactly right, so we don't have much confidence that it is
 * exactly right. We also are currently ignoring the undefined value. *)
Inductive step_instr:

  (∀prog s t a fr v st new_h.
   s.stack = fr::st ∧
   deallocate fr.stack_allocs s.heap = new_h ∧
   eval s a = Some v
   ⇒
   step_instr prog s
     (Ret (t, a)) Tau
     (update_result fr.result_var v
       <| ip := fr.ret;
          globals := s.globals;
          locals := fr.saved_locals;
          stack := st;
          heap := new_h;
          status := s.status |>)) ∧

  (∀prog s a l1 l2 tf l p.
   eval s a = Some <| poison := p; value := FlatV (W1V tf) |> ∧
   l = Some (if tf = 0w then l2 else l1)
   ⇒
   step_instr prog s
     (Br a l1 l2) Tau
     (s with ip := <| f := s.ip.f; b := l; i := Phi_ip s.ip.b |>)) ∧

  (* TODO *)
  (∀prog s r t a args l1 l2. step_instr prog s (Invoke r t a args l1 l2) Tau s) ∧

  (∀prog s a exit_code v1.
   eval s a = Some v1 ∧
   signed_v_to_int v1.value = Some exit_code
   ⇒
   step_instr prog s
     (Exit a) (Exit exit_code)
     (s with status := Complete exit_code)) ∧

  (∀prog s r nuw nsw t a1 a2 v3 v1 v2.
   eval s a1 = Some v1 ∧
   eval s a2 = Some v2 ∧
   do_sub nuw nsw v1 v2 t = Some v3
   ⇒
   step_instr prog s
     (Sub r nuw nsw t a1 a2) Tau
     (inc_pc (update_result r v3 s))) ∧

  (∀prog s r t a const_indices v ns result.
   eval s a = Some v ∧
   (* The manual implies (but does not explicitly state) that the indices are
    * interpreted as signed numbers *)
   map (λci. signed_v_to_num (eval_const s.globals ci)) const_indices = map Some ns ∧
   extract_value v.value ns = Some result
   ⇒
   step_instr prog s
     (Extractvalue r (t, a) const_indices) Tau
     (inc_pc (update_result r <| poison := v.poison; value := result |> s))) ∧

  (∀prog s r t1 a1 t2 a2 const_indices result v1 v2 ns.
   eval s a1 = Some v1 ∧
   eval s a2 = Some v2 ∧
   (* The manual implies (but does not explicitly state) that the indices are
    * interpreted as signed numbers *)
   map (λci. signed_v_to_num (eval_const s.globals ci)) const_indices = map Some ns ∧
   insert_value v1.value v2.value ns = Some result
   ⇒
   step_instr prog s
     (Insertvalue r (t1, a1) (t2, a2) const_indices) Tau
     (inc_pc (update_result r
                <| poison := (v1.poison ∨ v2.poison); value := result |> s))) ∧

  (∀prog s r t t1 a1 ptr new_h v n n2.
   eval s a1 = Some v ∧
   (* TODO Question is the number to allocate interpreted as a signed or
    * unsigned quantity. E.g., if we allocate i8 0xFF does that do 255 or -1? *)
   signed_v_to_num v.value = Some n ∧
   allocate s.heap (n * sizeof t) v.poison (n2, new_h) ∧
   mk_ptr n2 = Some ptr
   ⇒
   step_instr prog s
     (Alloca r t (t1, a1)) Tau
     (inc_pc (update_result r <| poison := v.poison; value := ptr |>
                (s with heap := new_h)))) ∧

  (∀prog s r t t1 a1 pbytes w interval freeable p1.
   eval s a1 = Some <| poison := p1; value := FlatV (PtrV w) |> ∧
   interval = Interval freeable (w2n w) (w2n w + sizeof t) ∧
   is_allocated interval s.heap ∧
   pbytes = get_bytes s.heap interval ∧
   first_class_type t
   ⇒
   step_instr prog s
     (Load r t (t1, a1)) Tau
     (inc_pc (update_result r <| poison := (T ∈ set (map fst pbytes));
                                 value := fst (bytes_to_llvm_value t (map snd pbytes)) |>
                            s))) ∧

  (∀prog s t1 a1 t2 a2 obs p2 bytes w v1 freeable interval.
   eval s a2 = Some <| poison := p2; value := FlatV (PtrV w) |> ∧
   eval s a1 = Some v1 ∧
   interval = Interval freeable (w2n w) (w2n w + sizeof t1) ∧
   is_allocated interval s.heap ∧
   bytes = llvm_value_to_bytes v1.value ∧
   length bytes = sizeof t1 ∧
   get_obs s w bytes obs
   ⇒
   step_instr prog s
     (Store (t1, a1) (t2, a2)) obs
     (inc_pc (s with heap := set_bytes p2 bytes (w2n w) s.heap))) ∧

  (∀prog s r t t1 a1 tindices v1 i1 indices v w1 i is off ptr.
   map (eval s o snd) tindices = map Some (i1::indices) ∧
   eval s a1 = Some v ∧
   v.value = FlatV (PtrV w1) ∧
   (* The manual states that the indices are interpreted as signed numbers *)
   signed_v_to_num i1.value = Some i ∧
   map (λx. signed_v_to_num x.value) indices = map Some is ∧
   get_offset t1 is = Some off ∧
   mk_ptr (w2n w1 + sizeof t1 * i + off) = Some ptr
   ⇒
   step_instr prog s
    (Gep r t ((PtrT t1), a1) tindices) Tau
    (inc_pc (update_result r
               <| poison := (v1.poison ∨ i1.poison ∨ exists (λv. v.poison) indices);
                  value := ptr |>
               s))) ∧

  (∀prog s cop r t1 a1 t v1 v2.
   eval s a1 = Some v1 ∧
   do_cast cop v1.value t = Some v2
   ⇒
   step_instr prog s
    (Cast r cop (t1, a1) t) Tau
    (inc_pc (update_result r <| poison := v1.poison; value := v2 |> s))) ∧

  (∀prog s r c t a1 a2 v3 v1 v2.
   eval s a1 = Some v1 ∧
   eval s a2 = Some v2 ∧
   do_icmp c v1 v2 = Some v3
   ⇒
   step_instr prog s
    (Icmp r c t a1 a2) Tau
    (inc_pc (update_result r v3 s))) ∧

  (∀prog s r t fname targs d vs.
   alookup prog fname = Some d ∧
   map (eval s o snd) targs = map Some vs
   ⇒
   step_instr prog s
     (Call r t fname targs) Tau
     (* Jump to the entry block of the function which has no phis *)
     <| ip := <| f := fname; b := None; i := Offset 0 |>;
        locals := alist_to_fmap (zip (map snd d.params, vs));
        globals := s.globals;
        stack :=
          <| ret := s.ip with i := inc_bip s.ip.i;
             saved_locals := s.locals;
             result_var := r;
             stack_allocs := [] |> :: s.stack;
        heap := s.heap;
        status := s.status |>)(* ∧

  (* TODO *)
  (step_instr prog s (Cxa_allocate_exn r a) Tau s) ∧
  (* TODO *)
  (step_instr prog s (Cxa_throw a1 a2 a3) Tau s) ∧
  (* TODO *)
  (step_instr prog s (Cxa_begin_catch r a) Tau s) ∧
  (* TODO *)
  (step_instr prog s (Cxa_end_catch) Tau s) ∧
  (* TODO *)
  (step_instr prog s (Cxa_get_exception_ptr r a) Tau s)
  *)
End

Inductive get_instr:
 (∀prog ip idx b d.
  alookup prog ip.f = Some d ∧
  alookup d.blocks ip.b = Some b ∧
  ip.i = Offset idx ∧
  idx < length b.body
  ⇒
  get_instr prog ip (Inl (el idx b.body))) ∧
 (∀prog ip from_l phis d b landing.
  alookup prog ip.f = Some d ∧
  alookup d.blocks ip.b = Some b ∧
  ip.i = Phi_ip from_l ∧
  b.h = Head phis landing
  ⇒
  get_instr prog ip (Inr (from_l, phis)))
End

Inductive step:
 (∀p s l s' i.
  get_instr p s.ip (Inl i) ∧
  step_instr p s i l s'
  ⇒
  step p s l s') ∧

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
 (∀p s updates from_l phis.
  get_instr p s.ip (Inr (from_l, phis)) ∧
  map (do_phi from_l s) phis = map Some updates
  ⇒
  step p s Tau (inc_pc (s with locals := s.locals |++ updates)))
End

Inductive sem_step:
  (∀p s1 l s2.
   step p s1 l s2 ∧
   s1.status = Partial
   ⇒
   sem_step p s1 l s2) ∧
  (∀p s1.
   (¬∃l s2. step p s1 l s2) ∧
   s1.status = Partial
   ⇒
   sem_step p s1 Error (s1 with status := Stuck))
End

(* The semantics of a program will be the finite traces of stores to global
 * variables.
 * *)
Definition sem_def:
  sem p s1 =
    { ((last path).status, filter ($≠ Tau) l)  | (path, l) |
      toList (labels path) = Some l ∧ finite path ∧ okpath (sem_step p) path ∧ first path = s1 }
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
    ∃dec block.
      alookup p ip.f = Some dec ∧ alookup dec.blocks ip.b = Some block ∧
      ((∃idx. ip.i = Offset idx ∧ idx < length block.body) ∨
       (∃from_l. ip.i = Phi_ip from_l ∧ block.h ≠ Entry ∧ alookup dec.blocks from_l ≠ None))
End

Definition instr_to_labs_def:
  (instr_to_labs (Br _ l1 l2) = [l1; l2]) ∧
  (instr_to_labs _ = [])
End

Definition phi_contains_label_def:
  phi_contains_label l (Phi _ _ ls) ⇔ alookup ls l ≠ None
End

Definition prog_ok_def:
  prog_ok p ⇔
    ((* All blocks end with terminators and terminators only appear at the end.
      * All labels mentioned in branches actually exist, and target non-entry
      * blocks, whose phi nodes have entries for the label of the block that the
      * branch is from. *)
     ∀fname dec bname block.
       alookup p fname = Some dec ∧
       alookup dec.blocks bname = Some block
       ⇒
       block.body ≠ [] ∧ terminator (last block.body) ∧
       every (λi. ¬terminator i) (front block.body) ∧
       every (λlab. ∃b phis land. alookup dec.blocks (Some lab) = Some b ∧
                        b.h = Head phis land ∧ every (phi_contains_label bname) phis)
             (instr_to_labs (last block.body))) ∧
    ((* All functions have an entry block *)
     ∀fname dec.
       alookup p fname = Some dec ⇒
       ∃block. alookup dec.blocks None = Some block ∧ block.h = Entry) ∧
    ((* All non-entry blocks have a proper header *)
     ∀fname dec l b.
       alookup p fname = Some dec ∧ alookup dec.blocks (Some l) = Some b ⇒
       b.h ≠ Entry) ∧
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
    s.ip.i = Offset 0 ∧
    s.locals = fempty ∧
    s.stack = [] ∧
    s.status = Partial ∧
    globals_ok s ∧
    heap_ok s.heap ∧
    fdom s.globals = fdom global_init ∧
    s.heap.valid_addresses =  { A n | n < 256 ** pointer_size } ∧
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
