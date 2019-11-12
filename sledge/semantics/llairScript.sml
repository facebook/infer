(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* A mini-LLAIR model, based on the files in sledge/src/llair *)

open HolKernel boolLib bossLib Parse;
open settingsTheory memory_modelTheory;

new_theory "llair";

numLib.prefer_num ();

(* ----- Abstract syntax ----- *)

Datatype:
  typ =
  | FunctionT typ (typ list)
  (* How many bits the integer occupies *)
  | IntegerT num
  | PointerT typ
  | ArrayT typ num
  | TupleT (typ list)
End

Datatype:
  var = Var_name string typ
End

Datatype:
  label = Lab_name string string
End

(* Based on the constructor functions in exp.mli rather than the type definition *)
Datatype:
  exp =
  | Var var
  | Nondet
  (* Args: function name, block name *)
  | Label label
  (* Args: byte, size *)
  | Splat exp exp
  (* Args: size, byte array *)
  | Memory exp exp
  (* Byte array concatenation *)
  | Concat (exp list)
  | Integer int typ
  | Eq exp exp
  | Lt exp exp
  | Ult exp exp
  | Sub typ exp exp
  | Record (exp list)
  (* Args: Record, index *)
  | Select exp exp
  (* Args: Record, index, value *)
  | Update exp exp exp
  (* Args: number of bits, integer expression, result type *)
  | Signed num exp typ
  (* Args: number of bits, integer expression, result type *)
  | Unsigned num exp typ
End

Datatype:
  inst =
  (* Args: the list of variable, expression assignments to do *)
  | Move ((var # exp) list)
  (* Args: result reg, pointer, length *)
  | Load var exp num
  (* Args: pointer, value, length *)
  | Store exp exp num
  (* Args: destination, contents, length *)
  | Memset exp exp exp
  (* Args: destination, source, length *)
  | Memcpy exp exp exp
  (* Args: destination, source, length *)
  | Memmov exp exp exp
  (* Args : result, number of elements, size *)
  | Alloc var exp exp
  (* Args: pointer *)
  | Free exp
  (* Args: result reg *)
  | NondetI var
  | Abort
End

Datatype:
  term =
  (* Args: key, branch table, default exp *)
  | Switch exp ((int # label) list) label
  (* Args: int to switch on, jump table *)
  | Iswitch exp (label list)
  (* Args:  result reg, function to call, arguments, return type of callee,
   * return target, exception target *)
  | Call var label (exp list) typ label label
  | Return exp
  | Throw exp
  | Unreachable
  | Exit exp
End

Datatype:
  block = <| cmnd : inst list; term : term |>
End

(* The llair code doesn't have params here yet, but it will need to *)
Datatype:
  func = <| params : var list;
            locals : var set;
            entry : label;
            cfg : (label, block) alist;
            freturn : var;
            fthrow : var |>
End

(* The int is how much space the global needs *)
Datatype:
  global = <| var : var; init : (exp # int) option; typ: typ |>
End

Datatype:
  llair = <| glob_init : global list; functions : (string, func) alist |>
End

(* ----- Semantic states ----- *)

(* These are the values that can be stored in registers. The implementation uses
 * integers with a bit-width to represent numbers and pointers. Here we
 * interpret the bit width b as meaning the int should be in the range [-2^(b-1),2^(b-1))
 *)
Datatype:
  flat_v =
  | IntV int num
End

Type v = ``:flat_v reg_v``

Datatype:
  frame = <| ret : label; exn_ret : label; ret_var : var; saved_locals : var |-> v; |>
End

Datatype:
  state =
    <| bp : label; (* Pointer to the next block to execute *)
       glob_addrs : var |-> num;
       locals : var |-> v;
       stack : frame list;
       heap : unit heap;
       status : trace_type |>
End

(* Assume that all pointers can fit in 64 bits *)
Definition pointer_size_def:
  pointer_size = 64
End

(* ----- Semantic transitions ----- *)

(* The size of a type in bytes, rounded up *)
Definition sizeof_def:
  (sizeof (IntegerT n) = (n+7) DIV 8) ∧
  (sizeof (PointerT t) = (pointer_size+7) DIV 8) ∧
  (sizeof (ArrayT t n) = n * sizeof t) ∧
  (sizeof (TupleT ts) = sum (map sizeof ts))
Termination
  WF_REL_TAC `measure typ_size` >> simp [] >>
  Induct >> rw [definition "typ_size_def"] >> simp [] >>
  first_x_assum drule >> decide_tac
End

(* The size of a type in bits *)
Definition sizeof_bits_def:
  (sizeof_bits (IntegerT n) = n) ∧
  (sizeof_bits (PointerT t) = pointer_size) ∧
  (sizeof_bits (ArrayT t n) = n * sizeof_bits t) ∧
  (sizeof_bits (TupleT ts) = sum (map sizeof_bits ts))
Termination
  WF_REL_TAC `measure typ_size` >> simp [] >>
  Induct >> rw [definition "typ_size_def"] >> simp [] >>
  first_x_assum drule >> decide_tac
End

Definition first_class_type_def:
  (first_class_type (IntegerT _) ⇔ T) ∧
  (first_class_type (PointerT _) ⇔ T) ∧
  (first_class_type (ArrayT t _) ⇔ first_class_type t) ∧
  (first_class_type (TupleT ts) ⇔ every first_class_type ts) ∧
  (first_class_type _ ⇔ F)
Termination
  WF_REL_TAC `measure typ_size` >>
  rw [] >>
  Induct_on `ts` >> rw [definition "typ_size_def"] >>
  res_tac >> decide_tac
End

Inductive value_type:
  (∀n i. value_type (IntegerT n) (FlatV (IntV i n))) ∧
  (∀t vs n.
   every (value_type t) vs ∧ length vs = n ∧ first_class_type t
   ⇒
   value_type (ArrayT t n) (AggV vs)) ∧
  (∀ts vs.
   list_rel value_type ts vs
   ⇒
   value_type (TupleT ts) (AggV vs))
End

Definition bool2v_def:
  bool2v b = FlatV (IntV (if b then 1 else 0) 1)
End

(* The natural number, interpreted as unsigned, fits in the given number of bits *)
Definition nfits_def:
  nfits (n:num) size ⇔
    0 < size ∧ n < 2 ** size
End

Definition signed2unsigned_def:
  signed2unsigned i size =
    if i < 0 then Num (2 ** size + i) else Num i
End

(* Convert an integer to an unsigned number, following the 2's complement
 * representation, assuming (ifits i size). This is what OCaml's Z.extract does,
 * which is used in LLAIR for Convert expressions and unsigned operations, e.g.,
 * <. *)
Definition i2n_def:
  i2n (IntV i size) : num =
    signed2unsigned i size
End

(* Convert an unsigned number into the integer that it would be in 2's
 * compliment with the given size, assuming (nfits n size) *)
Definition n2i_def:
  n2i n size =
    if 2 ** (size - 1) ≤ n then
      (IntV (&n - &(2 ** size)) size)
    else
      (IntV (&n) size)
End

Inductive eval_exp:
  (∀s v r.
   flookup s.locals v = Some r
   ⇒
   eval_exp s (Var v) r) ∧

  (* TODO: Nondet I guess we need to know the type here *)
  (* TODO: Label *)

  (∀s e1 e2 n byte n_size.
   eval_exp s e1 (FlatV (IntV byte 8)) ∧
   (* This idiom means that e2 evaluates to a non-negative integer n, and is
    * used throughout *)
   eval_exp s e2 (FlatV (IntV (&n) n_size))
   ⇒
   eval_exp s (Splat e1 e2) (AggV (replicate n (FlatV (IntV byte 8))))) ∧

   (* TODO Question: What if size <> vals? *)
  (∀s e1 e2 l vals n_size.
   eval_exp s e1 (AggV vals) ∧
   eval_exp s e2 (FlatV (IntV (&l) n_size)) ∧
   l = length vals
   ⇒
   eval_exp s (Memory e1 e2) (AggV vals)) ∧

  (∀s es vals.
   list_rel (eval_exp s) es (map AggV vals)
   ⇒
   eval_exp s (Concat es) (AggV (flat vals))) ∧

  (∀s i size.
   eval_exp s (Integer i (IntegerT size)) (FlatV (IntV (truncate_2comp i size) size))) ∧

  (* TODO Question: Should the same integer with two different sizes be equal *)
  (∀s e1 e2 r1 r2.
   eval_exp s e1 r1 ∧
   eval_exp s e2 r2
   ⇒
   eval_exp s (Eq e1 e2) (bool2v (r1 = r2))) ∧

  (∀s e1 e2 i1 size1 i2 size2.
   eval_exp s e1 (FlatV (IntV i1 size1)) ∧
   eval_exp s e2 (FlatV (IntV i2 size2)) ∧
   ifits i1 size1 ∧
   ifits i2 size2
   ⇒
   eval_exp s (Lt e1 e2) (bool2v (i1 < i2))) ∧

  (∀s e1 e2 i1 i2 size1 size2.
   eval_exp s e1 (FlatV (IntV i1 size1)) ∧
   eval_exp s e2 (FlatV (IntV i2 size2)) ∧
   ifits i1 size1 ∧
   ifits i2 size2
   ⇒
   eval_exp s (Ult e1 e2) (bool2v (i2n (IntV i1 size1) < i2n (IntV i2 size2)))) ∧

  (∀s size e1 e2 i1 i2.
   eval_exp s e1 (FlatV (IntV i1 size)) ∧
   eval_exp s e2 (FlatV (IntV i2 size))
   ⇒
   eval_exp s (Sub (IntegerT size) e1 e2) (FlatV (IntV (truncate_2comp (i1 - i2) size) size))) ∧

  (∀s es vals.
   list_rel (eval_exp s) es vals
   ⇒
   eval_exp s (Record es) (AggV vals)) ∧

  (∀s e1 e2 vals idx idx_size.
   eval_exp s e1 (AggV vals) ∧
   eval_exp s e2 (FlatV (IntV (&idx) idx_size)) ∧
   idx < length vals
   ⇒
   eval_exp s (Select e1 e2) (el idx vals)) ∧

  (∀s e1 e2 e3 vals r idx idx_size.
   eval_exp s e1 (AggV vals) ∧
   eval_exp s e2 (FlatV (IntV (&idx) idx_size)) ∧
   eval_exp s e3 r ∧
   idx < length vals
   ⇒
   eval_exp s (Update e1 e2 e3) (AggV (list_update r idx vals))) ∧

  (∀s e i size to_t size1.
   eval_exp s e (FlatV (IntV i size1)) ∧
   size < sizeof_bits to_t
   ⇒
   eval_exp s (Unsigned size e to_t)
     (FlatV (IntV (&(signed2unsigned (truncate_2comp i size) size)) (sizeof_bits to_t)))) ∧

  (∀s e size size1 i to_t.
   eval_exp s e (FlatV (IntV i size1)) ∧
   size ≤ sizeof_bits to_t
   ⇒
   eval_exp s (Signed size e to_t) (FlatV (IntV (truncate_2comp i size) (sizeof_bits to_t))))

End

(* BEGIN Functions to interface to the generic memory model *)
Definition type_to_shape_def:
  (type_to_shape (IntegerT n) = Flat (sizeof (IntegerT n)) (IntegerT n)) ∧
  (type_to_shape (PointerT t) = Flat (sizeof (PointerT t)) (PointerT t)) ∧
  (type_to_shape (ArrayT t n) = Array (type_to_shape t) n) ∧
  (type_to_shape (TupleT ts) = Tuple (map type_to_shape ts))
Termination
  WF_REL_TAC `measure typ_size` >>
  rw [] >>
  Induct_on `ts` >> rw [definition "typ_size_def"] >>
  res_tac >> decide_tac
End

Definition convert_value_def:
  (convert_value (IntegerT size) n =
    if size = 1 then
      IntV (if n = 0 then 0 else -1) size
    else
      n2i n size) ∧
  (convert_value (PointerT t) n =
    n2i n pointer_size)
End

Definition bytes_to_llair_value_def:
  bytes_to_llair_value t bs =
    (bytes_to_value (λn t w. convert_value t w) (type_to_shape t) bs)
End

Definition unconvert_value_def:
  unconvert_value (IntV i size) = ((size + 7) DIV 8, i2n (IntV i size))
End

Definition llair_value_to_bytes_def:
  llair_value_to_bytes v =
    value_to_bytes unconvert_value v
End
(* END Functions to interface to the generic memory model *)

Definition update_results_def:
  update_results xvs s = s with locals := s.locals |++ xvs
End

Inductive get_obs:
  (∀s ptr bytes x. flookup s.glob_addrs x = Some ptr ⇒ get_obs s ptr bytes (W x bytes)) ∧
  (∀s ptr bytes. ptr ∉ FRANGE s.glob_addrs ⇒ get_obs s ptr bytes Tau)
End

Inductive step_inst:
  (∀s ves rs.
    list_rel (eval_exp s) (map snd ves) rs
    ⇒
    step_inst s
    (Move ves) Tau
    (update_results (map (λ(v,r). (v, r)) (zip (map fst ves, rs))) s)) ∧

  (∀s x t e size ptr freeable interval bytes.
   eval_exp s e (FlatV ptr) ∧
   interval = Interval freeable (i2n ptr) (i2n ptr + size) ∧
   is_allocated interval s.heap ∧
   bytes = map snd (get_bytes s.heap interval)
   ⇒
   step_inst s
    (Load (Var_name x t) e size) Tau
    (update_results [(Var_name x t, fst (bytes_to_llair_value t bytes))] s)) ∧

  (∀s e1 e2 size ptr bytes freeable interval r obs.
   eval_exp s e1 (FlatV ptr) ∧
   eval_exp s e2 r ∧
   interval = Interval freeable (i2n ptr) (i2n ptr + size) ∧
   is_allocated interval s.heap ∧
   bytes = llair_value_to_bytes r ∧
   length bytes = size ∧
   get_obs s (i2n ptr) bytes obs
   ⇒
   step_inst s
     (Store e1 e2 size) obs
     (s with heap := set_bytes () bytes (i2n ptr) s.heap)) ∧

  (* TODO memset *)

  (∀s e1 e2 e3 dest_ptr src_ptr size src_interval freeable1 freeable2 bytes.
   eval_exp s e1 (FlatV dest_ptr) ∧
   eval_exp s e2 (FlatV src_ptr) ∧
   eval_exp s e3 (FlatV size) ∧
   src_interval = Interval freeable1 (i2n src_ptr) (i2n src_ptr + i2n size) ∧
   is_allocated src_interval s.heap ∧
   is_allocated (Interval freeable2 (i2n dest_ptr) (i2n dest_ptr + i2n size)) s.heap ∧
   (* TODO Question: should we allow overlap? *)
   bytes = map snd (get_bytes s.heap src_interval)
   ⇒
   step_inst s
    (Memcpy e1 e2 e3) Tau
    (s with heap := set_bytes () bytes (i2n dest_ptr) s.heap)) ∧

  (* TODO memmove *)

  (∀s v e1 e2 n size ptr new_h size_size.
   eval_exp s e1 (FlatV n) ∧
   eval_exp s e2 (FlatV (IntV (&size) size_size)) ∧
   allocate s.heap (i2n n * size) () (ptr, new_h) ∧
   nfits ptr pointer_size
   ⇒
   step_inst s
     (Alloc v e1 e2) Tau
     (update_results [(v, FlatV (n2i ptr pointer_size))] (s with heap := new_h))) ∧

  (∀s e ptr.
   eval_exp s e (FlatV ptr)
   ⇒
   step_inst s
     (Free e) Tau
     (s with heap := deallocate [A (i2n ptr)] s.heap)) ∧

  (∀s x t nondet.
   value_type t nondet
   ⇒
   step_inst s
     (NondetI (Var_name x t)) Tau
     (update_results [(Var_name x t, nondet)] s))

End

Inductive step_term:

  (∀prog s e table default idx fname bname idx_size.
   eval_exp s e (FlatV (IntV idx idx_size)) ∧
   Lab_name fname bname = (case alookup table idx of Some lab => lab | None => default)
   ⇒
   step_term prog s
     (Switch e table default) Tau
     (s with bp := Lab_name fname bname)) ∧

  (∀prog s e labs i idx idx_size.
   eval_exp s e (FlatV (IntV (&idx) idx_size)) ∧
   idx < length labs
   ⇒
   step_term prog s
     (Iswitch e labs) Tau
     (s with bp := el i labs)) ∧

  (∀prog s v fname bname es t ret1 ret2 vals f.
   alookup prog.functions fname = Some f ∧
   f.entry = Lab_name fname bname ∧
   list_rel (eval_exp s) es vals
   ⇒
   step_term prog s
     (Call v (Lab_name fname bname) es t ret1 ret2) Tau
     <| bp := Lab_name fname bname;
        glob_addrs := s.glob_addrs;
        locals := alist_to_fmap (zip (f.params, vals));
        stack :=
          <| ret := ret1;
             exn_ret := ret2;
             ret_var := v;
             saved_locals := s.locals |> :: s.stack;
        heap := s.heap |>) ∧

  (∀prog s e r top rest.
   eval_exp s e r ∧
   s.stack = top::rest
   ⇒
   step_term prog s
     (Return e) Tau
     <| bp := top.ret;
        glob_addrs := s.glob_addrs;
        locals := top.saved_locals |+ (top.ret_var, r);
        stack := rest;
        heap := s.heap |>) ∧

  (∀prog s e i size.
   eval_exp s e (FlatV (IntV i size))
   ⇒
   step_term prog s (Exit e) (Exit i) (s with status := Complete i))
  (* TODO Throw *)

End

(* With function calls terminating blocks, it's very easy to get rid of the
 * instruction pointer and do a big-step evaluation for each block *)
Inductive step_block:

  (∀prog s1 t l s2.
   step_term prog s1 t l s2
   ⇒
   step_block prog s1 [] t [l] s2) ∧

  (∀prog s1 t.
   ¬(∃s2 (l:var obs). step_term prog s1 t l s2)
   ⇒
   step_block prog s1 [] t [Error] (s1 with status := Stuck)) ∧

  (∀prog s1 i1 is t.
   (¬∃l s2. step_inst s1 i1 l s2)
   ⇒
   step_block prog s1 (i1::is) t [Error] (s1 with status := Stuck)) ∧

  (∀prog s1 i l is ls t s2 s3.
   step_inst s1 i l s2 ∧
   step_block prog s2 is t ls s3
   ⇒
   step_block prog s1 (i::is) t (l::ls) s3)

End

Inductive get_block:
  ∀prog bp fname bname f b.
    bp = Lab_name fname bname ∧
    alookup prog.functions fname = Some f ∧
    alookup f.cfg bp = Some b
    ⇒
    get_block prog bp b
End

Inductive step:
  ∀prog s b ls s'.
    get_block prog s.bp b ∧
    step_block prog s b.cmnd b.term ls s' ∧
    s.status = Partial
    ⇒
    step prog s ls s'
End

Definition sem_def:
  sem p s1 =
    { l1 | ∃path l2. l1 ∈ observation_prefixes ((last path).status, flat l2) ∧
           toList (labels path) = Some l2 ∧
           finite path ∧ okpath (step p) path ∧ first path = s1 }
End

export_theory ();
