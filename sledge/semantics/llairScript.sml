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
  | Switch exp ((num # label) list) label
  (* Args: int to switch on, jump table *)
  | Iswitch exp (label list)
  (* Args:  result reg, function to call, arguments, return type of callee,
   * return target, exception target *)
  | Call var label (exp list) typ label label
  | Return exp
  | Throw exp
  | Unreachable
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
  llair = <| globals : global list; functions : (string, func) alist |>
End

(* ----- Semantic states ----- *)

(* These are the values that can be stored in registers. The implementation uses
 * integers with a bit-width to represent numbers, and keeps locations and sizes
 * separate.
 *)
Datatype:
  flat_v =
  | LocV num
  | SizeV num
  | IntV int num
End

Type v = ``:flat_v reg_v``

Datatype:
  frame = <| ret : label; exn_ret : label; ret_var : var; saved_locals : var |-> v; |>
End

Datatype:
  state =
    <| bp : label; (* Pointer to the next block to execute *)
       globals : var |-> word64;
       locals : var |-> v;
       stack : frame list;
       pointer_size : 'a itself;
       heap : unit heap |>
End

(* ----- Semantic transitions ----- *)

Definition sizeof_def:
  (sizeof (:'a) (IntegerT n) = n) ∧
  (sizeof (:'a) (PointerT t) = dimindex (:'a)) ∧
  (sizeof (:'a) (ArrayT t n) = n * sizeof (:'a) t) ∧
  (sizeof (:'a) (TupleT ts) = sum (map (sizeof (:'a)) ts))
Termination
  WF_REL_TAC `measure (typ_size o snd)` >> simp [] >>
  Induct >> rw [definition "typ_size_def"] >> simp [] >>
  first_x_assum drule >> decide_tac
End

Definition type_to_shape_def:
  (type_to_shape (:'a) (IntegerT n) = Flat (sizeof (:'a) (IntegerT n)) (IntegerT n)) ∧
  (type_to_shape (:'a) (ArrayT t n) = Array (type_to_shape (:'a) t) n) ∧
  (type_to_shape (:'a) (TupleT ts) = Tuple (map (type_to_shape (:'a)) ts))
Termination
  WF_REL_TAC `measure (typ_size o snd)` >>
  rw [] >>
  Induct_on `ts` >> rw [definition "typ_size_def"] >>
  res_tac >> decide_tac
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
  (value_type (IntegerT n) (FlatV (IntV i n))) ∧
  (value_type (PointerT _) (FlatV (LocV n))) ∧
  (every (value_type t) vs ∧ length vs = n ∧ first_class_type t
   ⇒
   value_type (ArrayT t n) (AggV vs)) ∧
  (list_rel value_type ts vs
   ⇒
   value_type (StrT ts) (AggV vs))
End

Definition bool2v_def:
  bool2v b = FlatV (IntV (if b then 1 else 0) 1)
End

Definition int2unsigned_def:
  int2unsigned (i:int) size =
    if i < 0 then
      256 ** size + i
    else
      i
End

Definition fits_def:
  fits (i:int) size ⇔
    0 < size ∧ 0 - (256 ** (size - 1)) ≤ i ∧ i < 256 ** (size - 1)
End

(* TODO: We never create anything that is a SizeV *)
Inductive eval_exp:
  (∀s v r.
   flookup s.locals v = Some r
   ⇒
   eval_exp s (Var v) r) ∧

  (* TODO: Nondet I guess we need to know the type here *)
  (* TODO: Label *)

  (∀s e1 e2 size byte.
   eval_exp s e1 (FlatV (IntV byte 1)) ∧
   eval_exp s e2 (FlatV (SizeV size))
   ⇒
   eval_exp s (Splat e1 e2) (AggV (replicate size (FlatV (IntV byte 1))))) ∧

   (* TODO Question: What if size <> vals? *)
  (∀s e1 e2 size vals.
   eval_exp s e1 (AggV vals) ∧
   eval_exp s e2 (FlatV (SizeV size)) ∧
   size = length vals
   ⇒
   eval_exp s (Memory e1 e2) (AggV vals)) ∧

  (∀s es vals.
   list_rel (eval_exp s) es (map AggV vals)
   ⇒
   eval_exp s (Concat es) (AggV (flat vals))) ∧

  (∀s i size.
   eval_exp s (Integer i (IntegerT size)) (FlatV (IntV i size))) ∧

  (* TODO Question: Should the same integer with two different sizes be equal *)
  (∀s e1 e2 r1 r2.
   eval_exp s e1 r1 ∧
   eval_exp s e2 r2
   ⇒
   eval_exp s (Eq e1 e2) (bool2v (r1 = r2))) ∧

  (∀s e1 e2 i1 size1 i2 size2.
   eval_exp s e1 (FlatV (IntV i1 size1)) ∧
   eval_exp s e2 (FlatV (IntV i2 size2)) ∧
   fits i1 size1 ∧
   fits i2 size2
   ⇒
   eval_exp s (Lt e1 e2) (bool2v (i1 < i2))) ∧

  (∀s e1 e2 i1 size1 i2 size2.
   eval_exp s e1 (FlatV (IntV i1 size1)) ∧
   eval_exp s e2 (FlatV (IntV i2 size2)) ∧
   fits i1 size1 ∧
   fits i2 size2
   ⇒
   eval_exp s (Ult e1 e2) (bool2v (int2unsigned i1 size1 < int2unsigned i2 size2))) ∧

  (∀s size e1 e2 i1 i2.
   eval_exp s e1 (FlatV (IntV i1 size)) ∧
   eval_exp s e2 (FlatV (IntV i2 size)) ∧
   fits i1 size ∧
   fits i2 size
   ⇒
   eval_exp s (Sub (IntegerT size) e1 e2) (FlatV (IntV (i1 - i2) size))) ∧

  (∀s es vals.
   list_rel (eval_exp s) es vals
   ⇒
   eval_exp s (Record es) (AggV vals)) ∧

  (∀s e1 e2 vals size.
   eval_exp s e1 (AggV vals) ∧
   eval_exp s e2 (FlatV (SizeV size)) ∧
   size < length vals
   ⇒
   eval_exp s (Select e1 e2) (el size vals)) ∧

  (∀s e1 e2 e3 vals size r.
   eval_exp s e1 (AggV vals) ∧
   eval_exp s e2 (FlatV (SizeV size)) ∧
   eval_exp s e3 r ∧
   size < length vals
   ⇒
   eval_exp s (Update e1 e2 e3) (AggV (list_update r size vals)))

End

Definition convert_value_def:
  (convert_value (IntegerT n) w = IntV (w2i w) n) ∧
  (convert_value (PointerT t) w = LocV (w2n w))
End

Definition bytes_to_llair_value_def:
  bytes_to_llair_value (:'a) t bs =
    (bytes_to_value (λn t (w:'a word). convert_value t w) (type_to_shape (:'a) t) bs)
End

Definition unconvert_value_def:
  (unconvert_value (:'a) (IntV i size) = (size, i2w i)) ∧
  (unconvert_value (:'a) (LocV n) = (dimindex (:'a), n2w n))
End

Definition llair_value_to_bytes_def:
  llair_value_to_bytes (:'a) v =
    value_to_bytes (unconvert_value (:'a) : flat_v -> num # 'a word) v
End

Definition update_results_def:
  update_results xvs s = s with locals := s.locals |++ xvs
End

Inductive step_inst:
  (∀s ves r.
    eval_exp s e r
    ⇒
    step_inst s
    (Move ves)
    (update_results (map (λ(v,e). (v, r)) ves) s)) ∧

  (∀(s:'a state) x t e size ptr freeable interval bytes.
   eval_exp s e (FlatV (LocV ptr)) ∧
   interval = Interval freeable ptr (ptr + size) ∧
   is_allocated interval s.heap ∧
   bytes = map snd (get_bytes s.heap interval)
   ⇒
   step_inst s
    (Load (Var_name x t) e size)
    (update_results [(Var_name x t, fst (bytes_to_llair_value (:'a) t bytes))] s)) ∧

  (∀s e1 e2 size ptr bytes freeable interval r.
   eval_exp s e1 (FlatV (LocV ptr)) ∧
   eval_exp s e2 r ∧
   interval = Interval freeable ptr (ptr + size) ∧
   is_allocated interval s.heap ∧
   bytes = llair_value_to_bytes (:'a) r ∧
   length bytes = size
   ⇒
   step_inst s
     (Store e1 e2 size)
     (s with heap := set_bytes () bytes ptr s.heap)) ∧

  (* TODO memset *)

  (∀s e1 e2 e3 dest_ptr src_ptr size src_interval freeable1 freeable2 bytes.
   eval_exp s e1 (FlatV (LocV dest_ptr)) ∧
   eval_exp s e2 (FlatV (LocV src_ptr)) ∧
   eval_exp s e3 (FlatV (SizeV size)) ∧
   src_interval = Interval freeable1 src_ptr (src_ptr + size) ∧
   is_allocated src_interval s.heap ∧
   is_allocated (Interval freeable2 dest_ptr (dest_ptr + size)) s.heap ∧
   (* TODO Question: should we allow overlap? *)
   bytes = map snd (get_bytes s.heap src_interval)
   ⇒
   step_inst s
    (Memcpy e1 e2 e3)
    (s with heap := set_bytes () bytes dest_ptr s.heap)) ∧

  (* TODO memmove *)

  (∀s v e1 e2 n size ptr new_h.
   eval_exp s e1 (FlatV (SizeV n)) ∧
   eval_exp s e2 (FlatV (SizeV size)) ∧
   allocate s.heap (n * size) () (ptr, new_h)
   ⇒
   step_inst s
     (Alloc v e1 e2)
     (update_results [(v, FlatV (LocV ptr))] (s with heap := new_h))) ∧

  (∀s e ptr.
   eval_exp s e (FlatV (LocV ptr))
   ⇒
   step_inst s
     (Free e)
     (s with heap := deallocate [A ptr] s.heap)) ∧

  (∀s x t nondet.
   value_type t nondet
   ⇒
   step_inst s
     (NondetI (Var_name x t))
     (update_results [(Var_name x t, nondet)] s))

End

Inductive step_term:

  (∀prog s e table default size fname bname bname'.
   eval_exp s e (FlatV (SizeV size)) ∧
   Lab_name fname bname = (case alookup table size of Some lab => lab | None => default) ∧
   s.bp = Lab_name fname bname'
   ⇒
   step_term prog s
     (Switch e table default)
     (s with bp := Lab_name fname bname)) ∧

  (∀prog s e labs size.
   eval_exp s e (FlatV (SizeV size)) ∧
   size < length labs
   ⇒
   step_term prog s
     (Iswitch e labs)
     (s with bp := el size labs)) ∧

  (∀prog s v fname bname es t ret1 ret2 vals f.
   alookup prog.functions fname = Some f ∧
   f.entry = Lab_name fname bname ∧
   list_rel (eval_exp s) es vals
   ⇒
   step_term prog s
     (Call v (Lab_name fname bname) es t ret1 ret2)
     <| bp := Lab_name fname bname;
        globals := s.globals;
        locals := alist_to_fmap (zip (f.params, vals));
        stack :=
          <| ret := ret1;
             exn_ret := ret2;
             ret_var := v;
             saved_locals := s.locals |> :: s.stack;
        pointer_size := s.pointer_size;
        heap := s.heap |>) ∧

  (∀prog s e r top rest.
   eval_exp s e r ∧
   s.stack = top::rest
   ⇒
   step_term prog s
     (Return e)
     <| bp := top.ret;
        globals := s.globals;
        locals := top.saved_locals |+ (top.ret_var, r);
        stack := rest;
        pointer_size := s.pointer_size;
        heap := s.heap |>)

  (* TODO Throw *)

End

(* With function calls terminating blocks, it's very easy to get rid of the
 * instruction pointer and do a big-step evaluation for each block *)
Inductive step_block:

  (!prog s1 t s2.
   step_term prog s1 t s2
   ⇒
   step_block prog s1 [] t s2) ∧

  (!prog s1 i is t s2 s3.
   step_inst s1 i s2 ∧
   step_block prog s2 is t s3
   ⇒
   step_block prog s1 (i::is) t s3)

End

Inductive step:
  (∀prog s fname bname f b s'.
   s.bp = Lab_name fname bname ∧
   alookup prog.functions fname = Some f ∧
   alookup f.cfg s.bp = Some b ∧
   step_block prog s b.cmnd b.term s'
   ⇒
   step prog s s')
End

export_theory ();
