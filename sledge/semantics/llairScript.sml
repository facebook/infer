(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* A mini-LLAIR model, based on the files in sledge/src/llair *)

open HolKernel boolLib bossLib Parse;
open settingsTheory;

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
  var = Var_name string
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
  llair = <| globals : global list; functions : (label, func) alist |>
End

(* ----- Semantic states ----- *)

(* TODO Given the similarities with LLVM, consider moving some definitions into
 * a common predecessor theory *)

Datatype:
  addr = A num
End

(* These are the values that can be stored in registers. The implementation uses
 * integers with a bit-width to represent numbers, and keeps locations and sizes
 * separate.
 *)
Datatype:
  v =
  | LocV num
  | SizeV num
  | IntV int num
  | AggV (v list)
End

Datatype:
  pc = <| l : label; i : num |>
End

Datatype:
  frame = <| ret : pc; exn_ret : pc; ret_var : var; saved_locals : var |-> v; |>
End

Datatype:
  state =
    <| ip : pc;
       globals : var |-> word64;
       locals : var |-> v;
       stack : frame list;
       (* The set of allocated ranges.
        * The llvm model had a bool to indicate whether the range is free-able
        * or not, since the memory that the globals is in should never be freed.
        * llair does not currently catch this error, so we won't either. If
        * llair wants to catch the error in the future, then we can adapt the
        * semantics. *)
       allocations : (num # num) set;
       (* A byte addressed heap *)
       heap : addr |-> word8 |>
End

(* ----- Semantic transitions ----- *)

Definition eval_exp_def:
  eval_exp = ARB: state -> exp -> v
End

Definition update_results_def:
  update_results xvs s = s with locals := s.locals |++ xvs
End

Definition inc_pc_def:
  inc_pc s = s with ip := (s.ip with i := s.ip.i + 1)
End

(*
Inductive step_inst:
  (step_inst (prog : llair) s
    (Assign ves)
    (inc_pc (update_results (map (λ(v,e). (v, eval_exp s e)) ves) s)))
End
*)

export_theory ();
