(*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils

open LAst

let concatmap (sep : string) (f : 'a -> string) (l : 'a list) : string =
  String.concat sep (List.map f l)

let pretty_variable : variable -> string = function
  | Global id -> "@" ^ id
  | Local id -> "%" ^ id

let pretty_constant : constant -> string = function
  | Cint i -> string_of_int i
  | Cnull -> "null"

let pretty_operand : operand -> string = function
  | Var var -> pretty_variable var
  | Const const -> pretty_constant const

let rec pretty_typ : typ -> string = function
  | Tint width -> "i" ^ string_of_int width
  | Tfloat (* just one type for now *) -> "float"
  | Tptr tp -> pretty_typ tp ^ "*"
  | Tvector (sz, tp) -> "<" ^ string_of_int sz ^ " x " ^ pretty_typ tp ^ ">"
  | Tlabel -> "label"
  | Tmetadata -> "metadata"
  | Tarray (sz, tp) -> "[" ^ string_of_int sz ^ " x " ^ pretty_typ tp ^ "]"

let pretty_ret_typ : typ option -> string = function
  | None -> "void"
  | Some tp -> pretty_typ tp

let pretty_instr : instr -> string = function
  | Ret None -> "ret void"
  | Ret (Some (tp, op)) -> "ret " ^ pretty_typ tp ^ " " ^ pretty_operand op
  | UncondBranch label -> "br label " ^ pretty_variable label
  | CondBranch (op, label1, label2) ->
      "br i1 " ^ pretty_operand op ^ ", label " ^ pretty_variable label1 ^
      ", label " ^ pretty_variable label2

let pretty_instr_ln (i : instr) : string = pretty_instr i ^ "\n"

let pretty_func_def : func_def -> string = function
    FuncDef (name, ret_tp, params, instrs) ->
      "define " ^ pretty_ret_typ ret_tp ^ " " ^ pretty_variable name ^ "(" ^
      concatmap ", " (fun (tp, id) -> pretty_typ tp ^ " " ^ id) params ^ ") {\n" ^
      concatmap "" pretty_instr_ln instrs ^ "}\n"

let pretty_prog : prog -> string = function
    Prog func_defs -> concatmap "" pretty_func_def func_defs
