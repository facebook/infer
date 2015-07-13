(*
 * Copyright (c) 2015 - Facebook.
 * All rights reserved.
 *)
open Ast

let concatmap (sep : string) (f : 'a -> string) (l : 'a list) : string = String.concat sep (List.map f l)

let rec pretty_prog : prog -> string = function
  Prog defs -> concatmap "" pretty_func_def defs

and pretty_func_def : func_def -> string = function
  FuncDef (name, ret_tp, params, instrs) ->
    "define " ^ pretty_ret_typ ret_tp ^ " " ^ name ^ "(" ^
    concatmap ", " (fun (tp, id) -> pretty_typ tp ^ " " ^ id) params ^ ") {\n" ^
    concatmap "" pretty_instr_ln instrs ^ "}\n"

and pretty_ret_typ : typ option -> string = function
  | None -> "void"
  | Some tp -> pretty_typ tp

and pretty_typ : typ -> string = function
  | Tint width -> "i" ^ string_of_int width
  | Tfloat (* just one type for now *) -> "float"
  | Tptr tp -> pretty_typ tp ^ "*"
  | Tvector (sz, tp) -> "<" ^ string_of_int sz ^ " x " ^ pretty_typ tp ^ ">"
  | Tarray (sz, tp) -> "[" ^ string_of_int sz ^ " x " ^ pretty_typ tp ^ "]"

and pretty_instr_ln (i : instr) : string = pretty_instr i ^ "\n"

and pretty_instr : instr -> string = function
  | Ret None -> "ret void"
  | Ret (Some (tp, v)) -> "ret " ^ pretty_typ tp ^ " " ^ pretty_value v

and pretty_value : value -> string = function
  | True -> "true"
  | False -> "false"
  | Intlit i -> string_of_int i
  | Null -> "null"
