(*
 * Copyright (c) 2015 - Facebook.
 * All rights reserved.
 *)
type prog = Prog of func_def list
and func_def = FuncDef of string * typ option * (typ * string) list * instr list
and typ =
  | Tint of int
  | Tfloat (* just one type for now *)
  | Tptr of typ
  | Tvector of int * typ
  | Tarray of int * typ
and instr =
  | Ret of (typ * value) option
and value =
  | True
  | False
  | Intlit of int
  | Null
