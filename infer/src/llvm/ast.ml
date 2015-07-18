(*
* Copyright (c) 2015 - present Facebook, Inc.
* All rights reserved.
*
* This source code is licensed under the BSD style license found in the
* LICENSE file in the root directory of this source tree. An additional grant
* of patent rights can be found in the PATENTS file in the same directory.
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
