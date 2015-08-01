(*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

(** Representation of LLVM constructs *)

type variable =
  | Global of string
  | Local of string

type constant =
  | Cint of int
  | Cnull

type operand =
  | Var of variable
  | Const of constant

type typ =
  | Tint of int
  | Tfloat (* just one type for now *)
  | Tptr of typ
  | Tvector of int * typ
  | Tarray of int * typ
  | Tlabel
  | Tmetadata

type instr =
  | Ret of (typ * operand) option
  | UncondBranch of variable
  | CondBranch of operand * variable * variable
  | Load of variable * typ * variable
  | Store of operand * typ * variable

type func_def = FuncDef of variable * typ option * (typ * string) list * instr list

type prog = Prog of func_def list

let string_of_variable : variable -> string = function
  | Global str | Local str -> str
