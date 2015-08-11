(*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

(** Representation of LLVM constructs *)

type variable_id =
  | Name of string
  | Number of int

type variable =
  | Global of variable_id
  | Local of variable_id

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
  | Tfunc of typ option * typ list
  | Tlabel
  | Tmetadata

type metadata =
  | MetadataVar of int
  | MetadataString of string
  | MetadataNode of metadata_component list

and metadata_component =
  | TypOperand of typ option * operand
  | Metadata of metadata

type instr =
  | Ret of (typ * operand) option
  | UncondBranch of variable
  | CondBranch of operand * variable * variable
  | Load of variable * typ * variable
  | Store of operand * typ * variable
  | Alloc of variable * typ * int (* return variable, element type, number of elements *)
  | Binop

type annotation = Annotation of int

type annotated_instr = instr * annotation option

type func_def = FuncDef of variable * typ option * (typ * string) list * annotated_instr list

type prog = Prog of func_def list

let string_of_variable : variable -> string = function
  | Global var_id | Local var_id ->
      begin match var_id with
      | Name str -> str
      | Number i -> string_of_int i
      end
