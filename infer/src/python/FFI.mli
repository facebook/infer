(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

module rec Constant : sig
  type t =
    | PYCBool of bool
    | PYCInt of int64
    | PYCString of string
    | PYCTuple of t array
    | PYCCode of Code.t
    | PYCNone
  [@@deriving show, compare]

  val create : Pytypes.pyobject -> t
end

and Code : sig
  type t =
    { co_name: string
    ; co_filename: string
    ; co_flags: int
    ; co_cellvars: string array
    ; co_freevars: string array
    ; co_names: string array
    ; co_varnames: string array
    ; co_nlocals: int
    ; co_argcount: int
    ; co_firstlineno: int
    ; co_posonlyargcount: int
    ; co_stacksize: int
    ; co_kwonlyargcount: int
    ; co_lnotab: char array
    ; co_consts: Constant.t array
    ; instructions: Instruction.t list }
  [@@deriving show, compare]

  val create : Pytypes.pyobject -> t
end

and Instruction : sig
  type t =
    { opname: string
    ; opcode: int
    ; arg: int
    ; argval: Constant.t
    ; offset: int
    ; starts_line: int option
    ; is_jump_target: bool }
  [@@deriving show, compare]

  val create : Pytypes.pyobject -> t
end

val from_string : source:string -> filename:string -> Code.t [@@warning "-unused-value-declaration"]

val from_file : is_binary:bool -> string -> Code.t [@@warning "-unused-value-declaration"]
