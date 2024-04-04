(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** This file is about translating Python3.8 [code] objects into an OCaml representation. Some
    interesting readings:

    @see <https://towardsdatascience.com/understanding-python-bytecode-e7edaae8734d>
    @see <https://docs.python.org/3.8/library/dis.html> *)

open! IStd

module Error : sig
  type kind

  type t = Logging.error * kind

  val pp_kind : Format.formatter -> kind -> unit
end

module rec Constant : sig
  type t = private
    | PYCBool of bool
    | PYCInt of Z.t
    | PYCFloat of float
    | PYCComplex of {real: float; imag: float}
    | PYCString of string
    | PYCInvalidUnicode of int array
    | PYCBytes of bytes
    | PYCTuple of t array
    | PYCFrozenSet of t list
    | PYCCode of Code.t
    | PYCNone
  [@@deriving compare]

  val show : ?full:bool -> t -> string
  [@@warning "-unused-value-declaration"]
  (** Only shows the name of a [PYCCode] constant if [full] is [false]. Otherwise, shows everything. *)

  val pp : Format.formatter -> t -> unit

  val as_code : t -> Code.t option

  val as_name : t -> string option
end

and Code : sig
  type t = private
    { co_name: string
    ; co_filename: string
    ; co_flags: int
    ; co_cellvars: string array
          (** A tuple containing the names of nonlocal variables. These are the local variables of a
              function accessed by its inner functions. *)
    ; co_freevars: string array
          (** A tuple containing the names of free variables. Free variables are the local variables
              of an outer function which are accessed by its inner function. *)
    ; co_names: string array
          (** A tuple containing the names used by the bytecode which can be global variables,
              functions, and classes or also attributes loaded from objects. There are "global
              names". Local variables & the like are going in other arrays *)
    ; co_varnames: string array
          (** A tuple containing the local names used by the bytecode (arguments first, then the
              local variables). *)
    ; co_nlocals: int
    ; co_argcount: int
    ; co_firstlineno: int
    ; co_posonlyargcount: int
    ; co_stacksize: int
    ; co_kwonlyargcount: int
    ; co_lnotab: char array
    ; co_consts: Constant.t array
          (** A tuple containing the literals used by the bytecode. By experience, it is only [int],
              [string], [tuple]s, [None] or [code] objects *)
    ; instructions: Instruction.t list }
  [@@deriving show, compare]

  val full_show : t -> string [@@warning "-unused-value-declaration"]

  val is_closure : t -> bool

  val get_arguments : t -> string array

  val get_locals : t -> string array
end

and Instruction : sig
  (** @see <https://docs.python.org/3.8/library/dis.html#dis.Instruction> *)
  type t = private
    { opname: string
    ; opcode: int
    ; arg: int
    ; argval: Constant.t
    ; offset: int
    ; starts_line: int option
    ; is_jump_target: bool }
  [@@deriving show, compare]
end

val from_string : source:string -> filename:string -> (Code.t, Error.t) result
[@@warning "-unused-value-declaration"]
(** Compiles the python program describes by [source] into a [Code.t] object *)

val from_file : is_binary:bool -> string -> (Code.t, Error.t) result
(** Generates a [Code.t] object from a file. If the file is a source file, the builtin [compile]
    from Python is called to generate the bytecode. If the file is already a bytecode file, it is
    used right away using the Python module Marshal *)
