(*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Single abstraction for all the kinds of variables in SIL *)

type t = private LogicalVar of Ident.t | ProgramVar of Pvar.t [@@deriving compare]

val equal : t -> t -> bool

val compare_modulo_this : t -> t -> int

val of_id : Ident.t -> t

val of_pvar : Pvar.t -> t

val of_formal_index : int -> t
(** Create a variable representing the ith formal of the current procedure *)

val get_all_vars_in_exp : Exp.t -> t Sequence.t
(** Get all free and program vars *)

val to_exp : t -> Exp.t

val is_global : t -> bool

val is_return : t -> bool

val is_footprint : t -> bool

val appears_in_source_code : t -> bool
(** return true if this variable appears in source code (i.e., is not a LogicalVar or a
    frontend-generated ProgramVar) *)

val is_cpp_temporary : t -> bool

val get_footprint_index : t -> int option

val pp : Format.formatter -> t -> unit

module Map : PrettyPrintable.PPMap with type key = t
