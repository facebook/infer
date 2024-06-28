(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Single abstraction for all the kinds of variables in SIL *)

type t = private LogicalVar of Ident.t | ProgramVar of Pvar.t
[@@deriving compare, yojson_of, sexp, hash, equal, normalize]

val compare_modulo_this : t -> t -> int

val of_id : Ident.t -> t

val of_pvar : Pvar.t -> t

(* val of_formal_index : int -> t *)
(** Create a variable representing the ith formal of the current procedure *)

val get_all_vars_in_exp : Exp.t -> t Sequence.t
(** Get all free and program vars *)

val to_exp : t -> Exp.t

val get_ident : t -> Ident.t option

val get_pvar : t -> Pvar.t option

val is_pvar : t -> bool

val is_global : t -> bool

val is_return : t -> bool

val is_none : t -> bool

val is_this : t -> bool

val is_artificial : t -> bool

val appears_in_source_code : t -> bool
(** return true if this variable appears in source code (i.e., is not a LogicalVar or a
    frontend-generated ProgramVar) *)

val is_cpp_temporary : t -> bool

val is_cpp_unnamed_param : t -> bool

val get_footprint_index : t -> int option

val pp : Format.formatter -> t -> unit

module Map : PrettyPrintable.PPMap with type key = t

module Set : PrettyPrintable.PPSet with type elt = t
