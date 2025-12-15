(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

module Atom : sig
  type t [@@deriving compare]

  val pp : F.formatter -> t -> unit
end

type term = App of Atom.t * Atom.t | Atom of Atom.t

val pp_term : F.formatter -> term -> unit

type t

val pp_nested_term : t -> F.formatter -> Atom.t -> unit

val init : debug:bool -> t

type header = private Atom.t

val pp_header : F.formatter -> header -> unit

val mk_header : t -> string -> header

val mk_app : t -> left:Atom.t -> right:Atom.t -> Atom.t

val mk_term : t -> header -> Atom.t list -> Atom.t

val merge : t -> Atom.t -> term -> unit

val is_equiv : t -> Atom.t -> Atom.t -> bool

val representative : t -> Atom.t -> Atom.t

val representative_of_header : t -> header -> Atom.t

val iter_term_roots : t -> header -> f:(Atom.t -> unit) -> unit

val equiv_atoms : t -> Atom.t -> Atom.t list

type app_equation = {rhs: Atom.t; left: Atom.t; right: Atom.t}

val equiv_terms : t -> Atom.t -> app_equation list

val show_stats : t -> unit

val debug : t -> unit
