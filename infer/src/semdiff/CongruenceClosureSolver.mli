(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

type value = string

module Atom : sig
  type t = private {index: int; value: value}

  val pp : F.formatter -> t -> unit
end

type term = App of Atom.t * Atom.t | Atom of Atom.t

val pp_term : F.formatter -> term -> unit

type t

val init : debug:bool -> t

val mk_atom : t -> value -> Atom.t

val merge : t -> Atom.t -> term -> unit

val representative : t -> Atom.t -> Atom.t

val show_stats : t -> unit
