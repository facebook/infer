(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

module Kind : sig
  type t [@@deriving compare, equal]

  val pp : F.formatter -> t -> unit

  val of_string : string -> t

  val hash : t -> int

  val sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t
end

type origin = Argument of {index: int} | ReturnValue [@@deriving compare, equal]

val pp_origin : F.formatter -> origin -> unit

type t = {kinds: Kind.t list; proc_name: Procname.t; origin: origin; data_flow_only: bool}
[@@deriving compare, equal]

val pp : F.formatter -> t -> unit
