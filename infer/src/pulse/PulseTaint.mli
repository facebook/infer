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

  val of_string : string -> t

  val hash : t -> int

  val sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t
end

type origin = Argument of {index: int} | ReturnValue

type t = {kind: Kind.t; proc_name: Procname.t; origin: origin} [@@deriving compare, equal]

val pp : F.formatter -> t -> unit
