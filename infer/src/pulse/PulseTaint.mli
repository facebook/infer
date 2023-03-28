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

  val sexp_of_t : t -> Sexp.t

  val mark_data_flow_only : t -> unit

  val is_data_flow_only : t -> bool
end

type origin =
  | Argument of {index: int}
  | ReturnValue
  | Allocation of {typ: string}
  | Field of {name: string; origin: origin}
[@@deriving compare, equal]

type t =
  {kinds: Kind.t list; proc_name: Procname.t; origin: origin; block_passed_to: Procname.t option}
[@@deriving compare, equal]

val pp : F.formatter -> t -> unit
