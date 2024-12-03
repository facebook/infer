(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

module Span : sig
  type block = {first: int; last: int} [@@deriving compare, equal]

  type t = Every | Blocks of block list [@@deriving compare, equal]
end

type t = Span.t IString.Map.t

type error = UserError of (unit -> string)

val parse_lines : ?file:string -> string list -> t * error list

val is_suppressed : suppressions:t -> issue_type:string -> line:int -> bool

val pp_parse_result : F.formatter -> t * error list -> unit [@@warning "-unused-value-declaration"]
(* used in unit tests *)
