(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

module Span : sig
  type block = {first: int; last: int} [@@deriving compare, equal]

  type t = Every | Blocks of block list [@@deriving compare, equal]
end

type t = Span.t String.Map.t

val parse_lines : ?file:string -> string list -> t

val is_suppressed : suppressions:t -> issue_type:string -> line:int -> bool
