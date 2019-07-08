(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** utilities for importing JSON specifications of sources/sinks into Quandary*)

module Source : sig
  type t = {procedure: string; kinds: string list; index: string}

  val of_json : [> `List of Yojson.Basic.t list] -> t list
end

module Sink : sig
  type t = {procedure: string; kinds: string list; index: string}

  val of_json : [> `List of Yojson.Basic.t list] -> t list
end

module Sanitizer : sig
  type t = {procedure: string; kind: string}

  val of_json : [> `List of Yojson.Basic.t list] -> t list
end

val is_endpoint : string -> bool
