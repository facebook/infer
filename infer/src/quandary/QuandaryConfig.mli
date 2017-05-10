(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

(** utilities for importing JSON specifications of sources/sinks into Quandary*)

module Source : sig
  type t = { procedure : string; kind : string; index : string; }

  val of_json : [> `List of Yojson.Basic.json list ] -> t list
end

module Sink : sig
  type t = { procedure : string; kind : string; index : string; }

  val of_json : [> `List of Yojson.Basic.json list ] -> t list
end

module Endpoint : sig
  type t = string (** name of endpoint class *)

  val of_json : [> `List of Yojson.Basic.json list ] -> t list
end
