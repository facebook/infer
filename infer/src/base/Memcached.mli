(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

(** Interface for managing a (single) memcached daemon and getting/setting OCaml values *)

val connect : unit -> unit
(** connect to a running memcached server -- only call this from processes which do not fork *)

val disconnect : unit -> unit
(** disconnect after having connected first *)

val start : unit -> unit
(** start a memcached daemon and set up an epilogue to kill it on exit -- only for top-level *)

val flush_all : unit -> unit
(** empty the cache *)

(** type to marshal, plus a unique label that will be colon-prepended to a key, 
    roughly signifying a table *)
module type Value = sig
  type t

  val label : string
end

module type Server = sig
  module Value : Value

  val get : key:string -> Value.t option
  (** get a value, [None] means no [key] exists *)

  val set : key:string -> Value.t -> unit
  (** set a [key]/value pair. NB we swallow failures due to exceeding max value size currently. 
    This will need to be changed before memcached is used as a primary store. *)
end

module Make (V : Value) : Server with module Value = V
