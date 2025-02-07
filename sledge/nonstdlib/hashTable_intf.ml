(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! NS0

module type HashedType = Stdlib.Hashtbl.HashedType

module type S = sig
  include CCHashtbl.S

  val create : ?size:int -> unit -> 'a t
  val set : 'a t -> key:key -> data:'a -> unit
  val add_multi : 'a list t -> key:key -> data:'a -> unit
  val update : 'a t -> key -> f:('a option -> 'a option) -> unit
  val find_exn : 'a t -> key -> 'a
  val find : 'a t -> key -> 'a option
  val find_or_add : 'a t -> key -> default:(unit -> 'a) -> 'a
  val iteri : 'a t -> f:(key:key -> data:'a -> unit) -> unit
  val fold : 'a t -> 's -> f:(key:key -> data:'a -> 's -> 's) -> 's
end
