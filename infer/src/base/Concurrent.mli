(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** A thread-safe queue. *)
module Queue : sig
  type 'a t

  val create : ?capacity:int -> unit -> 'a t

  val enqueue : 'a -> 'a t -> unit

  val dequeue : 'a t -> 'a
  (** Dequeues an item if available, or blocks until an item is enqueued. *)

  val dequeue_opt : 'a t -> 'a option
  (** Dequeues an item if available, does not block. *)

  val wait_until_non_empty : 'a t -> unit
end

module type Map = sig
  type key

  type 'a t

  val empty : unit -> 'a t

  val clear : 'a t -> unit

  val add : 'a t -> key -> 'a -> unit

  val filter : 'a t -> (key -> 'a -> bool) -> unit

  val find_opt : 'a t -> key -> 'a option

  val remove : 'a t -> key -> unit
end

(** a simple thread safe map that uses an atomic reference to a persistent map plus a mutex to
    sequentialize updates *)
module MakeMap (M : Stdlib.Map.S) : Map with type key = M.key

module type Hashtbl = sig
  type key

  type 'a t

  val create : int -> 'a t

  val clear : 'a t -> unit

  val replace : 'a t -> key -> 'a -> unit

  val find_opt : 'a t -> key -> 'a option

  val remove : 'a t -> key -> unit
end

(** a thread safe hashtable *)
module MakeHashtbl (Hash : Stdlib.Hashtbl.S) : Hashtbl with type key = Hash.key
