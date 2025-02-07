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
  module Hash : Stdlib.Hashtbl.S

  type key

  type 'a t

  val create : int -> 'a t

  val clear : 'a t -> unit

  val find_opt : 'a t -> key -> 'a option

  val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b

  val iter : (key -> 'a -> unit) -> 'a t -> unit

  val length : 'a t -> int

  val remove : 'a t -> key -> unit

  val replace : 'a t -> key -> 'a -> unit

  val with_hashtable : ('a Hash.t -> 'b) -> 'a t -> 'b
  (** Execute the given function on the underlying hashtable in a critical section *)

  val wrap_hashtable : 'a Hash.t -> 'a t
  (** Put a hashtable into a thread-safe wrapper; original hashtable must not be directly accessed. *)
end

(** a thread safe hashtable *)
module MakeHashtbl (H : Stdlib.Hashtbl.S) : Hashtbl with type key = H.key with module Hash = H
