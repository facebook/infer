(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** A thread-safe queue. *)
type 'a t

val create : ?capacity:int -> unit -> 'a t

val enqueue : 'a -> 'a t -> unit

val dequeue : 'a t -> 'a
(** Dequeues an item if available, or blocks until an item is enqueued. *)

val dequeue_opt : 'a t -> 'a option
(** Dequeues an item if available, does not block. *)

val wait_until_non_empty : 'a t -> unit
