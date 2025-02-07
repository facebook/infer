(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** A wrapper around a lazy value that can be forced in a thread-safe manner. NB values made with
    [make] cannot be marshalled, but values made with [from_val] can. *)
type 'a t

val make : 'a Lazy.t -> 'a t

val from_val : 'a -> 'a t

val from_val_option : 'a option -> 'a t option

val force : 'a t -> 'a [@@warning "-unused-value-declaration"]

val force_option : 'a t option -> 'a option

val freeze : 'a t -> 'a t
(** Convert an existing value into one that can be marshalled, forcing the underlying lazy
    expression in the process. *)
