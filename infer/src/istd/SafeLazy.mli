(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** A wrapper around a lazy value that can be forced in a thread-safe manner. NB values can only be
    marshalled if they were created by [from_val] or they have already been [force]d. *)
type 'a t

val make : 'a Lazy.t -> 'a t

val from_val : 'a -> 'a t

val from_val_option : 'a option -> 'a t option

val force : 'a t -> 'a

val force_option : 'a t option -> 'a option
