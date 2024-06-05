(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Hash set interface for mutably constructing and iterating over unordered collections. *)

module type S = sig
  type elt

  type t

  val create : int -> t

  val singleton : elt -> t

  val add : elt -> t -> unit

  val remove : elt -> t -> unit

  val remove_all : elt Iter.t -> t -> unit

  val iter : t -> elt Iter.t

  val seq : t -> elt Seq.t

  val of_seq : elt Seq.t -> t

  val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a

  val length : t -> int

  val mem : t -> elt -> bool

  val clear : t -> unit

  val union_into : into:t -> t -> unit

  val is_empty : t -> bool
end

module Make (Key : Caml.Hashtbl.HashedType) : S with type elt = Key.t
