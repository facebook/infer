(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Hash table the size of which is limited by LRU *)

module type S = sig
  type key

  type 'a t

  val create : initial_size:int -> max_size:int -> 'a t

  val find_opt : 'a t -> key -> 'a option

  val replace : 'a t -> key -> 'a -> unit

  val remove : 'a t -> key -> unit

  val clear : 'a t -> unit

  val pp :
       pp_key:(Format.formatter -> key -> unit)
    -> pp_v:(Format.formatter -> 'a -> unit)
    -> Format.formatter
    -> 'a t
    -> unit

  val bindings : 'a t -> (key * 'a) list
  (** visible for testing mainly; makes linear number of hashtable lookups *)
end

module Make (Key : Caml.Hashtbl.HashedType) : S with type key = Key.t
