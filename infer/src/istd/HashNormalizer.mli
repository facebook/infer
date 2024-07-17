(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! Core

(** a hashed type with a normalization function which respects equality *)
module type NormalizedT = sig
  include Caml.Hashtbl.HashedType

  val normalize : t -> t
end

(** normalizer module which uses a hashtable to store normalized representatives *)
module type S = sig
  (** type the normalizer works on *)
  type t

  val hash_normalize : t -> t
  (** return equal normalized representative *)

  val hash_normalize_opt : t option -> t option
  (** helper for normalizing options; does not store option value in hashtable *)

  val hash_normalize_list : t list -> t list
  (** helper for normalizing lists; does not store list (or sublists) in hashtable *)
end

module Make (T : NormalizedT) : S with type t = T.t

(** normalizer for strings, lists are recursively normalized *)
module String : S with type t = string

module Int64 : S with type t = int64

val reset_all_normalizers : unit -> unit
(** reset hashtables in all normalizers made with [Make] *)

val register_reset : (unit -> unit) -> unit
(** register a function to run when [reset_all_normalizers] is run *)
