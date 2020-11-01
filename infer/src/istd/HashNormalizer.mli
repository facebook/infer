(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** a hashed type with a normalization function which respects equality *)
module type NormalizedT = sig
  include Caml.Hashtbl.HashedType

  val normalize : t -> t
end

(** normalizer module which uses a hashtable to store normalized representatives *)
module type S = sig
  (** type the normalizer works on *)
  type t

  val normalize : t -> t
  (** return equal normalized representative *)

  val reset : unit -> unit
  (** reset underlying hashtable *)
end

module Make (T : NormalizedT) : S with type t = T.t

(** normalizer for strings *)
module StringNormalizer : S with type t = string
