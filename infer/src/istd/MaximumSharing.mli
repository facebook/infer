(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(**
  Current implementation will stack overflow on deep values (TODO: a tailrec version).
*)

module Sharer : sig
  type t

  val create : unit -> t

  val normalize_value : t -> 'a -> 'a
end

module ForHashtbl (H : Caml.Hashtbl.S) : sig
  val normalize : 'a H.t -> 'a H.t
  (** Duplicate a hash table with maximum sharing. *)
end
