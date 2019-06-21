(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

exception MaximumSharingLazyValue

(**
  Current implementation will stack overflow on deep (TODO: a tailrec version)
  or circular values (much harder to detect sharing, also not needed for now).
*)

module ForHashtbl (H : Caml.Hashtbl.S) : sig
  val normalize : 'a H.t -> 'a H.t
  (** Duplicate a hash table with maximum sharing. *)
end
