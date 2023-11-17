(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Low-level combinators built on [Fmt]. Sophisticated pretty-printing functions should rather go
    in {!Pp}. *)

val colon_sp : 'a Fmt.t
(** [colon_sp] is [Fmt.any ":@ "]. It prints a colon and a space break hint. *)

module Labelled : sig
  (** Adapters of [Fmt] functions using labelled interfaces *)

  val iter : ?sep:unit Fmt.t -> ('a -> f:('elt -> unit) -> unit) -> 'elt Fmt.t -> 'a Fmt.t
    [@@warning "-unused-value-declaration"]
  (** [Fmt.iter] using a Core-style labelled iterator function *)

  val iter_bindings :
       ?sep:unit Fmt.t
    -> ('a -> f:(key:'key -> data:'data -> unit) -> unit)
    -> ('key * 'data) Fmt.t
    -> 'a Fmt.t
  (** [Fmt.iter_bindings] using a Core-style (key, data) labelled iterator function *)
end
