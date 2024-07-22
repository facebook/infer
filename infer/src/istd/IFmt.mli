(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type 'a t := 'a Fmt.t

(** Low-level combinators built on [Fmt]. Sophisticated pretty-printing functions should rather go
    in {!Pp}. *)

val colon_sp : 'a t
(** [colon_sp] is [Fmt.any ":@ "]. It prints a colon and a space break hint. *)

val if' : bool -> 'a t -> 'a t
[@@warning "-unused-value-declaration"]
(** Should be directly in Fmt >=0.9.1.

    [if' bool pp] is [pp] if [bool] is [true] and {!Fmt.nop} otherwise. *)

module Labelled : sig
  (** Adapters of [Fmt] functions using labelled interfaces *)

  val iter : ?sep:unit t -> ('a -> f:('elt -> unit) -> unit) -> 'elt t -> 'a t
  [@@warning "-unused-value-declaration"]
  (** [Fmt.iter] using a Core-style labelled iterator function *)

  val iter_bindings :
    ?sep:unit t -> ('a -> f:(key:'key -> data:'data -> unit) -> unit) -> ('key * 'data) t -> 'a t
  (** [Fmt.iter_bindings] using a Core-style (key, data) labelled iterator function *)
end
