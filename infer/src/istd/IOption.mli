(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! Core

val value_default_f : f:(unit -> 'a) -> 'a option -> 'a
(** Like [Option.value ~default:(f ())] but [f] is called only if [None]. *)

val if_none_evalopt : f:(unit -> 'a option) -> 'a option -> 'a option
(** [if_none_evalopt ~f x] evaluates to [f ()] if [x = None], otherwise returns [x]. Useful for
    chaining matchers where the first returning non-[None] determines the result. *)

val if_none_eval : f:(unit -> 'a) -> 'a option -> 'a
(** [if_none_eval ~f x] evaluates to [y] if [x=Some y] else to [f ()]. Useful for terminating chains
    built with [if_none_evalopt]. This is exactly the same as [value_default_f] but with a better
    name. *)

val exists2 : 'a option -> 'b option -> f:('a -> 'b -> bool) -> bool
(** Like [Option.exists] but gets two parameters. *)

val iter2 : 'a option -> 'b option -> f:('a -> 'b -> unit) -> unit
(** Like [Option.iter] but gets two parameters. *)

val map_changed : 'a option -> equal:('a -> 'a -> bool) -> f:('a -> 'a) -> 'a option
(** Like [Option.map] but maintain physical equality *)

val continue : default:'a -> 'a option -> ('a -> 'a option) -> 'a option
(** Like [Option.bind] but keeps continuing the further evaluation with [default] input when the
    first parameter is [None]. *)

include sig
  [@@@warning "-32-60"]

  (** Provides signatures for OCaml 4.08 binding operators *)
  module Let_syntax : sig
    include module type of Option.Let_syntax

    val ( let+ ) : 'a option -> ('a -> 'b) -> 'b option

    val ( and+ ) : 'a option -> 'b option -> ('a * 'b) option

    val ( let* ) : 'a option -> ('a -> 'b option) -> 'b option

    val ( and* ) : 'a option -> 'b option -> ('a * 'b) option
  end
end
