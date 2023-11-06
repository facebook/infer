(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

[@@@warning "-32-60"]

type 'a t = Unsat | Sat of 'a [@@deriving equal]

val pp : (F.formatter -> 'a -> unit) -> F.formatter -> 'a t -> unit

(** for [open]ing to get [Sat] and [Unsat] in the namespace *)
module Types : sig
  type nonrec 'a sat_unsat_t = 'a t = Unsat | Sat of 'a
end

val map : ('a -> 'b) -> 'a t -> 'b t

val bind : ('a -> 'b t) -> 'a t -> 'b t

val sat : 'a t -> 'a option

val of_option : 'a option -> 'a t

val list_fold : 'a list -> init:'accum -> f:('accum -> 'a -> 'accum t) -> 'accum t

val to_list : 'a t -> 'a list

val filter : 'a t list -> 'a list
(** keep only [Sat _] elements *)

val seq_fold : 'a Caml.Seq.t -> init:'accum -> f:('accum -> 'a -> 'accum t) -> 'accum t

module Import : sig
  include module type of Types

  val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t

  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t

  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t

  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
end
