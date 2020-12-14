(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

[@@@warning "-32-60"]

type 'a t = Unsat | Sat of 'a

(** for [open]ing to get [Sat] and [Unsat] in the namespace *)
module Types : sig
  type nonrec 'a sat_unsat_t = 'a t = Unsat | Sat of 'a
end

val map : ('a -> 'b) -> 'a t -> 'b t

val bind : ('a -> 'b t) -> 'a t -> 'b t

module Import : sig
  include module type of Types

  val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t

  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t

  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t

  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
end
