(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format

(** An abstract value, eg an address in memory. *)
type t = private int [@@deriving compare, yojson_of]

val equal : t -> t -> bool

val mk_fresh : unit -> t

val pp : F.formatter -> t -> unit

val of_id : int -> t

module Constants : sig
  val get_int : IntLit.t -> t
  (** Get or create an abstract value associated with a constant {!IntLit.t}. The idea is that
      clients will record in the abstract state that the returned [t] is equal to the given integer.
      If the same integer is queried later on then this module will return the same abstract
      variable. *)
end

module Set : PrettyPrintable.PPSet with type elt = t

module Map : sig
  include PrettyPrintable.PPMap with type key = t

  val yojson_of_t : ('a -> Yojson.Safe.t) -> 'a t -> Yojson.Safe.t
end

(** internal state of the module

    Under the hood a "next fresh" reference counter is maintained to produce fresh [t]. The
    [Constants] module also remembers a mapping from certain constants to their corresponding [t].
    Both of these should be per-procedure only so internal state bookkeeping has to be performed by
    the interprocedural analysis. *)
module State : sig
  type t

  val get : unit -> t

  val set : t -> unit

  val reset : unit -> unit
end
