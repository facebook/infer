(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format

(** An abstract value (or "symbolic variable"), eg an address in memory. *)
type t = private int [@@deriving compare, equal, hash, yojson_of]

val mk_fresh : unit -> t
(** create an abstract value guaranteed not to appear in the current state *)

val mk_fresh_restricted : unit -> t
(** a special class of variables that represent non-negative ("restricted") values; variables
    returned by [mk_fresh] are called "unrestricted" by opposition *)

val mk_fresh_same_kind : t -> t
(** creates a fresh restricted or unrestricted abstract value based on the kind of abstract value
    given *)

val is_restricted : t -> bool
(** was the variable created with [mk_fresh_restricted], i.e. it represents non-negative values
    (hence its domain is {e restricted}) *)

val is_unrestricted : t -> bool
(** was the variable created with [mk_fresh], i.e. it represents any value, positive, negative, or
    zero (hence its domain is {e unrestricted}) *)

val pp : F.formatter -> t -> unit

val compare_unrestricted_first : t -> t -> int
(** an alternative comparison function that sorts unrestricted variables before restricted variables *)

module Set : PrettyPrintable.PPSet with type elt = t

module Map : sig
  include PrettyPrintable.PPMap with type key = t

  val yojson_of_t : ('a -> Yojson.Safe.t) -> 'a t -> Yojson.Safe.t
end
