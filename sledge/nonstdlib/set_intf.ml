(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! NS0

module type S = sig
  type elt
  type t [@@deriving compare, equal, sexp_of]

  module Provide_of_sexp (_ : sig
    type t = elt [@@deriving of_sexp]
  end) : sig
    val t_of_sexp : Sexp.t -> t
  end

  (** {1 Construct} *)

  val empty : t
  val of_ : elt -> t
  val of_option : elt option -> t
  val of_list : elt list -> t
  val add : t -> elt -> t
  val add_option : elt option -> t -> t
  val add_list : elt list -> t -> t
  val diff : t -> t -> t
  val inter : t -> t -> t
  val union : t -> t -> t
  val diff_inter : t -> t -> t * t
  val union_list : t list -> t

  (** {1 Query} *)

  val is_empty : t -> bool
  val cardinal : t -> int
  val mem : t -> elt -> bool
  val is_subset : t -> of_:t -> bool
  val disjoint : t -> t -> bool
  val max_elt : t -> elt option
  val only_elt : t -> elt option

  val pop_exn : t -> elt * t
  (** Find and remove an unspecified element. [O(1)]. *)

  val elements : t -> elt list

  (** {1 Transform} *)

  val map : t -> f:(elt -> elt) -> t
  val filter : t -> f:(elt -> bool) -> t

  (** {1 Traverse} *)

  val iter : t -> f:(elt -> unit) -> unit
  val exists : t -> f:(elt -> bool) -> bool
  val for_all : t -> f:(elt -> bool) -> bool
  val fold : t -> init:'a -> f:('a -> elt -> 'a) -> 'a

  (** {1 Pretty-print} *)

  val pp :
       ?pre:(unit, unit) fmt
    -> ?suf:(unit, unit) fmt
    -> ?sep:(unit, unit) fmt
    -> elt pp
    -> t pp

  val pp_diff : elt pp -> (t * t) pp
end
