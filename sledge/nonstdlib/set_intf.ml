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
  val add : elt -> t -> t
  val add_option : elt option -> t -> t
  val add_list : elt list -> t -> t
  val remove : elt -> t -> t
  val diff : t -> t -> t
  val inter : t -> t -> t
  val union : t -> t -> t
  val diff_inter : t -> t -> t * t
  val union_list : t list -> t

  (** {1 Query} *)

  val is_empty : t -> bool
  val cardinal : t -> int
  val mem : elt -> t -> bool
  val subset : t -> of_:t -> bool
  val disjoint : t -> t -> bool
  val max_elt : t -> elt option
  val only_elt : t -> elt option
  val classify : t -> [`Zero | `One of elt | `Many]

  val pop_exn : t -> elt * t
  (** Find and remove an unspecified element. [O(1)]. *)

  (** {1 Transform} *)

  val map : t -> f:(elt -> elt) -> t
  val filter : t -> f:(elt -> bool) -> t

  (** {1 Traverse} *)

  val iter : t -> f:(elt -> unit) -> unit
  val exists : t -> f:(elt -> bool) -> bool
  val for_all : t -> f:(elt -> bool) -> bool
  val fold : t -> 's -> f:(elt -> 's -> 's) -> 's

  (** {1 Convert} *)

  val to_iter : t -> elt iter
  val of_iter : elt iter -> t

  (** {1 Pretty-print} *)

  val pp :
       ?pre:(unit, unit) fmt
    -> ?suf:(unit, unit) fmt
    -> ?sep:(unit, unit) fmt
    -> elt pp
    -> t pp

  val pp_diff : elt pp -> (t * t) pp
end
