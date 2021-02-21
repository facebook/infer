(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! NS0

module type S = sig
  type elt
  type compare_elt
  type t [@@deriving compare, equal, sexp_of]

  include Comparer.S with type t := t

  module Provide_hash (_ : sig
    type t = elt [@@deriving hash]
  end) : sig
    type t [@@deriving hash]
  end
  with type t := t

  module Provide_of_sexp (_ : sig
    type t = elt [@@deriving of_sexp]
  end) : sig
    type t [@@deriving of_sexp]
  end
  with type t := t

  (** {1 Pretty-print} *)

  val pp_full :
       ?pre:(unit, unit) fmt
    -> ?suf:(unit, unit) fmt
    -> ?sep:(unit, unit) fmt
    -> elt pp
    -> t pp

  module Provide_pp (_ : sig
    type t = elt

    val pp : t pp
  end) : sig
    val pp : t pp
    val pp_diff : (t * t) pp
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
  val diff_inter_diff : t -> t -> t * t * t
  val union_list : t list -> t

  (** {1 Query} *)

  val is_empty : t -> bool
  val cardinal : t -> int
  val mem : elt -> t -> bool
  val subset : t -> of_:t -> bool
  val disjoint : t -> t -> bool
  val max_elt : t -> elt option
  val only_elt : t -> elt option
  val classify : t -> elt zero_one_many

  val choose : t -> elt option
  (** Find an unspecified element. Different elements may be chosen for
      equivalent sets. [O(1)]. *)

  val choose_exn : t -> elt
  (** Find an unspecified element. Different elements may be chosen for
      equivalent sets. [O(1)]. *)

  val pop : t -> (elt * t) option
  (** Find and remove an unspecified element. [O(1)]. *)

  val pop_exn : t -> elt * t
  (** Find and remove an unspecified element. [O(1)]. *)

  (** {1 Transform} *)

  val map : t -> f:(elt -> elt) -> t
  val flat_map : t -> f:(elt -> t) -> t
  val filter : t -> f:(elt -> bool) -> t
  val partition : t -> f:(elt -> bool) -> t * t

  (** {1 Traverse} *)

  val iter : t -> f:(elt -> unit) -> unit
  val exists : t -> f:(elt -> bool) -> bool
  val for_all : t -> f:(elt -> bool) -> bool
  val fold : t -> 's -> f:(elt -> 's -> 's) -> 's
  val reduce : t -> f:(elt -> elt -> elt) -> elt option

  (** {1 Convert} *)

  val to_list : t -> elt list
  val to_iter : t -> elt iter
  val of_iter : elt iter -> t
end
