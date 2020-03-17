(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Import0

module type S = sig
  type elt
  type t

  val compare : t -> t -> int
  val equal : t -> t -> bool
  val sexp_of_t : t -> Sexp.t
  val t_of_sexp : (Sexp.t -> elt) -> Sexp.t -> t
  val pp : elt pp -> t pp
  val pp_diff : elt pp -> (t * t) pp

  (* initial constructors *)
  val empty : t
  val of_ : elt -> t
  val of_option : elt option -> t
  val of_list : elt list -> t
  val of_vector : elt IArray.t -> t

  (* constructors *)
  val add : t -> elt -> t
  val add_option : elt option -> t -> t
  val add_list : elt list -> t -> t
  val remove : t -> elt -> t
  val filter : t -> f:(elt -> bool) -> t
  val union : t -> t -> t
  val union_list : t list -> t
  val diff : t -> t -> t
  val inter : t -> t -> t
  val diff_inter : t -> t -> t * t

  (* queries *)
  val is_empty : t -> bool
  val mem : t -> elt -> bool
  val is_subset : t -> of_:t -> bool
  val disjoint : t -> t -> bool
  val max_elt : t -> elt option

  (* traversals *)
  val fold : t -> init:'s -> f:('s -> elt -> 's) -> 's
end
