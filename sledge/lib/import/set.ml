(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Import0
include Set_intf

module Make (Elt : OrderedType) : S with type elt = Elt.t = struct
  module S = Caml.Set.Make (Elt)

  type elt = Elt.t
  type t = S.t

  let compare = S.compare
  let equal = S.equal
  let sexp_of_t s = List.sexp_of_t Elt.sexp_of_t (S.elements s)

  let t_of_sexp elt_of_sexp sexp =
    S.of_list (List.t_of_sexp elt_of_sexp sexp)

  let pp pp_elt fs x = List.pp ",@ " pp_elt fs (S.elements x)

  let pp_diff pp_elt fs (xs, ys) =
    let lose = S.diff xs ys and gain = S.diff ys xs in
    if not (S.is_empty lose) then Format.fprintf fs "-- %a" (pp pp_elt) lose ;
    if not (S.is_empty gain) then Format.fprintf fs "++ %a" (pp pp_elt) gain

  let empty = S.empty
  let of_ x = S.add x empty
  let of_option = Option.fold ~f:(fun x y -> S.add y x) ~init:empty
  let of_list = S.of_list
  let of_vector x = S.of_list (IArray.to_list x)
  let add s e = S.add e s
  let add_option yo x = Option.fold ~f:(fun x y -> S.add y x) ~init:x yo
  let add_list ys x = List.fold ~f:(fun x y -> S.add y x) ~init:x ys
  let remove s e = S.remove e s
  let filter s ~f = S.filter f s
  let union = S.union
  let union_list ss = List.fold ss ~init:empty ~f:union
  let diff = S.diff
  let inter = S.inter
  let diff_inter x y = (S.diff x y, S.inter x y)
  let is_empty = S.is_empty
  let mem s e = S.mem e s
  let is_subset x ~of_ = S.subset x of_
  let disjoint = S.disjoint
  let max_elt = S.max_elt_opt
  let fold s ~init:z ~f = S.fold (fun z x -> f x z) s z
end
