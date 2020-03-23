(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

include Set_intf

module Make (Elt : sig
  type t [@@deriving compare, sexp_of]
end) : S with type elt = Elt.t = struct
  module EltSet = Core.Set.Make_plain (Elt)
  module Elt = EltSet.Elt

  type elt = Elt.t

  include EltSet.Tree

  let pp pp_elt fs x = List.pp ",@ " pp_elt fs (elements x)

  let pp_diff pp_elt fs (xs, ys) =
    let lose = diff xs ys and gain = diff ys xs in
    if not (is_empty lose) then Format.fprintf fs "-- %a" (pp pp_elt) lose ;
    if not (is_empty gain) then Format.fprintf fs "++ %a" (pp pp_elt) gain

  let of_ x = add empty x
  let of_option = Option.fold ~f:add ~init:empty
  let of_iarray a = of_array (IArray.to_array a)
  let add_option xo s = Option.fold ~f:add ~init:s xo
  let add_list xs s = List.fold ~f:add ~init:s xs
  let diff_inter s t = (diff s t, inter s t)

  let rec disjoint s1 s2 =
    match choose s1 with
    | None -> true
    | _ when is_empty s2 -> true
    | _ when s1 == s2 -> false
    | Some x -> (
        let l1, _, r1 = split s1 x in
        match split s2 x with
        | _, Some _, _ -> false
        | l2, None, r2 -> disjoint l1 l2 && disjoint r1 r2 )
end
