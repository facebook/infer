(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open NS0
include Set_intf

module Make (Elt : sig
  type t [@@deriving compare, sexp_of]
end) : S with type elt = Elt.t = struct
  module EltSet = Core.Set.Make_plain (Elt)
  module Elt = EltSet.Elt

  type elt = Elt.t

  include EltSet.Tree

  let hash_fold_t hash_fold_elt s m =
    fold ~f:hash_fold_elt ~init:(Hash.fold_int s (length m)) m

  let pp ?pre ?suf ?(sep = (",@ " : (unit, unit) fmt)) pp_elt fs x =
    List.pp ?pre ?suf sep pp_elt fs (elements x)

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

  let choose_exn s =
    let@ {return} = with_return in
    binary_search_segmented s `Last_on_left ~segment_of:return |> ignore ;
    raise (Not_found_s (Atom __LOC__))

  let choose s = try Some (choose_exn s) with Not_found_s _ -> None

  let pop_exn s =
    let elt = choose_exn s in
    (elt, remove s elt)

  let pop s = choose s |> Option.map ~f:(fun elt -> (elt, remove s elt))
end
