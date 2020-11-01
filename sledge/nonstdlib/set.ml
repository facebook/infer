(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! NS0
include Set_intf

module Make (Elt : sig
  type t [@@deriving compare, sexp_of]
end) : S with type elt = Elt.t = struct
  module S = CCSet.Make (Elt)

  type elt = Elt.t
  type t = S.t [@@deriving compare, equal]

  let sexp_of_t s = S.to_list s |> Sexplib.Conv.sexp_of_list Elt.sexp_of_t

  module Provide_of_sexp (Elt : sig
    type t = elt [@@deriving of_sexp]
  end) =
  struct
    let t_of_sexp s =
      s |> Sexplib.Conv.list_of_sexp Elt.t_of_sexp |> S.of_list
  end

  let empty = S.empty
  let of_ = S.singleton
  let of_option xo = Option.map_or ~f:S.singleton xo ~default:empty
  let of_list = S.of_list
  let add x s = S.add x s
  let add_option = Option.fold ~f:add
  let add_list xs s = S.add_list s xs
  let remove x s = S.remove x s
  let diff = S.diff
  let inter = S.inter
  let union = S.union
  let diff_inter s t = (diff s t, inter s t)
  let union_list ss = List.fold ~f:union ss empty
  let is_empty = S.is_empty
  let cardinal = S.cardinal
  let mem = S.mem
  let subset s ~of_:t = S.subset s t
  let disjoint = S.disjoint
  let max_elt = S.max_elt_opt

  let root_elt s =
    let exception Found in
    let found = ref None in
    try
      S.for_all
        (fun elt ->
          found := Some elt ;
          raise Found )
        s
      |> ignore ;
      None
    with Found -> !found

  let choose = root_elt
  let choose_exn m = Option.get_exn (choose m)

  let only_elt s =
    match root_elt s with
    | Some elt -> (
      match S.split elt s with
      | l, _, r when is_empty l && is_empty r -> Some elt
      | _ -> None )
    | None -> None

  let classify s =
    match root_elt s with
    | None -> `Zero
    | Some elt -> (
      match S.split elt s with
      | l, true, r when is_empty l && is_empty r -> `One elt
      | _ -> `Many )

  let pop_exn s =
    let elt = choose_exn s in
    (elt, S.remove elt s)

  let map s ~f = S.map f s
  let filter s ~f = S.filter f s
  let iter s ~f = S.iter f s
  let exists s ~f = S.exists f s
  let for_all s ~f = S.for_all f s
  let fold s z ~f = S.fold f s z
  let to_iter = S.to_iter
  let of_iter = S.of_iter

  let pp ?pre ?suf ?(sep = (",@ " : (unit, unit) fmt)) pp_elt fs x =
    List.pp ?pre ?suf sep pp_elt fs (S.elements x)

  let pp_diff pp_elt fs (xs, ys) =
    let lose = diff xs ys and gain = diff ys xs in
    if not (is_empty lose) then Format.fprintf fs "-- %a" (pp pp_elt) lose ;
    if not (is_empty gain) then Format.fprintf fs "++ %a" (pp pp_elt) gain
end
