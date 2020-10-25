(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! NS0
include Map_intf

module Make (Key : sig
  type t [@@deriving compare, sexp_of]
end) : S with type key = Key.t = struct
  module M = CCMap.Make (Key)

  type key = Key.t
  type 'a t = 'a M.t [@@deriving compare, equal]

  let sexp_of_t sexp_of_data m =
    M.to_list m
    |> Sexplib.Conv.sexp_of_list
         (Sexplib.Conv.sexp_of_pair Key.sexp_of_t sexp_of_data)

  module Provide_of_sexp (Key : sig
    type t = key [@@deriving of_sexp]
  end) =
  struct
    let t_of_sexp data_of_sexp s =
      s
      |> Sexplib.Conv.list_of_sexp
           (Sexplib.Conv.pair_of_sexp Key.t_of_sexp data_of_sexp)
      |> M.of_list
  end

  let empty = M.empty
  let singleton = M.singleton

  let add_exn ~key ~data m =
    assert (not (M.mem key m)) ;
    M.add key data m

  let add ~key ~data m = M.add key data m

  let add_multi ~key ~data m =
    M.update key
      (function Some vs -> Some (data :: vs) | None -> Some [data])
      m

  let remove key m = M.remove key m
  let merge l r ~f = M.merge_safe l r ~f

  let merge_endo t u ~f =
    let change = ref false in
    let t' =
      merge t u ~f:(fun key side ->
          let f_side = f key side in
          ( match (side, f_side) with
          | (`Both (data, _) | `Left data), Some data' when data' == data ->
              ()
          | _ -> change := true ) ;
          f_side )
    in
    if !change then t' else t

  let union x y ~f = M.union f x y
  let partition m ~f = M.partition f m
  let is_empty = M.is_empty

  let root_key m =
    let exception Found in
    let found = ref None in
    try
      M.find_first
        (fun key ->
          found := Some key ;
          raise Found )
        m
      |> ignore ;
      None
    with
    | Found -> !found
    | Not_found -> None

  let root_binding m =
    let exception Found in
    let found = ref None in
    try
      M.for_all
        (fun key data ->
          found := Some (key, data) ;
          raise Found )
        m
      |> ignore ;
      None
    with
    | Found -> !found
    | Not_found -> None

  let is_singleton m =
    match root_key m with
    | Some k ->
        let l, _, r = M.split k m in
        is_empty l && is_empty r
    | None -> false

  let length = M.cardinal

  let only_binding m =
    match root_key m with
    | Some k -> (
      match M.split k m with
      | l, Some v, r when is_empty l && is_empty r -> Some (k, v)
      | _ -> None )
    | None -> None

  let classify m =
    match root_key m with
    | None -> `Zero
    | Some k -> (
      match M.split k m with
      | l, Some v, r when is_empty l && is_empty r -> `One (k, v)
      | _ -> `Many )

  let choose_key = root_key
  let choose = root_binding
  let choose_exn m = Option.get_exn (choose m)
  let min_binding = M.min_binding_opt
  let mem k m = M.mem k m
  let find_exn k m = M.find k m
  let find k m = M.find_opt k m

  let find_multi k m =
    match M.find_opt k m with None -> [] | Some vs -> vs

  let find_and_remove k m =
    let found = ref None in
    let m =
      M.update k
        (fun v ->
          found := v ;
          None )
        m
    in
    Option.map ~f:(fun v -> (v, m)) !found

  let pop m = choose m |> Option.map ~f:(fun (k, v) -> (k, v, remove k m))

  let pop_min_binding m =
    min_binding m |> Option.map ~f:(fun (k, v) -> (k, v, remove k m))

  let change k m ~f = M.update k f m
  let update k m ~f = M.update k (fun v -> Some (f v)) m
  let map m ~f = M.map f m
  let mapi m ~f = M.mapi (fun key data -> f ~key ~data) m
  let map_endo t ~f = map_endo map t ~f
  let filter_mapi m ~f = M.filter_map (fun key data -> f ~key ~data) m
  let iter m ~f = M.iter (fun _ data -> f data) m
  let iteri m ~f = M.iter (fun key data -> f ~key ~data) m
  let existsi m ~f = M.exists (fun key data -> f ~key ~data) m
  let for_alli m ~f = M.for_all (fun key data -> f ~key ~data) m
  let fold m s ~f = M.fold (fun key data acc -> f ~key ~data acc) m s
  let keys = M.keys
  let values = M.values
  let to_iter = M.to_iter
  let to_list = M.to_list
  let of_iter = M.of_iter
  let of_list = M.of_list

  let to_iter2 l r =
    let seq = ref Iter.empty in
    M.merge_safe l r ~f:(fun k vv ->
        seq := Iter.cons (k, vv) !seq ;
        None )
    |> ignore ;
    !seq

  let symmetric_diff l r ~eq =
    Iter.filter_map (to_iter2 l r) ~f:(fun (k, vv) ->
        match vv with
        | `Both (lv, rv) when eq lv rv -> None
        | `Both vv -> Some (k, `Unequal vv)
        | `Left lv -> Some (k, `Left lv)
        | `Right rv -> Some (k, `Right rv) )

  let pp pp_k pp_v fs m =
    Format.fprintf fs "@[<1>[%a]@]"
      (List.pp ",@ " (fun fs (k, v) ->
           Format.fprintf fs "@[%a@ @<2>↦ %a@]" pp_k k pp_v v ))
      (Iter.to_list (to_iter m))

  let pp_diff pp_key pp_val pp_diff_val ~eq fs (x, y) =
    let pp_diff_elt fs = function
      | k, `Left v ->
          Format.fprintf fs "-- [@[%a@ @<2>↦ %a@]]" pp_key k pp_val v
      | k, `Right v ->
          Format.fprintf fs "++ [@[%a@ @<2>↦ %a@]]" pp_key k pp_val v
      | k, `Unequal vv ->
          Format.fprintf fs "[@[%a@ @<2>↦ %a@]]" pp_key k pp_diff_val vv
    in
    let sd = Iter.to_list (symmetric_diff ~eq x y) in
    if not (List.is_empty sd) then
      Format.fprintf fs "[@[<hv>%a@]];@ " (List.pp ";@ " pp_diff_elt) sd
end
