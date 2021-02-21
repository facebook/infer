(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! NS0
include NSMap_intf

type ('key, +'a, 'compare_key) t = ('key, 'a, 'compare_key) Map.t
[@@deriving compare, equal, sexp]

type ('compare_key, 'compare_a) compare =
  ('compare_key, 'compare_a) Map.compare
[@@deriving compare, equal, sexp]

module Make_from_Comparer (Key : sig
  type t [@@deriving equal, sexp_of]

  include Comparer.S with type t := t
end) =
struct
  module M = Map.Make [@inlined] (Key)

  type key = Key.t
  type compare_key = Key.compare
  type 'a t = 'a M.t [@@deriving compare]

  type 'compare_a compare = 'compare_a M.compare
  [@@deriving compare, equal, sexp]

  let comparer = M.comparer

  include M.Provide_equal (Key)
  include M.Provide_sexp_of (Key)
  module Provide_of_sexp = M.Provide_of_sexp

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

  let merge l r ~f =
    let combine k lo ro =
      match (lo, ro) with
      | Some lv, Some rv -> f k (`Both (lv, rv))
      | Some lv, None -> f k (`Left lv)
      | None, Some rv -> f k (`Right rv)
      | None, None -> None
    in
    M.merge combine l r

  let merge_endo l r ~f =
    let change = ref false in
    let l' =
      merge l r ~f:(fun key side ->
          let f_side = f key side in
          ( match (side, f_side) with
          | (`Both (data, _) | `Left data), Some data' when data' == data ->
              ()
          | _ -> change := true ) ;
          f_side )
    in
    if !change then l' else l

  let union x y ~f = M.union f x y

  let union_absent t u =
    let change = ref false in
    let t' =
      M.merge
        (fun _ v1 v2 ->
          match v1 with
          | Some _ -> v1
          | None ->
              change := true ;
              v2 )
        t u
    in
    if !change then t' else t

  let partition m ~f = M.partition f m

  let partition_map m ~f =
    M.fold
      (fun k v (l, r) ->
        match (f k v : _ Either.t) with
        | Left a -> (M.add k a l, r)
        | Right b -> (l, M.add k b r) )
      m (empty, empty)

  let is_empty = M.is_empty
  let is_singleton = M.is_singleton
  let length = M.cardinal
  let only_binding = M.only_binding
  let classify = M.classify
  let choose = M.choose_opt
  let choose_exn = M.choose
  let min_binding = M.min_binding_opt
  let mem k m = M.mem k m
  let find_exn k m = M.find k m
  let find k m = M.find_opt k m

  let find_multi k m =
    match M.find_opt k m with None -> [] | Some vs -> vs

  let find_update x m ~f =
    let found = ref None in
    let m =
      M.update x
        (function
          | None -> f None
          | some_v ->
              found := some_v ;
              f some_v )
        m
    in
    (!found, m)

  let find_and_remove = find_update ~f:(fun _ -> None)

  let find_or_add k v m =
    find_update k ~f:(function None -> Some v | some_v -> some_v) m

  let pop_min_binding m =
    min_binding m |> Option.map ~f:(fun (k, v) -> (k, v, remove k m))

  let update k m ~f = M.update k f m
  let map m ~f = M.map f m
  let mapi m ~f = M.mapi (fun key data -> f ~key ~data) m
  let map_endo t ~f = map_endo map t ~f
  let filter_mapi m ~f = M.filter_map (fun key data -> f ~key ~data) m
  let iter m ~f = M.iter (fun _ data -> f data) m
  let iteri m ~f = M.iter (fun key data -> f ~key ~data) m
  let existsi m ~f = M.exists (fun key data -> f ~key ~data) m
  let for_alli m ~f = M.for_all (fun key data -> f ~key ~data) m
  let fold m s ~f = M.fold (fun key data acc -> f ~key ~data acc) m s
  let keys m = Iter.from_iter (fun f -> M.iter (fun k _ -> f k) m)
  let values m = Iter.from_iter (fun f -> M.iter (fun _ v -> f v) m)
  let to_iter m = Iter.from_iter (fun f -> M.iter (fun k v -> f (k, v)) m)
  let of_iter s = Iter.fold s M.empty ~f:(fun (k, v) m -> M.add k v m)
  let to_list = M.bindings
  let of_list l = List.fold_left l M.empty ~f:(fun m (k, v) -> M.add k v m)

  let symmetric_diff l r ~eq =
    let seq = ref Iter.empty in
    let yield x = seq := Iter.cons x !seq in
    merge l r ~f:(fun k vv ->
        ( match vv with
        | `Both (lv, rv) when eq lv rv -> ()
        | `Both vv -> yield (k, `Unequal vv)
        | `Left lv -> yield (k, `Left lv)
        | `Right rv -> yield (k, `Right rv) ) ;
        None )
    |> ignore ;
    !seq

  let pp pp_k pp_v fs m =
    Format.fprintf fs "@[<1>[%a]@]"
      (List.pp ",@ " (fun fs (k, v) ->
           Format.fprintf fs "@[%a@ @<2>↦ %a@]" pp_k k pp_v v ))
      (Iter.to_list (to_iter m))

  let pp_diff ?(pre = ("[@[<hv>" : (unit, unit) fmt))
      ?(suf = ("@]];@ " : (unit, unit) fmt))
      ?(sep = (";@ " : (unit, unit) fmt)) pp_key pp_val pp_diff_val ~eq fs
      (x, y) =
    let pp_diff_elt fs = function
      | k, `Left v ->
          Format.fprintf fs "-- [@[%a@ @<2>↦ %a@]]" pp_key k pp_val v
      | k, `Right v ->
          Format.fprintf fs "++ [@[%a@ @<2>↦ %a@]]" pp_key k pp_val v
      | k, `Unequal vv ->
          Format.fprintf fs "[@[%a@ @<2>↦ %a@]]" pp_key k pp_diff_val vv
    in
    let sd = Iter.to_list (symmetric_diff ~eq x y) in
    List.pp ~pre ~suf sep pp_diff_elt fs sd
end
[@@inline]

module Make (Key : sig
  type t [@@deriving compare, equal, sexp_of]
end) =
Make_from_Comparer (struct
  include Key
  include Comparer.Make (Key)
end)
[@@inline]
