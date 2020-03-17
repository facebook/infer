(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Import0
include Map_intf

module Make (Key : OrderedType) : S with type key = Key.t = struct
  module M = Stdlib.Map.Make (Key)

  type key = Key.t
  type 'a t = 'a M.t

  let compare = M.compare
  let equal = M.equal

  let sexp_of_t sexp_of_val m =
    List.sexp_of_t
      (Sexplib.Conv.sexp_of_pair Key.sexp_of_t sexp_of_val)
      (M.bindings m)

  let t_of_sexp key_of_sexp val_of_sexp sexp =
    List.fold_left
      ~f:(fun m (k, v) -> M.add k v m)
      ~init:M.empty
      (List.t_of_sexp
         (Sexplib.Conv.pair_of_sexp key_of_sexp val_of_sexp)
         sexp)

  let pp pp_k pp_v fs m =
    Format.fprintf fs "@[<1>[%a]@]"
      (List.pp ",@ " (fun fs (k, v) ->
           Format.fprintf fs "@[%a @<2>↦ %a@]" pp_k k pp_v v ))
      (M.bindings m)

  let pp_diff ~data_equal pp_key pp_val pp_diff_val fs (x, y) =
    let pp_diff_val fs = function
      | k, `Left v ->
          Format.fprintf fs "-- [@[%a@ @<2>↦ %a@]]" pp_key k pp_val v
      | k, `Right v ->
          Format.fprintf fs "++ [@[%a@ @<2>↦ %a@]]" pp_key k pp_val v
      | k, `Unequal vv ->
          Format.fprintf fs "[@[%a@ @<2>↦ %a@]]" pp_key k pp_diff_val vv
    in
    let sd =
      M.merge
        (fun _ v1o v2o ->
          match (v1o, v2o) with
          | Some v1, Some v2 when not (data_equal v1 v2) ->
              Some (`Unequal (v1, v2))
          | Some v1, None -> Some (`Left v1)
          | None, Some v2 -> Some (`Right v2)
          | _ -> None )
        x y
    in
    if not (M.is_empty sd) then
      Format.fprintf fs "[@[<hv>%a@]];@ "
        (List.pp ";@ " pp_diff_val)
        (M.bindings sd)

  let empty = M.empty
  let set m ~key ~data = M.add key data m

  let add_exn m ~key ~data =
    M.update key
      (function None -> Some data | Some _ -> raise Duplicate)
      m

  let add_multi m ~key ~data =
    M.update key
      (function None -> Some [data] | Some vs -> Some (data :: vs))
      m

  let remove m k = M.remove k m
  let update m k ~f = M.update k (fun vo -> Some (f vo)) m

  let merge m n ~f =
    M.merge
      (fun k v1o v2o ->
        match (v1o, v2o) with
        | Some v1, Some v2 -> f ~key:k (`Both (v1, v2))
        | Some v1, None -> f ~key:k (`Left v1)
        | None, Some v2 -> f ~key:k (`Right v2)
        | None, None -> None )
      m n

  let merge_skewed m n ~combine =
    M.merge
      (fun k v1o v2o ->
        match (v1o, v2o) with
        | Some v1, Some v2 -> Some (combine ~key:k v1 v2)
        | Some _, None -> v1o
        | None, Some _ -> v2o
        | None, None -> None )
      m n

  let map m ~f = M.map f m
  let filter_keys m ~f = M.filter (fun k _ -> f k) m

  let filter_mapi m ~f =
    M.fold
      (fun k v m ->
        match f ~key:k ~data:v with Some v' -> M.add k v' m | None -> m )
      m M.empty

  let is_empty = M.is_empty
  let length = M.cardinal
  let mem m k = M.mem k m
  let find m k = M.find_opt k m

  let find_and_remove m k =
    let found = ref None in
    let m =
      M.update k
        (fun v ->
          found := v ;
          None )
        m
    in
    Option.map ~f:(fun v -> (v, m)) !found

  let find_multi m k = try M.find k m with Not_found -> []
  let data m = M.fold (fun _ v s -> v :: s) m []
  let to_alist = M.bindings
  let iter m ~f = M.iter (fun _ v -> f v) m
  let iteri m ~f = M.iter (fun k v -> f ~key:k ~data:v) m
  let for_alli m ~f = M.for_all (fun key data -> f ~key ~data) m
  let fold m ~init ~f = M.fold (fun key data s -> f ~key ~data s) m init
end
