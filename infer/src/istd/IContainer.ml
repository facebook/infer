(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

type 'a singleton_or_more = Empty | Singleton of 'a | More

let singleton_or_more ~fold t =
  With_return.with_return (fun {return} ->
      fold t ~init:Empty ~f:(fun acc item ->
          match acc with Empty -> Singleton item | _ -> return More ) )


let mem_nth ~fold t index =
  With_return.with_return (fun {return} ->
      let (_ : int) =
        fold t ~init:index ~f:(fun index _ -> if index <= 0 then return true else index - 1)
      in
      false )


let forto excl ~init ~f =
  let rec aux excl ~f acc i = if i >= excl then acc else aux excl ~f (f acc i) (i + 1) in
  aux excl ~f init 0


let forto_right excl ~init ~f =
  let rec aux ~f acc i = if i < 0 then acc else aux ~f (f acc i) (i - 1) in
  aux ~f init (excl - 1)


let to_rev_list ~fold t = fold t ~init:[] ~f:(fun tl hd -> hd :: tl)

let rev_filter_to_list ~fold t ~f =
  fold t ~init:[] ~f:(fun acc item -> if f item then item :: acc else acc)


let rev_map_to_list ~fold t ~f = fold t ~init:[] ~f:(fun acc item -> f item :: acc)

let rev_filter_map_to_list ~fold t ~f =
  fold t ~init:[] ~f:(fun acc item -> IList.opt_cons (f item) acc)


let iter_consecutive ~fold t ~f =
  let (_ : _ option) =
    fold t ~init:None ~f:(fun prev_opt curr ->
        (match prev_opt with Some prev -> f prev curr | None -> ()) ;
        Some curr )
  in
  ()


let pp_collection ?(hov = false) ~fold ~pp_item fmt c =
  let f prev_opt item =
    prev_opt |> Option.iter ~f:(F.fprintf fmt "@[<h>%a,@]@ " pp_item) ;
    Some item
  in
  let pp_aux fmt c = fold c ~init:None ~f |> Option.iter ~f:(F.fprintf fmt "@[<h>%a@] " pp_item) in
  if hov then F.fprintf fmt "@[<hov 2>" else F.fprintf fmt "@[<hv 2>" ;
  F.fprintf fmt "{ %a}@]" pp_aux c


let filter ~fold ~filter t ~init ~f =
  fold t ~init ~f:(fun acc item -> if filter item then f acc item else acc)


let fold_of_pervasives_set_fold fold collection ~init ~f =
  fold (fun elt accum -> f accum elt) collection init


let map ~f:g fold t ~init ~f = fold t ~init ~f:(fun acc item -> f acc (g item))

let fold_of_pervasives_map_fold fold collection ~init ~f =
  fold (fun item value accum -> f accum (item, value)) collection init


let iter_result ~fold collection ~f =
  Container.fold_result ~fold ~init:() ~f:(fun () item -> f item) collection


let fold_result_until ~fold ~init ~f ~finish collection =
  with_return (fun {return} ->
      Result.map ~f:finish
        (Container.fold_result ~fold collection ~init ~f:(fun acc item ->
             match (f acc item : _ Continue_or_stop.t) with
             | Continue x ->
                 x
             | Stop x ->
                 return (Result.Ok x) ) ) )
