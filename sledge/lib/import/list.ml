(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Import0
include Core.List

let rec pp ?pre ?suf sep pp_elt fs = function
  | [] -> ()
  | x :: xs ->
      Option.iter pre ~f:(Format.fprintf fs) ;
      pp_elt fs x ;
      ( match xs with
      | [] -> ()
      | xs -> Format.fprintf fs "%( %)%a" sep (pp sep pp_elt) xs ) ;
      Option.iter suf ~f:(Format.fprintf fs)

let pop_exn =
  let not_found = Not_found_s (Atom "pop_exn") in
  function x :: xs -> (x, xs) | [] -> raise not_found

let find_map_remove xs ~f =
  let rec find_map_remove_ ys = function
    | [] -> None
    | x :: xs -> (
      match f x with
      | Some x' -> Some (x', rev_append ys xs)
      | None -> find_map_remove_ (x :: ys) xs )
  in
  find_map_remove_ [] xs

let fold_option xs ~init ~f =
  with_return
  @@ fun {return} ->
  Some
    (fold xs ~init ~f:(fun acc elt ->
         match f acc elt with Some res -> res | None -> return None ))

let map_preserving_phys_equal map t ~f =
  let change = ref false in
  let t' =
    map t ~f:(fun x ->
        let x' = f x in
        if not (x' == x) then change := true ;
        x' )
  in
  if !change then t' else t

let filter_map_preserving_phys_equal filter_map t ~f =
  let change = ref false in
  let t' =
    filter_map t ~f:(fun x ->
        let x'_opt = f x in
        ( match x'_opt with
        | Some x' when x' == x -> ()
        | _ -> change := true ) ;
        x'_opt )
  in
  if !change then t' else t

let filter_map_preserving_phys_equal t ~f =
  filter_map_preserving_phys_equal filter_map t ~f

let map_preserving_phys_equal t ~f = map_preserving_phys_equal map t ~f

let rev_map_unzip xs ~f =
  fold xs ~init:([], []) ~f:(fun (ys, zs) x ->
      let y, z = f x in
      (y :: ys, z :: zs) )

let remove_exn =
  let not_found = Not_found_s (Atom "remove_exn") in
  fun ?(equal = phys_equal) xs x ->
    let rec remove_ ys = function
      | [] -> raise not_found
      | z :: xs ->
          if equal x z then rev_append ys xs else remove_ (z :: ys) xs
    in
    remove_ [] xs

let remove ?equal xs x =
  try Some (remove_exn ?equal xs x) with Not_found_s _ -> None

let rec rev_init n ~f =
  if n = 0 then []
  else
    let n = n - 1 in
    let xs = rev_init n ~f in
    f n :: xs

let symmetric_diff ~compare xs ys =
  let rec symmetric_diff_ xxs yys : _ Either.t list =
    match (xxs, yys) with
    | x :: xs, y :: ys ->
        let ord = compare x y in
        if ord = 0 then symmetric_diff_ xs ys
        else if ord < 0 then First x :: symmetric_diff_ xs yys
        else Second y :: symmetric_diff_ xxs ys
    | xs, [] -> map ~f:Either.first xs
    | [], ys -> map ~f:Either.second ys
  in
  symmetric_diff_ (sort ~compare xs) (sort ~compare ys)

let pp_diff ~compare sep pp_elt fs (xs, ys) =
  let pp_diff_elt fs elt =
    match elt with
    | First x -> Format.fprintf fs "-- %a" pp_elt x
    | Second y -> Format.fprintf fs "++ %a" pp_elt y
  in
  pp sep pp_diff_elt fs (symmetric_diff ~compare xs ys)
