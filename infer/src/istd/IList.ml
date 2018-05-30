(*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

let rec take_append n ~tail l =
  if n <= 0 then tail
  else match l with [] -> tail | x :: tl -> take_append (n - 1) ~tail:(x :: tail) tl


(** like map, but returns the original list if unchanged *)
let map_changed ~equal ~f l =
  let rec aux unchanged_prefix_length = function
    | [] ->
        l
    | x :: tl ->
        let x' = f x in
        if not (equal x x') then take_append unchanged_prefix_length ~tail:(x' :: List.map ~f tl) l
        else aux (unchanged_prefix_length + 1) tl
  in
  aux 0 l


(** like filter, but returns the original list if unchanged *)
let filter_changed ~f l =
  let res_rev, changed =
    List.fold_left l ~init:([], false) ~f:(fun (l, changed) x ->
        if f x then (x :: l, changed) else (l, true) )
  in
  if changed then List.rev res_rev else l


(** Remove consecutive equal irrelevant elements from a list (according to the given comparison and
    relevance functions) *)
let remove_irrelevant_duplicates ~equal ~f l =
  let rec remove acc = function
    | [] ->
        List.rev acc
    | [x] ->
        List.rev (x :: acc)
    | x :: (y :: l'' as l') ->
        if equal x y then
          match (f x, f y) with
          | false, _ ->
              remove acc l'
          | true, false ->
              remove acc (x :: l'')
          | true, true ->
              remove (x :: acc) l'
        else remove (x :: acc) l'
  in
  remove [] l


(** The function works on sorted lists without duplicates, and keeps only one copy of elements that
    appear in both lists. *)
let rec merge_sorted_nodup ~cmp ~res xs1 xs2 =
  match (xs1, xs2) with
  | [], _ ->
      List.rev_append res xs2
  | _, [] ->
      List.rev_append res xs1
  | x1 :: xs1', x2 :: xs2' ->
      let n = cmp x1 x2 in
      if Int.equal n 0 then merge_sorted_nodup ~cmp ~res:(x1 :: res) xs1' xs2'
      else if n < 0 then merge_sorted_nodup ~cmp ~res:(x1 :: res) xs1' xs2
      else merge_sorted_nodup ~cmp ~res:(x2 :: res) xs1 xs2'


let inter ~cmp xs ys =
  let rev_sort xs = List.sort ~compare:(fun x y -> cmp y x) xs in
  let rev_xs = rev_sort xs in
  let rev_ys = rev_sort ys in
  let rec inter_ is rev_xxs rev_yys =
    match (rev_xxs, rev_yys) with
    | [], _ | _, [] ->
        is
    | x :: rev_xs, y :: rev_ys ->
        let c = cmp x y in
        if Int.equal c 0 then inter_ (x :: is) rev_xs rev_ys
        else if c < 0 then inter_ is rev_xs rev_yys
        else inter_ is rev_xxs rev_ys
  in
  inter_ [] rev_xs rev_ys


(** like fold, but apply [f_last] to the last element *)
let rec fold_last l ~init ~f ~f_last =
  match l with
  | [] ->
      init
  | [last] ->
      f_last init last
  | hd :: tl ->
      fold_last tl ~init:(f init hd) ~f ~f_last


let append_no_duplicates (type a) ~(cmp: a -> a -> int) =
  (* roughly based on [Core.List.stable_dedup_staged] but also takes care of the append and takes
     into account the invariant that [list1] and [list2] do not contain duplicates individually *)
  let module Set = Set.Make (struct
    type t = a

    let compare = cmp

    (* we never calls these *)
    let t_of_sexp _ = assert false

    let sexp_of_t _ = assert false
  end) in
  Staged.stage (fun (list1: a list) (list2: a list) ->
      let set1 = Set.of_list list1 in
      let res_rev =
        List.fold_left list2 ~init:(List.rev list1) ~f:(fun res_rev x ->
            if Set.mem set1 x then res_rev else x :: res_rev )
      in
      List.rev res_rev )


let merge_dedup l1 l2 ~compare =
  let rec loop acc l1 l2 =
    match (l1, l2) with
    | [], l2 ->
        List.rev_append acc l2
    | l1, [] ->
        List.rev_append acc l1
    | h1 :: t1, h2 :: t2 ->
        let cmp = compare h1 h2 in
        if Int.equal cmp 0 then loop (h1 :: acc) t1 t2
        else if cmp < 0 then loop (h1 :: acc) t1 l2
        else loop (h2 :: acc) l1 t2
  in
  loop [] l1 l2


(* Remove when Base.List.drop perf is fixed *)
let rec drop list index =
  match list with _ :: tl when index > 0 -> drop tl (index - 1) | _ -> list


let mem_nth list index = drop list index |> List.is_empty |> not

let opt_cons opt list = match opt with Some x -> x :: list | None -> list
