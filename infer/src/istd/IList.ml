(*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

(** like map, but returns the original list if unchanged *)
let map_changed (f: 'a -> 'a) l =
  let l', changed =
    List.fold_left
      (fun (l_acc, changed) e ->
        let e' = f e in
        (e' :: l_acc, changed || e' != e) )
      ([], false) l
  in
  if changed then List.rev l' else l


(** like filter, but returns the original list if unchanged *)
let filter_changed (f: 'a -> bool) l =
  let l', changed =
    List.fold_left
      (fun (l_acc, changed) e -> if f e then (e :: l_acc, changed) else (l_acc, true))
      ([], false) l
  in
  if changed then List.rev l' else l


(** Remove consecutive equal irrelevant elements from a list
    (according to the given comparison and relevance functions) *)
let remove_irrelevant_duplicates compare relevant l =
  let rec remove compare acc = function
    | [] ->
        List.rev acc
    | [x] ->
        List.rev (x :: acc)
    | x :: (y :: l'' as l') ->
        if compare x y = 0 then
          match (relevant x, relevant y) with
          | false, _ ->
              remove compare acc l'
          | true, false ->
              remove compare acc (x :: l'')
          | true, true ->
              remove compare (x :: acc) l'
        else remove compare (x :: acc) l'
  in
  remove compare [] l


(** The function works on sorted lists without duplicates *)
let rec merge_sorted_nodup compare res xs1 xs2 =
  match (xs1, xs2) with
  | [], _ ->
      List.rev_append res xs2
  | _, [] ->
      List.rev_append res xs1
  | x1 :: xs1', x2 :: xs2' ->
      let n = compare x1 x2 in
      if n = 0 then merge_sorted_nodup compare (x1 :: res) xs1' xs2'
      else if n < 0 then merge_sorted_nodup compare (x1 :: res) xs1' xs2
      else merge_sorted_nodup compare (x2 :: res) xs1 xs2'


let inter compare xs ys =
  let rev_sort xs = List.sort (fun x y -> compare y x) xs in
  let rev_xs = rev_sort xs in
  let rev_ys = rev_sort ys in
  let rec inter_ is rev_xxs rev_yys =
    match (rev_xxs, rev_yys) with
    | [], _ | _, [] ->
        is
    | x :: rev_xs, y :: rev_ys ->
        let c = compare x y in
        if c = 0 then inter_ (x :: is) rev_xs rev_ys
        else if c < 0 then inter_ is rev_xs rev_yys
        else inter_ is rev_xxs rev_ys
  in
  inter_ [] rev_xs rev_ys


(** like fold, but apply f_last to the last element *)
let rec fold_last l ~init ~f ~f_last =
  match l with
  | [] ->
      init
  | [last] ->
      f_last init last
  | hd :: tl ->
      fold_last tl ~init:(f init hd) ~f ~f_last


let to_string f l =
  let rec aux l = match l with [] -> "" | [s] -> f s | s :: rest -> f s ^ ", " ^ aux rest in
  "[" ^ aux l ^ "]"


let uncons_exn = function [] -> failwith "uncons_exn" | hd :: tl -> (hd, tl)

let append_no_duplicates eq list1 list2 =
  let list2_no_dup = List.filter (fun x2 -> List.for_all (fun x1 -> not (eq x2 x1)) list1) list2 in
  list1 @ list2_no_dup
