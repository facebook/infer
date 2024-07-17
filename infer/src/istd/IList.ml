(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! Core

(* [take_append ~max list ~tail] takes the first max elements of [list] and appends [tail] to the result.
   Does not create garbage for [max] <= 1000. *)
let take_append =
  let rec take_append_non_tailrec ~max list ~tail =
    match list with
    | hd :: tl when max > 0 ->
        hd :: take_append_non_tailrec ~max:(max - 1) tl ~tail
    | _ ->
        tail
  in
  let rec take_append_tailrec ~max list acc ~tail =
    match list with
    | hd :: tl when max > 0 ->
        (take_append_tailrec [@tailcall]) ~max:(max - 1) tl (hd :: acc) ~tail
    | _ ->
        List.rev_append acc tail
  in
  fun list ~max ~tail ->
    if max <= 1000 then take_append_non_tailrec ~max list ~tail
    else take_append_tailrec ~max list [] ~tail


(** like map, but returns the original list if unchanged *)
let map_changed ~equal ~f l =
  let rec aux unchanged_prefix_length = function
    | [] ->
        l
    | x :: tl ->
        let x' = f x in
        if not (equal x x') then
          take_append ~max:unchanged_prefix_length ~tail:(x' :: List.map ~f tl) l
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
  let rev_sort xs = List.dedup_and_sort ~compare:(fun x y -> cmp y x) xs in
  let rev_xs = rev_sort xs in
  let rev_ys = rev_sort ys in
  let rec inter_ is rev_xxs rev_yys =
    match (rev_xxs, rev_yys) with
    | [], _ | _, [] ->
        is
    | x :: rev_xs, y :: rev_ys ->
        let c = cmp x y in
        if Int.equal c 0 then inter_ (x :: is) rev_xs rev_ys
        else if c > 0 then inter_ is rev_xs rev_yys
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


let split_last_rev l = match List.rev l with x :: xs -> Some (x, xs) | [] -> None

let append_no_duplicates (type a) ~(cmp : a -> a -> int) =
  (* roughly based on [Core.List.stable_dedup_staged] but also takes care of the append and takes
     into account the invariant that [list1] and [list2] do not contain duplicates individually *)
  let module Set = Set.Make (struct
    type t = a

    let compare = cmp

    (* we never calls these *)
    let t_of_sexp _ = assert false

    let sexp_of_t _ = assert false
  end) in
  Staged.stage (fun (list1 : a list) (list2 : a list) ->
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


let opt_cons opt list = match opt with Some x -> x :: list | None -> list

let remove_first =
  let rec aux list ~f rev_front =
    match list with
    | [] ->
        None
    | hd :: tl ->
        if f hd then Some (List.rev_append rev_front tl) else aux tl ~f (hd :: rev_front)
  in
  fun list ~f -> aux list ~f []


let pp_print_list ~max ?(pp_sep = Format.pp_print_cut) pp_v ppf =
  let rec aux n = function
    | [] ->
        ()
    | v :: rest ->
        if n >= max then Format.fprintf ppf " ..."
        else (
          pp_sep ppf () ;
          pp_v ppf v ;
          aux (n + 1) rest )
  in
  function
  | [] ->
      ()
  | [v] ->
      pp_v ppf v
  | v :: rest ->
      pp_v ppf v ;
      aux 1 rest


let fold2_result ~init ~f l1 l2 =
  List.fold2 l1 l2 ~init:(Ok init) ~f:(fun result x1 x2 ->
      Result.bind result ~f:(fun acc -> f acc x1 x2) )


let foldi_result ~init ~f l =
  List.foldi l ~init:(Ok init) ~f:(fun i result x -> Result.bind result ~f:(fun acc -> f i acc x))


let eval_until_first_some thunks = List.find_map thunks ~f:(fun f -> f ())

let rec product = function
  | [] ->
      [[]]
  | xs :: xss ->
      let yss = product xss in
      List.concat_map ~f:(fun x -> List.map ~f:(fun ys -> x :: ys) yss) xs


let move_last_to_first =
  let rec move_last_to_first_helper l rev_acc =
    match l with
    | [] ->
        []
    | [a] ->
        a :: List.rev rev_acc
    | hd :: tl ->
        move_last_to_first_helper tl (hd :: rev_acc)
  in
  fun l -> move_last_to_first_helper l []


let traverse_opt xs ~f =
  List.fold_until xs ~init:[]
    ~f:(fun acc x ->
      match f x with
      | Some r ->
          Continue_or_stop.Continue (r :: acc)
      | _ ->
          Continue_or_stop.Stop None )
    ~finish:(fun acc -> Some (List.rev acc))
