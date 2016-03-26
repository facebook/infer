(*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

let exists = List.exists
let filter = List.filter
let find = List.find
let fold_left = List.fold_left
let fold_left2 = List.fold_left2
let for_all = List.for_all
let for_all2 = List.for_all2
let hd = List.hd
let iter = List.iter
let iter2 = List.iter2
let iteri = List.iteri
let length = List.length
let nth = List.nth
let partition = List.partition
let rev = List.rev
let rev_append = List.rev_append
let rev_map = List.rev_map
let sort = List.sort
let stable_sort = List.stable_sort
let tl = List.tl

(** tail-recursive variant of List.fold_right *)
let fold_right f l a =
  let g x y = f y x in
  fold_left g a (rev l)

(** tail-recursive variant of List.combine *)
let combine =
  let rec combine acc l1 l2 = match l1, l2 with
    | [], [] -> acc
    | x1:: l1, x2:: l2 -> combine ((x1, x2):: acc) l1 l2
    | [], _:: _
    | _:: _, [] -> raise (Invalid_argument "IList.combine") in
  fun l1 l2 -> rev (combine [] l1 l2)

(** tail-recursive variant of List.split *)
let split =
  let rec split acc1 acc2 = function
    | [] -> (acc1, acc2)
    | (x, y):: l -> split (x:: acc1) (y:: acc2) l in
  fun l ->
    let acc1, acc2 = split [] [] l in
    rev acc1, rev acc2

(** Like List.mem but without builtin equality *)
let mem equal x l = exists (equal x) l

(** tail-recursive variant of List.flatten *)
let flatten =
  let rec flatten acc l = match l with
    | [] -> acc
    | x:: l' -> flatten (rev_append x acc) l' in
  fun l -> rev (flatten [] l)

let flatten_options list =
  fold_left (fun list -> function | Some x -> x:: list | None -> list) [] list
  |> rev

let rec drop_first n = function
  | xs when n == 0 -> xs
  | _ :: xs -> drop_first (n - 1) xs
  | [] -> []

let drop_last n list =
  rev (drop_first n (rev list))

(** Generic comparison of lists given a compare function for the elements of the list *)
let rec compare cmp l1 l2 =
  match l1, l2 with
  | [],[] -> 0
  | [], _ -> - 1
  | _, [] -> 1
  | x1:: l1', x2:: l2' ->
      let n = cmp x1 x2 in
      if n <> 0 then n else compare cmp l1' l2'

(** Generic equality of lists given a compare function for the elements of the list *)
let equal cmp l1 l2 =
  compare cmp l1 l2 = 0

(** Returns (reverse input_list) *)
let rec rev_with_acc acc = function
  | [] -> acc
  | x :: xs -> rev_with_acc (x:: acc) xs

(** tail-recursive variant of List.append *)
let append l1 l2 =
  rev_append (rev l1) l2

(** tail-recursive variant of List.map *)
let map f l =
  rev (rev_map f l)

(** tail-recursive variant of List.mapi *)
let mapi f l =
  let i = ref 0 in
  rev (rev_map
         (fun x ->
            incr i;
            f (!i - 1) x)
         l)

(** Remove consecutive equal elements from a list (according to the given comparison functions) *)
let remove_duplicates compare l =
  let rec remove compare acc = function
    | [] -> rev acc
    | [x] -> rev (x:: acc)
    | x:: ((y:: l'') as l') ->
        if compare x y = 0 then remove compare acc (x:: l'')
        else remove compare (x:: acc) l' in
  remove compare [] l

(** Remove consecutive equal irrelevant elements from a list (according to the given comparison and relevance functions) *)
let remove_irrelevant_duplicates compare relevant l =
  let rec remove compare acc = function
    | [] -> rev acc
    | [x] -> rev (x:: acc)
    | x:: ((y:: l'') as l') ->
        if compare x y = 0 then begin
          match relevant x, relevant y with
          | false, _ -> remove compare acc l'
          | true, false -> remove compare acc (x:: l'')
          | true, true -> remove compare (x:: acc) l'
        end
        else remove compare (x:: acc) l' in
  remove compare [] l

(** The function works on sorted lists without duplicates *)
let rec merge_sorted_nodup compare res xs1 xs2 =
  match xs1, xs2 with
  | [], _ ->
      rev_with_acc xs2 res
  | _, [] ->
      rev_with_acc xs1 res
  | x1 :: xs1', x2 :: xs2' ->
      let n = compare x1 x2 in
      if n = 0 then
        merge_sorted_nodup compare (x1 :: res) xs1' xs2'
      else if n < 0 then
        merge_sorted_nodup compare (x1 :: res) xs1' xs2
      else
        merge_sorted_nodup compare (x2 :: res) xs1 xs2'

let intersect compare l1 l2 =
  let l1_sorted = sort compare l1 in
  let l2_sorted = sort compare l2 in
  let rec f l1 l2 = match l1, l2 with
    | ([], _) | (_,[]) -> false
    | (x1:: l1', x2:: l2') ->
        let x_comparison = compare x1 x2 in
        if x_comparison = 0 then true
        else if x_comparison < 0 then f l1' l2
        else f l1 l2' in
  f l1_sorted l2_sorted

exception Fail

(** Apply [f] to pairs of elements; raise [Fail] if the two lists have different lenghts. *)
let map2 f l1 l2 =
  let rec go l1 l2 acc =
    match l1, l2 with
    | [],[] -> rev acc
    | x1 :: l1', x2 :: l2' ->
        let x' = f x1 x2 in
        go l1' l2' (x':: acc)
    | _ -> raise Fail in
  go l1 l2 []

let to_string f l =
  let rec aux l =
    match l with
    | [] -> ""
    | s:: [] -> (f s)
    | s:: rest -> (f s)^", "^(aux rest) in
  "["^(aux l)^"]"

(** Like List.mem_assoc but without builtin equality *)
let mem_assoc equal a l =
  exists (fun x -> equal a (fst x)) l

(** Like List.assoc but without builtin equality *)
let assoc equal a l =
  snd (find (fun x -> equal a (fst x)) l)
