(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! Core

(*
  Invariants:
  - elements of Concat are not empty (nor singletons)
  - arg of Rev is not empty, nor singleton, nor Rev
  - arg of Snoc is not empty
  ...ensure that:
  - an empty list is always represented by Empty,
  - a singleton is always represented by Cons(_, Empty)
  - the memory footprint is in Theta(length)

  Potential constructors to add later:
  - OfArray of 'a Array.t
  - Flatten of 'a t t
*)
type +'a t = Empty | Cons of 'a * 'a t | Snoc of 'a t * 'a | Concat of 'a t * 'a t | Rev of 'a t

let () =
  (* This is a pretty simple test to check that the runtime representation of OCaml lists is compatible with our lists and hence using [Obj.magic] for [of_list] is legit *)
  let exception RuntimeValue in
  assert (
    Polymorphic_compare.(Caml.Obj.repr [RuntimeValue] = Caml.Obj.repr (Cons (RuntimeValue, Empty)))
  )


(* Constructors *)

let of_list = Caml.Obj.magic

let empty = Empty

let singleton e = Cons (e, Empty)

let cons e l = Cons (e, l)

let snoc l e = match l with Empty -> singleton e | _ -> Snoc (l, e)

let append l1 l2 =
  match (l1, l2) with
  | Empty, l | l, Empty ->
      l
  | Cons (e, Empty), l2 ->
      Cons (e, l2)
  | l1, Cons (e, Empty) ->
      Snoc (l1, e)
  | l1, l2 ->
      Concat (l1, l2)


let rev l = match l with Empty | Cons (_, Empty) -> l | Rev l -> l | l -> Rev l

(* Deconstructors *)

let is_empty = function Empty -> true | _ -> false

let is_singleton = function Cons (e, Empty) -> Some e | _ -> None

let is_singleton_or_more = function
  | Empty ->
      IContainer.Empty
  | Cons (e, Empty) ->
      IContainer.Singleton e
  | _ ->
      IContainer.More


let rec hd_tl_exn : 'a t -> 'a * 'a t = function
  | Empty ->
      raise Caml.Not_found
  | Cons (hd, tl) ->
      (hd, tl)
  | Snoc (front, last) ->
      let hd, tl = hd_tl_exn front in
      (hd, snoc tl last)
  | Concat (l1, l2) ->
      let hd, tl1 = hd_tl_exn l1 in
      (hd, append tl1 l2)
  | Rev l ->
      let rev_tl, hd = front_last_exn l in
      (hd, rev rev_tl)


and front_last_exn : 'a t -> 'a t * 'a = function
  | Empty ->
      raise Caml.Not_found
  | Cons (hd, tl) ->
      let front, last = front_last_exn tl in
      (cons hd front, last)
  | Snoc (front, last) ->
      (front, last)
  | Concat (l1, l2) ->
      let front2, last = front_last_exn l2 in
      (append l1 front2, last)
  | Rev l ->
      let last, rev_front = hd_tl_exn l in
      (rev rev_front, last)


let rec hd_exn : 'a t -> 'a = function
  | Empty ->
      raise Caml.Not_found
  | Cons (hd, _) ->
      hd
  | Snoc (front, _) | Concat (front, _) ->
      hd_exn front
  | Rev l ->
      last_exn l


and last_exn : 'a t -> 'a = function
  | Empty ->
      raise Caml.Not_found
  | Snoc (_, last) ->
      last
  | Cons (_, tl) | Concat (_, tl) ->
      last_exn tl
  | Rev l ->
      hd_exn l


let hd = function Empty -> None | l -> Some (hd_exn l)

let last = function Empty -> None | l -> Some (last_exn l)

(* Traversing *)

let rec fold_left_tailrec l rem ~init ~f =
  match l with
  | Empty -> (
    match rem with Empty -> init | rem -> (fold_left_tailrec [@tailcall]) rem Empty ~init ~f )
  | Cons (hd, tl) ->
      let init = f init hd in
      (fold_left_tailrec [@tailcall]) tl rem ~init ~f
  | Snoc (front, last) ->
      (fold_left_tailrec [@tailcall]) front (cons last rem) ~init ~f
  | Concat (l1, l2) ->
      (fold_left_tailrec [@tailcall]) l1 (append l2 rem) ~init ~f
  | Rev l ->
      (fold_right_tailrec [@tailcall]) l (rev rem) ~init ~f


and fold_right_tailrec l rem ~init ~f =
  match l with
  | Empty -> (
    match rem with Empty -> init | rem -> (fold_right_tailrec [@tailcall]) rem Empty ~init ~f )
  | Cons (hd, tl) ->
      (fold_right_tailrec [@tailcall]) tl (snoc rem hd) ~init ~f
  | Snoc (front, last) ->
      let init = f init last in
      (fold_right_tailrec [@tailcall]) front rem ~init ~f
  | Concat (l1, l2) ->
      (fold_right_tailrec [@tailcall]) l2 (append rem l1) ~init ~f
  | Rev l ->
      (fold_left_tailrec [@tailcall]) l (rev rem) ~init ~f


let max_recursion = 1000

let rec fold_left_count l depth ~init ~f =
  match l with
  | Empty ->
      init
  | Cons (hd, tl) ->
      let init = f init hd in
      (fold_left_count [@tailcall]) tl depth ~init ~f
  | Snoc (front, last) ->
      let init =
        if depth < max_recursion then fold_left_count front (depth + 1) ~init ~f
        else fold_left_tailrec l Empty ~init ~f
      in
      f init last
  | Concat (l1, l2) ->
      if depth < max_recursion then
        let init = fold_left_count l1 (depth + 1) ~init ~f in
        (fold_left_count [@tailcall]) l2 depth ~init ~f
      else fold_left_tailrec l1 l2 ~init ~f
  | Rev l ->
      (fold_right_count [@tailcall]) l depth ~init ~f


and fold_right_count l depth ~init ~f =
  match l with
  | Empty ->
      init
  | Cons (hd, tl) ->
      let init =
        if depth < max_recursion then fold_right_count tl (depth + 1) ~init ~f
        else fold_right_tailrec tl Empty ~init ~f
      in
      f init hd
  | Snoc (front, last) ->
      let init = f init last in
      (fold_right_count [@tailcall]) front depth ~init ~f
  | Concat (l1, l2) ->
      if depth < max_recursion then
        let init = fold_right_count l2 (depth + 1) ~init ~f in
        (fold_right_count [@tailcall]) l1 depth ~init ~f
      else fold_right_tailrec l2 l1 ~init ~f
  | Rev l ->
      (fold_left_count [@tailcall]) l depth ~init ~f


let fold_left l ~init ~f = fold_left_count l 0 ~init ~f

let fold_right l ~init ~f = fold_right_count l 0 ~init ~f

let rec fold_unordered_tailrec l rem ~init ~f =
  match l with
  | Empty -> (
    match rem with Empty -> init | rem -> (fold_unordered_tailrec [@tailcall]) rem Empty ~init ~f )
  | Cons (e, l) | Snoc (l, e) ->
      let init = f init e in
      (fold_unordered_tailrec [@tailcall]) l rem ~init ~f
  | Concat (l1, l2) ->
      (fold_unordered_tailrec [@tailcall]) l1 (append l2 rem) ~init ~f
  | Rev l ->
      (fold_unordered_tailrec [@tailcall]) l rem ~init ~f


let rec fold_unordered_count l depth ~init ~f =
  match l with
  | Empty ->
      init
  | Cons (e, l) | Snoc (l, e) ->
      let init = f init e in
      (fold_unordered_count [@tailcall]) l depth ~init ~f
  | Concat (l1, l2) ->
      if depth < max_recursion then
        let init = fold_unordered_count l1 (depth + 1) ~init ~f in
        (fold_unordered_count [@tailcall]) l2 depth ~init ~f
      else fold_unordered_tailrec l1 l2 ~init ~f
  | Rev l ->
      (fold_unordered_count [@tailcall]) l depth ~init ~f


let fold_unordered l ~init ~f = fold_unordered_count l 0 ~init ~f
