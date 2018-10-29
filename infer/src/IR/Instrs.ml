(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

module RevArray : sig
  type 'a t

  val is_empty : 'a t -> bool

  val length : 'a t -> int

  val of_rev_array : 'a Array.t -> 'a t

  val get : 'a t -> int -> 'a

  val last_opt : 'a t -> 'a option

  val fold : ('a t, 'a, 'accum) Container.fold
end = struct
  type 'a t = 'a Array.t

  let is_empty = Array.is_empty

  let length = Array.length

  let of_rev_array a = a

  let get a index = a.(Array.length a - 1 - index)

  let last_opt a = if is_empty a then None else Some (Array.unsafe_get a 0)

  let fold a ~init ~f =
    let f = Fn.flip f in
    Array.fold_right a ~init ~f
end

type reversed

type not_reversed

type 'rev t =
  | NotReversed : Sil.instr Array.t -> not_reversed t
  | Reversed : Sil.instr RevArray.t -> reversed t

type not_reversed_t = not_reversed t

(* Some functions are only used on non-reversed arrays, let's specialize them.
  The argument of the type helps us make sure they can't be used otherwise. *)
(* Functions on non-reversed arrays *)

let of_array instrs = NotReversed instrs

let empty = of_array [||]

let singleton instr = of_array [|instr|]

let append_list (NotReversed instrs) list = NotReversed (Array.append instrs (Array.of_list list))

let of_list l = NotReversed (Array.of_list l)

let of_rev_list l = NotReversed (Array.of_list_rev l)

let filter_map (NotReversed instrs) ~f = NotReversed (Array.filter_map instrs ~f)

let map_changed =
  let aux_changed arr ~f i =
    for i = i to Array.length arr - 1 do
      Array.unsafe_get arr i |> f |> Array.unsafe_set arr i
    done ;
    arr
  in
  let rec aux_unchanged ~equal arr ~f i =
    if i >= Array.length arr then arr
    else
      let e = Array.unsafe_get arr i in
      let e' = f e in
      if equal e e' then aux_unchanged ~equal arr ~f (i + 1)
      else
        let arr = Array.copy arr in
        Array.unsafe_set arr i e' ;
        aux_changed arr ~f (i + 1)
  in
  fun ~equal (NotReversed instrs as t) ~f ->
    let instrs' = aux_unchanged ~equal instrs ~f 0 in
    if phys_equal instrs instrs' then t else NotReversed instrs'


let reverse_order (NotReversed instrs) = Reversed (RevArray.of_rev_array instrs)

(* Functions on both reversed and non-reversed arrays *)

let is_empty (type r) (t : r t) =
  match t with
  | NotReversed instrs ->
      Array.is_empty instrs
  | Reversed rev_instrs ->
      RevArray.is_empty rev_instrs


let fold (type r) (t : r t) ~init ~f =
  match t with
  | NotReversed instrs ->
      Array.fold instrs ~init ~f
  | Reversed rev_instrs ->
      RevArray.fold rev_instrs ~init ~f


let iter t ~f = Container.iter ~fold t ~f

let exists t ~f = Container.exists ~iter t ~f

let for_all t ~f = Container.for_all ~iter t ~f

let count (type r) (t : r t) =
  match t with
  | NotReversed instrs ->
      Array.length instrs
  | Reversed rev_instrs ->
      RevArray.length rev_instrs


let nth_exists t index = index < count t

let nth_exn (type r) (t : r t) index =
  match t with
  | NotReversed instrs ->
      instrs.(index)
  | Reversed rev_instrs ->
      RevArray.get rev_instrs index


let last (type r) (t : r t) =
  match t with
  | NotReversed instrs ->
      if is_empty t then None else Some (Array.last instrs)
  | Reversed rev_instrs ->
      RevArray.last_opt rev_instrs


let find_map t ~f = Container.find_map ~iter t ~f

let pp pe fmt t =
  iter t ~f:(fun instr -> F.fprintf fmt "%a;@\n" (Sil.pp_instr ~print_types:false pe) instr)
