(*
 * Copyright (c) Facebook, Inc. and its affiliates.
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

  val foldi : 'a t -> init:'accum -> f:(int -> 'accum -> 'a -> 'accum) -> 'accum
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


  let foldi a ~init ~f =
    let idx = ref (Array.length a) in
    Array.fold_right a ~init ~f:(fun elt acc ->
        decr idx ;
        f !idx acc elt )
end

type reversed

type not_reversed

(** [Empty] and [Singleton _] can have both directions. We do not attempt to make the representation
    canonic, e.g. [NotReversed [||]], [Reversed [||]], and [Empty] are all allowed despite
    representing the same value. *)
type _ t =
  | NotReversed : Sil.instr Array.t -> not_reversed t
  | Reversed : Sil.instr RevArray.t -> reversed t
  | Empty : _ t
  | Singleton : Sil.instr -> _ t

type not_reversed_t = not_reversed t

(** {2 Functions on non-reversed arrays}

    Some functions are only used on non-reversed arrays, let's specialize them. The argument of the
    type helps us make sure they can't be used otherwise. *)

let get_underlying_not_reversed = function
  | NotReversed instrs ->
      instrs
  | Empty ->
      [||]
  | Singleton instr ->
      [|instr|]


let empty = Empty

let singleton instr = Singleton instr

let append_list t list =
  let instrs = get_underlying_not_reversed t in
  NotReversed (Array.append instrs (Array.of_list list))


let prepend_list t list =
  let instrs = get_underlying_not_reversed t in
  NotReversed (Array.append (Array.of_list list) instrs)


let of_list l = NotReversed (Array.of_list l)

let filter_map t ~f =
  let instrs = get_underlying_not_reversed t in
  NotReversed (Array.filter_map instrs ~f)


let map_and_fold =
  let rec aux_changed arr ~f current i =
    if i >= Array.length arr then arr
    else
      let e = Array.unsafe_get arr i in
      let next, e' = f current e in
      Array.unsafe_set arr i e' ;
      aux_changed arr ~f next (i + 1)
  in
  let rec aux_unchanged arr ~f current i =
    if i >= Array.length arr then arr
    else
      let e = Array.unsafe_get arr i in
      let next, e' = f current e in
      if phys_equal e e' then aux_unchanged arr ~f next (i + 1)
      else
        let arr = Array.copy arr in
        Array.unsafe_set arr i e' ;
        aux_changed arr ~f next (i + 1)
  in
  fun t ~f ~init ->
    let instrs = get_underlying_not_reversed t in
    let instrs' = aux_unchanged instrs ~f init 0 in
    if phys_equal instrs instrs' then t else NotReversed instrs'


let map (t : not_reversed t) ~f =
  let f () e = ((), f e) in
  map_and_fold t ~f ~init:()


let check_instr_equality t instrs instrs' =
  if
    Int.equal (Array.length instrs) (Array.length instrs')
    && Array.for_all2_exn ~f:phys_equal instrs instrs'
  then t
  else NotReversed instrs'


let concat_map t ~f =
  let instrs = get_underlying_not_reversed t in
  let instrs' = Array.concat_map ~f instrs in
  check_instr_equality t instrs instrs'


let concat_map_and_fold t ~f ~init =
  let cm ~f ~init t = Array.concat (Array.to_list (Array.folding_map ~init ~f t)) in
  let instrs = get_underlying_not_reversed t in
  let instrs' = cm ~init ~f instrs in
  check_instr_equality t instrs instrs'


let reverse_order t =
  let instrs = get_underlying_not_reversed t in
  Reversed (RevArray.of_rev_array instrs)


(* Functions on both reversed and non-reversed arrays *)

let is_empty (type r) (t : r t) =
  match t with
  | Empty ->
      true
  | Singleton _ ->
      false
  | NotReversed instrs ->
      Array.is_empty instrs
  | Reversed rev_instrs ->
      RevArray.is_empty rev_instrs


let fold (type r) (t : r t) ~init ~f =
  match t with
  | Empty ->
      init
  | Singleton instr ->
      f init instr
  | NotReversed instrs ->
      Array.fold instrs ~init ~f
  | Reversed rev_instrs ->
      RevArray.fold rev_instrs ~init ~f


let foldi (type r) (t : r t) ~init ~f =
  match t with
  | Empty ->
      init
  | Singleton instr ->
      f 0 init instr
  | NotReversed instrs ->
      Array.foldi instrs ~init ~f
  | Reversed rev_instrs ->
      RevArray.foldi rev_instrs ~init ~f


let iter t ~f = Container.iter ~fold t ~f

let exists t ~f = Container.exists ~iter t ~f

let for_all t ~f = Container.for_all ~iter t ~f

let count (type r) (t : r t) =
  match t with
  | Empty ->
      0
  | Singleton _ ->
      1
  | NotReversed instrs ->
      Array.length instrs
  | Reversed rev_instrs ->
      RevArray.length rev_instrs


let nth_exists t index = index < count t

let nth_exn (type r) (t : r t) index =
  match t with
  | Empty ->
      [||].(index)
  | Singleton instr ->
      [|instr|].(index)
  | NotReversed instrs ->
      instrs.(index)
  | Reversed rev_instrs ->
      RevArray.get rev_instrs index


let last (type r) (t : r t) =
  match t with
  | Empty ->
      None
  | Singleton instr ->
      Some instr
  | NotReversed instrs ->
      if is_empty t then None else Some (Array.last instrs)
  | Reversed rev_instrs ->
      RevArray.last_opt rev_instrs


let find_map t ~f = Container.find_map ~iter t ~f

let pp ?(indent = true) ?(print_types = false) pe fmt t =
  if indent then F.fprintf fmt "@[<v>" ;
  iter t ~f:(fun instr ->
      F.fprintf fmt "%a;" (Sil.pp_instr ~print_types pe) instr ;
      F.fprintf fmt (if indent then "@," else "@\n") ) ;
  if indent then F.fprintf fmt "@]"


(** Return the list of normal ids occurring in the instructions *)
let instrs_get_normal_vars instrs =
  let do_instr res instr =
    Sil.exps_of_instr instr
    |> List.fold_left ~init:res ~f:(fun res e ->
           Exp.free_vars e
           |> Sequence.filter ~f:Ident.is_normal
           |> Ident.hashqueue_of_sequence ~init:res )
  in
  fold ~init:(Ident.HashQueue.create ()) ~f:do_instr instrs |> Ident.HashQueue.keys
