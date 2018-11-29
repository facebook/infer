(*
 * Copyright (c) 2016-present, Programming Research Laboratory (ROPAS)
 *                             Seoul National University, Korea
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
(* Abstract Array Block *)
open! IStd
open AbsLoc
open! AbstractDomain.Types
module Bound = Bounds.Bound

module ArrInfo = struct
  type t = {offset: Itv.t; size: Itv.t; stride: Itv.t} [@@deriving compare]

  let top : t = {offset= Itv.top; size= Itv.top; stride= Itv.top}

  let make : offset:Itv.t -> size:Itv.t -> stride:Itv.t -> t =
   fun ~offset ~size ~stride -> {offset; size; stride}


  let join : t -> t -> t =
   fun a1 a2 ->
    if phys_equal a1 a2 then a2
    else
      { offset= Itv.join a1.offset a2.offset
      ; size= Itv.join a1.size a2.size
      ; stride= Itv.join a1.stride a2.stride }


  let widen : prev:t -> next:t -> num_iters:int -> t =
   fun ~prev ~next ~num_iters ->
    if phys_equal prev next then next
    else
      { offset= Itv.widen ~prev:prev.offset ~next:next.offset ~num_iters
      ; size= Itv.widen ~prev:prev.size ~next:next.size ~num_iters
      ; stride= Itv.widen ~prev:prev.stride ~next:next.stride ~num_iters }


  let ( <= ) : lhs:t -> rhs:t -> bool =
   fun ~lhs ~rhs ->
    if phys_equal lhs rhs then true
    else
      Itv.le ~lhs:lhs.offset ~rhs:rhs.offset
      && Itv.le ~lhs:lhs.size ~rhs:rhs.size
      && Itv.le ~lhs:lhs.stride ~rhs:rhs.stride


  let plus_offset : t -> Itv.t -> t = fun arr i -> {arr with offset= Itv.plus arr.offset i}

  let minus_offset : t -> Itv.t -> t = fun arr i -> {arr with offset= Itv.minus arr.offset i}

  let diff : t -> t -> Itv.t = fun arr1 arr2 -> Itv.minus arr1.offset arr2.offset

  let subst : t -> Bound.eval_sym -> t =
   fun arr eval_sym ->
    {arr with offset= Itv.subst arr.offset eval_sym; size= Itv.subst arr.size eval_sym}


  let pp : Format.formatter -> t -> unit =
   fun fmt arr -> Format.fprintf fmt "offset : %a, size : %a" Itv.pp arr.offset Itv.pp arr.size


  let get_symbols : t -> Itv.SymbolSet.t =
   fun arr ->
    let s1 = Itv.get_symbols arr.offset in
    let s2 = Itv.get_symbols arr.size in
    let s3 = Itv.get_symbols arr.stride in
    Itv.SymbolSet.union3 s1 s2 s3


  let normalize : t -> t =
   fun arr ->
    { offset= Itv.normalize arr.offset
    ; size= Itv.normalize arr.size
    ; stride= Itv.normalize arr.stride }


  let prune_comp : Binop.t -> t -> t -> t =
   fun c arr1 arr2 -> {arr1 with offset= Itv.prune_comp c arr1.offset arr2.offset}


  let prune_eq : t -> t -> t =
   fun arr1 arr2 -> {arr1 with offset= Itv.prune_eq arr1.offset arr2.offset}


  let prune_ne : t -> t -> t =
   fun arr1 arr2 -> {arr1 with offset= Itv.prune_ne arr1.offset arr2.offset}


  let set_length : Itv.t -> t -> t = fun size arr -> {arr with size}

  (* Set new stride only when the previous stride is a constant interval. *)
  let set_stride : Z.t -> t -> t =
   fun new_stride ({offset; size; stride} as arr) ->
    Option.value_map (Itv.is_const stride) ~default:arr ~f:(fun stride ->
        assert ((not Z.(equal stride zero)) && not Z.(equal new_stride zero)) ;
        if Z.equal new_stride stride then arr
        else
          let set itv = Itv.div_const (Itv.mult_const itv stride) new_stride in
          {offset= set offset; size= set size; stride= Itv.of_big_int new_stride} )


  let lift_cmp_itv cmp_itv arr1 arr2 =
    if Itv.eq arr1.stride arr2.stride && Itv.eq arr1.size arr2.size then
      cmp_itv arr1.offset arr2.offset
    else Boolean.Top
end

include AbstractDomain.Map (Allocsite) (ArrInfo)

let bot : t = empty

let unknown : t = add Allocsite.unknown ArrInfo.top bot

let is_bot : t -> bool = is_empty

let make : Allocsite.t -> offset:Itv.t -> size:Itv.t -> stride:Itv.t -> t =
 fun a ~offset ~size ~stride -> singleton a (ArrInfo.make ~offset ~size ~stride)


let offsetof : t -> Itv.t = fun a -> fold (fun _ arr -> Itv.join arr.ArrInfo.offset) a Itv.bot

let sizeof : t -> Itv.t = fun a -> fold (fun _ arr -> Itv.join arr.ArrInfo.size) a Itv.bot

let strideof : t -> Itv.t = fun a -> fold (fun _ arr -> Itv.join arr.ArrInfo.stride) a Itv.bot

let sizeof_byte : t -> Itv.t =
 fun a -> fold (fun _ arr -> Itv.join (Itv.mult arr.ArrInfo.size arr.ArrInfo.stride)) a Itv.bot


let plus_offset : t -> Itv.t -> t = fun arr i -> map (fun a -> ArrInfo.plus_offset a i) arr

let minus_offset : t -> Itv.t -> t = fun arr i -> map (fun a -> ArrInfo.minus_offset a i) arr

let diff : t -> t -> Itv.t =
 fun arr1 arr2 ->
  let diff_join k a2 acc =
    match find k arr1 with
    | a1 ->
        Itv.join acc (ArrInfo.diff a1 a2)
    | exception Caml.Not_found ->
        Itv.top
  in
  fold diff_join arr2 Itv.bot


let get_pow_loc : t -> PowLoc.t =
 fun array ->
  let pow_loc_of_allocsite k _ acc = PowLoc.add (Loc.of_allocsite k) acc in
  fold pow_loc_of_allocsite array PowLoc.bot


let subst : t -> Bound.eval_sym -> PowLoc.eval_locpath -> t =
 fun a eval_sym eval_locpath ->
  let subst1 l info acc =
    let info' = ArrInfo.subst info eval_sym in
    match Allocsite.get_param_path l with
    | None ->
        add l info' acc
    | Some path ->
        let locs = eval_locpath path in
        let add_allocsite l acc = match l with Loc.Allocsite a -> add a info' acc | _ -> acc in
        PowLoc.fold add_allocsite locs acc
  in
  fold subst1 a empty


let get_symbols : t -> Itv.SymbolSet.t =
 fun a ->
  fold (fun _ ai acc -> Itv.SymbolSet.union acc (ArrInfo.get_symbols ai)) a Itv.SymbolSet.empty


let normalize : t -> t = fun a -> map ArrInfo.normalize a

let do_prune : (ArrInfo.t -> ArrInfo.t -> ArrInfo.t) -> t -> t -> t =
 fun arr_info_prune a1 a2 ->
  match is_singleton_or_more a2 with
  | IContainer.Singleton (k, v2) ->
      if mem k a1 then add k (arr_info_prune (find k a1) v2) a1 else a1
  | _ ->
      a1


let prune_comp : Binop.t -> t -> t -> t = fun c a1 a2 -> do_prune (ArrInfo.prune_comp c) a1 a2

let prune_eq : t -> t -> t = fun a1 a2 -> do_prune ArrInfo.prune_eq a1 a2

let prune_ne : t -> t -> t = fun a1 a2 -> do_prune ArrInfo.prune_ne a1 a2

let set_length : Itv.t -> t -> t = fun length a -> map (ArrInfo.set_length length) a

let set_stride : Z.t -> t -> t = fun stride a -> map (ArrInfo.set_stride stride) a

let lift_cmp_itv cmp_itv cmp_loc arr1 arr2 =
  match (is_singleton_or_more arr1, is_singleton_or_more arr2) with
  | IContainer.Singleton (as1, ai1), IContainer.Singleton (as2, ai2) ->
      Boolean.EqualOrder.(
        of_equal
          {on_equal= ArrInfo.lift_cmp_itv cmp_itv ai1 ai2; on_not_equal= cmp_loc.on_not_equal}
          (Allocsite.eq as1 as2))
  | _ ->
      Boolean.Top
