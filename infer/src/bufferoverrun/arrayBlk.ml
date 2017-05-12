(*
 * Copyright (c) 2016 - present
 *
 * Programming Research Laboratory (ROPAS)
 * Seoul National University, Korea
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

(* Abstract Array Block *)
open! IStd
open AbsLoc

module ArrInfo =
struct
  type t =
    { offset : Itv.t;
      size : Itv.t;
      stride : Itv.t; }
  [@@deriving compare]

  type astate = t

  let bot : t
    = { offset = Itv.bot; size = Itv.bot; stride = Itv.bot; }

  let initial : t
    = bot

  let top : t
    = { offset = Itv.top; size = Itv.top; stride = Itv.top; }

  let input : t
    = { offset = Itv.zero; size = Itv.pos; stride = Itv.one; }

  let make : Itv.t * Itv.t * Itv.t -> t
    = fun (o, s, stride) -> { offset = o; size = s; stride = stride; }

  let join : t -> t -> t
    = fun a1 a2 ->
      if phys_equal a1 a2 then a2 else
        { offset = Itv.join a1.offset a2.offset;
          size = Itv.join a1.size a2.size;
          stride = Itv.join a1.stride a2.stride; }

  let widen : prev:t -> next:t -> num_iters:int -> t
    = fun ~prev ~next ~num_iters ->
      if phys_equal prev next then next else
        { offset = Itv.widen ~prev:prev.offset ~next:next.offset ~num_iters;
          size = Itv.widen ~prev:prev.size ~next:next.size ~num_iters;
          stride = Itv.widen ~prev:prev.stride ~next:next.stride ~num_iters; }

  let eq : t -> t -> bool
    = fun a1 a2 ->
      if phys_equal a1 a2 then true else
        Itv.eq a1.offset a2.offset
        && Itv.eq a1.size a2.size
        && Itv.eq a1.stride a2.stride

  let (<=) : lhs:t -> rhs:t -> bool
    = fun ~lhs ~rhs ->
      if phys_equal lhs rhs then true else
        Itv.le ~lhs:lhs.offset ~rhs:rhs.offset
        && Itv.le ~lhs:lhs.size ~rhs:rhs.size
        && Itv.le ~lhs:lhs.stride ~rhs:rhs.stride

  let weak_plus_size : t -> Itv.t -> t
    = fun arr i -> { arr with size = Itv.join arr.size (Itv.plus i arr.size) }

  let plus_offset : t -> Itv.t -> t
    = fun arr i -> { arr with offset = Itv.plus arr.offset i }

  let minus_offset : t -> Itv.astate -> t
    = fun arr i -> { arr with offset = Itv.minus arr.offset i }

  let diff : t -> t -> Itv.astate
    = fun arr1 arr2 ->
      let i1 = Itv.mult arr1.offset arr1.stride in
      let i2 = Itv.mult arr2.offset arr2.stride in
      Itv.minus i1 i2

  let subst : t -> Itv.Bound.t Itv.SubstMap.t -> t
    = fun arr subst_map ->
      { arr with offset = Itv.subst arr.offset subst_map;
                 size = Itv.subst arr.size subst_map; }

  let pp : Format.formatter -> t -> unit
    = fun fmt arr ->
      Format.fprintf fmt "offset : %a, size : %a"
        Itv.pp arr.offset Itv.pp arr.size

  let get_symbols : t -> Itv.Symbol.t list
    = fun arr ->
      let s1 = Itv.get_symbols arr.offset in
      let s2 = Itv.get_symbols arr.size in
      let s3 = Itv.get_symbols arr.stride in
      List.concat [s1; s2; s3]

  let normalize : t -> t
    = fun arr ->
      { offset = Itv.normalize arr.offset;
        size = Itv.normalize arr.size;
        stride = Itv.normalize arr.stride }

  let prune_comp : Binop.t -> t -> t -> t
    = fun c arr1 arr2 ->
      { arr1 with offset = Itv.prune_comp c arr1.offset arr2.offset }

  let prune_eq : t -> t -> t
    = fun arr1 arr2 -> { arr1 with offset = Itv.prune_eq arr1.offset arr2.offset }

  let prune_ne : t -> t -> t
    = fun arr1 arr2 -> { arr1 with offset = Itv.prune_ne arr1.offset arr2.offset }
end

module PPMap =
struct
  include PrettyPrintable.MakePPMap (Allocsite)

  let pp ~pp_value fmt m =
    let pp_item fmt (k, v) = F.fprintf fmt "(%a, %a)" pp_key k pp_value v in
    PrettyPrintable.pp_collection ~pp_item fmt (bindings m)
end

include AbstractDomain.Map (PPMap) (ArrInfo)

let bot : astate
  = empty

let is_bot : astate -> bool
  = is_empty

let make : Allocsite.t -> Itv.t -> Itv.t -> Itv.t -> astate
  = fun a o sz st -> add a (ArrInfo.make (o, sz, st)) bot

let offsetof : astate -> Itv.t
  = fun a -> fold (fun _ arr -> Itv.join arr.ArrInfo.offset) a Itv.bot

let sizeof : astate -> Itv.t
  = fun a -> fold (fun _ arr -> Itv.join arr.ArrInfo.size) a Itv.bot

let extern : string -> astate
  = fun allocsite -> add allocsite ArrInfo.top empty

let input : string -> astate
  = fun allocsite -> add allocsite ArrInfo.input empty

let weak_plus_size : astate -> Itv.t -> astate
  = fun arr i -> map (fun a -> ArrInfo.weak_plus_size a i) arr

let plus_offset : astate -> Itv.t -> astate
  = fun arr i -> map (fun a -> ArrInfo.plus_offset a i) arr

let minus_offset : astate -> Itv.t -> astate
  = fun arr i -> map (fun a -> ArrInfo.minus_offset a i) arr

let diff : astate -> astate -> Itv.t
  = fun arr1 arr2 ->
    let diff_join k a2 acc =
      match find k arr1 with
      | a1 -> Itv.join acc (ArrInfo.diff a1 a2)
      | exception Not_found -> Itv.top
    in
    fold diff_join arr2 Itv.bot

let get_pow_loc : astate -> PowLoc.t
  = fun array ->
    let pow_loc_of_allocsite k _ acc = PowLoc.add (Loc.of_allocsite k) acc in
    fold pow_loc_of_allocsite array PowLoc.bot

let subst : astate -> Itv.Bound.t Itv.SubstMap.t -> astate
  = fun a subst_map -> map (fun info -> ArrInfo.subst info subst_map) a

let get_symbols : astate -> Itv.Symbol.t list
  = fun a ->
    List.concat_map ~f:(fun (_, ai) -> ArrInfo.get_symbols ai) (bindings a)

let normalize : astate -> astate
  = fun a -> map ArrInfo.normalize a

let do_prune
  : (ArrInfo.t -> ArrInfo.t -> ArrInfo.t) -> astate -> astate -> astate
  = fun arr_info_prune a1 a2 ->
    if Int.equal (cardinal a2) 1 then
      let (k, v2) = choose a2 in
      if mem k a1 then add k (arr_info_prune (find k a1) v2) a1 else a1
    else a1

let prune_comp : Binop.t -> astate -> astate -> astate
  = fun c a1 a2 -> do_prune (ArrInfo.prune_comp c) a1 a2

let prune_eq : astate -> astate -> astate
  = fun a1 a2 -> do_prune ArrInfo.prune_eq a1 a2

let prune_ne : astate -> astate -> astate
  = fun a1 a2 -> do_prune ArrInfo.prune_ne a1 a2
