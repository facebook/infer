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

open! IStd
open AbsLoc
open! AbstractDomain.Types
module F = Format
module L = Logging
module ItvPure = Itv.ItvPure
module MF = MarkupFormatter
module PO = BufferOverrunProofObligations
module Trace = BufferOverrunTrace
module TraceSet = Trace.Set

(** unsound but ok for bug catching *)
let always_strong_update = true

module Val = struct
  type astate =
    {itv: Itv.astate; powloc: PowLoc.astate; arrayblk: ArrayBlk.astate; traces: TraceSet.t}

  type t = astate

  let bot : t = {itv= Itv.bot; powloc= PowLoc.bot; arrayblk= ArrayBlk.bot; traces= TraceSet.empty}

  let pp fmt x =
    if Config.bo_debug <= 1 then
      F.fprintf fmt "(%a, %a, %a)" Itv.pp x.itv PowLoc.pp x.powloc ArrayBlk.pp x.arrayblk
    else
      F.fprintf fmt "(%a, %a, %a, %a)" Itv.pp x.itv PowLoc.pp x.powloc ArrayBlk.pp x.arrayblk
        TraceSet.pp x.traces

  let unknown : t = {bot with itv= Itv.top; powloc= PowLoc.unknown; arrayblk= ArrayBlk.unknown}

  let ( <= ) ~lhs ~rhs =
    if phys_equal lhs rhs then true
    else Itv.( <= ) ~lhs:lhs.itv ~rhs:rhs.itv && PowLoc.( <= ) ~lhs:lhs.powloc ~rhs:rhs.powloc
      && ArrayBlk.( <= ) ~lhs:lhs.arrayblk ~rhs:rhs.arrayblk

  let widen ~prev ~next ~num_iters =
    if phys_equal prev next then prev
    else
      { itv= Itv.widen ~prev:prev.itv ~next:next.itv ~num_iters
      ; powloc= PowLoc.widen ~prev:prev.powloc ~next:next.powloc ~num_iters
      ; arrayblk= ArrayBlk.widen ~prev:prev.arrayblk ~next:next.arrayblk ~num_iters
      ; traces= TraceSet.join prev.traces next.traces }

  let join : t -> t -> t =
    fun x y ->
      if phys_equal x y then x
      else
        { itv= Itv.join x.itv y.itv
        ; powloc= PowLoc.join x.powloc y.powloc
        ; arrayblk= ArrayBlk.join x.arrayblk y.arrayblk
        ; traces= TraceSet.join x.traces y.traces }

  let rec joins : t list -> t = function [] -> bot | [a] -> a | a :: b -> join a (joins b)

  let get_itv : t -> Itv.t = fun x -> x.itv

  let get_pow_loc : t -> PowLoc.t = fun x -> x.powloc

  let get_array_blk : t -> ArrayBlk.astate = fun x -> x.arrayblk

  let get_array_locs : t -> PowLoc.t = fun x -> ArrayBlk.get_pow_loc x.arrayblk

  let get_all_locs : t -> PowLoc.t = fun x -> PowLoc.join x.powloc (get_array_locs x)

  let get_traces : t -> TraceSet.t = fun x -> x.traces

  let set_traces : TraceSet.t -> t -> t = fun traces x -> {x with traces}

  let of_itv itv = {bot with itv}

  let of_int n = of_itv (Itv.of_int n)

  let of_itv : Itv.t -> t = fun itv -> {bot with itv}

  let of_pow_loc : PowLoc.t -> t = fun x -> {bot with powloc= x}

  let of_array_blk : ArrayBlk.astate -> t = fun a -> {bot with arrayblk= a}

  let modify_itv : Itv.t -> t -> t = fun i x -> {x with itv= i}

  let make_sym : ?unsigned:bool -> Typ.Procname.t -> (unit -> int) -> t =
    fun ?(unsigned= false) pname new_sym_num ->
      {bot with itv= Itv.make_sym ~unsigned pname new_sym_num}

  let unknown_bit : t -> t = fun x -> {x with itv= Itv.top}

  let neg : t -> t = fun x -> {x with itv= Itv.neg x.itv}

  let lnot : t -> t = fun x -> {x with itv= Itv.lnot x.itv}

  let lift_itv : (Itv.t -> Itv.t -> Itv.t) -> t -> t -> t =
    fun f x y -> {bot with itv= f x.itv y.itv}

  let has_pointer : t -> bool = fun x -> not (PowLoc.is_bot x.powloc && ArrayBlk.is_bot x.arrayblk)

  let lift_cmp_itv : (Itv.t -> Itv.t -> Itv.t) -> t -> t -> t =
    fun f x y ->
      if has_pointer x || has_pointer y then {bot with itv= Itv.unknown_bool} else lift_itv f x y

  let plus : t -> t -> t =
    fun x y ->
      { x with
        itv= Itv.plus x.itv y.itv
      ; arrayblk= ArrayBlk.plus_offset x.arrayblk y.itv
      ; traces= TraceSet.join x.traces y.traces }

  let minus : t -> t -> t =
    fun x y ->
      let n = Itv.join (Itv.minus x.itv y.itv) (ArrayBlk.diff x.arrayblk y.arrayblk) in
      let a = ArrayBlk.minus_offset x.arrayblk y.itv in
      {bot with itv= n; arrayblk= a; traces= TraceSet.join x.traces y.traces}

  let mult : t -> t -> t =
    fun x y -> {(lift_itv Itv.mult x y) with traces= TraceSet.join x.traces y.traces}

  let div : t -> t -> t =
    fun x y -> {(lift_itv Itv.div x y) with traces= TraceSet.join x.traces y.traces}

  let mod_sem : t -> t -> t = lift_itv Itv.mod_sem

  let shiftlt : t -> t -> t = lift_itv Itv.shiftlt

  let shiftrt : t -> t -> t = lift_itv Itv.shiftrt

  let lt_sem : t -> t -> t = lift_cmp_itv Itv.lt_sem

  let gt_sem : t -> t -> t = lift_cmp_itv Itv.gt_sem

  let le_sem : t -> t -> t = lift_cmp_itv Itv.le_sem

  let ge_sem : t -> t -> t = lift_cmp_itv Itv.ge_sem

  let eq_sem : t -> t -> t = lift_cmp_itv Itv.eq_sem

  let ne_sem : t -> t -> t = lift_cmp_itv Itv.ne_sem

  let land_sem : t -> t -> t = lift_itv Itv.land_sem

  let lor_sem : t -> t -> t = lift_itv Itv.lor_sem

  let lift_prune1 : (Itv.t -> Itv.t) -> t -> t = fun f x -> {x with itv= f x.itv}

  let lift_prune2
      : (Itv.t -> Itv.t -> Itv.t) -> (ArrayBlk.astate -> ArrayBlk.astate -> ArrayBlk.astate) -> t
        -> t -> t =
    fun f g x y ->
      { x with
        itv= f x.itv y.itv
      ; arrayblk= g x.arrayblk y.arrayblk
      ; traces= TraceSet.join x.traces y.traces }

  let prune_zero : t -> t = lift_prune1 Itv.prune_zero

  let prune_comp : Binop.t -> t -> t -> t =
    fun c -> lift_prune2 (Itv.prune_comp c) (ArrayBlk.prune_comp c)

  let prune_eq : t -> t -> t = lift_prune2 Itv.prune_eq ArrayBlk.prune_eq

  let prune_ne : t -> t -> t = lift_prune2 Itv.prune_ne ArrayBlk.prune_eq

  let lift_pi : (ArrayBlk.astate -> Itv.t -> ArrayBlk.astate) -> t -> t -> t =
    fun f x y -> {bot with arrayblk= f x.arrayblk y.itv; traces= TraceSet.join x.traces y.traces}

  let plus_pi : t -> t -> t = fun x y -> lift_pi ArrayBlk.plus_offset x y

  let minus_pi : t -> t -> t = fun x y -> lift_pi ArrayBlk.minus_offset x y

  let minus_pp : t -> t -> t =
    fun x y ->
      (* when we cannot precisely follow the physical memory model, return top *)
      if not (PowLoc.is_bot x.powloc) && ArrayBlk.is_bot x.arrayblk
         || not (PowLoc.is_bot y.powloc) && ArrayBlk.is_bot y.arrayblk
      then {bot with itv= Itv.top}
      else
        {bot with itv= ArrayBlk.diff x.arrayblk y.arrayblk; traces= TraceSet.join x.traces y.traces}

  let get_symbols : t -> Itv.Symbol.t list =
    fun x -> List.append (Itv.get_symbols x.itv) (ArrayBlk.get_symbols x.arrayblk)

  let normalize : t -> t =
    fun x -> {x with itv= Itv.normalize x.itv; arrayblk= ArrayBlk.normalize x.arrayblk}

  let subst
      : t -> Itv.Bound.t bottom_lifted Itv.SubstMap.t * TraceSet.t Itv.SubstMap.t -> Location.t
        -> t =
    fun x (bound_map, trace_map) loc ->
      let symbols = get_symbols x in
      let traces_caller =
        List.fold symbols
          ~f:(fun traces symbol ->
            try TraceSet.join (Itv.SubstMap.find symbol trace_map) traces
            with Not_found -> traces)
          ~init:TraceSet.empty
      in
      let traces = TraceSet.instantiate ~traces_caller ~traces_callee:x.traces loc in
      {x with itv= Itv.subst x.itv bound_map; arrayblk= ArrayBlk.subst x.arrayblk bound_map; traces}
      |> normalize

  (* normalize bottom *)

  let add_trace_elem : Trace.elem -> t -> t =
    fun elem x ->
      let traces = TraceSet.add_elem elem x.traces in
      {x with traces}

  let add_trace_elem_last : Trace.elem -> t -> t =
    fun elem x ->
      let traces = TraceSet.add_elem_last elem x.traces in
      {x with traces}

  let pp_summary : F.formatter -> t -> unit =
    fun fmt x -> F.fprintf fmt "(%a, %a)" Itv.pp x.itv ArrayBlk.pp x.arrayblk

  module Itv = struct
    let nat = of_itv Itv.nat

    let m1_255 = of_itv Itv.m1_255

    let pos = of_itv Itv.pos

    let top = of_itv Itv.top
  end
end

module Stack = struct
  include AbstractDomain.Map (Loc) (Val)

  let bot = empty

  let find : Loc.t -> astate -> Val.t =
    fun l m ->
      try find l m
      with Not_found -> Val.bot

  let find_set : PowLoc.t -> astate -> Val.t =
    fun locs mem ->
      let find_join loc acc = Val.join acc (find loc mem) in
      PowLoc.fold find_join locs Val.bot

  let strong_update : PowLoc.t -> Val.astate -> astate -> astate =
    fun locs v mem -> PowLoc.fold (fun x -> add x v) locs mem

  let weak_update : PowLoc.t -> Val.astate -> astate -> astate =
    fun locs v mem -> PowLoc.fold (fun x -> add x (Val.join v (find x mem))) locs mem

  let pp_summary : F.formatter -> astate -> unit =
    fun fmt mem ->
      let pp_not_logical_var k v =
        if Loc.is_logical_var k then () else F.fprintf fmt "%a -> %a@," Loc.pp k Val.pp_summary v
      in
      iter pp_not_logical_var mem

  let remove_temps : Ident.t list -> astate -> astate =
    fun temps mem ->
      let remove_temp mem temp =
        let temp_loc = Loc.of_id temp in
        remove temp_loc mem
      in
      List.fold temps ~init:mem ~f:remove_temp
end

module Heap = struct
  module PPMap = struct
    include PrettyPrintable.MakePPMap (Loc)

    let pp_collection : pp_item:(F.formatter -> 'a -> unit) -> F.formatter -> 'a list -> unit =
      fun ~pp_item fmt c ->
        let pp_sep fmt () = F.fprintf fmt ",@," in
        F.pp_print_list ~pp_sep pp_item fmt c

    let pp : pp_value:(F.formatter -> 'a -> unit) -> F.formatter -> 'a t -> unit =
      fun ~pp_value fmt m ->
        let pp_item fmt (k, v) = F.fprintf fmt "%a -> %a" Loc.pp k pp_value v in
        F.fprintf fmt "@[<v 2>{ " ;
        pp_collection ~pp_item fmt (bindings m) ;
        F.fprintf fmt " }@]"
  end

  include AbstractDomain.Map (Loc) (Val)

  let bot = empty

  let find : Loc.t -> astate -> Val.t =
    fun l m ->
      try find l m
      with Not_found -> Val.Itv.top

  let find_set : PowLoc.t -> astate -> Val.t =
    fun locs mem ->
      let find_join loc acc = Val.join acc (find loc mem) in
      PowLoc.fold find_join locs Val.bot

  let strong_update : PowLoc.t -> Val.t -> astate -> astate =
    fun locs v mem -> PowLoc.fold (fun x -> add x v) locs mem

  let weak_update : PowLoc.t -> Val.t -> astate -> astate =
    fun locs v mem -> PowLoc.fold (fun x -> add x (Val.join v (find x mem))) locs mem

  let pp_summary : F.formatter -> astate -> unit =
    fun fmt mem ->
      let pp_map fmt (k, v) = F.fprintf fmt "%a -> %a" Loc.pp k Val.pp_summary v in
      F.fprintf fmt "@[<v 2>{ " ;
      F.pp_print_list pp_map fmt (bindings mem) ;
      F.fprintf fmt " }@]"

  let get_symbols : astate -> Itv.Symbol.t list =
    fun mem -> List.concat_map ~f:(fun (_, v) -> Val.get_symbols v) (bindings mem)

  let get_return : astate -> Val.t =
    fun mem ->
      let mem = filter (fun l _ -> Loc.is_return l) mem in
      if is_empty mem then Val.bot else snd (choose mem)
end

module AliasTarget = struct
  type t = Simple of Loc.t | Empty of Loc.t [@@deriving compare]

  let equal = [%compare.equal : t]

  let pp fmt = function Simple l -> Loc.pp fmt l | Empty l -> F.fprintf fmt "empty(%a)" Loc.pp l

  let of_simple l = Simple l

  let of_empty l = Empty l

  let use l = function Simple l' | Empty l' -> Loc.equal l l'

  let replace l = function Simple _ -> Simple l | Empty _ -> Empty l
end

(* Relations between values of logical variables(registers) and
   program variables

   "AliasTarget.Simple relation": Since Sil distinguishes logical and
   program variables, we need a relation for pruning values of program
   variables.  For example, a C statement "if(x){...}" is translated
   to "%r=load(x); if(%r){...}" in Sil.  At the load statement, we
   record the alias between the values of %r and x, then we can prune
   not only the value of %r, but also that of x inside the if branch.

   "AliasTarget.Empty relation": For pruning vector.size with
   vector::empty() results, we adopt a specific relation between %r
   and x, where %r=v.empty() and x=v.size.  So, if %r!=0, x is pruned
   by x=0.  On the other hand, if %r==0, x is pruned by x>=1.  *)
module AliasMap = struct
  module M = Caml.Map.Make (Ident)

  type t = AliasTarget.t M.t

  type astate = t

  let bot : t = M.empty

  let ( <= ) : lhs:t -> rhs:t -> bool =
    fun ~lhs ~rhs ->
      let is_in_rhs k v =
        match M.find k rhs with v' -> AliasTarget.equal v v' | exception Not_found -> false
      in
      M.for_all is_in_rhs lhs

  let join : t -> t -> t =
    fun x y ->
      let join_v _ v1_opt v2_opt =
        match (v1_opt, v2_opt) with
        | None, None
         -> None
        | Some v, None | None, Some v
         -> Some v
        | Some v1, Some v2
         -> if AliasTarget.equal v1 v2 then Some v1 else assert false
      in
      M.merge join_v x y

  let widen : prev:t -> next:t -> num_iters:int -> t =
    fun ~prev ~next ~num_iters:_ -> join prev next

  let pp : F.formatter -> t -> unit =
    fun fmt x ->
      let pp_sep fmt () = F.fprintf fmt ", @," in
      let pp1 fmt (k, v) = F.fprintf fmt "%a=%a" (Ident.pp Pp.text) k AliasTarget.pp v in
      (* F.fprintf fmt "@[<v 0>Logical Variables :@,"; *)
      F.fprintf fmt "@[<hov 2>{ @," ;
      F.pp_print_list ~pp_sep pp1 fmt (M.bindings x) ;
      F.fprintf fmt " }@]" ;
      F.fprintf fmt "@]"

  let load : Ident.t -> AliasTarget.t -> t -> t = fun id loc m -> M.add id loc m

  let store : Loc.t -> Exp.t -> t -> t =
    fun l _ m -> M.filter (fun _ y -> not (AliasTarget.use l y)) m

  let find : Ident.t -> t -> AliasTarget.t option =
    fun k m ->
      try Some (M.find k m)
      with Not_found -> None

  let remove_temps : Ident.t list -> t -> t =
    fun temps m ->
      let remove_temp m temp = M.remove temp m in
      List.fold temps ~init:m ~f:remove_temp
end

module AliasRet = struct
  type astate = Bot | L of AliasTarget.t | Top

  let bot = Bot

  let ( <= ) : lhs:astate -> rhs:astate -> bool =
    fun ~lhs ~rhs ->
      match (lhs, rhs) with
      | Bot, _ | _, Top
       -> true
      | Top, _ | _, Bot
       -> false
      | L loc1, L loc2
       -> AliasTarget.equal loc1 loc2

  let join : astate -> astate -> astate =
    fun x y ->
      match (x, y) with
      | Top, _ | _, Top
       -> Top
      | Bot, a | a, Bot
       -> a
      | L loc1, L loc2
       -> if AliasTarget.equal loc1 loc2 then x else Top

  let widen : prev:astate -> next:astate -> num_iters:int -> astate =
    fun ~prev ~next ~num_iters:_ -> join prev next

  let pp : F.formatter -> astate -> unit =
    fun fmt x ->
      match x with
      | Top
       -> F.fprintf fmt "T"
      | L loc
       -> AliasTarget.pp fmt loc
      | Bot
       -> F.fprintf fmt "_|_"

  let find : astate -> AliasTarget.t option = fun x -> match x with L loc -> Some loc | _ -> None
end

module Alias = struct
  include AbstractDomain.Pair (AliasMap) (AliasRet)

  let bot : astate = (AliasMap.bot, AliasRet.bot)

  let lift : (AliasMap.astate -> AliasMap.astate) -> astate -> astate =
    fun f a -> (f (fst a), snd a)

  let lift_v : (AliasMap.astate -> 'a) -> astate -> 'a = fun f a -> f (fst a)

  let find : Ident.t -> astate -> AliasTarget.t option = fun x -> lift_v (AliasMap.find x)

  let find_ret : astate -> AliasTarget.t option = fun x -> AliasRet.find (snd x)

  let load : Ident.t -> AliasTarget.t -> astate -> astate =
    fun id loc -> lift (AliasMap.load id loc)

  let store_simple : Loc.t -> Exp.t -> astate -> astate =
    fun loc e a ->
      let a = lift (AliasMap.store loc e) a in
      match e with
      | Exp.Var l when Loc.is_return loc
       -> let update_ret retl = (fst a, AliasRet.L retl) in
          Option.value_map (find l a) ~default:a ~f:update_ret
      | _
       -> a

  let store_empty : Val.t -> Loc.t -> Exp.t -> astate -> astate =
    fun formal loc e a ->
      let a = lift (AliasMap.store loc e) a in
      let locs = Val.get_all_locs formal in
      if PowLoc.is_singleton locs then
        (fst a, AliasRet.L (AliasTarget.of_empty (PowLoc.min_elt locs)))
      else a

  let remove_temps : Ident.t list -> astate -> astate =
    fun temps a -> (AliasMap.remove_temps temps (fst a), snd a)
end

module MemReach = struct
  type astate = {stack: Stack.astate; heap: Heap.astate; alias: Alias.astate}

  type t = astate

  let bot : t = {stack= Stack.bot; heap= Heap.bot; alias= Alias.bot}

  let ( <= ) ~lhs ~rhs =
    if phys_equal lhs rhs then true
    else Stack.( <= ) ~lhs:lhs.stack ~rhs:rhs.stack && Heap.( <= ) ~lhs:lhs.heap ~rhs:rhs.heap
      && Alias.( <= ) ~lhs:lhs.alias ~rhs:rhs.alias

  let widen ~prev ~next ~num_iters =
    if phys_equal prev next then prev
    else
      { stack= Stack.widen ~prev:prev.stack ~next:next.stack ~num_iters
      ; heap= Heap.widen ~prev:prev.heap ~next:next.heap ~num_iters
      ; alias= Alias.widen ~prev:prev.alias ~next:next.alias ~num_iters }

  let join : t -> t -> t =
    fun x y ->
      { stack= Stack.join x.stack y.stack
      ; heap= Heap.join x.heap y.heap
      ; alias= Alias.join x.alias y.alias }

  let pp : F.formatter -> t -> unit =
    fun fmt x ->
      F.fprintf fmt "Stack:@;" ;
      F.fprintf fmt "%a@;" Stack.pp x.stack ;
      F.fprintf fmt "Heap:@;" ;
      F.fprintf fmt "%a" Heap.pp x.heap

  let pp_summary : F.formatter -> t -> unit =
    fun fmt x ->
      F.fprintf fmt "@[<v 0>Parameters:@," ;
      F.fprintf fmt "%a" Heap.pp_summary x.heap ;
      F.fprintf fmt "@]"

  let find_stack : Loc.t -> t -> Val.t = fun k m -> Stack.find k m.stack

  let find_stack_set : PowLoc.t -> t -> Val.t = fun k m -> Stack.find_set k m.stack

  let find_heap : Loc.t -> t -> Val.t = fun k m -> Heap.find k m.heap

  let find_heap_set : PowLoc.t -> t -> Val.t = fun k m -> Heap.find_set k m.heap

  let find_set : PowLoc.t -> t -> Val.t =
    fun k m -> Val.join (find_stack_set k m) (find_heap_set k m)

  let find_alias : Ident.t -> t -> AliasTarget.t option = fun k m -> Alias.find k m.alias

  let find_simple_alias : Ident.t -> t -> Loc.t option =
    fun k m ->
      match Alias.find k m.alias with
      | Some AliasTarget.Simple l
       -> Some l
      | Some AliasTarget.Empty _ | None
       -> None

  let find_ret_alias : t -> AliasTarget.t option = fun m -> Alias.find_ret m.alias

  let load_alias : Ident.t -> AliasTarget.t -> t -> t =
    fun id loc m -> {m with alias= Alias.load id loc m.alias}

  let store_simple_alias : Loc.t -> Exp.t -> t -> t =
    fun loc e m -> {m with alias= Alias.store_simple loc e m.alias}

  let store_empty_alias : Val.t -> Loc.t -> Exp.t -> t -> t =
    fun formal loc e m -> {m with alias= Alias.store_empty formal loc e m.alias}

  let add_stack : Loc.t -> Val.t -> t -> t = fun k v m -> {m with stack= Stack.add k v m.stack}

  let add_heap : Loc.t -> Val.t -> t -> t = fun k v m -> {m with heap= Heap.add k v m.heap}

  let strong_update_stack : PowLoc.t -> Val.t -> t -> t =
    fun p v m -> {m with stack= Stack.strong_update p v m.stack}

  let strong_update_heap : PowLoc.t -> Val.t -> t -> t =
    fun p v m -> {m with heap= Heap.strong_update p v m.heap}

  let weak_update_stack : PowLoc.t -> Val.t -> t -> t =
    fun p v m -> {m with stack= Stack.weak_update p v m.stack}

  let weak_update_heap : PowLoc.t -> Val.t -> t -> t =
    fun p v m -> {m with heap= Heap.weak_update p v m.heap}

  let get_heap_symbols : t -> Itv.Symbol.t list = fun m -> Heap.get_symbols m.heap

  let get_return : t -> Val.t = fun m -> Heap.get_return m.heap

  let can_strong_update : PowLoc.t -> bool =
    fun ploc ->
      if always_strong_update then true
      else if Int.equal (PowLoc.cardinal ploc) 1 then Loc.is_var (PowLoc.choose ploc)
      else false

  let update_mem : PowLoc.t -> Val.t -> t -> t =
    fun ploc v s ->
      if can_strong_update ploc then strong_update_heap ploc v s
      else
        let () =
          L.(debug BufferOverrun Verbose) "Weak update for %a <- %a@." PowLoc.pp ploc Val.pp v
        in
        weak_update_heap ploc v s

  let remove_temps : Ident.t list -> t -> t =
    fun temps m ->
      {m with stack= Stack.remove_temps temps m.stack; alias= Alias.remove_temps temps m.alias}
end

module Mem = struct
  include AbstractDomain.BottomLifted (MemReach)

  type t = astate

  let bot : t = Bottom

  let init : t = NonBottom MemReach.bot

  let f_lift_default : 'a -> (MemReach.t -> 'a) -> t -> 'a =
    fun default f m -> match m with Bottom -> default | NonBottom m' -> f m'

  let f_lift : (MemReach.t -> MemReach.t) -> t -> t =
    fun f -> f_lift_default Bottom (fun m' -> NonBottom (f m'))

  let pp_summary : F.formatter -> t -> unit =
    fun fmt m ->
      match m with
      | Bottom
       -> F.fprintf fmt "unreachable"
      | NonBottom m'
       -> MemReach.pp_summary fmt m'

  let find_stack : Loc.t -> t -> Val.t = fun k -> f_lift_default Val.bot (MemReach.find_stack k)

  let find_stack_set : PowLoc.t -> t -> Val.t =
    fun k -> f_lift_default Val.bot (MemReach.find_stack_set k)

  let find_heap : Loc.t -> t -> Val.t = fun k -> f_lift_default Val.bot (MemReach.find_heap k)

  let find_heap_set : PowLoc.t -> t -> Val.t =
    fun k -> f_lift_default Val.bot (MemReach.find_heap_set k)

  let find_set : PowLoc.t -> t -> Val.t = fun k -> f_lift_default Val.bot (MemReach.find_set k)

  let find_alias : Ident.t -> t -> AliasTarget.t option =
    fun k -> f_lift_default None (MemReach.find_alias k)

  let find_simple_alias : Ident.t -> t -> Loc.t option =
    fun k -> f_lift_default None (MemReach.find_simple_alias k)

  let find_ret_alias : t -> AliasTarget.t option = f_lift_default None MemReach.find_ret_alias

  let load_alias : Ident.t -> AliasTarget.t -> t -> t =
    fun id loc -> f_lift (MemReach.load_alias id loc)

  let load_simple_alias : Ident.t -> Loc.t -> t -> t =
    fun id loc -> load_alias id (AliasTarget.Simple loc)

  let store_simple_alias : Loc.t -> Exp.t -> t -> t =
    fun loc e -> f_lift (MemReach.store_simple_alias loc e)

  let store_empty_alias : Val.t -> Loc.t -> Exp.t -> t -> t =
    fun formal loc e -> f_lift (MemReach.store_empty_alias formal loc e)

  let add_stack : Loc.t -> Val.t -> t -> t = fun k v -> f_lift (MemReach.add_stack k v)

  let add_heap : Loc.t -> Val.t -> t -> t = fun k v -> f_lift (MemReach.add_heap k v)

  let strong_update_stack : PowLoc.t -> Val.t -> t -> t =
    fun p v -> f_lift (MemReach.strong_update_stack p v)

  let strong_update_heap : PowLoc.t -> Val.t -> t -> t =
    fun p v -> f_lift (MemReach.strong_update_heap p v)

  let weak_update_stack : PowLoc.t -> Val.t -> t -> t =
    fun p v -> f_lift (MemReach.weak_update_stack p v)

  let weak_update_heap : PowLoc.t -> Val.t -> t -> t =
    fun p v -> f_lift (MemReach.weak_update_heap p v)

  let get_heap_symbols : t -> Itv.Symbol.t list = f_lift_default [] MemReach.get_heap_symbols

  let get_return : t -> Val.t = f_lift_default Val.bot MemReach.get_return

  let can_strong_update : PowLoc.t -> bool = MemReach.can_strong_update

  let update_mem : PowLoc.t -> Val.t -> t -> t = fun ploc v -> f_lift (MemReach.update_mem ploc v)

  let remove_temps : Ident.t list -> t -> t = fun temps -> f_lift (MemReach.remove_temps temps)
end

module Summary = struct
  type t = Mem.t * Mem.t * PO.ConditionSet.t

  let get_input : t -> Mem.t = fst3

  let get_output : t -> Mem.t = snd3

  let get_cond_set : t -> PO.ConditionSet.t = trd3

  let get_symbols : t -> Itv.Symbol.t list = fun s -> Mem.get_heap_symbols (get_input s)

  let get_return : t -> Val.t = fun s -> Mem.get_return (get_output s)

  let pp_symbols : F.formatter -> t -> unit =
    fun fmt s ->
      let pp_sep fmt () = F.fprintf fmt ", @," in
      F.fprintf fmt "@[<hov 2>Symbols: {" ;
      F.pp_print_list ~pp_sep Itv.Symbol.pp fmt (get_symbols s) ;
      F.fprintf fmt "}@]"

  let pp_symbol_map : F.formatter -> t -> unit = fun fmt s -> Mem.pp_summary fmt (get_input s)

  let pp_return : F.formatter -> t -> unit =
    fun fmt s -> F.fprintf fmt "Return value: %a" Val.pp_summary (get_return s)

  let pp_summary : F.formatter -> t -> unit =
    fun fmt s ->
      F.fprintf fmt "%a@,%a@,%a" pp_symbol_map s pp_return s PO.ConditionSet.pp_summary
        (get_cond_set s)

  let pp : F.formatter -> t -> unit =
    fun fmt (entry_mem, exit_mem, condition_set) ->
      F.fprintf fmt "%a@,%a@,%a@," Mem.pp entry_mem Mem.pp exit_mem PO.ConditionSet.pp
        condition_set
end
