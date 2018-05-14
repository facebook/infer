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


  let unknown : traces:TraceSet.t -> t =
   fun ~traces -> {itv= Itv.top; powloc= PowLoc.unknown; arrayblk= ArrayBlk.unknown; traces}


  let unknown_from : callee_pname:_ -> location:_ -> t =
   fun ~callee_pname ~location ->
    let traces =
      Trace.UnknownFrom (callee_pname, location) |> Trace.singleton |> TraceSet.singleton
    in
    unknown ~traces


  let ( <= ) ~lhs ~rhs =
    if phys_equal lhs rhs then true
    else
      Itv.( <= ) ~lhs:lhs.itv ~rhs:rhs.itv && PowLoc.( <= ) ~lhs:lhs.powloc ~rhs:rhs.powloc
      && ArrayBlk.( <= ) ~lhs:lhs.arrayblk ~rhs:rhs.arrayblk


  let equal x y = phys_equal x y || (( <= ) ~lhs:x ~rhs:y && ( <= ) ~lhs:y ~rhs:x)

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


  let get_itv : t -> Itv.t = fun x -> x.itv

  let get_pow_loc : t -> PowLoc.t = fun x -> x.powloc

  let get_array_blk : t -> ArrayBlk.astate = fun x -> x.arrayblk

  let get_array_locs : t -> PowLoc.t = fun x -> ArrayBlk.get_pow_loc x.arrayblk

  let get_all_locs : t -> PowLoc.t = fun x -> PowLoc.join x.powloc (get_array_locs x)

  let get_traces : t -> TraceSet.t = fun x -> x.traces

  let set_traces : TraceSet.t -> t -> t = fun traces x -> {x with traces}

  let of_itv ?(traces= TraceSet.empty) itv = {bot with itv; traces}

  let of_int n = of_itv (Itv.of_int n)

  let of_pow_loc : PowLoc.t -> t = fun x -> {bot with powloc= x}

  let of_array_blk : ArrayBlk.astate -> t = fun a -> {bot with arrayblk= a}

  let modify_itv : Itv.t -> t -> t = fun i x -> {x with itv= i}

  let make_sym : ?unsigned:bool -> Typ.Procname.t -> Itv.Counter.t -> t =
   fun ?(unsigned= false) pname new_sym_num ->
    {bot with itv= Itv.make_sym ~unsigned pname new_sym_num}


  let unknown_bit : t -> t = fun x -> {x with itv= Itv.top}

  let neg : t -> t = fun x -> {x with itv= Itv.neg x.itv}

  let lnot : t -> t = fun x -> {x with itv= Itv.lnot x.itv |> Itv.of_bool}

  let lift_itv : (Itv.t -> Itv.t -> Itv.t) -> t -> t -> t =
   fun f x y -> {bot with itv= f x.itv y.itv; traces= TraceSet.join x.traces y.traces}


  let has_pointer : t -> bool = fun x -> not (PowLoc.is_bot x.powloc && ArrayBlk.is_bot x.arrayblk)

  let lift_cmp_itv : (Itv.t -> Itv.t -> Itv.Boolean.t) -> t -> t -> t =
   fun f x y ->
    let b = if has_pointer x || has_pointer y then Itv.Boolean.top else f x.itv y.itv in
    let itv = Itv.of_bool b in
    {bot with itv; traces= TraceSet.join x.traces y.traces}


  let plus_a : t -> t -> t = lift_itv Itv.plus

  let minus_a : t -> t -> t = lift_itv Itv.minus

  let mult : t -> t -> t = lift_itv Itv.mult

  let div : t -> t -> t = lift_itv Itv.div

  let mod_sem : t -> t -> t = lift_itv Itv.mod_sem

  let shiftlt : t -> t -> t = lift_itv Itv.shiftlt

  let shiftrt : t -> t -> t = lift_itv Itv.shiftrt

  let lt_sem : t -> t -> t = lift_cmp_itv Itv.lt_sem

  let gt_sem : t -> t -> t = lift_cmp_itv Itv.gt_sem

  let le_sem : t -> t -> t = lift_cmp_itv Itv.le_sem

  let ge_sem : t -> t -> t = lift_cmp_itv Itv.ge_sem

  let eq_sem : t -> t -> t = lift_cmp_itv Itv.eq_sem

  let ne_sem : t -> t -> t = lift_cmp_itv Itv.ne_sem

  let land_sem : t -> t -> t = lift_cmp_itv Itv.land_sem

  let lor_sem : t -> t -> t = lift_cmp_itv Itv.lor_sem

  let lift_prune1 : (Itv.t -> Itv.t) -> t -> t = fun f x -> {x with itv= f x.itv}

  let lift_prune2
      : (Itv.t -> Itv.t -> Itv.t) -> (ArrayBlk.astate -> ArrayBlk.astate -> ArrayBlk.astate) -> t
        -> t -> t =
   fun f g x y ->
    { x with
      itv= f x.itv y.itv
    ; arrayblk= g x.arrayblk y.arrayblk
    ; traces= TraceSet.join x.traces y.traces }


  let prune_eq_zero : t -> t = lift_prune1 Itv.prune_eq_zero

  let prune_ne_zero : t -> t = lift_prune1 Itv.prune_ne_zero

  let prune_comp : Binop.t -> t -> t -> t =
   fun c -> lift_prune2 (Itv.prune_comp c) (ArrayBlk.prune_comp c)


  let prune_eq : t -> t -> t = lift_prune2 Itv.prune_eq ArrayBlk.prune_eq

  let prune_ne : t -> t -> t = lift_prune2 Itv.prune_ne ArrayBlk.prune_ne

  let is_pointer_to_non_array x = not (PowLoc.is_bot x.powloc) && ArrayBlk.is_bot x.arrayblk

  (* In the pointer arithmetics, it returns top, if we cannot
     precisely follow the physical memory model, e.g., (&x + 1). *)
  let lift_pi : (ArrayBlk.astate -> Itv.t -> ArrayBlk.astate) -> t -> t -> t =
   fun f x y ->
    let traces = TraceSet.join x.traces y.traces in
    if is_pointer_to_non_array x then {bot with itv= Itv.top; traces}
    else {bot with arrayblk= f x.arrayblk y.itv; traces}


  let plus_pi : t -> t -> t = fun x y -> lift_pi ArrayBlk.plus_offset x y

  let minus_pi : t -> t -> t = fun x y -> lift_pi ArrayBlk.minus_offset x y

  let minus_pp : t -> t -> t =
   fun x y ->
    let itv =
      if is_pointer_to_non_array x && is_pointer_to_non_array y then Itv.top
      else ArrayBlk.diff x.arrayblk y.arrayblk
    in
    {bot with itv; traces= TraceSet.join x.traces y.traces}


  let get_symbols : t -> Itv.Symbol.t list =
   fun x -> List.append (Itv.get_symbols x.itv) (ArrayBlk.get_symbols x.arrayblk)


  let normalize : t -> t =
   fun x -> {x with itv= Itv.normalize x.itv; arrayblk= ArrayBlk.normalize x.arrayblk}


  let subst
      : t -> Itv.Bound.t bottom_lifted Itv.SymbolMap.t * TraceSet.t Itv.SymbolMap.t -> Location.t
        -> t =
   fun x (bound_map, trace_map) location ->
    let symbols = get_symbols x in
    let traces_caller =
      List.fold symbols
        ~f:(fun traces symbol ->
          try TraceSet.join (Itv.SymbolMap.find symbol trace_map) traces with Caml.Not_found ->
            traces )
        ~init:TraceSet.empty
    in
    let traces = TraceSet.instantiate ~traces_caller ~traces_callee:x.traces location in
    {x with itv= Itv.subst x.itv bound_map; arrayblk= ArrayBlk.subst x.arrayblk bound_map; traces}
    |> normalize


  (* normalize bottom *)

  let add_trace_elem : Trace.elem -> t -> t =
   fun elem x ->
    let traces = TraceSet.add_elem elem x.traces in
    {x with traces}


  let pp_summary : F.formatter -> t -> unit =
   fun fmt x -> F.fprintf fmt "(%a, %a)" Itv.pp x.itv ArrayBlk.pp x.arrayblk


  let set_array_size : Itv.t -> t -> t =
   fun size v -> {v with arrayblk= ArrayBlk.set_size size v.arrayblk}


  module Itv = struct
    let nat = of_itv Itv.nat

    let m1_255 = of_itv Itv.m1_255

    let top = of_itv Itv.top

    let zero = of_itv Itv.zero
  end
end

module Stack = struct
  include AbstractDomain.Map (Loc) (Val)

  let bot = empty

  let find : Loc.t -> astate -> Val.t = fun l m -> try find l m with Caml.Not_found -> Val.bot

  let find_set : PowLoc.t -> astate -> Val.t =
   fun locs mem ->
    let find_join loc acc = Val.join acc (find loc mem) in
    PowLoc.fold find_join locs Val.bot


  let remove_temps : Ident.t list -> astate -> astate =
   fun temps mem ->
    let remove_temp mem temp =
      let temp_loc = Loc.of_id temp in
      remove temp_loc mem
    in
    List.fold temps ~init:mem ~f:remove_temp
end

module Heap = struct
  include AbstractDomain.Map (Loc) (Val)

  let bot = empty

  let find : Loc.t -> astate -> Val.t =
   fun l m -> try find l m with Caml.Not_found -> Val.Itv.top


  let find_set : PowLoc.t -> astate -> Val.t =
   fun locs mem ->
    let find_join loc acc = Val.join acc (find loc mem) in
    PowLoc.fold find_join locs Val.bot


  let transform : f:(Val.t -> Val.t) -> PowLoc.t -> astate -> astate =
   fun ~f locs mem -> PowLoc.fold (fun loc -> find loc mem |> f |> add loc) locs mem


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


  let get_return : astate -> Val.t =
   fun mem ->
    let mem = filter (fun l _ -> Loc.is_return l) mem in
    if is_empty mem then Val.bot else snd (choose mem)


  let range : filter_loc:(Loc.t -> bool) -> astate -> Itv.NonNegativePolynomial.astate =
   fun ~filter_loc mem ->
    fold
      (fun loc v acc ->
        if filter_loc loc then
          v |> Val.get_itv |> Itv.range |> Itv.ItvRange.to_top_lifted_polynomial
          |> Itv.NonNegativePolynomial.mult acc
        else acc )
      mem Itv.NonNegativePolynomial.one
end

module AliasTarget = struct
  type t = Simple of Loc.t | Empty of Loc.t [@@deriving compare]

  let equal = [%compare.equal : t]

  let pp fmt = function Simple l -> Loc.pp fmt l | Empty l -> F.fprintf fmt "empty(%a)" Loc.pp l

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
      match M.find k rhs with v' -> AliasTarget.equal v v' | exception Caml.Not_found -> false
    in
    M.for_all is_in_rhs lhs


  let join : t -> t -> t =
   fun x y ->
    let join_v _ v1_opt v2_opt =
      match (v1_opt, v2_opt) with
      | None, None ->
          None
      | Some v, None | None, Some v ->
          Some v
      | Some v1, Some v2 ->
          if AliasTarget.equal v1 v2 then Some v1 else assert false
    in
    M.merge join_v x y


  let widen : prev:t -> next:t -> num_iters:int -> t =
   fun ~prev ~next ~num_iters:_ -> join prev next


  let pp : F.formatter -> t -> unit =
   fun fmt x ->
    let pp_sep fmt () = F.fprintf fmt ", @," in
    let pp1 fmt (k, v) = F.fprintf fmt "%a=%a" Ident.pp k AliasTarget.pp v in
    (* F.fprintf fmt "@[<v 0>Logical Variables :@,"; *)
    F.fprintf fmt "@[<hov 2>{ @," ;
    F.pp_print_list ~pp_sep pp1 fmt (M.bindings x) ;
    F.fprintf fmt " }@]" ;
    F.fprintf fmt "@]"


  let load : Ident.t -> AliasTarget.t -> t -> t = fun id loc m -> M.add id loc m

  let store : Loc.t -> Exp.t -> t -> t =
   fun l _ m -> M.filter (fun _ y -> not (AliasTarget.use l y)) m


  let find : Ident.t -> t -> AliasTarget.t option = fun k m -> M.find_opt k m

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
    | Bot, _ | _, Top ->
        true
    | Top, _ | _, Bot ->
        false
    | L loc1, L loc2 ->
        AliasTarget.equal loc1 loc2


  let join : astate -> astate -> astate =
   fun x y ->
    match (x, y) with
    | Top, _ | _, Top ->
        Top
    | Bot, a | a, Bot ->
        a
    | L loc1, L loc2 ->
        if AliasTarget.equal loc1 loc2 then x else Top


  let widen : prev:astate -> next:astate -> num_iters:int -> astate =
   fun ~prev ~next ~num_iters:_ -> join prev next


  let pp : F.formatter -> astate -> unit =
   fun fmt x ->
    match x with
    | Top ->
        F.pp_print_char fmt 'T'
    | L loc ->
        AliasTarget.pp fmt loc
    | Bot ->
        F.pp_print_string fmt "_|_"


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
    | Exp.Var l when Loc.is_return loc ->
        let update_ret retl = (fst a, AliasRet.L retl) in
        Option.value_map (find l a) ~default:a ~f:update_ret
    | _ ->
        a


  let store_empty : Val.t -> Loc.t -> Exp.t -> astate -> astate =
   fun formal loc e a ->
    let a = lift (AliasMap.store loc e) a in
    let locs = Val.get_all_locs formal in
    if PowLoc.is_singleton locs then
      (fst a, AliasRet.L (AliasTarget.of_empty (PowLoc.min_elt locs)))
    else a


  let remove_temps : Ident.t list -> astate -> astate =
   fun temps a -> (AliasMap.remove_temps temps (fst a), snd a)


  let pp : F.formatter -> astate -> unit =
   fun fmt (aliasmap, aliasret) ->
    F.fprintf fmt "AliasMap:@;%a@;AliasRet:@;%a@;" AliasMap.pp aliasmap AliasRet.pp aliasret
end

module PrunePairs = struct
  module PrunePair = struct
    (* PrunePair.t is a pair of an abstract location and an abstract
       value where the abstract location was updated with the abstract
       value in the latest pruning. *)
    type t = Loc.t * Val.t

    let equal ((l1, v1) as x) ((l2, v2) as y) =
      phys_equal x y || (Loc.equal l1 l2 && Val.equal v1 v2)
  end

  type t = PrunePair.t list

  let empty = []

  let equal x y = List.equal x y ~equal:PrunePair.equal
end

module LatestPrune = struct
  (* Latest p: The pruned pairs 'p' has pruning information (which
     abstract locations are updated by which abstract values) in the
     latest pruning.

     TrueBranch (x, p): After a pruning, the variable 'x' is assigned
     by 1.  There is no other memory updates after the latest pruning.

     FalseBranch (x, p): After a pruning, the variable 'x' is assigned
     by 0.  There is no other memory updates after the latest pruning.

     V (x, ptrue, pfalse): After two non-sequential prunings ('ptrue'
     and 'pfalse'), the variable 'x' is assigned by 1 and 0,
     respectively.  There is no other memory updates after the latest
     prunings.

     Top: No information about the latest pruning. *)
  type astate =
    | Latest of PrunePairs.t
    | TrueBranch of Pvar.t * PrunePairs.t
    | FalseBranch of Pvar.t * PrunePairs.t
    | V of Pvar.t * PrunePairs.t * PrunePairs.t
    | Top

  let ( <= ) ~lhs ~rhs =
    if phys_equal lhs rhs then true
    else
      match (lhs, rhs) with
      | _, Top ->
          true
      | Top, _ ->
          false
      | Latest p1, Latest p2 ->
          PrunePairs.equal p1 p2
      | TrueBranch (x1, p1), TrueBranch (x2, p2)
      | FalseBranch (x1, p1), FalseBranch (x2, p2)
      | TrueBranch (x1, p1), V (x2, p2, _)
      | FalseBranch (x1, p1), V (x2, _, p2) ->
          Pvar.equal x1 x2 && PrunePairs.equal p1 p2
      | V (x1, ptrue1, pfalse1), V (x2, ptrue2, pfalse2) ->
          Pvar.equal x1 x2 && PrunePairs.equal ptrue1 ptrue2 && PrunePairs.equal pfalse1 pfalse2
      | _, _ ->
          false


  let join x y =
    match (x, y) with
    | _, _ when ( <= ) ~lhs:x ~rhs:y ->
        y
    | _, _ when ( <= ) ~lhs:y ~rhs:x ->
        x
    | FalseBranch (x', pfalse), TrueBranch (y', ptrue)
    | TrueBranch (x', ptrue), FalseBranch (y', pfalse)
      when Pvar.equal x' y' ->
        V (x', ptrue, pfalse)
    | _, _ ->
        Top


  let widen ~prev ~next ~num_iters:_ = join prev next

  let top = Top
end

module MemReach = struct
  type astate =
    {stack: Stack.astate; heap: Heap.astate; alias: Alias.astate; latest_prune: LatestPrune.astate}

  type t = astate

  let init : t = {stack= Stack.bot; heap= Heap.bot; alias= Alias.bot; latest_prune= LatestPrune.top}

  let ( <= ) ~lhs ~rhs =
    if phys_equal lhs rhs then true
    else
      Stack.( <= ) ~lhs:lhs.stack ~rhs:rhs.stack && Heap.( <= ) ~lhs:lhs.heap ~rhs:rhs.heap
      && Alias.( <= ) ~lhs:lhs.alias ~rhs:rhs.alias
      && LatestPrune.( <= ) ~lhs:lhs.latest_prune ~rhs:rhs.latest_prune


  let widen ~prev ~next ~num_iters =
    if phys_equal prev next then prev
    else
      { stack= Stack.widen ~prev:prev.stack ~next:next.stack ~num_iters
      ; heap= Heap.widen ~prev:prev.heap ~next:next.heap ~num_iters
      ; alias= Alias.widen ~prev:prev.alias ~next:next.alias ~num_iters
      ; latest_prune= LatestPrune.widen ~prev:prev.latest_prune ~next:next.latest_prune ~num_iters
      }


  let join : t -> t -> t =
   fun x y ->
    { stack= Stack.join x.stack y.stack
    ; heap= Heap.join x.heap y.heap
    ; alias= Alias.join x.alias y.alias
    ; latest_prune= LatestPrune.join x.latest_prune y.latest_prune }


  let pp : F.formatter -> t -> unit =
   fun fmt x ->
    F.fprintf fmt "Stack:@;%a@;Heap:@;%a@;%a" Stack.pp x.stack Heap.pp x.heap Alias.pp x.alias


  let pp_summary : F.formatter -> t -> unit =
   fun fmt x -> F.fprintf fmt "@[<v 0>Parameters:@,%a@]" Heap.pp_summary x.heap


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
    | Some (AliasTarget.Simple l) ->
        Some l
    | Some (AliasTarget.Empty _) | None ->
        None


  let find_ret_alias : t -> AliasTarget.t option = fun m -> Alias.find_ret m.alias

  let load_alias : Ident.t -> AliasTarget.t -> t -> t =
   fun id loc m -> {m with alias= Alias.load id loc m.alias}


  let store_simple_alias : Loc.t -> Exp.t -> t -> t =
   fun loc e m -> {m with alias= Alias.store_simple loc e m.alias}


  let store_empty_alias : Val.t -> Loc.t -> Exp.t -> t -> t =
   fun formal loc e m -> {m with alias= Alias.store_empty formal loc e m.alias}


  let add_stack : Loc.t -> Val.t -> t -> t = fun k v m -> {m with stack= Stack.add k v m.stack}

  let add_heap : Loc.t -> Val.t -> t -> t = fun k v m -> {m with heap= Heap.add k v m.heap}

  let strong_update_heap : PowLoc.t -> Val.t -> t -> t =
   fun p v m -> {m with heap= Heap.strong_update p v m.heap}


  let transform_heap : f:(Val.t -> Val.t) -> PowLoc.t -> t -> t =
   fun ~f p m -> {m with heap= Heap.transform ~f p m.heap}


  let weak_update_heap : PowLoc.t -> Val.t -> t -> t =
   fun p v m -> {m with heap= Heap.weak_update p v m.heap}


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


  let transform_mem : f:(Val.t -> Val.t) -> PowLoc.t -> t -> t =
   fun ~f ploc s -> transform_heap ~f ploc s


  let remove_temps : Ident.t list -> t -> t =
   fun temps m ->
    {m with stack= Stack.remove_temps temps m.stack; alias= Alias.remove_temps temps m.alias}


  let set_prune_pairs : PrunePairs.t -> t -> t =
   fun prune_pairs m -> {m with latest_prune= LatestPrune.Latest prune_pairs}


  let apply_latest_prune : Exp.t -> t -> t =
   fun e m ->
    match (m.latest_prune, e) with
    | LatestPrune.V (x, prunes, _), Exp.Var r
    | LatestPrune.V (x, _, prunes), Exp.UnOp (Unop.LNot, Exp.Var r, _) -> (
      match find_simple_alias r m with
      | Some (Loc.Var (Var.ProgramVar y)) when Pvar.equal x y ->
          List.fold_left prunes ~init:m ~f:(fun acc (l, v) -> update_mem (PowLoc.singleton l) v acc)
      | _ ->
          m )
    | _ ->
        m


  let update_latest_prune : Exp.t -> Exp.t -> t -> t =
   fun e1 e2 m ->
    match (e1, e2, m.latest_prune) with
    | Lvar x, Const (Const.Cint i), LatestPrune.Latest p ->
        if IntLit.isone i then {m with latest_prune= LatestPrune.TrueBranch (x, p)}
        else if IntLit.iszero i then {m with latest_prune= LatestPrune.FalseBranch (x, p)}
        else {m with latest_prune= LatestPrune.Top}
    | _, _, _ ->
        {m with latest_prune= LatestPrune.Top}


  let get_new_heap_locs : prev:t -> next:t -> PowLoc.t =
   fun ~prev ~next ->
    let add_loc loc _val acc = if Heap.mem loc prev.heap then acc else PowLoc.add loc acc in
    Heap.fold add_loc next.heap PowLoc.empty


  let get_reachable_locs_from : PowLoc.t -> t -> PowLoc.t =
    let add_reachable1 ~root loc v acc =
      if Loc.equal root loc then PowLoc.union acc (Val.get_all_locs v)
      else if Loc.is_field_of ~loc:root ~field_loc:loc then PowLoc.add loc acc
      else acc
    in
    let rec add_from_locs heap locs acc = PowLoc.fold (add_from_loc heap) locs acc
    and add_from_loc heap loc acc =
      if PowLoc.mem loc acc then acc
      else
        let reachable_locs = Heap.fold (add_reachable1 ~root:loc) heap PowLoc.empty in
        add_from_locs heap reachable_locs (PowLoc.add loc acc)
    in
    fun locs m -> add_from_locs m.heap locs PowLoc.empty


  let heap_range : filter_loc:(Loc.t -> bool) -> t -> Itv.NonNegativePolynomial.astate =
   fun ~filter_loc {heap} -> Heap.range ~filter_loc heap
end

module Mem = struct
  include AbstractDomain.BottomLifted (MemReach)

  type t = astate

  let bot : t = Bottom

  let init : t = NonBottom MemReach.init

  let f_lift_default : 'a -> (MemReach.t -> 'a) -> t -> 'a =
   fun default f m -> match m with Bottom -> default | NonBottom m' -> f m'


  let f_lift_default2 : 'a -> (MemReach.t -> MemReach.t -> 'a) -> t -> t -> 'a =
   fun default f m1 m2 ->
    match (m1, m2) with
    | Bottom, _ | _, Bottom ->
        default
    | NonBottom m1', NonBottom m2' ->
        f m1' m2'


  let f_lift : (MemReach.t -> MemReach.t) -> t -> t =
   fun f -> f_lift_default Bottom (fun m' -> NonBottom (f m'))


  let pp_summary : F.formatter -> t -> unit =
   fun fmt m ->
    match m with
    | Bottom ->
        F.pp_print_string fmt "unreachable"
    | NonBottom m' ->
        MemReach.pp_summary fmt m'


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

  let strong_update_heap : PowLoc.t -> Val.t -> t -> t =
   fun p v -> f_lift (MemReach.strong_update_heap p v)


  let weak_update_heap : PowLoc.t -> Val.t -> t -> t =
   fun p v -> f_lift (MemReach.weak_update_heap p v)


  let get_return : t -> Val.t = f_lift_default Val.bot MemReach.get_return

  let get_new_heap_locs : prev:t -> next:t -> PowLoc.t =
   fun ~prev ~next ->
    f_lift_default2 PowLoc.empty
      (fun m1 m2 -> MemReach.get_new_heap_locs ~prev:m1 ~next:m2)
      prev next


  let get_reachable_locs_from : PowLoc.t -> t -> PowLoc.t =
   fun locs -> f_lift_default PowLoc.empty (MemReach.get_reachable_locs_from locs)


  let update_mem : PowLoc.t -> Val.t -> t -> t = fun ploc v -> f_lift (MemReach.update_mem ploc v)

  let transform_mem : f:(Val.t -> Val.t) -> PowLoc.t -> t -> t =
   fun ~f ploc -> f_lift (MemReach.transform_mem ~f ploc)


  let remove_temps : Ident.t list -> t -> t = fun temps -> f_lift (MemReach.remove_temps temps)

  let set_prune_pairs : PrunePairs.t -> t -> t =
   fun prune_pairs -> f_lift (MemReach.set_prune_pairs prune_pairs)


  let apply_latest_prune : Exp.t -> t -> t = fun e -> f_lift (MemReach.apply_latest_prune e)

  let update_latest_prune : Exp.t -> Exp.t -> t -> t =
   fun e1 e2 -> f_lift (MemReach.update_latest_prune e1 e2)
end

module Summary = struct
  type t = Mem.t * Mem.t * PO.ConditionSet.t

  let get_input : t -> Mem.t = fst3

  let get_output : t -> Mem.t = snd3

  let get_cond_set : t -> PO.ConditionSet.t = trd3

  let get_return : t -> Val.t = fun s -> Mem.get_return (get_output s)

  let pp_symbol_map : F.formatter -> t -> unit = fun fmt s -> Mem.pp_summary fmt (get_input s)

  let pp_return : F.formatter -> t -> unit =
   fun fmt s -> F.fprintf fmt "Return value: %a" Val.pp_summary (get_return s)


  let pp_summary : F.formatter -> t -> unit =
   fun fmt s ->
    F.fprintf fmt "%a@,%a@,%a" pp_symbol_map s pp_return s PO.ConditionSet.pp_summary
      (get_cond_set s)


  let pp : F.formatter -> t -> unit =
   fun fmt (entry_mem, exit_mem, condition_set) ->
    F.fprintf fmt "%a@,%a@,%a@," Mem.pp entry_mem Mem.pp exit_mem PO.ConditionSet.pp condition_set
end
