(*
 * Copyright (c) 2016 - present
 * Kihong Heo (http://ropas.snu.ac.kr/~khheo)
 * Sungkeun Cho (http://ropas.snu.ac.kr/~skcho)
 * Kwangkeun Yi (http://ropas.snu.ac.kr/~kwang)
 *
 * ROSAEC(Research On Software Analysis for Error-free Computing) Center
 * Programming Research Laboratory
 * Seoul National University, Korea
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd
open AbsLoc

module F = Format
module L = Logging

module Condition =
struct
  type t =
    { idx : Itv.astate;
      size : Itv.astate;
      proc_name : Procname.t;
      loc : Location.t;
      trace : trace;
      id : string }
  [@@deriving compare]
  and trace = Intra of Procname.t
            | Inter of Procname.t * Procname.t * Location.t
  [@@deriving compare]

  and astate = t

  let set_size_pos : t -> t
  = fun c ->
    let size =
      if Itv.Bound.le (Itv.lb c.size) Itv.Bound.zero
      then Itv.make Itv.Bound.zero (Itv.ub c.size)
      else c.size
    in
    { c with size }

  let string_of_location : Location.t -> string
  = fun loc -> 
    let fname = SourceFile.to_string loc.Location.file in
    let pos = Location.to_string loc in
    F.fprintf F.str_formatter "%s:%s" fname pos;
    F.flush_str_formatter ()
   
  let pp_location : F.formatter -> t -> unit
  = fun fmt c ->
    F.fprintf fmt "%s" (string_of_location c.loc)

  let pp : F.formatter -> t -> unit
  = fun fmt c ->
    let c = set_size_pos c in
    if Config.bo_debug <= 1 then 
      F.fprintf fmt "%a < %a at %a" Itv.pp c.idx Itv.pp c.size pp_location c
    else
      match c.trace with 
        Inter (_, pname, loc) ->
          let pname = Procname.to_string pname in
          F.fprintf fmt "%a < %a at %a by call %s() at %s" 
            Itv.pp c.idx Itv.pp c.size pp_location c pname (string_of_location loc)
      | Intra _ -> F.fprintf fmt "%a < %a at %a" Itv.pp c.idx Itv.pp c.size pp_location c

  let pp_element : F.formatter -> t -> unit
  = pp

  let get_location : t -> Location.t
  = fun c -> c.loc

  let get_trace : t -> trace
  = fun c -> c.trace

  let get_proc_name : t -> Procname.t
  = fun c -> c.proc_name

  let make : Procname.t -> Location.t -> string -> idx:Itv.t -> size:Itv.t -> t
  = fun proc_name loc id ~idx ~size ->
    { proc_name; idx; size; loc; id ; trace = Intra proc_name }

  let filter1 : t -> bool
  = fun c ->
    c.idx = Itv.top || c.size = Itv.top 
    || (try Itv.lb c.idx = Itv.Bound.MInf with _ -> false)
    || (try Itv.lb c.size = Itv.Bound.MInf with _ -> false)
    || (Itv.eq c.idx Itv.nat && Itv.eq c.size Itv.nat) 
 
  let filter2 : t -> bool
  = fun c ->
    (not (Itv.is_finite c.idx) || not (Itv.is_finite c.size)) (* basically, alarms involving infinify are filtered *)
    &&                                                        (* except the following cases :                      *)
    not ((Itv.Bound.is_not_infty (Itv.lb c.idx) &&            (* idx non-infty lb < 0 *)
          Itv.Bound.lt (Itv.lb c.idx) Itv.Bound.zero)
         ||
         (Itv.Bound.is_not_infty (Itv.lb c.idx) &&            (* idx non-infty lb > size lb *)
          (Itv.Bound.gt (Itv.lb c.idx) (Itv.lb c.size)))
         ||
         (Itv.Bound.is_not_infty (Itv.lb c.idx) &&            (* idx non-infty lb > size ub *)
          (Itv.Bound.gt (Itv.lb c.idx) (Itv.ub c.size)))
         ||
         (Itv.Bound.is_not_infty (Itv.ub c.idx) &&            (* idx non-infty ub > size lb *)
          (Itv.Bound.gt (Itv.ub c.idx) (Itv.lb c.size)))
         ||
         (Itv.Bound.is_not_infty (Itv.ub c.idx) &&            (* idx non-infty ub > size ub *)
          (Itv.Bound.gt (Itv.ub c.idx) (Itv.ub c.size))))

  let check : t -> bool
  = fun c ->
    if Config.bo_debug <= 1 && (Itv.is_symbolic c.idx || Itv.is_symbolic c.size)
    then true
    else if Config.bo_filtering >= 1 && filter1 c then true
    else if Config.bo_filtering >= 2 && filter2 c then true
    else
      let c = set_size_pos c in
      let not_overrun = Itv.lt_sem c.idx c.size in
      let not_underrun = Itv.le_sem Itv.zero c.idx in
      (Itv.eq not_overrun Itv.one) && (Itv.eq not_underrun Itv.one)

  let invalid : t -> bool
  = fun x -> Itv.invalid x.idx || Itv.invalid x.size

  let to_string : t -> string
  = fun c ->
    let c = set_size_pos c in
    "Offset : " ^ Itv.to_string c.idx ^ " Size : " ^ Itv.to_string c.size 
    ^ " @ " ^ string_of_location c.loc
    ^ (match c.trace with 
         Inter (_, pname, _) ->
          " by call "
           ^ Procname.to_string pname
           ^ "() " 
       | Intra _ -> "")

  let subst : t -> Itv.Bound.t Itv.SubstMap.t -> Procname.t -> Procname.t -> Location.t -> t
  = fun c subst_map caller_pname callee_pname loc ->
    if Itv.is_symbolic c.idx || Itv.is_symbolic c.size then 
      { c with idx = Itv.subst c.idx subst_map;
               size = Itv.subst c.size subst_map; 
               trace = Inter (caller_pname, callee_pname, loc) }
    else c
end

module ConditionSet =
struct
  module PPSet = PrettyPrintable.MakePPSet (Condition)
  include AbstractDomain.FiniteSet (PPSet)

  module Map = Caml.Map.Make (struct
      type t = Location.t [@@deriving compare]
    end)

  let add_bo_safety
    : Procname.t -> Location.t -> string -> idx:Itv.t -> size:Itv.t -> t -> t
  = fun pname loc id ~idx ~size cond ->
    add (Condition.make pname loc id ~idx ~size) cond

  let subst : t -> Itv.Bound.t Itv.SubstMap.t -> Procname.t -> Procname.t -> Location.t -> t
  = fun x subst_map caller_pname callee_pname loc ->
    fold (fun e -> add (Condition.subst e subst_map caller_pname callee_pname loc)) x empty

  let group : t -> t Map.t
  = fun x ->
    fold (fun cond map -> 
        let old_set = try Map.find cond.loc map with _ -> empty in
        Map.add cond.loc (add cond old_set) map) x Map.empty

  let pp_summary : F.formatter -> t -> unit
  = fun fmt x ->
    let pp_sep fmt () = F.fprintf fmt ", @," in
    let pp_element fmt v = Condition.pp fmt v in
    F.fprintf fmt "@[<v 0>Safety conditions:@,";
    F.fprintf fmt "@[<hov 2>{ ";
    F.pp_print_list ~pp_sep pp_element fmt (elements x);
    F.fprintf fmt " }@]";
    F.fprintf fmt "@]"

  let pp : Format.formatter -> t -> unit
  = fun fmt x ->
    let pp_sep fmt () = F.fprintf fmt ", @," in
    let pp_element fmt v = Condition.pp fmt v in
    F.fprintf fmt "@[<v 2>Safety conditions :@,";
    F.fprintf fmt "@[<hov 1>{";
    F.pp_print_list ~pp_sep pp_element fmt (elements x);
    F.fprintf fmt " }@]";
    F.fprintf fmt "@]"

  let rm_invalid : t -> t
  = fun x -> filter (fun c -> not (Condition.invalid c)) x
end

module Val =
struct
  include AbstractDomain.Pair3 (Itv) (PowLoc) (ArrayBlk)

  type t = astate

  let bot : t
  = initial

  let rec joins : t list -> t
  = function
    | [] -> bot
    | [a] -> a
    | a :: b -> join a (joins b)

  let get_itv : t -> Itv.t
  = fst3

  let get_pow_loc : t -> PowLoc.t
  = snd3

  let get_array_blk : t -> ArrayBlk.astate
  = trd3

  let get_all_locs : t -> PowLoc.t
  = fun (_, p, a) -> ArrayBlk.get_pow_loc a |> PowLoc.join p

  let top_itv : t
  = (Itv.top, PowLoc.bot, ArrayBlk.bot)

  let pos_itv : t
  = (Itv.pos, PowLoc.bot, ArrayBlk.bot)

  let nat_itv : t
  = (Itv.nat, PowLoc.bot, ArrayBlk.bot)

  let of_int : int -> t
  = fun n -> (Itv.of_int n, PowLoc.bot, ArrayBlk.bot)

  let of_pow_loc : PowLoc.t -> t
  = fun x -> (Itv.bot, x, ArrayBlk.bot)

  let of_array_blk : ArrayBlk.astate -> t
  = fun a -> (Itv.bot, PowLoc.bot, a)

  let zero : t
  = of_int 0

  let get_new_sym : Procname.t -> t
  = fun pname -> (Itv.get_new_sym pname, PowLoc.bot, ArrayBlk.bot)

  let make_sym : Procname.t -> int -> t
  = fun pname i -> (Itv.make_sym pname i, PowLoc.bot, ArrayBlk.bot)

  let unknown_bit : t -> t
  = fun (_, x, a) -> (Itv.top, x, a)

  let neg : t -> t
  = fun (n, x, a) -> (Itv.neg n, x, a)

  let lnot : t -> t
  = fun (n, x, a) -> (Itv.lnot n, x, a)

  let lift_itv_func : (Itv.t -> Itv.t -> Itv.t) -> t -> t -> t
  = fun f (n1, _, _) (n2, _, _) -> (f n1 n2, PowLoc.bot, ArrayBlk.bot)

  let plus : t -> t -> t
  = fun (n1, _, a1) (n2, _, _) ->
    (Itv.plus n1 n2, PowLoc.bot, ArrayBlk.plus_offset a1 n2)

  let minus : t -> t -> t
  = fun (n1, _, a1) (n2, _, a2) ->
    let n = Itv.join (Itv.minus n1 n2) (ArrayBlk.diff a1 a2) in
    let a = ArrayBlk.minus_offset a1 n2 in
    (n, PowLoc.bot, a)

  let mult : t -> t -> t
  = lift_itv_func Itv.mult

  let div : t -> t -> t
  = lift_itv_func Itv.div

  let mod_sem : t -> t -> t
  = lift_itv_func Itv.mod_sem

  let shiftlt : t -> t -> t
  = lift_itv_func Itv.shiftlt

  let shiftrt : t -> t -> t
  = lift_itv_func Itv.shiftrt

  let lt_sem : t -> t -> t
  = lift_itv_func Itv.lt_sem

  let gt_sem : t -> t -> t
  = lift_itv_func Itv.gt_sem

  let le_sem : t -> t -> t
  = lift_itv_func Itv.le_sem

  let ge_sem : t -> t -> t
  = lift_itv_func Itv.ge_sem

  let eq_sem : t -> t -> t
  = lift_itv_func Itv.eq_sem

  let ne_sem : t -> t -> t
  = lift_itv_func Itv.ne_sem

  let land_sem : t -> t -> t
  = lift_itv_func Itv.land_sem

  let lor_sem : t -> t -> t
  = lift_itv_func Itv.lor_sem

  let lift_prune1 : (Itv.t -> Itv.t) -> t -> t
  = fun f (n, x, a) -> (f n, x, a)

  let lift_prune2
    : (Itv.t -> Itv.t -> Itv.t)
      -> (ArrayBlk.astate -> ArrayBlk.astate -> ArrayBlk.astate) -> t -> t -> t
  = fun f g (n1, x1, a1) (n2, _, a2) -> (f n1 n2, x1, g a1 a2)

  let prune_zero : t -> t
  = lift_prune1 Itv.prune_zero

  let prune_comp : Binop.t -> t -> t -> t
  = fun c -> lift_prune2 (Itv.prune_comp c) (ArrayBlk.prune_comp c)

  let prune_eq : t -> t -> t
  = lift_prune2 Itv.prune_eq ArrayBlk.prune_eq

  let prune_ne : t -> t -> t
  = lift_prune2 Itv.prune_ne ArrayBlk.prune_eq

  let plus_pi : t -> t -> t
  = fun (_, _, a1) (n2, _, _) ->
    (Itv.bot, PowLoc.bot, ArrayBlk.plus_offset a1 n2)

  let minus_pi : t -> t -> t
  = fun (_, _, a1) (n2, _, _) ->
    (Itv.bot, PowLoc.bot, ArrayBlk.minus_offset a1 n2)

  let minus_pp : t -> t -> t
  = fun (_, _, a1) (_, _, a2) -> (ArrayBlk.diff a1 a2, PowLoc.bot, ArrayBlk.bot)

  let subst : t -> Itv.Bound.t Itv.SubstMap.t -> t
  = fun (i, p, a) subst_map ->
    (Itv.subst i subst_map, p, ArrayBlk.subst a subst_map)

  let get_symbols : t -> Itv.Symbol.t list
  = fun (i, _, a) -> IList.append (Itv.get_symbols i) (ArrayBlk.get_symbols a)

  let normalize : t -> t
  = fun (i, l, a) -> (Itv.normalize i, l, ArrayBlk.normalize a)

  let pp_summary : F.formatter -> t -> unit
  = fun fmt (i, _, a) -> F.fprintf fmt "(%a, %a)" Itv.pp i ArrayBlk.pp a
end

module Stack =
struct
  module PPMap =
  struct
    module Ord = struct include Loc let pp_key = pp end
    include PrettyPrintable.MakePPMap (Ord)

    let pp_collection
      : pp_item:(F.formatter -> 'a -> unit) -> F.formatter -> 'a list -> unit
    = fun ~pp_item fmt c ->
      let pp_sep fmt () = F.fprintf fmt ",@," in
      F.pp_print_list ~pp_sep pp_item fmt c

    let pp
      : pp_value:(F.formatter -> 'a -> unit) -> F.formatter -> 'a t -> unit
    = fun ~pp_value fmt m ->
      let pp_item fmt (k, v) =
        F.fprintf fmt "%a -> %a" Ord.pp_key k pp_value v
      in
      F.fprintf fmt "@[<v 2>{ ";
      pp_collection ~pp_item fmt (bindings m);
      F.fprintf fmt " }@]"
  end

  include AbstractDomain.Map (PPMap) (Val)

  let find : Loc.t -> astate -> Val.t
  = fun l m ->
    try find l m with
    | Not_found -> Val.bot

  let find_set : PowLoc.t -> astate -> Val.t
  = fun locs mem ->
    let find_join loc acc = Val.join acc (find loc mem) in
    PowLoc.fold find_join locs Val.bot

  let strong_update : PowLoc.t -> Val.astate -> astate -> astate
  = fun locs v mem ->
    PowLoc.fold (fun x -> add x v) locs mem

  let weak_update : PowLoc.t -> Val.astate -> astate -> astate
  = fun locs v mem ->
    PowLoc.fold (fun x -> add x (Val.join v (find x mem))) locs mem

  let pp_summary : F.formatter -> astate -> unit
  = fun fmt mem ->
    let pp_not_logical_var k v =
      if Loc.is_logical_var k then () else
        F.fprintf fmt "%a -> %a@," Loc.pp k Val.pp_summary v
    in
    iter pp_not_logical_var mem
end

module Heap =
struct
  module PPMap =
  struct
    module Ord = struct include Loc let pp_key = pp end
    include PrettyPrintable.MakePPMap (Ord)

    let pp_collection
      : pp_item:(F.formatter -> 'a -> unit) -> F.formatter -> 'a list -> unit
    = fun ~pp_item fmt c ->
      let pp_sep fmt () = F.fprintf fmt ",@," in
      F.pp_print_list ~pp_sep pp_item fmt c

    let pp : pp_value:(F.formatter -> 'a -> unit) -> F.formatter -> 'a t -> unit
    = fun ~pp_value fmt m ->
      let pp_item fmt (k, v) =
        F.fprintf fmt "%a -> %a" Ord.pp_key k pp_value v
      in
      F.fprintf fmt "@[<v 2>{ ";
      pp_collection ~pp_item fmt (bindings m);
      F.fprintf fmt " }@]"
  end

  include AbstractDomain.Map (PPMap) (Val)

  let find : Loc.t -> astate -> Val.t
  = fun l m ->
    try find l m with
    | Not_found -> Val.bot

  let find_set : PowLoc.t -> astate -> Val.t
  = fun locs mem ->
    let find_join loc acc = Val.join acc (find loc mem) in
    PowLoc.fold find_join locs Val.bot

  let strong_update : PowLoc.t -> Val.t -> astate -> astate
  = fun locs v mem ->
    PowLoc.fold (fun x -> add x v) locs mem

  let weak_update : PowLoc.t -> Val.t -> astate -> astate
  = fun locs v mem ->
    PowLoc.fold (fun x -> add x (Val.join v (find x mem))) locs mem

  let pp_summary : F.formatter -> astate -> unit
  = fun fmt mem ->
    let pp_map fmt (k, v) = F.fprintf fmt "%a -> %a" Loc.pp k Val.pp_summary v in
    F.fprintf fmt "@[<v 2>{ ";
    F.pp_print_list pp_map fmt (bindings mem);
    F.fprintf fmt " }@]"

  let get_symbols : astate -> Itv.Symbol.t list
  = fun mem ->
    IList.flatten (IList.map (fun (_, v) -> Val.get_symbols v) (bindings mem))

  let get_result : astate -> Val.t
  = fun mem ->
    let mem = filter (fun l _ -> Loc.is_return l) mem in
    if is_empty mem then Val.bot else snd (choose mem)
end

module Alias =
struct
  module M = Caml.Map.Make (Ident)

  type t = Pvar.t M.t

  type astate = t

  let initial : t
  = M.empty

  let (<=) : lhs:t -> rhs:t -> bool
  = fun ~lhs ~rhs ->
    let is_in_rhs k v =
      match M.find k rhs with
      | v' -> Pvar.equal v v'
      | exception Not_found -> false
    in
    M.for_all is_in_rhs lhs

  let join : t -> t -> t
  = fun x y ->
    let join_v _ v1_opt v2_opt =
      match v1_opt, v2_opt with
      | None, None -> None
      | Some v, None
      | None, Some v -> Some v
      | Some v1, Some v2 -> if Pvar.equal v1 v2 then Some v1 else assert false
    in
    M.merge join_v x y

  let widen : prev:t -> next:t -> num_iters:int -> t
  = fun ~prev ~next ~num_iters:_ -> join prev next

  let pp : F.formatter -> t -> unit
  = fun fmt x ->
    let pp_sep fmt () = F.fprintf fmt ", @," in
    let pp1 fmt (k, v) =
      F.fprintf fmt "%a=%a" (Ident.pp Pp.text) k (Pvar.pp Pp.text) v
    in
    (* F.fprintf fmt "@[<v 0>Logical Variables :@,"; *)
    F.fprintf fmt "@[<hov 2>{ @,";
    F.pp_print_list ~pp_sep pp1 fmt (M.bindings x);
    F.fprintf fmt " }@]";
    F.fprintf fmt "@]"

  let load : Ident.t -> Exp.t -> t -> t
  = fun id exp m ->
    match exp with
    | Exp.Lvar x -> M.add id x m
    | _ -> m

  let store : Exp.t -> Exp.t -> t -> t
  = fun e _ m ->
    match e with
    | Exp.Lvar x -> M.filter (fun _ y -> not (Pvar.equal x y)) m
    | _ -> m

  let find : Ident.t -> t -> Pvar.t option
  = fun k m -> try Some (M.find k m) with Not_found -> None
end

module Mem =
struct
  include AbstractDomain.Pair3 (Stack) (Heap) (Alias)

  type t = astate

  let pp : F.formatter -> t -> unit
  = fun fmt (stack, heap, _) ->
    F.fprintf fmt "Stack :@,";
    F.fprintf fmt "%a@," Stack.pp stack;
    F.fprintf fmt "Heap :@,";
    F.fprintf fmt "%a" Heap.pp heap

  let pp_summary : F.formatter -> t -> unit
  = fun fmt (_, heap, _) ->
    F.fprintf fmt "@[<v 0>Parameters :@,";
    F.fprintf fmt "%a" Heap.pp_summary heap ;
    F.fprintf fmt "@]"

  let find_stack : Loc.t -> t -> Val.t
  = fun k m -> Stack.find k (fst3 m)

  let find_stack_set : PowLoc.t -> t -> Val.t
  = fun k m -> Stack.find_set k (fst3 m)

  let find_heap : Loc.t -> t -> Val.t
  = fun k m -> Heap.find k (snd3 m)

  let find_heap_set : PowLoc.t -> t -> Val.t
  = fun k m -> Heap.find_set k (snd3 m)

  let find_alias : Ident.t -> t -> Pvar.t option
  = fun k m -> Alias.find k (trd3 m)

  let load_alias : Ident.t -> Exp.t -> t -> t
  = fun id e m -> (fst3 m, snd3 m, Alias.load id e (trd3 m))

  let store_alias : Exp.t -> Exp.t -> t -> t
  = fun e1 e2 m -> (fst3 m, snd3 m, Alias.store e1 e2 (trd3 m))

  let add_stack : Loc.t -> Val.t -> t -> t
  = fun k v m -> (Stack.add k v (fst3 m), snd3 m, trd3 m)

  let add_heap : Loc.t -> Val.t -> t -> t
  = fun k v m -> (fst3 m, Heap.add k v (snd3 m), trd3 m)

  let strong_update_stack : PowLoc.t -> Val.t -> t -> t
  = fun p v m -> (Stack.strong_update p v (fst3 m), snd3 m, trd3 m)

  let strong_update_heap : PowLoc.t -> Val.t -> t -> t
  = fun p v m -> (fst3 m, Heap.strong_update p v (snd3 m), trd3 m)

  let weak_update_stack : PowLoc.t -> Val.t -> t -> t
  = fun p v m -> (Stack.weak_update p v (fst3 m), snd3 m, trd3 m)

  let weak_update_heap : PowLoc.t -> Val.t -> t -> t
  = fun p v m -> (fst3 m, Heap.weak_update p v (snd3 m), trd3 m)

  let get_heap_symbols : t -> Itv.Symbol.t list
  = fun (_, m, _) -> Heap.get_symbols m

  let get_result : t -> Val.t
  = fun (_, m, _) -> Heap.get_result m

  let can_strong_update : PowLoc.t -> bool
  = fun ploc ->
    if PowLoc.cardinal ploc = 1 then Loc.is_var (PowLoc.choose ploc) else false

  let update_mem : PowLoc.t -> Val.t -> t -> t
  = fun ploc v s ->
    if can_strong_update ploc
    then strong_update_heap ploc v s
    else weak_update_heap ploc v s
end

module Summary =
struct
  type t = Mem.t * Mem.t * ConditionSet.t

  let get_input : t -> Mem.t
  = fst3

  let get_output : t -> Mem.t
  = snd3

  let get_cond_set : t -> ConditionSet.t
  = trd3

  let get_symbols : t -> Itv.Symbol.t list
  = fun s -> Mem.get_heap_symbols (get_input s)

  let get_result : t -> Val.t
  = fun s -> Mem.get_result (get_output s)

  let pp_symbols : F.formatter -> t -> unit
  = fun fmt s ->
    let pp_sep fmt () = F.fprintf fmt ", @," in
    F.fprintf fmt "@[<hov 2>Symbols: {";
    F.pp_print_list ~pp_sep Itv.Symbol.pp fmt (get_symbols s);
    F.fprintf fmt "}@]"

  let pp_symbol_map : F.formatter -> t -> unit
  = fun fmt s -> Mem.pp_summary fmt (get_input s)

  let pp_return : F.formatter -> t -> unit
  = fun fmt s -> F.fprintf fmt "Return value: %a" Val.pp_summary (get_result s)

  let pp_summary : F.formatter -> t -> unit
  = fun fmt s ->
    F.fprintf fmt "%a@,%a@,%a" pp_symbol_map s pp_return s
      ConditionSet.pp_summary (get_cond_set s)

  let pp : F.formatter -> t -> unit
  = fun fmt (entry_mem, exit_mem, condition_set) ->
    F.fprintf fmt "%a@,%a@,%a@"
      Mem.pp entry_mem Mem.pp exit_mem ConditionSet.pp condition_set
end
