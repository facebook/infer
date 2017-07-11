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
module F = Format
module L = Logging
module Domain = BufferOverrunDomain
module Trace = BufferOverrunTrace
module TraceSet = Trace.Set
open Domain

module Make (CFG : ProcCfg.S) = struct
  exception Not_implemented

  let eval_const : Const.t -> Val.t = function
    | Const.Cint intlit -> (
      try Val.of_int (IntLit.to_int intlit)
      with _ -> Val.Itv.top )
    | Const.Cfloat f
     -> f |> int_of_float |> Val.of_int
    | _
     -> Val.Itv.top

  (* TODO *)

  let sizeof_ikind : Typ.ikind -> int = function
    | Typ.IChar | Typ.ISChar | Typ.IUChar | Typ.IBool
     -> 1
    | Typ.IInt | Typ.IUInt
     -> 4
    | Typ.IShort | Typ.IUShort
     -> 2
    | Typ.ILong | Typ.IULong
     -> 4
    | Typ.ILongLong | Typ.IULongLong
     -> 8
    | Typ.I128 | Typ.IU128
     -> 16

  let sizeof_fkind : Typ.fkind -> int = function
    | Typ.FFloat
     -> 4
    | Typ.FDouble | Typ.FLongDouble
     -> 8

  (* NOTE: assume 32bit machine *)
  let rec sizeof (typ: Typ.t) : int =
    match typ.desc with
    | Typ.Tint ikind
     -> sizeof_ikind ikind
    | Typ.Tfloat fkind
     -> sizeof_fkind fkind
    | Typ.Tvoid
     -> 1
    | Typ.Tptr (_, _)
     -> 4
    | Typ.Tstruct _ | Typ.TVar _
     -> 4 (* TODO *)
    | Typ.Tarray (_, Some length, Some stride)
     -> IntLit.to_int stride * IntLit.to_int length
    | Typ.Tarray (typ, Some length, None)
     -> sizeof typ * IntLit.to_int length
    | _
     -> 4

  let rec must_alias : Exp.t -> Exp.t -> Mem.astate -> bool =
    fun e1 e2 m ->
      match (e1, e2) with
      | Exp.Var x1, Exp.Var x2 -> (
        match (Mem.find_alias x1 m, Mem.find_alias x2 m) with
        | Some x1', Some x2'
         -> Pvar.equal x1' x2'
        | _, _
         -> false )
      | Exp.UnOp (uop1, e1', _), Exp.UnOp (uop2, e2', _)
       -> Unop.equal uop1 uop2 && must_alias e1' e2' m
      | Exp.BinOp (bop1, e11, e12), Exp.BinOp (bop2, e21, e22)
       -> Binop.equal bop1 bop2 && must_alias e11 e21 m && must_alias e12 e22 m
      | Exp.Exn t1, Exp.Exn t2
       -> must_alias t1 t2 m
      | Exp.Const c1, Exp.Const c2
       -> Const.equal c1 c2
      | Exp.Cast (t1, e1'), Exp.Cast (t2, e2')
       -> Typ.equal t1 t2 && must_alias e1' e2' m
      | Exp.Lvar x1, Exp.Lvar x2
       -> Pvar.equal x1 x2
      | Exp.Lfield (e1, fld1, _), Exp.Lfield (e2, fld2, _)
       -> must_alias e1 e2 m && Typ.Fieldname.equal fld1 fld2
      | Exp.Lindex (e11, e12), Exp.Lindex (e21, e22)
       -> must_alias e11 e21 m && must_alias e12 e22 m
      | Exp.Sizeof {nbytes= Some nbytes1}, Exp.Sizeof {nbytes= Some nbytes2}
       -> Int.equal nbytes1 nbytes2
      | ( Exp.Sizeof {typ= t1; dynamic_length= dynlen1; subtype= subt1}
        , Exp.Sizeof {typ= t2; dynamic_length= dynlen2; subtype= subt2} )
       -> Typ.equal t1 t2 && must_alias_opt dynlen1 dynlen2 m
          && Int.equal (Subtype.compare subt1 subt2) 0
      | _, _
       -> false

  and must_alias_opt : Exp.t option -> Exp.t option -> Mem.astate -> bool =
    fun e1_opt e2_opt m ->
      match (e1_opt, e2_opt) with
      | Some e1, Some e2
       -> must_alias e1 e2 m
      | None, None
       -> true
      | _, _
       -> false

  let comp_rev : Binop.t -> Binop.t = function
    | Binop.Lt
     -> Binop.Gt
    | Binop.Gt
     -> Binop.Lt
    | Binop.Le
     -> Binop.Ge
    | Binop.Ge
     -> Binop.Le
    | Binop.Eq
     -> Binop.Eq
    | Binop.Ne
     -> Binop.Ne
    | _
     -> assert false

  let comp_not : Binop.t -> Binop.t = function
    | Binop.Lt
     -> Binop.Ge
    | Binop.Gt
     -> Binop.Le
    | Binop.Le
     -> Binop.Gt
    | Binop.Ge
     -> Binop.Lt
    | Binop.Eq
     -> Binop.Ne
    | Binop.Ne
     -> Binop.Eq
    | _
     -> assert false

  let rec must_alias_cmp : Exp.t -> Mem.astate -> bool =
    fun e m ->
      match e with
      | Exp.BinOp (Binop.Lt, e1, e2) | Exp.BinOp (Binop.Gt, e1, e2) | Exp.BinOp (Binop.Ne, e1, e2)
       -> must_alias e1 e2 m
      | Exp.BinOp (Binop.LAnd, e1, e2)
       -> must_alias_cmp e1 m || must_alias_cmp e2 m
      | Exp.BinOp (Binop.LOr, e1, e2)
       -> must_alias_cmp e1 m && must_alias_cmp e2 m
      | Exp.UnOp (Unop.LNot, Exp.UnOp (Unop.LNot, e1, _), _)
       -> must_alias_cmp e1 m
      | Exp.UnOp (Unop.LNot, Exp.BinOp ((Binop.Lt as c), e1, e2), _)
      | Exp.UnOp (Unop.LNot, Exp.BinOp ((Binop.Gt as c), e1, e2), _)
      | Exp.UnOp (Unop.LNot, Exp.BinOp ((Binop.Le as c), e1, e2), _)
      | Exp.UnOp (Unop.LNot, Exp.BinOp ((Binop.Ge as c), e1, e2), _)
      | Exp.UnOp (Unop.LNot, Exp.BinOp ((Binop.Eq as c), e1, e2), _)
      | Exp.UnOp (Unop.LNot, Exp.BinOp ((Binop.Ne as c), e1, e2), _)
       -> must_alias_cmp (Exp.BinOp (comp_not c, e1, e2)) m
      | Exp.UnOp (Unop.LNot, Exp.BinOp (Binop.LOr, e1, e2), t)
       -> let e1' = Exp.UnOp (Unop.LNot, e1, t) in
          let e2' = Exp.UnOp (Unop.LNot, e2, t) in
          must_alias_cmp (Exp.BinOp (Binop.LAnd, e1', e2')) m
      | Exp.UnOp (Unop.LNot, Exp.BinOp (Binop.LAnd, e1, e2), t)
       -> let e1' = Exp.UnOp (Unop.LNot, e1, t) in
          let e2' = Exp.UnOp (Unop.LNot, e2, t) in
          must_alias_cmp (Exp.BinOp (Binop.LOr, e1', e2')) m
      | _
       -> false

  let rec eval : Exp.t -> Mem.astate -> Location.t -> Val.t =
    fun exp mem loc ->
      if must_alias_cmp exp mem then Val.of_int 0
      else
        match exp with
        | Exp.Var id
         -> Mem.find_stack (Var.of_id id |> Loc.of_var) mem
        | Exp.Lvar pvar
         -> let ploc = pvar |> Loc.of_pvar |> PowLoc.singleton in
            let arr = Mem.find_stack_set ploc mem in
            ploc |> Val.of_pow_loc |> Val.join arr
        | Exp.UnOp (uop, e, _)
         -> eval_unop uop e mem loc
        | Exp.BinOp (bop, e1, e2)
         -> eval_binop bop e1 e2 mem loc
        | Exp.Const c
         -> eval_const c
        | Exp.Cast (_, e)
         -> eval e mem loc
        | Exp.Lfield (e, fn, _)
         -> eval e mem loc |> Val.get_array_locs |> Fn.flip PowLoc.append_field fn
            |> Val.of_pow_loc
        | Exp.Lindex (e1, _)
         -> let arr = eval e1 mem loc |> Val.get_array_blk in
            (* must have array blk *)
            (* let idx = eval e2 mem loc in *)
            let ploc = if ArrayBlk.is_bot arr then PowLoc.unknown else ArrayBlk.get_pow_loc arr in
            (* if nested array, add the array blk *)
            let arr = Mem.find_heap_set ploc mem in
            Val.join (Val.of_pow_loc ploc) arr
        | Exp.Sizeof {nbytes= Some size}
         -> Val.of_int size
        | Exp.Sizeof {typ; nbytes= None}
         -> Val.of_int (sizeof typ)
        | Exp.Exn _ | Exp.Closure _
         -> Val.Itv.top

  and eval_unop : Unop.t -> Exp.t -> Mem.astate -> Location.t -> Val.t =
    fun unop e mem loc ->
      let v = eval e mem loc in
      match unop with
      | Unop.Neg
       -> Val.neg v
      | Unop.BNot
       -> Val.unknown_bit v
      | Unop.LNot
       -> Val.lnot v

  and eval_binop : Binop.t -> Exp.t -> Exp.t -> Mem.astate -> Location.t -> Val.t =
    fun binop e1 e2 mem loc ->
      let v1 = eval e1 mem loc in
      let v2 = eval e2 mem loc in
      match binop with
      | Binop.PlusA
       -> Val.join (Val.plus v1 v2) (Val.plus_pi v1 v2)
      | Binop.PlusPI
       -> Val.plus_pi v1 v2
      | Binop.MinusA
       -> Val.joins [Val.minus v1 v2; Val.minus_pi v1 v2; Val.minus_pp v1 v2]
      | Binop.MinusPI
       -> Val.minus_pi v1 v2
      | Binop.MinusPP
       -> Val.minus_pp v1 v2
      | Binop.Mult
       -> Val.mult v1 v2
      | Binop.Div
       -> Val.div v1 v2
      | Binop.Mod
       -> Val.mod_sem v1 v2
      | Binop.Shiftlt
       -> Val.shiftlt v1 v2
      | Binop.Shiftrt
       -> Val.shiftrt v1 v2
      | Binop.Lt
       -> Val.lt_sem v1 v2
      | Binop.Gt
       -> Val.gt_sem v1 v2
      | Binop.Le
       -> Val.le_sem v1 v2
      | Binop.Ge
       -> Val.ge_sem v1 v2
      | Binop.Eq
       -> Val.eq_sem v1 v2
      | Binop.Ne
       -> Val.ne_sem v1 v2
      | Binop.BAnd | Binop.BXor | Binop.BOr
       -> Val.unknown_bit v1
      | Binop.LAnd
       -> Val.land_sem v1 v2
      | Binop.LOr
       -> Val.lor_sem v1 v2

  let rec eval_locs : Exp.t -> Mem.astate -> Location.t -> Val.t =
    fun exp mem loc ->
      match exp with
      | Exp.Var id -> (
        match Mem.find_alias id mem with
        | Some pvar
         -> Var.of_pvar pvar |> Loc.of_var |> PowLoc.singleton |> Val.of_pow_loc
        | None
         -> Val.bot )
      | Exp.Lvar pvar
       -> pvar |> Loc.of_pvar |> PowLoc.singleton |> Val.of_pow_loc
      | Exp.BinOp (bop, e1, e2)
       -> eval_binop bop e1 e2 mem loc
      | Exp.Cast (_, e)
       -> eval_locs e mem loc
      | Exp.Lfield (e, fn, _)
       -> eval e mem loc |> Val.get_all_locs |> Fn.flip PowLoc.append_field fn |> Val.of_pow_loc
      | Exp.Lindex (e1, e2)
       -> let arr = eval e1 mem loc in
          let idx = eval e2 mem loc in
          Val.plus_pi arr idx
      | Exp.Const _ | Exp.UnOp _ | Exp.Sizeof _ | Exp.Exn _ | Exp.Closure _
       -> Val.bot

  let get_allocsite : Typ.Procname.t -> CFG.node -> int -> int -> string =
    fun proc_name node inst_num dimension ->
      let proc_name = Typ.Procname.to_string proc_name in
      let node_num = CFG.hash node |> string_of_int in
      let inst_num = string_of_int inst_num in
      let dimension = string_of_int dimension in
      proc_name ^ "-" ^ node_num ^ "-" ^ inst_num ^ "-" ^ dimension |> Allocsite.make

  let eval_array_alloc
      : Typ.Procname.t -> CFG.node -> Typ.t -> ?stride:int -> Itv.t -> Itv.t -> int -> int -> Val.t =
    fun pdesc node typ ?stride:stride0 offset size inst_num dimension ->
      let allocsite = get_allocsite pdesc node inst_num dimension in
      let int_stride = match stride0 with None -> sizeof typ | Some stride -> stride in
      let stride = Itv.of_int int_stride in
      ArrayBlk.make allocsite offset size stride |> Val.of_array_blk

  let prune_unop : Exp.t -> Mem.astate -> Mem.astate =
    fun e mem ->
      match e with
      | Exp.Var x -> (
        match Mem.find_alias x mem with
        | Some x'
         -> let lv = Loc.of_pvar x' in
            let v = Mem.find_heap lv mem in
            let v' = Val.prune_zero v in
            Mem.update_mem (PowLoc.singleton lv) v' mem
        | None
         -> mem )
      | Exp.UnOp (Unop.LNot, Exp.Var x, _) -> (
        match Mem.find_alias x mem with
        | Some x'
         -> let lv = Loc.of_pvar x' in
            let v = Mem.find_heap lv mem in
            let itv_v = if Itv.is_bot (Val.get_itv v) then Itv.bot else Itv.false_sem in
            let v' = Val.modify_itv itv_v v in
            Mem.update_mem (PowLoc.singleton lv) v' mem
        | None
         -> mem )
      | _
       -> mem

  let prune_binop_left : Exp.t -> Location.t -> Mem.astate -> Mem.astate =
    fun e loc mem ->
      match e with
      | Exp.BinOp ((Binop.Lt as comp), Exp.Var x, e')
      | Exp.BinOp ((Binop.Gt as comp), Exp.Var x, e')
      | Exp.BinOp ((Binop.Le as comp), Exp.Var x, e')
      | Exp.BinOp ((Binop.Ge as comp), Exp.Var x, e') -> (
        match Mem.find_alias x mem with
        | Some x'
         -> let lv = Loc.of_pvar x' in
            let v = Mem.find_heap lv mem in
            let v' = Val.prune_comp comp v (eval e' mem loc) in
            Mem.update_mem (PowLoc.singleton lv) v' mem
        | None
         -> mem )
      | Exp.BinOp (Binop.Eq, Exp.Var x, e') -> (
        match Mem.find_alias x mem with
        | Some x'
         -> let lv = Loc.of_pvar x' in
            let v = Mem.find_heap lv mem in
            let v' = Val.prune_eq v (eval e' mem loc) in
            Mem.update_mem (PowLoc.singleton lv) v' mem
        | None
         -> mem )
      | Exp.BinOp (Binop.Ne, Exp.Var x, e') -> (
        match Mem.find_alias x mem with
        | Some x'
         -> let lv = Loc.of_pvar x' in
            let v = Mem.find_heap lv mem in
            let v' = Val.prune_ne v (eval e' mem loc) in
            Mem.update_mem (PowLoc.singleton lv) v' mem
        | None
         -> mem )
      | _
       -> mem

  let prune_binop_right : Exp.t -> Location.t -> Mem.astate -> Mem.astate =
    fun e loc mem ->
      match e with
      | Exp.BinOp ((Binop.Lt as c), e', Exp.Var x)
      | Exp.BinOp ((Binop.Gt as c), e', Exp.Var x)
      | Exp.BinOp ((Binop.Le as c), e', Exp.Var x)
      | Exp.BinOp ((Binop.Ge as c), e', Exp.Var x)
      | Exp.BinOp ((Binop.Eq as c), e', Exp.Var x)
      | Exp.BinOp ((Binop.Ne as c), e', Exp.Var x)
       -> prune_binop_left (Exp.BinOp (comp_rev c, Exp.Var x, e')) loc mem
      | _
       -> mem

  let is_unreachable_constant : Exp.t -> Location.t -> Mem.astate -> bool =
    fun e loc m -> Val.( <= ) ~lhs:(eval e m loc) ~rhs:(Val.of_int 0)

  let prune_unreachable : Exp.t -> Location.t -> Mem.astate -> Mem.astate =
    fun e loc mem -> if is_unreachable_constant e loc mem then Mem.bot else mem

  let rec prune : Exp.t -> Location.t -> Mem.astate -> Mem.astate =
    fun e loc mem ->
      let mem =
        mem |> prune_unreachable e loc |> prune_unop e |> prune_binop_left e loc
        |> prune_binop_right e loc
      in
      match e with
      | Exp.BinOp (Binop.Ne, e, Exp.Const Const.Cint i) when IntLit.iszero i
       -> prune e loc mem
      | Exp.BinOp (Binop.Eq, e, Exp.Const Const.Cint i) when IntLit.iszero i
       -> prune (Exp.UnOp (Unop.LNot, e, None)) loc mem
      | Exp.UnOp (Unop.Neg, Exp.Var x, _)
       -> prune (Exp.Var x) loc mem
      | Exp.BinOp (Binop.LAnd, e1, e2)
       -> mem |> prune e1 loc |> prune e2 loc
      | Exp.UnOp (Unop.LNot, Exp.BinOp (Binop.LOr, e1, e2), t)
       -> mem |> prune (Exp.UnOp (Unop.LNot, e1, t)) loc |> prune (Exp.UnOp (Unop.LNot, e2, t)) loc
      | Exp.UnOp (Unop.LNot, Exp.BinOp ((Binop.Lt as c), e1, e2), _)
      | Exp.UnOp (Unop.LNot, Exp.BinOp ((Binop.Gt as c), e1, e2), _)
      | Exp.UnOp (Unop.LNot, Exp.BinOp ((Binop.Le as c), e1, e2), _)
      | Exp.UnOp (Unop.LNot, Exp.BinOp ((Binop.Ge as c), e1, e2), _)
      | Exp.UnOp (Unop.LNot, Exp.BinOp ((Binop.Eq as c), e1, e2), _)
      | Exp.UnOp (Unop.LNot, Exp.BinOp ((Binop.Ne as c), e1, e2), _)
       -> prune (Exp.BinOp (comp_not c, e1, e2)) loc mem
      | _
       -> mem

  let get_formals : Procdesc.t -> (Pvar.t * Typ.t) list =
    fun pdesc ->
      let proc_name = Procdesc.get_proc_name pdesc in
      Procdesc.get_formals pdesc |> List.map ~f:(fun (name, typ) -> (Pvar.mk name proc_name, typ))

  let get_matching_pairs
      : Tenv.t -> Val.t -> Val.t -> Typ.t -> Mem.astate -> Mem.astate
        -> (Itv.Bound.t * Itv.Bound.t * TraceSet.t) list =
    fun tenv formal actual typ caller_mem callee_mem ->
      let get_itv v = Val.get_itv v in
      let get_offset v = v |> Val.get_array_blk |> ArrayBlk.offsetof in
      let get_size v = v |> Val.get_array_blk |> ArrayBlk.sizeof in
      let get_field_name (fn, _, _) = fn in
      let deref_field v fn mem =
        Mem.find_heap_set (PowLoc.append_field (Val.get_all_locs v) fn) mem
      in
      let deref_ptr v mem = Mem.find_heap_set (Val.get_array_locs v) mem in
      let add_pair_itv itv1 itv2 traces l =
        let open Itv in
        if itv1 <> bot && itv1 <> top && itv2 <> bot then (lb itv1, lb itv2, traces)
          :: (ub itv1, ub itv2, traces) :: l
        else if itv1 <> bot && itv1 <> top && Itv.eq itv2 bot then
          (lb itv1, Bound.Bot, TraceSet.empty) :: (ub itv1, Bound.Bot, TraceSet.empty) :: l
        else l
      in
      let add_pair_val v1 v2 pairs =
        pairs |> add_pair_itv (get_itv v1) (get_itv v2) (Val.get_traces v2)
        |> add_pair_itv (get_offset v1) (get_offset v2) (Val.get_traces v2)
        |> add_pair_itv (get_size v1) (get_size v2) (Val.get_traces v2)
      in
      let add_pair_field v1 v2 pairs fn =
        let v1' = deref_field v1 fn callee_mem in
        let v2' = deref_field v2 fn caller_mem in
        add_pair_val v1' v2' pairs
      in
      let add_pair_ptr typ v1 v2 pairs =
        match typ.Typ.desc with
        | Typ.Tptr ({desc= Tstruct typename}, _) -> (
          match Tenv.lookup tenv typename with
          | Some str
           -> let fns = List.map ~f:get_field_name str.Typ.Struct.fields in
              List.fold ~f:(add_pair_field v1 v2) ~init:pairs fns
          | _
           -> pairs )
        | Typ.Tptr (_, _)
         -> let v1' = deref_ptr v1 callee_mem in
            let v2' = deref_ptr v2 caller_mem in
            add_pair_val v1' v2' pairs
        | _
         -> pairs
      in
      [] |> add_pair_val formal actual |> add_pair_ptr typ formal actual

  let subst_map_of_pairs
      : (Itv.Bound.t * Itv.Bound.t * TraceSet.t) list
        -> Itv.Bound.t Itv.SubstMap.t * TraceSet.t Itv.SubstMap.t =
    fun pairs ->
      let add_pair (bound_map, trace_map) (formal, actual, traces) =
        match formal with
        | Itv.Bound.Linear (_, se1) when Itv.SymLinear.is_zero se1
         -> (bound_map, trace_map)
        | Itv.Bound.Linear (0, se1) when Itv.SymLinear.cardinal se1 > 0
         -> let symbol, coeff = Itv.SymLinear.choose se1 in
            if Int.equal coeff 1 then
              (Itv.SubstMap.add symbol actual bound_map, Itv.SubstMap.add symbol traces trace_map)
            else assert false
        | Itv.Bound.MinMax (Itv.Bound.Max, 0, symbol)
         -> (Itv.SubstMap.add symbol actual bound_map, Itv.SubstMap.add symbol traces trace_map)
        | _
         -> assert false
      in
      List.fold ~f:add_pair ~init:(Itv.SubstMap.empty, Itv.SubstMap.empty) pairs

  let rec list_fold2_def
      : default:Val.t -> f:('a -> Val.t -> 'b -> 'b) -> 'a list -> Val.t list -> init:'b -> 'b =
    fun ~default ~f xs ys ~init:acc ->
      match (xs, ys) with
      | [], _
       -> acc
      | x :: xs', []
       -> list_fold2_def ~default ~f xs' ys ~init:(f x default acc)
      | [x], _ :: _
       -> f x (List.fold ~f:Val.join ~init:Val.bot ys) acc
      | x :: xs', y :: ys'
       -> list_fold2_def ~default ~f xs' ys' ~init:(f x y acc)

  let get_subst_map
      : Tenv.t -> Procdesc.t -> (Exp.t * 'a) list -> Mem.astate -> Mem.astate -> Location.t
        -> Itv.Bound.t Itv.SubstMap.t * TraceSet.t Itv.SubstMap.t =
    fun tenv callee_pdesc params caller_mem callee_entry_mem loc ->
      let add_pair (formal, typ) actual l =
        let formal = Mem.find_heap (Loc.of_pvar formal) callee_entry_mem in
        let new_matching = get_matching_pairs tenv formal actual typ caller_mem callee_entry_mem in
        List.rev_append new_matching l
      in
      let formals = get_formals callee_pdesc in
      let actuals = List.map ~f:(fun (a, _) -> eval a caller_mem loc) params in
      list_fold2_def ~default:Val.Itv.top ~f:add_pair formals actuals ~init:[]
      |> subst_map_of_pairs
end
