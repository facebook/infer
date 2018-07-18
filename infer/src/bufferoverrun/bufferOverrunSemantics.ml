(*
 * Copyright (c) 2016-present
 *
 * Programming Research Laboratory (ROPAS)
 * Seoul National University, Korea
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open AbsLoc
open! AbstractDomain.Types
open BufferOverrunDomain

let eval_const : Const.t -> Val.t = function
  | Const.Cint intlit ->
      Option.value_map ~default:Val.Itv.top ~f:Val.of_int (IntLit.to_int intlit)
  | Const.Cfloat f ->
      f |> int_of_float |> Val.of_int
  | _ ->
      Val.Itv.top


(* TODO *)

let sizeof_ikind : Typ.ikind -> int = function
  | Typ.IChar | Typ.ISChar | Typ.IUChar | Typ.IBool ->
      1
  | Typ.IInt | Typ.IUInt ->
      4
  | Typ.IShort | Typ.IUShort ->
      2
  | Typ.ILong | Typ.IULong ->
      4
  | Typ.ILongLong | Typ.IULongLong ->
      8
  | Typ.I128 | Typ.IU128 ->
      16


let sizeof_fkind : Typ.fkind -> int = function
  | Typ.FFloat ->
      4
  | Typ.FDouble | Typ.FLongDouble ->
      8


(* NOTE: assume 32bit machine *)
let rec sizeof (typ: Typ.t) : int =
  match typ.desc with
  | Typ.Tint ikind ->
      sizeof_ikind ikind
  | Typ.Tfloat fkind ->
      sizeof_fkind fkind
  | Typ.Tvoid ->
      1
  | Typ.Tptr (_, _) ->
      4
  | Typ.Tstruct _ | Typ.TVar _ ->
      4 (* TODO *)
  | Typ.Tarray {length= Some length; stride= Some stride} ->
      IntLit.to_int_exn stride * IntLit.to_int_exn length
  | Typ.Tarray {elt; length= Some length; stride= None} ->
      sizeof elt * IntLit.to_int_exn length
  | _ ->
      4


let rec must_alias : Exp.t -> Exp.t -> Mem.astate -> bool =
 fun e1 e2 m ->
  match (e1, e2) with
  | Exp.Var x1, Exp.Var x2 -> (
    match (Mem.find_alias x1 m, Mem.find_alias x2 m) with
    | Some x1', Some x2' ->
        AliasTarget.equal x1' x2'
    | _, _ ->
        false )
  | Exp.UnOp (uop1, e1', _), Exp.UnOp (uop2, e2', _) ->
      Unop.equal uop1 uop2 && must_alias e1' e2' m
  | Exp.BinOp (bop1, e11, e12), Exp.BinOp (bop2, e21, e22) ->
      Binop.equal bop1 bop2 && must_alias e11 e21 m && must_alias e12 e22 m
  | Exp.Exn t1, Exp.Exn t2 ->
      must_alias t1 t2 m
  | Exp.Const c1, Exp.Const c2 ->
      Const.equal c1 c2
  | Exp.Cast (t1, e1'), Exp.Cast (t2, e2') ->
      Typ.equal t1 t2 && must_alias e1' e2' m
  | Exp.Lvar x1, Exp.Lvar x2 ->
      Pvar.equal x1 x2
  | Exp.Lfield (e1, fld1, _), Exp.Lfield (e2, fld2, _) ->
      must_alias e1 e2 m && Typ.Fieldname.equal fld1 fld2
  | Exp.Lindex (e11, e12), Exp.Lindex (e21, e22) ->
      must_alias e11 e21 m && must_alias e12 e22 m
  | Exp.Sizeof {nbytes= Some nbytes1}, Exp.Sizeof {nbytes= Some nbytes2} ->
      Int.equal nbytes1 nbytes2
  | ( Exp.Sizeof {typ= t1; dynamic_length= dynlen1; subtype= subt1}
    , Exp.Sizeof {typ= t2; dynamic_length= dynlen2; subtype= subt2} ) ->
      Typ.equal t1 t2 && must_alias_opt dynlen1 dynlen2 m
      && Int.equal (Subtype.compare subt1 subt2) 0
  | _, _ ->
      false


and must_alias_opt : Exp.t option -> Exp.t option -> Mem.astate -> bool =
 fun e1_opt e2_opt m ->
  match (e1_opt, e2_opt) with
  | Some e1, Some e2 ->
      must_alias e1 e2 m
  | None, None ->
      true
  | _, _ ->
      false


let comp_rev : Binop.t -> Binop.t = function
  | Binop.Lt ->
      Binop.Gt
  | Binop.Gt ->
      Binop.Lt
  | Binop.Le ->
      Binop.Ge
  | Binop.Ge ->
      Binop.Le
  | Binop.Eq ->
      Binop.Eq
  | Binop.Ne ->
      Binop.Ne
  | _ ->
      assert false


let comp_not : Binop.t -> Binop.t = function
  | Binop.Lt ->
      Binop.Ge
  | Binop.Gt ->
      Binop.Le
  | Binop.Le ->
      Binop.Gt
  | Binop.Ge ->
      Binop.Lt
  | Binop.Eq ->
      Binop.Ne
  | Binop.Ne ->
      Binop.Eq
  | _ ->
      assert false


let rec must_alias_cmp : Exp.t -> Mem.astate -> bool =
 fun e m ->
  match e with
  | Exp.BinOp (Binop.Lt, e1, e2) | Exp.BinOp (Binop.Gt, e1, e2) | Exp.BinOp (Binop.Ne, e1, e2) ->
      must_alias e1 e2 m
  | Exp.BinOp (Binop.LAnd, e1, e2) ->
      must_alias_cmp e1 m || must_alias_cmp e2 m
  | Exp.BinOp (Binop.LOr, e1, e2) ->
      must_alias_cmp e1 m && must_alias_cmp e2 m
  | Exp.UnOp (Unop.LNot, Exp.UnOp (Unop.LNot, e1, _), _) ->
      must_alias_cmp e1 m
  | Exp.UnOp (Unop.LNot, Exp.BinOp ((Binop.Lt as c), e1, e2), _)
  | Exp.UnOp (Unop.LNot, Exp.BinOp ((Binop.Gt as c), e1, e2), _)
  | Exp.UnOp (Unop.LNot, Exp.BinOp ((Binop.Le as c), e1, e2), _)
  | Exp.UnOp (Unop.LNot, Exp.BinOp ((Binop.Ge as c), e1, e2), _)
  | Exp.UnOp (Unop.LNot, Exp.BinOp ((Binop.Eq as c), e1, e2), _)
  | Exp.UnOp (Unop.LNot, Exp.BinOp ((Binop.Ne as c), e1, e2), _) ->
      must_alias_cmp (Exp.BinOp (comp_not c, e1, e2)) m
  | Exp.UnOp (Unop.LNot, Exp.BinOp (Binop.LOr, e1, e2), t) ->
      let e1' = Exp.UnOp (Unop.LNot, e1, t) in
      let e2' = Exp.UnOp (Unop.LNot, e2, t) in
      must_alias_cmp (Exp.BinOp (Binop.LAnd, e1', e2')) m
  | Exp.UnOp (Unop.LNot, Exp.BinOp (Binop.LAnd, e1, e2), t) ->
      let e1' = Exp.UnOp (Unop.LNot, e1, t) in
      let e2' = Exp.UnOp (Unop.LNot, e2, t) in
      must_alias_cmp (Exp.BinOp (Binop.LOr, e1', e2')) m
  | _ ->
      false


let rec eval : Exp.t -> Mem.astate -> Val.t =
 fun exp mem ->
  if must_alias_cmp exp mem then Val.of_int 0
  else
    match exp with
    | Exp.Var id ->
        Mem.find_stack (Var.of_id id |> Loc.of_var) mem
    | Exp.Lvar pvar ->
        let ploc = pvar |> Loc.of_pvar |> PowLoc.singleton in
        let arr = Mem.find_stack_set ploc mem in
        ploc |> Val.of_pow_loc |> Val.join arr
    | Exp.UnOp (uop, e, _) ->
        eval_unop uop e mem
    | Exp.BinOp (bop, e1, e2) ->
        eval_binop bop e1 e2 mem
    | Exp.Const c ->
        eval_const c
    | Exp.Cast (_, e) ->
        eval e mem
    | Exp.Lfield (e, fn, _) ->
        eval e mem |> Val.get_all_locs |> PowLoc.append_field ~fn |> Val.of_pow_loc
    | Exp.Lindex (e1, e2) ->
        eval_lindex e1 e2 mem
    | Exp.Sizeof {nbytes= Some size} ->
        Val.of_int size
    | Exp.Sizeof {typ; nbytes= None} ->
        Val.of_int (sizeof typ)
    | Exp.Exn _ | Exp.Closure _ ->
        Val.Itv.top


(* NOTE: multidimensional array is not supported yet *)
and eval_lindex array_exp index_exp mem =
  let array_v, index_v = (eval array_exp mem, eval index_exp mem) in
  let arr = Val.plus_pi array_v index_v in
  if ArrayBlk.is_bot (Val.get_array_blk arr) then
    match array_exp with
    | Exp.Lfield _ when not (PowLoc.is_bot (Val.get_pow_loc array_v)) ->
        (* It handles the case accessing an array field of struct,
             e.g., x.f[n] .  Since our abstract domain distinguishes
             memory sections for each array fields in struct, it finds
             the memory section using the abstract memory, though the
             memory lookup is not required to evaluate the address of
             x.f[n] in the concrete semantics.  *)
        Val.plus_pi (Mem.find_set (Val.get_pow_loc array_v) mem) index_v
    | _ ->
        Val.of_pow_loc PowLoc.unknown
  else arr


and eval_unop : Unop.t -> Exp.t -> Mem.astate -> Val.t =
 fun unop e mem ->
  let v = eval e mem in
  match unop with
  | Unop.Neg ->
      Val.neg v
  | Unop.BNot ->
      Val.unknown_bit v
  | Unop.LNot ->
      Val.lnot v


and eval_binop : Binop.t -> Exp.t -> Exp.t -> Mem.astate -> Val.t =
 fun binop e1 e2 mem ->
  let v1 = eval e1 mem in
  let v2 = eval e2 mem in
  match binop with
  | Binop.PlusA ->
      Val.plus_a v1 v2
  | Binop.PlusPI ->
      Val.plus_pi v1 v2
  | Binop.MinusA ->
      Val.minus_a v1 v2
  | Binop.MinusPI ->
      Val.minus_pi v1 v2
  | Binop.MinusPP ->
      Val.minus_pp v1 v2
  | Binop.Mult ->
      Val.mult v1 v2
  | Binop.Div ->
      Val.div v1 v2
  | Binop.Mod ->
      Val.mod_sem v1 v2
  | Binop.Shiftlt ->
      Val.shiftlt v1 v2
  | Binop.Shiftrt ->
      Val.shiftrt v1 v2
  | Binop.Lt ->
      Val.lt_sem v1 v2
  | Binop.Gt ->
      Val.gt_sem v1 v2
  | Binop.Le ->
      Val.le_sem v1 v2
  | Binop.Ge ->
      Val.ge_sem v1 v2
  | Binop.Eq ->
      Val.eq_sem v1 v2
  | Binop.Ne ->
      Val.ne_sem v1 v2
  | Binop.BAnd | Binop.BXor | Binop.BOr ->
      Val.unknown_bit v1
  | Binop.LAnd ->
      Val.land_sem v1 v2
  | Binop.LOr ->
      Val.lor_sem v1 v2


(* It returns the array value of the input expression.  For example,
   when "x" is a program variable, (eval_arr "x") returns array blocks
   the "x" is pointing to, on the other hand, (eval "x") returns the
   abstract location of "x". *)
let rec eval_arr : Exp.t -> Mem.astate -> Val.t =
 fun exp mem ->
  match exp with
  | Exp.Var id -> (
    match Mem.find_alias id mem with
    | Some (AliasTarget.Simple loc) ->
        Mem.find_heap loc mem
    | Some (AliasTarget.Empty _) | None ->
        Val.bot )
  | Exp.Lvar pvar ->
      Mem.find_set (PowLoc.singleton (Loc.of_pvar pvar)) mem
  | Exp.BinOp (bop, e1, e2) ->
      eval_binop bop e1 e2 mem
  | Exp.Cast (_, e) ->
      eval_arr e mem
  | Exp.Lfield (e, fn, _) ->
      let locs = eval e mem |> Val.get_all_locs |> PowLoc.append_field ~fn in
      Mem.find_heap_set locs mem
  | Exp.Lindex (e1, e2) ->
      let arr = eval e1 mem in
      let idx = eval e2 mem in
      Val.plus_pi arr idx
  | Exp.Const _ | Exp.UnOp _ | Exp.Sizeof _ | Exp.Exn _ | Exp.Closure _ ->
      Val.bot


let get_allocsite : Typ.Procname.t -> node_hash:int -> inst_num:int -> dimension:int -> string =
 fun proc_name ~node_hash ~inst_num ~dimension ->
  let proc_name = Typ.Procname.to_string proc_name in
  let node_num = string_of_int node_hash in
  let inst_num = string_of_int inst_num in
  let dimension = string_of_int dimension in
  proc_name ^ "-" ^ node_num ^ "-" ^ inst_num ^ "-" ^ dimension |> Allocsite.make


let eval_array_alloc
    : Typ.Procname.t -> node_hash:int -> Typ.t -> stride:int option -> offset:Itv.t -> size:Itv.t
      -> inst_num:int -> dimension:int -> Val.t =
 fun pdesc ~node_hash typ ~stride ~offset ~size ~inst_num ~dimension ->
  let allocsite = get_allocsite pdesc ~node_hash ~inst_num ~dimension in
  let int_stride = match stride with None -> sizeof typ | Some stride -> stride in
  let stride = Itv.of_int int_stride in
  ArrayBlk.make allocsite ~offset ~size ~stride |> Val.of_array_blk


module Prune = struct
  type astate = {prune_pairs: PrunePairs.t; mem: Mem.astate}

  let update_mem_in_prune lv v {prune_pairs; mem} =
    let prune_pairs = (lv, v) :: prune_pairs in
    let mem = Mem.update_mem (PowLoc.singleton lv) v mem in
    {prune_pairs; mem}


  let prune_unop : Exp.t -> astate -> astate =
   fun e ({mem} as astate) ->
    match e with
    | Exp.Var x -> (
      match Mem.find_alias x mem with
      | Some (AliasTarget.Simple lv) ->
          let v = Mem.find_heap lv mem in
          let v' = Val.prune_ne_zero v in
          update_mem_in_prune lv v' astate
      | Some (AliasTarget.Empty lv) ->
          let v = Mem.find_heap lv mem in
          let v' = Val.prune_eq_zero v in
          update_mem_in_prune lv v' astate
      | None ->
          astate )
    | Exp.UnOp (Unop.LNot, Exp.Var x, _) -> (
      match Mem.find_alias x mem with
      | Some (AliasTarget.Simple lv) ->
          let v = Mem.find_heap lv mem in
          let v' = Val.prune_eq_zero v in
          update_mem_in_prune lv v' astate
      | Some (AliasTarget.Empty lv) ->
          let v = Mem.find_heap lv mem in
          let itv_v = Itv.prune_comp Binop.Ge (Val.get_itv v) Itv.one in
          let v' = Val.modify_itv itv_v v in
          update_mem_in_prune lv v' astate
      | None ->
          astate )
    | _ ->
        astate


  let prune_binop_left : Exp.t -> astate -> astate =
   fun e ({mem} as astate) ->
    match e with
    | Exp.BinOp ((Binop.Lt as comp), Exp.Var x, e')
    | Exp.BinOp ((Binop.Gt as comp), Exp.Var x, e')
    | Exp.BinOp ((Binop.Le as comp), Exp.Var x, e')
    | Exp.BinOp ((Binop.Ge as comp), Exp.Var x, e') -> (
      match Mem.find_simple_alias x mem with
      | Some lv ->
          let v = Mem.find_heap lv mem in
          let v' = Val.prune_comp comp v (eval e' mem) in
          update_mem_in_prune lv v' astate
      | None ->
          astate )
    | Exp.BinOp (Binop.Eq, Exp.Var x, e') -> (
      match Mem.find_simple_alias x mem with
      | Some lv ->
          let v = Mem.find_heap lv mem in
          let v' = Val.prune_eq v (eval e' mem) in
          update_mem_in_prune lv v' astate
      | None ->
          astate )
    | Exp.BinOp (Binop.Ne, Exp.Var x, e') -> (
      match Mem.find_simple_alias x mem with
      | Some lv ->
          let v = Mem.find_heap lv mem in
          let v' = Val.prune_ne v (eval e' mem) in
          update_mem_in_prune lv v' astate
      | None ->
          astate )
    | _ ->
        astate


  let prune_binop_right : Exp.t -> astate -> astate =
   fun e astate ->
    match e with
    | Exp.BinOp ((Binop.Lt as c), e', Exp.Var x)
    | Exp.BinOp ((Binop.Gt as c), e', Exp.Var x)
    | Exp.BinOp ((Binop.Le as c), e', Exp.Var x)
    | Exp.BinOp ((Binop.Ge as c), e', Exp.Var x)
    | Exp.BinOp ((Binop.Eq as c), e', Exp.Var x)
    | Exp.BinOp ((Binop.Ne as c), e', Exp.Var x) ->
        prune_binop_left (Exp.BinOp (comp_rev c, Exp.Var x, e')) astate
    | _ ->
        astate


  let is_unreachable_constant : Exp.t -> Mem.astate -> bool =
   fun e m -> Val.( <= ) ~lhs:(eval e m) ~rhs:(Val.of_int 0)


  let prune_unreachable : Exp.t -> astate -> astate =
   fun e ({mem} as astate) ->
    if is_unreachable_constant e mem then {astate with mem= Mem.bot} else astate


  let rec prune_helper e astate =
    let astate =
      astate |> prune_unreachable e |> prune_unop e |> prune_binop_left e |> prune_binop_right e
    in
    match e with
    | Exp.BinOp (Binop.Ne, e, Exp.Const (Const.Cint i)) when IntLit.iszero i ->
        prune_helper e astate
    | Exp.BinOp (Binop.Eq, e, Exp.Const (Const.Cint i)) when IntLit.iszero i ->
        prune_helper (Exp.UnOp (Unop.LNot, e, None)) astate
    | Exp.UnOp (Unop.Neg, Exp.Var x, _) ->
        prune_helper (Exp.Var x) astate
    | Exp.BinOp (Binop.LAnd, e1, e2) ->
        astate |> prune_helper e1 |> prune_helper e2
    | Exp.UnOp (Unop.LNot, Exp.BinOp (Binop.LOr, e1, e2), t) ->
        astate |> prune_helper (Exp.UnOp (Unop.LNot, e1, t))
        |> prune_helper (Exp.UnOp (Unop.LNot, e2, t))
    | Exp.UnOp (Unop.LNot, Exp.BinOp ((Binop.Lt as c), e1, e2), _)
    | Exp.UnOp (Unop.LNot, Exp.BinOp ((Binop.Gt as c), e1, e2), _)
    | Exp.UnOp (Unop.LNot, Exp.BinOp ((Binop.Le as c), e1, e2), _)
    | Exp.UnOp (Unop.LNot, Exp.BinOp ((Binop.Ge as c), e1, e2), _)
    | Exp.UnOp (Unop.LNot, Exp.BinOp ((Binop.Eq as c), e1, e2), _)
    | Exp.UnOp (Unop.LNot, Exp.BinOp ((Binop.Ne as c), e1, e2), _) ->
        prune_helper (Exp.BinOp (comp_not c, e1, e2)) astate
    | _ ->
        astate


  let prune : Exp.t -> Mem.astate -> Mem.astate =
   fun e mem ->
    let mem = Mem.apply_latest_prune e mem in
    let {mem; prune_pairs} = prune_helper e {mem; prune_pairs= PrunePairs.empty} in
    Mem.set_prune_pairs prune_pairs mem
end

let get_formals : Procdesc.t -> (Pvar.t * Typ.t) list =
 fun pdesc ->
  let proc_name = Procdesc.get_proc_name pdesc in
  Procdesc.get_formals pdesc |> List.map ~f:(fun (name, typ) -> (Pvar.mk name proc_name, typ))


let get_matching_pairs
    : Tenv.t -> Val.t -> Val.t -> Typ.t -> Mem.astate -> Mem.astate
      -> callee_ret_alias:AliasTarget.t option
      -> (Itv.Bound.t * Itv.Bound.t bottom_lifted * TraceSet.t) list * AliasTarget.t option =
 fun tenv formal actual typ caller_mem callee_mem ~callee_ret_alias ->
  let get_itv v = Val.get_itv v in
  let get_offset v = v |> Val.get_array_blk |> ArrayBlk.offsetof in
  let get_size v = v |> Val.get_array_blk |> ArrayBlk.sizeof in
  let get_field_name (fn, _, _) = fn in
  let append_field v fn = PowLoc.append_field (Val.get_all_locs v) ~fn in
  let deref_field v fn mem = Mem.find_heap_set (append_field v fn) mem in
  let deref_ptr v mem =
    let array_locs = Val.get_array_locs v in
    let locs = if PowLoc.is_empty array_locs then Val.get_pow_loc v else array_locs in
    Mem.find_heap_set locs mem
  in
  let ret_alias = ref None in
  let add_ret_alias v1 v2 =
    match callee_ret_alias with
    | Some ret_loc ->
        if
          PowLoc.is_singleton v1 && PowLoc.is_singleton v2
          && AliasTarget.use (PowLoc.min_elt v1) ret_loc
        then ret_alias := Some (AliasTarget.replace (PowLoc.min_elt v2) ret_loc)
    | None ->
        ()
  in
  let add_pair_itv itv1 itv2 traces l =
    let open Itv in
    if itv1 <> bot && itv1 <> top then
      if Itv.eq itv2 bot then
        (lb itv1, Bottom, TraceSet.empty) :: (ub itv1, Bottom, TraceSet.empty) :: l
      else (lb itv1, NonBottom (lb itv2), traces) :: (ub itv1, NonBottom (ub itv2), traces) :: l
    else l
  in
  let add_pair_val v1 v2 pairs =
    add_ret_alias (Val.get_all_locs v1) (Val.get_all_locs v2) ;
    pairs |> add_pair_itv (get_itv v1) (get_itv v2) (Val.get_traces v2)
    |> add_pair_itv (get_offset v1) (get_offset v2) (Val.get_traces v2)
    |> add_pair_itv (get_size v1) (get_size v2) (Val.get_traces v2)
  in
  let add_pair_field v1 v2 pairs fn =
    add_ret_alias (append_field v1 fn) (append_field v2 fn) ;
    let v1' = deref_field v1 fn callee_mem in
    let v2' = deref_field v2 fn caller_mem in
    add_pair_val v1' v2' pairs
  in
  let add_pair_ptr typ v1 v2 pairs =
    add_ret_alias (Val.get_all_locs v1) (Val.get_all_locs v2) ;
    match typ.Typ.desc with
    | Typ.Tptr ({desc= Tstruct typename}, _) -> (
      match Tenv.lookup tenv typename with
      | Some str ->
          let fns = List.map ~f:get_field_name str.Typ.Struct.fields in
          List.fold ~f:(add_pair_field v1 v2) ~init:pairs fns
      | _ ->
          pairs )
    | Typ.Tptr (_, _) ->
        let v1' = deref_ptr v1 callee_mem in
        let v2' = deref_ptr v2 caller_mem in
        add_pair_val v1' v2' pairs
    | _ ->
        pairs
  in
  let pairs = [] |> add_pair_val formal actual |> add_pair_ptr typ formal actual in
  (pairs, !ret_alias)


let subst_map_of_pairs
    : (Itv.Bound.t * Itv.Bound.t bottom_lifted * TraceSet.t) list
      -> Itv.Bound.t bottom_lifted Itv.SymbolMap.t * TraceSet.t Itv.SymbolMap.t =
 fun pairs ->
  let add_pair (bound_map, trace_map) (formal, actual, traces) =
    if Itv.Bound.is_const formal |> Option.is_some then (bound_map, trace_map)
    else
      let symbol = Itv.Bound.get_one_symbol formal in
      (Itv.SymbolMap.add symbol actual bound_map, Itv.SymbolMap.add symbol traces trace_map)
  in
  List.fold ~f:add_pair ~init:(Itv.SymbolMap.empty, Itv.SymbolMap.empty) pairs


let rec list_fold2_def
    : default:Val.t -> f:('a -> Val.t -> 'b -> 'b) -> 'a list -> Val.t list -> init:'b -> 'b =
 fun ~default ~f xs ys ~init:acc ->
  match (xs, ys) with
  | [], _ ->
      acc
  | x :: xs', [] ->
      list_fold2_def ~default ~f xs' ys ~init:(f x default acc)
  | [x], _ :: _ ->
      f x (List.fold ~f:Val.join ~init:Val.bot ys) acc
  | x :: xs', y :: ys' ->
      list_fold2_def ~default ~f xs' ys' ~init:(f x y acc)


let get_subst_map
    : Tenv.t -> Procdesc.t -> (Exp.t * 'a) list -> Mem.astate -> Mem.astate
      -> callee_ret_alias:AliasTarget.t option
      -> (Itv.Bound.t bottom_lifted Itv.SymbolMap.t * TraceSet.t Itv.SymbolMap.t)
         * AliasTarget.t option =
 fun tenv callee_pdesc params caller_mem callee_entry_mem ~callee_ret_alias ->
  let add_pair (formal, typ) actual (l, ret_alias) =
    let formal = Mem.find_heap (Loc.of_pvar formal) callee_entry_mem in
    let new_matching, ret_alias' =
      get_matching_pairs tenv formal actual typ caller_mem callee_entry_mem ~callee_ret_alias
    in
    (List.rev_append new_matching l, Option.first_some ret_alias ret_alias')
  in
  let formals = get_formals callee_pdesc in
  let actuals = List.map ~f:(fun (a, _) -> eval a caller_mem) params in
  let pairs, ret_alias =
    list_fold2_def ~default:Val.Itv.top ~f:add_pair formals actuals ~init:([], None)
  in
  (subst_map_of_pairs pairs, ret_alias)
