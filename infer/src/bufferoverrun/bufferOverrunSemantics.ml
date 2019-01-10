(*
 * Copyright (c) 2016-present, Programming Research Laboratory (ROPAS)
 *                             Seoul National University, Korea
 * Copyright (c) 2017-present, Facebook, Inc.
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
      Val.of_big_int (IntLit.to_big_int intlit)
  | Const.Cfloat f ->
      f |> int_of_float |> Val.of_int
  | _ ->
      Val.Itv.top


let rec must_alias : Exp.t -> Exp.t -> Mem.t -> bool =
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


and must_alias_opt : Exp.t option -> Exp.t option -> Mem.t -> bool =
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


let rec must_alias_cmp : Exp.t -> Mem.t -> bool =
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


let set_array_stride integer_type_widths typ v =
  match typ with
  | Typ.({desc= Tptr ({desc= Tint ikind}, Pk_pointer)}) ->
      let width = Typ.width_of_ikind integer_type_widths ikind in
      Val.set_array_stride (Z.of_int (width / 8)) v
  | _ ->
      v


let rec eval : Typ.IntegerWidths.t -> Exp.t -> Mem.t -> Val.t =
 fun integer_type_widths exp mem ->
  if (not (Language.curr_language_is Java)) && must_alias_cmp exp mem then Val.Itv.zero
  else
    match exp with
    | Exp.Var id ->
        Mem.find_stack (Var.of_id id |> Loc.of_var) mem
    | Exp.Lvar pvar ->
        let loc = Loc.of_pvar pvar in
        if Mem.is_stack_loc loc mem then Mem.find loc mem else Val.of_loc loc
    | Exp.UnOp (uop, e, _) ->
        eval_unop integer_type_widths uop e mem
    | Exp.BinOp (bop, e1, e2) ->
        eval_binop integer_type_widths bop e1 e2 mem
    | Exp.Const c ->
        eval_const c
    | Exp.Cast (t, e) ->
        let v = eval integer_type_widths e mem in
        let v = set_array_stride integer_type_widths t v in
        if Typ.is_unsigned_int t && Val.is_mone v then
          (* We treat "(unsigned int)-1" case specially, because programmers often exploit it to get
             the max number of the type. *)
          let ikind = Option.value_exn (Typ.get_ikind_opt t) in
          Val.of_itv ~traces:(Val.get_traces v) (Itv.max_of_ikind integer_type_widths ikind)
        else v
    | Exp.Lfield (e, fn, _) ->
        let v = eval integer_type_widths e mem in
        let locs = Val.get_all_locs v |> PowLoc.append_field ~fn in
        Val.of_pow_loc locs ~traces:(Val.get_traces v)
    | Exp.Lindex (e1, e2) ->
        eval_lindex integer_type_widths e1 e2 mem
    | Exp.Sizeof {nbytes= Some size} ->
        Val.of_int size
    | Exp.Sizeof {nbytes= None} ->
        Val.Itv.nat
    | Exp.Exn _ | Exp.Closure _ ->
        Val.Itv.top


and eval_lindex integer_type_widths array_exp index_exp mem =
  let array_v = eval integer_type_widths array_exp mem in
  if ArrayBlk.is_bot (Val.get_array_blk array_v) then
    match array_exp with
    | Exp.Lfield _ ->
        let array_locs = Val.get_pow_loc array_v in
        if PowLoc.is_bot array_locs then Val.unknown_locs
        else
          (* It handles the case accessing an array field of struct,
             e.g., x.f[n] .  Since our abstract domain distinguishes
             memory sections for each array fields in struct, it finds
             the memory section using the abstract memory, though the
             memory lookup is not required to evaluate the address of
             x.f[n] in the concrete semantics.  *)
          let index_v = eval integer_type_widths index_exp mem in
          Val.plus_pi (Mem.find_set array_locs mem) index_v
    | _ ->
        Val.unknown_locs
  else
    match array_exp with
    | Exp.Lindex _ ->
        (* It handles multi-dimensional arrays. *)
        Mem.find_set (Val.get_all_locs array_v) mem
    | _ ->
        let index_v = eval integer_type_widths index_exp mem in
        Val.plus_pi array_v index_v


and eval_unop : Typ.IntegerWidths.t -> Unop.t -> Exp.t -> Mem.t -> Val.t =
 fun integer_type_widths unop e mem ->
  let v = eval integer_type_widths e mem in
  match unop with
  | Unop.Neg ->
      Val.neg v
  | Unop.BNot ->
      Val.unknown_bit v
  | Unop.LNot ->
      Val.lnot v


and eval_binop : Typ.IntegerWidths.t -> Binop.t -> Exp.t -> Exp.t -> Mem.t -> Val.t =
 fun integer_type_widths binop e1 e2 mem ->
  let v1 = eval integer_type_widths e1 mem in
  let v2 = eval integer_type_widths e2 mem in
  match binop with
  | Binop.PlusA _ ->
      Val.plus_a v1 v2
  | Binop.PlusPI ->
      Val.plus_pi v1 v2
  | Binop.MinusA _ ->
      Val.minus_a v1 v2
  | Binop.MinusPI ->
      Val.minus_pi v1 v2
  | Binop.MinusPP ->
      Val.minus_pp v1 v2
  | Binop.Mult _ ->
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
  | Binop.BAnd ->
      Val.band_sem v1 v2
  | Binop.BXor | Binop.BOr ->
      Val.unknown_bit v1
  | Binop.LAnd ->
      Val.land_sem v1 v2
  | Binop.LOr ->
      Val.lor_sem v1 v2


(**
  [eval_locs exp mem] is like [eval_locs exp mem |> Val.get_all_locs]
  but takes some shortcuts to avoid computing useless and/or problematic intermediate values
*)
let rec eval_locs : Exp.t -> Mem.t -> PowLoc.t =
 fun exp mem ->
  match exp with
  | Var id ->
      Mem.find_stack (Var.of_id id |> Loc.of_var) mem |> Val.get_all_locs
  | Lvar pvar ->
      let loc = Loc.of_pvar pvar in
      if Mem.is_stack_loc loc mem then Mem.find loc mem |> Val.get_all_locs
      else PowLoc.singleton loc
  | BinOp ((Binop.MinusPI | Binop.PlusPI), e, _) | Cast (_, e) ->
      eval_locs e mem
  | BinOp _ | Closure _ | Const _ | Exn _ | Sizeof _ | UnOp _ ->
      PowLoc.empty
  | Lfield (e, fn, _) ->
      eval_locs e mem |> PowLoc.append_field ~fn
  | Lindex (((Lfield _ | Lindex _) as e), _) ->
      Mem.find_set (eval_locs e mem) mem |> Val.get_all_locs
  | Lindex (e, _) ->
      eval_locs e mem


(* It returns the array value of the input expression.  For example,
   when "x" is a program variable, (eval_arr "x") returns array blocks
   the "x" is pointing to, on the other hand, (eval "x") returns the
   abstract location of "x". *)
let rec eval_arr : Typ.IntegerWidths.t -> Exp.t -> Mem.t -> Val.t =
 fun integer_type_widths exp mem ->
  match exp with
  | Exp.Var id -> (
    match Mem.find_alias id mem with
    | Some (AliasTarget.Simple loc) ->
        Mem.find loc mem
    | Some (AliasTarget.Empty _ | AliasTarget.Nullity _) | None ->
        Val.bot )
  | Exp.Lvar pvar ->
      Mem.find (Loc.of_pvar pvar) mem
  | Exp.BinOp (bop, e1, e2) ->
      eval_binop integer_type_widths bop e1 e2 mem
  | Exp.Cast (t, e) ->
      let v = eval_arr integer_type_widths e mem in
      set_array_stride integer_type_widths t v
  | Exp.Lfield (e, fn, _) ->
      let locs = eval_locs e mem |> PowLoc.append_field ~fn in
      Mem.find_set locs mem
  | Exp.Lindex (e, _) ->
      let locs = eval_locs e mem in
      Mem.find_set locs mem
  | Exp.Const _ | Exp.UnOp _ | Exp.Sizeof _ | Exp.Exn _ | Exp.Closure _ ->
      Val.bot


module ParamBindings = struct
  include PrettyPrintable.MakePPMap (struct
    include Pvar

    let pp = pp Pp.text
  end)

  let make formals actuals =
    let rec add_binding formals actuals acc =
      match (formals, actuals) with
      | (formal, _) :: formals', actual :: actuals' ->
          add_binding formals' actuals' (add formal actual acc)
      | _, _ ->
          acc
    in
    add_binding formals actuals empty
end

let rec eval_sympath_partial ~strict params p mem =
  match p with
  | Symb.SymbolPath.Pvar x -> (
    try ParamBindings.find x params with Caml.Not_found ->
      L.d_printfln_escaped "Symbol %a is not found in parameters." (Pvar.pp Pp.text) x ;
      Val.Itv.top )
  | Symb.SymbolPath.Callsite {cs} ->
      L.d_printfln_escaped "Symbol for %a is not expected to be in parameters." Typ.Procname.pp
        (CallSite.pname cs) ;
      Mem.find (Loc.of_allocsite (Allocsite.make_symbol p)) mem
  | Symb.SymbolPath.Deref _ | Symb.SymbolPath.Field _ ->
      let locs = eval_locpath ~strict params p mem in
      Mem.find_set locs mem


and eval_locpath ~strict params p mem =
  let res =
    match p with
    | Symb.SymbolPath.Pvar _ | Symb.SymbolPath.Callsite _ ->
        let v = eval_sympath_partial ~strict params p mem in
        Val.get_all_locs v
    | Symb.SymbolPath.Deref (_, p) ->
        let v = eval_sympath_partial ~strict params p mem in
        Val.get_all_locs v
    | Symb.SymbolPath.Field (fn, p) ->
        let locs = eval_locpath ~strict params p mem in
        PowLoc.append_field ~fn locs
  in
  if PowLoc.is_empty res && not strict then (
    L.d_printfln_escaped "Location value for %a is not found." Symb.SymbolPath.pp_partial p ;
    PowLoc.unknown )
  else res


let eval_sympath ~strict params sympath mem =
  match sympath with
  | Symb.SymbolPath.Normal p ->
      let v = eval_sympath_partial ~strict params p mem in
      (Val.get_itv v, Val.get_traces v)
  | Symb.SymbolPath.Offset p ->
      let v = eval_sympath_partial ~strict params p mem in
      (ArrayBlk.offsetof (Val.get_array_blk v), Val.get_traces v)
  | Symb.SymbolPath.Length p ->
      let v = eval_sympath_partial ~strict params p mem in
      (ArrayBlk.sizeof (Val.get_array_blk v), Val.get_traces v)


(* We have two modes (strict and non-strict) on evaluating location paths. When a location to
   substitute is not found:
   - non-strict mode (which is used by default): it returns the unknown location.
   - strict mode (which is used only in the substitution of condition of proof obligation): it
     returns the bottom location. *)
let mk_eval_sym_trace integer_type_widths callee_pdesc actual_exps caller_mem =
  let params =
    let formals = Procdesc.get_pvar_formals callee_pdesc in
    let actuals = List.map ~f:(fun (a, _) -> eval integer_type_widths a caller_mem) actual_exps in
    ParamBindings.make formals actuals
  in
  let eval_sym s bound_end =
    let sympath = Symb.Symbol.path s in
    let itv, _ = eval_sympath ~strict:false params sympath caller_mem in
    Symb.Symbol.assert_bound_end s bound_end ;
    Itv.get_bound itv bound_end
  in
  let trace_of_sym s =
    let sympath = Symb.Symbol.path s in
    let itv, traces = eval_sympath ~strict:false params sympath caller_mem in
    if Itv.eq itv Itv.bot then TraceSet.empty else traces
  in
  let eval_locpath ~strict partial = eval_locpath ~strict params partial caller_mem in
  fun ~strict -> {eval_sym; trace_of_sym; eval_locpath= eval_locpath ~strict}


let mk_eval_sym integer_type_widths callee_pdesc actual_exps caller_mem =
  let eval_sym_trace =
    mk_eval_sym_trace integer_type_widths callee_pdesc actual_exps caller_mem ~strict:false
  in
  eval_sym_trace.eval_sym


let get_sym_f integer_type_widths mem e = Val.get_sym (eval integer_type_widths e mem)

let get_offset_sym_f integer_type_widths mem e =
  Val.get_offset_sym (eval integer_type_widths e mem)


let get_size_sym_f integer_type_widths mem e = Val.get_size_sym (eval integer_type_widths e mem)

module Prune = struct
  type t = {prune_pairs: PrunePairs.t; mem: Mem.t}

  let update_mem_in_prune lv v {prune_pairs; mem} =
    let prune_pairs = PrunePairs.add lv v prune_pairs in
    let mem = Mem.update_mem (PowLoc.singleton lv) v mem in
    {prune_pairs; mem}


  let prune_unop : Exp.t -> t -> t =
   fun e ({mem} as astate) ->
    match e with
    | Exp.Var x -> (
      match Mem.find_alias x mem with
      | Some (AliasTarget.Simple lv) ->
          let v = Mem.find lv mem in
          let v' = Val.prune_ne_zero v in
          update_mem_in_prune lv v' astate
      | Some (AliasTarget.Empty lv) ->
          let v = Mem.find lv mem in
          let v' = Val.prune_length_eq_zero v in
          update_mem_in_prune lv v' astate
      | Some (AliasTarget.Nullity lv) ->
          let v = Mem.find lv mem in
          let v' = Val.prune_eq_zero v in
          update_mem_in_prune lv v' astate
      | None ->
          astate )
    | Exp.UnOp (Unop.LNot, Exp.Var x, _) -> (
      match Mem.find_alias x mem with
      | Some (AliasTarget.Simple lv) ->
          let v = Mem.find lv mem in
          let v' = Val.prune_eq_zero v in
          update_mem_in_prune lv v' astate
      | Some (AliasTarget.Empty lv) ->
          let v = Mem.find lv mem in
          let v' = Val.prune_length_ge_one v in
          update_mem_in_prune lv v' astate
      | Some (AliasTarget.Nullity lv) ->
          let v = Mem.find lv mem in
          let itv_v = Itv.prune_comp Binop.Ge (Val.get_itv v) Itv.one in
          let v' = Val.modify_itv itv_v v in
          update_mem_in_prune lv v' astate
      | None ->
          astate )
    | _ ->
        astate


  let rec prune_binop_left : Typ.IntegerWidths.t -> Exp.t -> t -> t =
   fun integer_type_widths e ({mem} as astate) ->
    match e with
    | Exp.BinOp (comp, Exp.Cast (_, e1), e2) ->
        prune_binop_left integer_type_widths (Exp.BinOp (comp, e1, e2)) astate
    | Exp.BinOp ((Binop.Lt as comp), Exp.Var x, e')
    | Exp.BinOp ((Binop.Gt as comp), Exp.Var x, e')
    | Exp.BinOp ((Binop.Le as comp), Exp.Var x, e')
    | Exp.BinOp ((Binop.Ge as comp), Exp.Var x, e') -> (
      match Mem.find_simple_alias x mem with
      | Some lv ->
          let v = Mem.find lv mem in
          let v' = Val.prune_comp comp v (eval integer_type_widths e' mem) in
          update_mem_in_prune lv v' astate
      | None ->
          astate )
    | Exp.BinOp (Binop.Eq, Exp.Var x, e') -> (
      match Mem.find_simple_alias x mem with
      | Some lv ->
          let v = Mem.find lv mem in
          let v' = Val.prune_eq v (eval integer_type_widths e' mem) in
          update_mem_in_prune lv v' astate
      | None ->
          astate )
    | Exp.BinOp (Binop.Ne, Exp.Var x, e') -> (
      match Mem.find_simple_alias x mem with
      | Some lv ->
          let v = Mem.find lv mem in
          let v' = Val.prune_ne v (eval integer_type_widths e' mem) in
          update_mem_in_prune lv v' astate
      | None ->
          astate )
    | Exp.BinOp
        ( ((Binop.Lt | Binop.Gt | Binop.Le | Binop.Ge | Binop.Eq | Binop.Ne) as comp)
        , Exp.BinOp (Binop.PlusA t, e1, e2)
        , e3 ) ->
        (* NOTE: The transposition may introduce a new integer overflow or hide an existing one.
           Be careful when you take into account integer overflows in the abstract semantics [eval]
           in the future. *)
        astate
        |> prune_binop_left integer_type_widths
             (Exp.BinOp (comp, e1, Exp.BinOp (Binop.MinusA t, e3, e2)))
        |> prune_binop_left integer_type_widths
             (Exp.BinOp (comp, e2, Exp.BinOp (Binop.MinusA t, e3, e1)))
    | Exp.BinOp
        ( ((Binop.Lt | Binop.Gt | Binop.Le | Binop.Ge | Binop.Eq | Binop.Ne) as comp)
        , Exp.BinOp (Binop.MinusA t, e1, e2)
        , e3 ) ->
        astate
        |> prune_binop_left integer_type_widths
             (Exp.BinOp (comp, e1, Exp.BinOp (Binop.PlusA t, e3, e2)))
        |> prune_binop_left integer_type_widths
             (Exp.BinOp (comp_rev comp, e2, Exp.BinOp (Binop.MinusA t, e1, e3)))
    | _ ->
        astate


  let prune_binop_right : Typ.IntegerWidths.t -> Exp.t -> t -> t =
   fun integer_type_widths e astate ->
    match e with
    | Exp.BinOp (((Binop.Lt | Binop.Gt | Binop.Le | Binop.Ge | Binop.Eq | Binop.Ne) as c), e1, e2)
      ->
        prune_binop_left integer_type_widths (Exp.BinOp (comp_rev c, e2, e1)) astate
    | _ ->
        astate


  let is_unreachable_constant : Typ.IntegerWidths.t -> Exp.t -> Mem.t -> bool =
   fun integer_type_widths e m ->
    let v = eval integer_type_widths e m in
    Itv.( <= ) ~lhs:(Val.get_itv v) ~rhs:(Itv.of_int 0)
    && PowLoc.is_bot (Val.get_pow_loc v)
    && ArrayBlk.is_bot (Val.get_array_blk v)


  let prune_unreachable : Typ.IntegerWidths.t -> Exp.t -> t -> t =
   fun integer_type_widths e ({mem} as astate) ->
    if is_unreachable_constant integer_type_widths e mem || Mem.is_relation_unsat mem then
      {astate with mem= Mem.bot}
    else astate


  let rec prune_helper integer_type_widths e astate =
    let astate =
      astate
      |> prune_unreachable integer_type_widths e
      |> prune_unop e
      |> prune_binop_left integer_type_widths e
      |> prune_binop_right integer_type_widths e
    in
    let is_const_zero x =
      match Exp.ignore_integer_cast x with
      | Exp.Const (Const.Cint i) ->
          IntLit.iszero i
      | _ ->
          false
    in
    match e with
    | Exp.BinOp (Binop.Ne, e1, e2) when is_const_zero e2 ->
        prune_helper integer_type_widths e1 astate
    | Exp.BinOp (Binop.Eq, e1, e2) when is_const_zero e2 ->
        prune_helper integer_type_widths (Exp.UnOp (Unop.LNot, e1, None)) astate
    | Exp.UnOp (Unop.Neg, Exp.Var x, _) ->
        prune_helper integer_type_widths (Exp.Var x) astate
    | Exp.BinOp (Binop.LAnd, e1, e2) ->
        astate |> prune_helper integer_type_widths e1 |> prune_helper integer_type_widths e2
    | Exp.UnOp (Unop.LNot, Exp.BinOp (Binop.LOr, e1, e2), t) ->
        astate
        |> prune_helper integer_type_widths (Exp.UnOp (Unop.LNot, e1, t))
        |> prune_helper integer_type_widths (Exp.UnOp (Unop.LNot, e2, t))
    | Exp.UnOp (Unop.LNot, Exp.BinOp ((Binop.Lt as c), e1, e2), _)
    | Exp.UnOp (Unop.LNot, Exp.BinOp ((Binop.Gt as c), e1, e2), _)
    | Exp.UnOp (Unop.LNot, Exp.BinOp ((Binop.Le as c), e1, e2), _)
    | Exp.UnOp (Unop.LNot, Exp.BinOp ((Binop.Ge as c), e1, e2), _)
    | Exp.UnOp (Unop.LNot, Exp.BinOp ((Binop.Eq as c), e1, e2), _)
    | Exp.UnOp (Unop.LNot, Exp.BinOp ((Binop.Ne as c), e1, e2), _) ->
        prune_helper integer_type_widths (Exp.BinOp (comp_not c, e1, e2)) astate
    | _ ->
        astate


  let prune : Typ.IntegerWidths.t -> Exp.t -> Mem.t -> Mem.t =
   fun integer_type_widths e mem ->
    let mem = Mem.apply_latest_prune e mem in
    let mem =
      let constrs = Relation.Constraints.of_exp e ~get_sym_f:(get_sym_f integer_type_widths mem) in
      Mem.meet_constraints constrs mem
    in
    let {mem; prune_pairs} =
      prune_helper integer_type_widths e {mem; prune_pairs= PrunePairs.empty}
    in
    Mem.set_prune_pairs prune_pairs mem
end

let get_matching_pairs :
       Tenv.t
    -> Typ.IntegerWidths.t
    -> Val.t
    -> Val.t
    -> Exp.t option
    -> Typ.t
    -> Mem.t
    -> Mem.t
    -> (Relation.Var.t * Relation.SymExp.t option) list =
 fun tenv integer_type_widths callee_v actual actual_exp_opt typ caller_mem callee_exit_mem ->
  let get_offset_sym v = Val.get_offset_sym v in
  let get_size_sym v = Val.get_size_sym v in
  let get_field_name (fn, _, _) = fn in
  let append_field v fn = PowLoc.append_field (Val.get_all_locs v) ~fn in
  let deref_field v fn mem = Mem.find_set (append_field v fn) mem in
  let deref_ptr v mem =
    let array_locs = Val.get_array_locs v in
    let locs = if PowLoc.is_empty array_locs then Val.get_pow_loc v else array_locs in
    Mem.find_set locs mem
  in
  let add_pair_sym_main_value v1 v2 ~e2_opt l =
    Option.value_map (Val.get_sym_var v1) ~default:l ~f:(fun var ->
        let sym_exp_opt =
          Option.first_some
            (Relation.SymExp.of_exp_opt
               ~get_sym_f:(get_sym_f integer_type_widths caller_mem)
               e2_opt)
            (Relation.SymExp.of_sym (Val.get_sym v2))
        in
        (var, sym_exp_opt) :: l )
  in
  let add_pair_sym s1 s2 l =
    Option.value_map (Relation.Sym.get_var s1) ~default:l ~f:(fun var ->
        (var, Relation.SymExp.of_sym s2) :: l )
  in
  let add_pair_val v1 v2 ~e2_opt rel_pairs =
    rel_pairs
    |> add_pair_sym_main_value v1 v2 ~e2_opt
    |> add_pair_sym (get_offset_sym v1) (get_offset_sym v2)
    |> add_pair_sym (get_size_sym v1) (get_size_sym v2)
  in
  let add_pair_field v1 v2 pairs fn =
    let v1' = deref_field v1 fn callee_exit_mem in
    let v2' = deref_field v2 fn caller_mem in
    add_pair_val v1' v2' ~e2_opt:None pairs
  in
  let add_pair_ptr typ v1 v2 pairs =
    match typ.Typ.desc with
    | Typ.Tptr ({desc= Tstruct typename}, _) -> (
      match Tenv.lookup tenv typename with
      | Some str ->
          let fns = List.map ~f:get_field_name str.Typ.Struct.fields in
          List.fold ~f:(add_pair_field v1 v2) ~init:pairs fns
      | _ ->
          pairs )
    | Typ.Tptr (_, _) ->
        let v1' = deref_ptr v1 callee_exit_mem in
        let v2' = deref_ptr v2 caller_mem in
        add_pair_val v1' v2' ~e2_opt:None pairs
    | _ ->
        pairs
  in
  [] |> add_pair_val callee_v actual ~e2_opt:actual_exp_opt |> add_pair_ptr typ callee_v actual


let subst_map_of_rel_pairs :
    (Relation.Var.t * Relation.SymExp.t option) list -> Relation.SubstMap.t =
 fun pairs ->
  let add_pair rel_map (x, e) = Relation.SubstMap.add x e rel_map in
  List.fold pairs ~init:Relation.SubstMap.empty ~f:add_pair


let rec list_fold2_def :
       default:Val.t * Exp.t option
    -> f:('a -> Val.t * Exp.t option -> 'b -> 'b)
    -> 'a list
    -> (Val.t * Exp.t option) list
    -> init:'b
    -> 'b =
 fun ~default ~f xs ys ~init:acc ->
  match (xs, ys) with
  | [], _ ->
      acc
  | x :: xs', [] ->
      list_fold2_def ~default ~f xs' ys ~init:(f x default acc)
  | [x], _ :: _ ->
      let v = List.fold ys ~init:Val.bot ~f:(fun acc (y, _) -> Val.join y acc) in
      let exp_opt = match ys with [(_, exp_opt)] -> exp_opt | _ -> None in
      f x (v, exp_opt) acc
  | x :: xs', y :: ys' ->
      list_fold2_def ~default ~f xs' ys' ~init:(f x y acc)


let get_subst_map :
       Tenv.t
    -> Typ.IntegerWidths.t
    -> Procdesc.t
    -> (Exp.t * 'a) list
    -> Mem.t
    -> Mem.t
    -> Relation.SubstMap.t =
 fun tenv integer_type_widths callee_pdesc params caller_mem callee_exit_mem ->
  let add_pair (formal, typ) (actual, actual_exp) rel_l =
    let callee_v = Mem.find (Loc.of_pvar formal) callee_exit_mem in
    let new_rel_matching =
      get_matching_pairs tenv integer_type_widths callee_v actual actual_exp typ caller_mem
        callee_exit_mem
    in
    List.rev_append new_rel_matching rel_l
  in
  let formals = Procdesc.get_pvar_formals callee_pdesc in
  let actuals =
    List.map ~f:(fun (a, _) -> (eval integer_type_widths a caller_mem, Some a)) params
  in
  let rel_pairs =
    list_fold2_def ~default:(Val.Itv.top, None) ~f:add_pair formals actuals ~init:[]
  in
  subst_map_of_rel_pairs rel_pairs
