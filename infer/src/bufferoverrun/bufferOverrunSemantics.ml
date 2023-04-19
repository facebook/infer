(*
 * Copyright (c) 2016-present, Programming Research Laboratory (ROPAS)
 *                             Seoul National University, Korea
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open AbsLoc
open! AbstractDomain.Types
open BufferOverrunDomain
module BoField = BufferOverrunField
module L = Logging
module TraceSet = BufferOverrunTrace.Set

let eval_const : IntegerWidths.t -> Const.t -> Val.t =
 fun integer_type_widths -> function
  | Const.Cint intlit ->
      Val.of_big_int (IntLit.to_big_int intlit)
  | Const.Cstr s ->
      Val.of_literal_string integer_type_widths s
  | Const.Cfun _ ->
      (* Const.Cfun represents the address of a function call.
         For now, return just non-null unknown pointer.
         TODO: set func_ptrs field to be a singleton set representing the function. To do that,
         FuncPtr would have to be changed to store functions represented by Procname. Currently,
         FuncPtr only stores functions represented as symbolic paths and closures.*)
      {Val.unknown with itv= Itv.one}
  | _ ->
      Val.Itv.top


let rec must_alias : Exp.t -> Exp.t -> Mem.t -> bool =
 fun e1 e2 m ->
  match (e1, e2) with
  | Exp.Var x1, Exp.Var x2 ->
      let same_alias rhs1 tgt1 rhs2 tgt2 =
        KeyRhs.equal rhs1 rhs2 && AliasTarget.equal tgt1 tgt2 && not (Mem.is_rep_multi_loc rhs1 m)
      in
      AliasTargets.exists2 same_alias (Mem.find_alias_id x1 m) (Mem.find_alias_id x2 m)
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
      must_alias e1 e2 m && Fieldname.equal fld1 fld2
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
  | Typ.{desc= Tptr ({desc= Tint ikind}, Pk_pointer)} ->
      let width = IntegerWidths.width_of_ikind integer_type_widths ikind in
      Val.set_array_stride (Z.of_int (width / 8)) v
  | _ ->
      v


let rec eval : IntegerWidths.t -> Exp.t -> Mem.t -> Val.t =
 fun integer_type_widths exp mem ->
  if (not (Language.curr_language_is Java)) && must_alias_cmp exp mem then Val.Itv.zero
  else
    match exp with
    | Exp.Var id ->
        Mem.find_stack (Var.of_id id |> Loc.of_var) mem
    | Exp.Lvar pvar ->
        let loc = Loc.of_pvar pvar in
        if Mem.is_stack_loc loc mem || Loc.is_global loc then Mem.find loc mem else Val.of_loc loc
    | Exp.UnOp (uop, e, _) ->
        eval_unop integer_type_widths uop e mem
    | Exp.BinOp (bop, e1, e2) -> (
        let v = eval_binop integer_type_widths bop e1 e2 mem in
        match bop with
        | Binop.(PlusA _ | MinusA _ | MinusPP) ->
            Val.set_itv_updated_by_addition v
        | Binop.(Mult _ | DivI | DivF | Mod | Shiftlt | Shiftrt) ->
            Val.set_itv_updated_by_multiplication v
        | Binop.(PlusPI | MinusPI | Lt | Gt | Le | Ge | Eq | Ne | LAnd | LOr | BAnd | BXor | BOr) ->
            Val.set_itv_updated_by_unknown v )
    | Exp.Const c ->
        eval_const integer_type_widths c
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
             x.f[n] in the concrete semantics. *)
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


and eval_unop : IntegerWidths.t -> Unop.t -> Exp.t -> Mem.t -> Val.t =
 fun integer_type_widths unop e mem ->
  let v = eval integer_type_widths e mem in
  match unop with Unop.Neg -> Val.neg v | Unop.BNot -> Val.unknown_bit v | Unop.LNot -> Val.lnot v


and eval_binop : IntegerWidths.t -> Binop.t -> Exp.t -> Exp.t -> Mem.t -> Val.t =
 fun integer_type_widths binop e1 e2 mem ->
  let v1 = eval integer_type_widths e1 mem in
  let v2 = eval integer_type_widths e2 mem in
  match (binop : Binop.t) with
  | PlusA _ ->
      Val.plus_a v1 v2
  | PlusPI ->
      Val.plus_pi v1 v2
  | MinusA _ ->
      Val.minus_a v1 v2
  | MinusPI ->
      Val.minus_pi v1 v2
  | MinusPP ->
      Val.minus_pp v1 v2
  | Mult _ ->
      Val.mult v1 v2
  | DivI | DivF ->
      Val.div v1 v2
  | Mod ->
      Val.mod_sem v1 v2
  | Shiftlt ->
      Val.shiftlt v1 v2
  | Shiftrt ->
      Val.shiftrt v1 v2
  | Lt ->
      Val.lt_sem v1 v2
  | Gt ->
      Val.gt_sem v1 v2
  | Le ->
      Val.le_sem v1 v2
  | Ge ->
      Val.ge_sem v1 v2
  | Eq ->
      Val.eq_sem v1 v2
  | Ne ->
      Val.ne_sem v1 v2
  | BAnd ->
      Val.band_sem v1 v2
  | BXor | BOr ->
      Val.unknown_bit v1
  | LAnd ->
      Val.land_sem v1 v2
  | LOr ->
      Val.lor_sem v1 v2


(** [eval_locs exp mem] is like [eval exp mem |> Val.get_all_locs] but takes some shortcuts to avoid
    computing useless and/or problematic intermediate values *)
let rec eval_locs : Exp.t -> Mem.t -> PowLoc.t =
 fun exp mem ->
  match exp with
  | Const (Cstr s) ->
      PowLoc.singleton (Loc.of_allocsite (Allocsite.literal_string s))
  | Var id ->
      Mem.find_stack (Var.of_id id |> Loc.of_var) mem |> Val.get_all_locs
  | Lvar pvar ->
      let loc = Loc.of_pvar pvar in
      if Mem.is_stack_loc loc mem then Mem.find loc mem |> Val.get_all_locs
      else PowLoc.singleton loc
  | BinOp ((Binop.MinusPI | Binop.PlusPI), e, _) | Cast (_, e) ->
      eval_locs e mem
  | BinOp _ | Closure _ | Const _ | Exn _ | Sizeof _ | UnOp _ ->
      PowLoc.bot
  | Lfield (e, fn, _) ->
      eval_locs e mem |> PowLoc.append_field ~fn
  | Lindex (((Lfield _ | Lindex _) as e), _) ->
      Mem.find_set (eval_locs e mem) mem |> Val.get_all_locs
  | Lindex (e, _) ->
      if Language.curr_language_is Java then Mem.find_set (eval_locs e mem) mem |> Val.get_all_locs
      else eval_locs e mem


(* It returns the array value of the input expression.  For example,
   when "x" is a program variable, (eval_arr "x") returns array blocks
   the "x" is pointing to, on the other hand, (eval "x") returns the
   abstract location of "x". *)
let rec eval_arr : IntegerWidths.t -> Exp.t -> Mem.t -> Val.t =
 fun integer_type_widths exp mem ->
  match exp with
  | Exp.Var id ->
      let loc =
        AliasTargets.find_simple_alias (Mem.find_alias_id id mem)
        |> IOption.value_default_f ~f:(fun () -> Loc.of_id id)
      in
      Mem.find loc mem
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
      let locs = eval_arr integer_type_widths e mem |> Val.get_all_locs in
      Mem.find_set locs mem
  | Exp.Const _ | Exp.UnOp _ | Exp.Sizeof _ | Exp.Exn _ | Exp.Closure _ ->
      Val.bot


let rec is_stack_exp : Exp.t -> Mem.t -> bool =
 fun exp mem ->
  match exp with
  | Var _ ->
      true
  | Lvar pvar ->
      Mem.is_stack_loc (Loc.of_pvar pvar) mem
  | Cast (_, e) ->
      is_stack_exp e mem
  | _ ->
      false


module ParamBindings = struct
  include PrettyPrintable.MakePPMap (Mangled)

  let make formals actuals =
    let rec add_binding formals actuals acc =
      match (formals, actuals) with
      | (formal, _) :: formals', actual :: actuals' ->
          add_binding formals' actuals' (add (Pvar.get_name formal) actual acc)
      | _, _ ->
          acc
    in
    add_binding formals actuals empty
end

(* There are three modes of ondemand evaluations.

   EvalNormal: Given a symbolic value of an unknown function [Symb.SymbolPath.Callsite], it returns
   a symbolic interval value.

   EvalPOCond: Given a symbolic value of an unknown function, it returns the top interval value.
   This is used when substituting condition expressions of proof obligations.

   EvalPOReachability: This is similar to [EvalPOCond], but it returns the bottom location, instead
   of the unknown location, when a location to substitute is not found.  This is used when
   substituting reachabilities of proof obligations.

   EvalCost: This is similar to [EvalNormal], but it is designed to be used in substitutions of
   the cost results, avoiding precision loss by joining of symbolic values.  Normal join of two
   different symbolic values, [s1] and [s2], becomes top due to the limitation of our domain.  On
   the other hand, in this mode, it returns an upperbound [s1+s2] for the case, because the cost
   values only care about the upperbounds. *)
type eval_mode = EvalNormal | EvalPOCond | EvalPOReachability | EvalCost

let is_cost_mode = function EvalCost -> true | _ -> false

let eval_sympath_modeled_partial ~mode p =
  match (mode, p) with
  | (EvalNormal | EvalCost), BoField.Prim (Symb.SymbolPath.Callsite _) ->
      Itv.of_modeled_path p |> Val.of_itv
  | _, _ ->
      (* We only have modeled function calls created in costModels. *)
      assert false


let rec eval_sympath_partial ~mode params p mem =
  match p with
  | BoField.Prim (Symb.SymbolPath.Pvar x) -> (
    try ParamBindings.find (Pvar.get_name x) params
    with Caml.Not_found ->
      L.d_printfln_escaped "Symbol %a is not found in parameters." (Pvar.pp Pp.text) x ;
      Val.Itv.top )
  | BoField.Prim (Symb.SymbolPath.Callsite {ret_typ; cs; obj_path}) -> (
    match mode with
    | EvalNormal | EvalCost ->
        L.d_printfln_escaped "Symbol for %a is not expected to be in parameters." Procname.pp
          (CallSite.pname cs) ;
        let obj_path =
          Option.bind obj_path ~f:(fun obj_path ->
              eval_sympath_partial ~mode params obj_path mem
              |> Val.get_pow_loc |> PowLoc.min_elt_opt |> Option.bind ~f:Loc.get_path )
        in
        let p = Symb.SymbolPath.of_callsite ?obj_path ~ret_typ cs in
        Mem.find (Loc.of_allocsite (Allocsite.make_symbol p)) mem
    | EvalPOCond | EvalPOReachability ->
        Val.Itv.top )
  | BoField.(Field _ | StarField _ | Prim (Symb.SymbolPath.Deref _)) ->
      let locs = eval_locpath ~mode params p mem in
      Mem.find_set locs mem


(* Import a path p from a callee as a set of locations to a caller.
   There are the following cases:
   (1) p involves dereference of a formal parameter:
     Substitute the formal parameter with actual expression evaluating dereferences:
     Example: p = "**A.f1", A is a formal parameter, actual is &B, B points
       to C and D.
       Return: {"C.f1", "D.f1"}
   (2) p involves a formal parameter passed by value:
     Return unknown location.
     Example: p = A, A is a formal parameter, actual is B
     Return: {"unknown"}
     The reason why it is not possible to import A as B is that there is a mapping
     from formal locations to evaluations of actual expressions in ParamBindings and
     not to actual expression. That is, if the actual expression is A, we can get its
     value, not the location itself. This is also why we can import dereferences precisely
     (we get powloc part of such value). *)
and eval_locpath ~mode params p mem =
  let res =
    match p with
    | BoField.Prim (Symb.SymbolPath.Pvar _ | Symb.SymbolPath.Callsite _) ->
        PowLoc.unknown
    | BoField.Prim (Symb.SymbolPath.Deref (deref_kind, p_base)) ->
        let v = eval_sympath_partial ~mode params p_base mem in
        if Val.is_unknown v then
          (* The target of the pointer is unknown -> get the location representing the deref
               symbolically. *)
          let ptr_locs = eval_locpath ~mode params p_base mem in
          let create_deref ptr_loc =
            match Loc.get_path ptr_loc with
            | None ->
                Loc.unknown
            | Some path ->
                Loc.of_path (Symb.SymbolPath.deref ~deref_kind path)
          in
          PowLoc.fold
            (fun ptr_loc ptr_locs -> PowLoc.add (create_deref ptr_loc) ptr_locs)
            ptr_locs PowLoc.bot
        else Val.get_all_locs v
    | BoField.Field {fn; prefix= p} ->
        let locs = eval_locpath ~mode params p mem in
        PowLoc.append_field ~fn locs
    | BoField.StarField {last_field= fn; prefix} ->
        let locs = eval_locpath ~mode params prefix mem in
        PowLoc.append_star_field ~fn locs
  in
  if PowLoc.is_bot res then (
    match mode with
    | EvalPOReachability ->
        res
    | EvalNormal | EvalPOCond | EvalCost ->
        L.d_printfln_escaped "Location value for %a is not found." Symb.SymbolPath.pp_partial p ;
        PowLoc.unknown )
  else res


let eval_sympath ~mode params sympath mem =
  match sympath with
  | Symb.SymbolPath.Modeled p ->
      let v = eval_sympath_modeled_partial ~mode p in
      (Val.get_itv v, Val.get_traces v)
  | Symb.SymbolPath.Normal p ->
      let v = eval_sympath_partial ~mode params p mem in
      (Val.get_itv v, Val.get_traces v)
  | Symb.SymbolPath.Offset {p} ->
      let v = eval_sympath_partial ~mode params p mem in
      (ArrayBlk.get_offset ~cost_mode:(is_cost_mode mode) (Val.get_array_blk v), Val.get_traces v)
  | Symb.SymbolPath.Length {p} ->
      let v = eval_sympath_partial ~mode params p mem in
      (ArrayBlk.get_size ~cost_mode:(is_cost_mode mode) (Val.get_array_blk v), Val.get_traces v)


let mk_eval_sym_trace ?(is_args_ref = false) integer_type_widths
    (callee_formals : (Pvar.t * Typ.t) list) (actual_exps : (Exp.t * Typ.t) list)
    (captured_vars : (Exp.t * Pvar.t * Typ.t * CapturedVar.capture_mode) list) caller_mem =
  let actuals =
    if is_args_ref then
      match actual_exps with
      | [] ->
          []
      | (this, _) :: actual_exps ->
          let this_actual = eval integer_type_widths this caller_mem in
          let actuals =
            List.map actual_exps ~f:(fun (a, _) ->
                Mem.find_set (eval_locs a caller_mem) caller_mem )
          in
          this_actual :: actuals
    else
      List.map actual_exps ~f:(fun (a, _) ->
          match (a : Exp.t) with
          | Closure closure ->
              FuncPtr.Set.of_closure closure |> Val.of_func_ptrs
          | _ ->
              eval integer_type_widths a caller_mem )
  in
  let params =
    ParamBindings.make callee_formals actuals
    |> fun init ->
    List.fold captured_vars ~init ~f:(fun acc (_, pvar, _, _) ->
        let v = Mem.find (Loc.of_pvar pvar) caller_mem in
        ParamBindings.add (Pvar.get_name pvar) v acc )
  in
  let eval_sym ~mode s bound_end =
    let sympath = Symb.Symbol.path s in
    let itv, _ = eval_sympath ~mode params sympath caller_mem in
    Symb.Symbol.check_bound_end s bound_end ;
    Itv.get_bound itv bound_end
  in
  let eval_locpath ~mode partial = eval_locpath ~mode params partial caller_mem in
  let eval_func_ptrs ~mode partial =
    eval_sympath_partial ~mode params partial caller_mem |> Val.get_func_ptrs
  in
  let trace_of_sym s =
    let sympath = Symb.Symbol.path s in
    let itv, traces = eval_sympath ~mode:EvalNormal params sympath caller_mem in
    if Itv.eq itv Itv.bot then TraceSet.bottom else traces
  in
  fun ~mode ->
    { eval_sym= eval_sym ~mode
    ; eval_locpath= eval_locpath ~mode
    ; eval_func_ptrs= eval_func_ptrs ~mode
    ; trace_of_sym }


let mk_eval_sym_cost integer_type_widths callee_formals actual_exps captured_vars caller_mem =
  mk_eval_sym_trace integer_type_widths callee_formals actual_exps captured_vars caller_mem
    ~mode:EvalCost


(* This function evaluates the array length conservatively, which is useful when there are multiple
   array locations and their lengths are joined to top.  For example, if the [arr_locs] points to
   two arrays [a] and [b] and if their lengths are [a.length] and [b.length], this function
   evaluates its length as [\[0, a.length.ub + b.length.ub\]]. *)
let conservative_array_length ?traces arr_locs mem =
  let accum_add arr_loc acc = Mem.find arr_loc mem |> Val.array_sizeof |> Itv.plus acc in
  PowLoc.fold accum_add arr_locs Itv.zero
  |> Val.of_itv ?traces |> Val.get_range_of_iterator |> Val.set_itv_updated_by_addition


let eval_array_locs_length arr_locs mem =
  if PowLoc.is_bot arr_locs then Val.Itv.top
  else
    let arr = Mem.find_set arr_locs mem in
    let traces = Val.get_traces arr in
    let length = Val.get_array_blk arr |> ArrayBlk.get_size in
    match Itv.get_bound length Symb.BoundEnd.UpperBound with
    | NonBottom b when not (Bounds.Bound.is_pinf b) ->
        Val.of_itv ~traces length
    | _ ->
        conservative_array_length ~traces arr_locs mem


let eval_string_len exp mem = Mem.get_c_strlen (eval_locs exp mem) mem

module Prune = struct
  type t = {prune_pairs: PrunePairs.t; mem: Mem.t}

  let collection_length_of_iterator loc mem =
    let arr_locs =
      Mem.find loc mem |> Val.get_all_locs
      |> PowLoc.append_field ~fn:BufferOverrunField.java_collection_internal_array
    in
    eval_array_locs_length arr_locs mem


  let update_mem_in_prune lv v ?(pruning_exp = PruningExp.Unknown)
      ({prune_pairs; mem} as prune_state) =
    (* Explicitly disable prune of locations which can be only weakly updated.*)
    if AbsLoc.can_strong_update (PowLoc.singleton lv) then
      let prune_pairs = PrunePairs.add lv (PrunedVal.make v pruning_exp) prune_pairs in
      let mem = Mem.update_mem (PowLoc.singleton lv) v mem in
      {prune_pairs; mem}
    else prune_state


  let prune_has_next ~true_branch iterator ({mem} as astate) =
    let accum_prune_common ~prune_f loc acc =
      let length = collection_length_of_iterator iterator mem in
      let v = Mem.find loc mem in
      if Val.is_bot length || Val.is_bot v then acc
      else
        let v' = prune_f v length in
        update_mem_in_prune loc v' acc
    in
    let accum_pruned loc tgt acc =
      match tgt with
      | AliasTarget.IteratorSimple {i} when IntLit.(eq i zero) ->
          let prune_f = if true_branch then Val.prune_lt else Val.prune_eq in
          accum_prune_common ~prune_f loc acc
      | AliasTarget.IteratorOffset {alias_typ; i} when IntLit.(eq i zero) ->
          let prune_f v length =
            let f =
              if true_branch then Val.prune_length_lt
              else match alias_typ with Eq -> Val.prune_length_eq | Le -> Val.prune_length_le
            in
            f v (Val.get_itv length)
          in
          accum_prune_common ~prune_f loc acc
      | _ ->
          acc
    in
    AliasTargets.fold accum_pruned (Mem.find_alias_loc iterator mem) astate


  let prune_linked_list_index loc mem acc =
    let lv_linked_list_index = Loc.append_field loc BufferOverrunField.java_linked_list_index in
    Option.value_map (Mem.find_opt lv_linked_list_index mem) ~default:acc ~f:(fun index_v ->
        let linked_list_length = Loc.append_field loc BufferOverrunField.java_linked_list_length in
        Option.value_map (Loc.get_path linked_list_length) ~default:acc
          ~f:(fun linked_list_length ->
            let pruned_v =
              Val.of_itv (Itv.of_normal_path ~unsigned:true linked_list_length)
              |> Val.prune_binop Le index_v
            in
            update_mem_in_prune lv_linked_list_index pruned_v acc ) )


  let prune_iterator_offset_objc next_object mem acc =
    let tgts = Mem.find_alias_loc next_object mem in
    AliasTargets.fold
      (fun iterator tgt acc ->
        match tgt with
        | AliasTarget.IteratorNextObject _ ->
            let iterator_v = Mem.find iterator mem in
            let iterator_v' =
              { iterator_v with
                arrayblk= Val.get_array_blk iterator_v |> ArrayBlk.prune_offset_le_size }
            in
            update_mem_in_prune iterator iterator_v' acc
        | _ ->
            acc )
      tgts acc


  let prune_stack_var pruned_id prune ({mem} as astate) =
    let pruned_loc = Loc.of_id pruned_id in
    let pruned_val = Mem.find pruned_loc mem in
    let resulting_val = prune pruned_val in
    let mem = Mem.update_mem (PowLoc.singleton pruned_loc) resulting_val mem in
    (pruned_val, {astate with mem})


  let prune_unop : Exp.t -> t -> t =
   fun e ({mem} as astate) ->
    match e with
    | Exp.Var x ->
        (* (1) prune the stack variable corresponding to x *)
        let x_val, astate = prune_stack_var x Val.prune_ne_zero astate in
        (* (2) prune heap variables which are aliases of the stack variable *)
        let accum_prune_var rhs tgt acc =
          match tgt with
          | AliasTarget.Simple {i} when IntLit.iszero i ->
              (let v = Val.prune_eq x_val (Mem.find rhs mem) in
               if Val.is_bot v then acc
               else
                 let v' = Val.prune_ne_zero v in
                 let pruning_exp = PruningExp.make Binop.Ne ~lhs:v ~rhs:(Val.of_int 0) in
                 update_mem_in_prune rhs v' ~pruning_exp acc )
              |> prune_linked_list_index rhs mem
              |> prune_iterator_offset_objc rhs mem
          | AliasTarget.Empty ->
              let v = Mem.find rhs mem in
              if Val.is_bot v then acc
              else
                let v' = Val.prune_length_eq_zero v in
                update_mem_in_prune rhs v' acc
          | AliasTarget.Fgets ->
              let strlen_loc = Loc.of_c_strlen rhs in
              let v = Mem.find strlen_loc mem in
              if Val.is_bot v then acc
              else
                let v' = Val.prune_ge_one v in
                update_mem_in_prune strlen_loc v' acc
          | AliasTarget.IteratorHasNext _ ->
              prune_has_next ~true_branch:true rhs acc
          | _ ->
              acc
        in
        AliasTargets.fold accum_prune_var (Mem.find_alias_id x mem) astate
    | Exp.UnOp (Unop.LNot, Exp.Var x, _) ->
        (* (1) prune the stack variable corresponding to x *)
        let x_val, astate = prune_stack_var x Val.prune_eq_zero astate in
        (* (2) prune heap variables which are aliases of the stack variable *)
        let accum_prune_not_var rhs tgt acc =
          match tgt with
          | AliasTarget.Simple {i} when IntLit.iszero i ->
              let v = Val.prune_eq x_val (Mem.find rhs mem) in
              if Val.is_bot v then acc
              else
                let v' = Val.prune_eq_zero v in
                update_mem_in_prune rhs v' acc
          | AliasTarget.Empty ->
              let v = Mem.find rhs mem in
              if Val.is_bot v then acc
              else
                let v' = Val.prune_length_ge_one v in
                update_mem_in_prune rhs v' acc
          | AliasTarget.IteratorHasNext _ ->
              prune_has_next ~true_branch:false rhs acc
          | _ ->
              acc
        in
        AliasTargets.fold accum_prune_not_var (Mem.find_alias_id x mem) astate
    | _ ->
        astate


  let gen_prune_alias_functions ~prune_alias_core location integer_type_widths comp x e astate =
    (* [val_prune_eq] is applied when the alias type is [AliasTarget.Eq]. *)
    let val_prune_eq =
      match (comp : Binop.t) with
      | Lt | Gt | Le | Ge ->
          Val.prune_binop comp
      | Eq ->
          Val.prune_eq
      | Ne ->
          Val.prune_ne
      | _ ->
          assert false
    in
    (* [val_prune_le] is applied when the alias type is [AliasTarget.Le]. *)
    let val_prune_le =
      match (comp : Binop.t) with
      | Lt ->
          (* when [alias_target <= alias_key < e], prune [alias_target] with [alias_target < e] *)
          Some (Val.prune_binop comp)
      | Le | Eq ->
          (* when [alias_target <= alias_key = e] or [alias_target <= alias_key <= e], prune
             [alias_target] with [alias_target <= e] *)
          Some (Val.prune_binop Le)
      | Ne | Gt | Ge ->
          (* when [alias_target <= alias_key != e], [alias_target <= alias_key > e] or [alias_target
             <= alias_key >= e], no prune *)
          None
      | _ ->
          assert false
    in
    let make_pruning_exp = PruningExp.make comp in
    prune_alias_core ~val_prune_eq ~val_prune_le ~make_pruning_exp location integer_type_widths x e
      astate


  let prune_simple_alias =
    let prune_alias_core ~val_prune_eq ~val_prune_le:_ ~make_pruning_exp _location
        integer_type_widths x e ({mem} as astate) =
      let expr_val = eval integer_type_widths e mem in
      (* (1) prune the stack variable corresponding to x *)
      let x_val, astate = prune_stack_var x (fun loc_val -> val_prune_eq loc_val expr_val) astate in
      (* (2) prune the heap variable which is an simple alias of the stack variable *)
      List.fold (Mem.find_simple_alias x mem) ~init:astate ~f:(fun acc (lv, i) ->
          let lhs, rhs =
            if IntLit.iszero i then (x_val, expr_val)
            else
              let i_val = Val.of_int_lit i in
              (Val.minus_a x_val i_val, Val.minus_a expr_val i_val)
          in
          let lhs = Val.prune_eq lhs (Mem.find lv mem) in
          if Val.is_bot lhs || Val.is_bot rhs then acc
          else
            let v = val_prune_eq lhs rhs in
            let pruning_exp = make_pruning_exp ~lhs ~rhs in
            update_mem_in_prune lv v ~pruning_exp acc )
    in
    gen_prune_alias_functions ~prune_alias_core


  let prune_size_alias =
    let prune_alias_core ~val_prune_eq ~val_prune_le ~make_pruning_exp location integer_type_widths
        x e ({mem} as astate) =
      List.fold (Mem.find_size_alias x mem) ~init:astate
        ~f:(fun astate (alias_typ, lv, i, java_tmp) ->
          let array_v = Mem.find lv mem in
          let lhs =
            Val.get_array_blk array_v |> ArrayBlk.get_size
            |> Itv.plus (Itv.of_int_lit i)
            |> Val.of_itv |> Val.set_itv_updated_by_addition
          in
          let rhs = eval integer_type_widths e mem in
          if Val.is_bot lhs || Val.is_bot rhs then astate
          else
            let prune_target val_prune astate =
              let lhs' = val_prune lhs rhs in
              let array_v' =
                Val.set_array_length location ~length:(Val.minus_a lhs' (Val.of_int_lit i)) array_v
              in
              let pruning_exp = make_pruning_exp ~lhs:lhs' ~rhs in
              (update_mem_in_prune lv array_v' ~pruning_exp astate, lhs', pruning_exp)
            in
            match alias_typ with
            | AliasTarget.Eq ->
                let astate, size', pruning_exp = prune_target val_prune_eq astate in
                Option.value_map java_tmp ~default:astate ~f:(fun java_tmp ->
                    update_mem_in_prune java_tmp size' ~pruning_exp astate )
            | AliasTarget.Le ->
                let astate =
                  Option.value_map val_prune_le ~default:astate ~f:(fun val_prune_le ->
                      prune_target val_prune_le astate |> fun (astate, _, _) -> astate )
                in
                Option.value_map java_tmp ~default:astate ~f:(fun java_tmp ->
                    let v = val_prune_eq (Mem.find java_tmp mem) rhs in
                    let pruning_exp = make_pruning_exp ~lhs:v ~rhs in
                    update_mem_in_prune java_tmp v ~pruning_exp astate ) )
    in
    gen_prune_alias_functions ~prune_alias_core


  let rec prune_binop_left : Location.t -> IntegerWidths.t -> Exp.t -> t -> t =
   fun location integer_type_widths e astate ->
    match e with
    | Exp.BinOp (comp, Exp.Cast (_, e1), e2) ->
        prune_binop_left location integer_type_widths (Exp.BinOp (comp, e1, e2)) astate
    | Exp.BinOp
        (((Binop.Lt | Binop.Gt | Binop.Le | Binop.Ge | Binop.Eq | Binop.Ne) as comp), Exp.Var x, e')
      ->
        astate
        |> prune_simple_alias location integer_type_widths comp x e'
        |> prune_size_alias location integer_type_widths comp x e'
    | Exp.BinOp
        ( ((Binop.Lt | Binop.Gt | Binop.Le | Binop.Ge | Binop.Eq | Binop.Ne) as comp)
        , Exp.BinOp (Binop.PlusA t, e1, e2)
        , e3 ) ->
        (* NOTE: The transposition may introduce a new integer overflow or hide an existing one.
           Be careful when you take into account integer overflows in the abstract semantics [eval]
           in the future. *)
        astate
        |> prune_binop_left location integer_type_widths
             (Exp.BinOp (comp, e1, Exp.BinOp (Binop.MinusA t, e3, e2)))
        |> prune_binop_left location integer_type_widths
             (Exp.BinOp (comp, e2, Exp.BinOp (Binop.MinusA t, e3, e1)))
    | Exp.BinOp
        ( ((Binop.Lt | Binop.Gt | Binop.Le | Binop.Ge | Binop.Eq | Binop.Ne) as comp)
        , Exp.BinOp (Binop.MinusA t, e1, e2)
        , e3 ) ->
        astate
        |> prune_binop_left location integer_type_widths
             (Exp.BinOp (comp, e1, Exp.BinOp (Binop.PlusA t, e3, e2)))
        |> prune_binop_left location integer_type_widths
             (Exp.BinOp (comp_rev comp, e2, Exp.BinOp (Binop.MinusA t, e1, e3)))
    | _ ->
        astate


  let prune_binop_right : Location.t -> IntegerWidths.t -> Exp.t -> t -> t =
   fun location integer_type_widths e astate ->
    match e with
    | Exp.BinOp (((Binop.Lt | Binop.Gt | Binop.Le | Binop.Ge | Binop.Eq | Binop.Ne) as c), e1, e2)
      ->
        prune_binop_left location integer_type_widths (Exp.BinOp (comp_rev c, e2, e1)) astate
    | _ ->
        astate


  let prune_unreachable : IntegerWidths.t -> Exp.t -> t -> t =
   fun integer_type_widths e ({mem} as astate) ->
    match mem with
    | Mem.(Unreachable | ExcRaised) ->
        astate
    | Mem.Reachable _ ->
        let v = eval integer_type_widths e mem in
        let itv_v = Val.get_itv v in
        if
          Itv.leq ~lhs:itv_v ~rhs:(Itv.of_int 0)
          && PowLoc.is_bot (Val.get_pow_loc v)
          && ArrayBlk.is_bot (Val.get_array_blk v)
        then
          if Itv.is_bottom itv_v then
            let () =
              Logging.d_printfln_escaped "Warning: the condition expression is evaluated to bottom"
            in
            astate
          else {astate with mem= Mem.unreachable}
        else astate


  let rec prune_helper location integer_type_widths e astate =
    let astate =
      astate
      |> prune_unreachable integer_type_widths e
      |> prune_unop e
      |> prune_binop_left location integer_type_widths e
      |> prune_binop_right location integer_type_widths e
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
        prune_helper location integer_type_widths e1 astate
    | Exp.BinOp (Binop.Eq, e1, e2) when is_const_zero e2 ->
        prune_helper location integer_type_widths (Exp.UnOp (Unop.LNot, e1, None)) astate
    | Exp.UnOp (Unop.Neg, Exp.Var x, _) ->
        prune_helper location integer_type_widths (Exp.Var x) astate
    | Exp.BinOp (Binop.LAnd, e1, e2) ->
        astate
        |> prune_helper location integer_type_widths e1
        |> prune_helper location integer_type_widths e2
    | Exp.UnOp (Unop.LNot, Exp.BinOp (Binop.LOr, e1, e2), t) ->
        astate
        |> prune_helper location integer_type_widths (Exp.UnOp (Unop.LNot, e1, t))
        |> prune_helper location integer_type_widths (Exp.UnOp (Unop.LNot, e2, t))
    | Exp.UnOp (Unop.LNot, UnOp (Unop.LNot, e, _), _) ->
        prune_helper location integer_type_widths e astate
    | Exp.UnOp (Unop.LNot, Exp.BinOp ((Binop.Lt as c), e1, e2), _)
    | Exp.UnOp (Unop.LNot, Exp.BinOp ((Binop.Gt as c), e1, e2), _)
    | Exp.UnOp (Unop.LNot, Exp.BinOp ((Binop.Le as c), e1, e2), _)
    | Exp.UnOp (Unop.LNot, Exp.BinOp ((Binop.Ge as c), e1, e2), _)
    | Exp.UnOp (Unop.LNot, Exp.BinOp ((Binop.Eq as c), e1, e2), _)
    | Exp.UnOp (Unop.LNot, Exp.BinOp ((Binop.Ne as c), e1, e2), _) ->
        prune_helper location integer_type_widths (Exp.BinOp (comp_not c, e1, e2)) astate
    | Exp.Var ident -> (
      match Mem.find_cpp_iterator_alias ident astate.mem with
      | None ->
          astate
      | Some (iter_lhs, iter_rhs, binop) ->
          let iter_lhs_loc = Loc.of_pvar iter_lhs in
          let iter_lhs_v = Mem.find iter_lhs_loc astate.mem in
          let iter_rhs_loc = Loc.of_pvar iter_rhs in
          let iter_rhs_v = Mem.find iter_rhs_loc astate.mem in
          let iter_v = Val.prune_binop binop iter_lhs_v iter_rhs_v in
          update_mem_in_prune iter_lhs_loc iter_v astate )
    | _ ->
        astate


  let prune : Location.t -> IntegerWidths.t -> Exp.t -> Mem.t -> Mem.t =
   fun location integer_type_widths e mem ->
    let mem, prune_pairs = Mem.apply_latest_prune e mem in
    let {mem; prune_pairs} = prune_helper location integer_type_widths e {mem; prune_pairs} in
    if PrunePairs.is_reachable prune_pairs then Mem.set_prune_pairs prune_pairs mem
    else Mem.unreachable
end
