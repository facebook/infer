(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module FuncArg = ProcnameDispatcher.Call.FuncArg
module IRAttributes = Attributes
open PulseBasicInterface

module PvarSpecialization = struct
  let try_keep_original ~default orig new_ ~f = if phys_equal orig new_ then default else f new_

  let try_keep_original2 ~default orig1 new1 orig2 new2 ~f =
    if phys_equal orig1 new1 && phys_equal orig2 new2 then default else f new1 new2


  let exec_pvar pname pvar = Pvar.specialize_pvar pvar pname

  let exec_var pname var =
    let open Var in
    match var with
    | LogicalVar _ ->
        var
    | ProgramVar pvar ->
        try_keep_original ~default:var pvar (exec_pvar pname pvar) ~f:of_pvar


  let rec exec_exp pname e =
    let open Exp in
    match e with
    | Var _ | Const _ ->
        e
    | UnOp (unop, e1, typ) ->
        try_keep_original ~default:e e1 (exec_exp pname e1) ~f:(fun e1' -> UnOp (unop, e1', typ))
    | BinOp (binop, e1, e2) ->
        try_keep_original2 ~default:e e1 (exec_exp pname e1) e2 (exec_exp pname e2)
          ~f:(fun e1' e2' -> BinOp (binop, e1', e2'))
    | Exn e1 ->
        try_keep_original ~default:e e1 (exec_exp pname e1) ~f:(fun e1' -> Exn e1')
    | Closure {name; captured_vars} ->
        let updated = ref false in
        let captured_vars =
          List.map captured_vars ~f:(fun ((e, pvar, typ, captured_mode) as captured_var) ->
              try_keep_original2 ~default:captured_var e (exec_exp pname e) pvar
                (exec_pvar pname pvar) ~f:(fun e' pvar' ->
                  updated := true ;
                  (e', pvar', typ, captured_mode) ) )
        in
        if !updated then Closure {name; captured_vars} else e
    | Cast (typ, e1) ->
        try_keep_original ~default:e e1 (exec_exp pname e1) ~f:(fun e1' -> Cast (typ, e1'))
    | Lvar pvar ->
        try_keep_original ~default:e pvar (exec_pvar pname pvar) ~f:(fun pvar' -> Lvar pvar')
    | Lfield (e1, fn, typ) ->
        try_keep_original ~default:e e1 (exec_exp pname e1) ~f:(fun e1' -> Lfield (e1', fn, typ))
    | Lindex (e1, e2) ->
        try_keep_original2 ~default:e e1 (exec_exp pname e1) e2 (exec_exp pname e2)
          ~f:(fun e1' e2' -> Lindex (e1', e2'))
    | Sizeof {typ; nbytes; dynamic_length; subtype} ->
        Option.value_map dynamic_length ~default:e ~f:(fun dynamic_length ->
            try_keep_original ~default:e dynamic_length (exec_exp pname dynamic_length)
              ~f:(fun dynamic_length' ->
                Sizeof {typ; nbytes; dynamic_length= Some dynamic_length'; subtype} ) )


  let exec_metadata pname metadata =
    let open Sil in
    match metadata with
    | Abstract _ | CatchEntry _ | EndBranches | Skip | TryEntry _ | TryExit _ ->
        metadata
    | ExitScope (vars, loc) ->
        let updated = ref false in
        let vars' =
          List.map vars ~f:(fun var ->
              try_keep_original ~default:var var (exec_var pname var) ~f:(fun var' ->
                  updated := true ;
                  var' ) )
        in
        if !updated then ExitScope (vars', loc) else metadata
    | Nullify (pvar, loc) ->
        try_keep_original ~default:metadata pvar (exec_pvar pname pvar) ~f:(fun pvar' ->
            Nullify (pvar', loc) )
    | VariableLifetimeBegins (pvar, typ, loc) ->
        try_keep_original ~default:metadata pvar (exec_pvar pname pvar) ~f:(fun pvar' ->
            VariableLifetimeBegins (pvar', typ, loc) )


  let exec_args pname args =
    let updated = ref false in
    let args' =
      List.map args ~f:(fun ((exp, typ) as exp_typ) ->
          let exp' = exec_exp pname exp in
          try_keep_original ~default:exp_typ exp exp' ~f:(fun exp' ->
              updated := true ;
              (exp', typ) ) )
    in
    if !updated then args' else args


  let exec_instr pname instr =
    let open Sil in
    match instr with
    | Load {id; e; root_typ; loc} ->
        try_keep_original ~default:instr e (exec_exp pname e) ~f:(fun e' ->
            Load {id; e= e'; root_typ; typ= root_typ; loc} )
    | Store {e1; root_typ; typ; e2; loc} ->
        try_keep_original2 ~default:instr e1 (exec_exp pname e1) e2 (exec_exp pname e2)
          ~f:(fun e1' e2' -> Store {e1= e1'; root_typ; typ; e2= e2'; loc})
    | Call (ret_id_typ, call_exp, args, loc, call_flags) ->
        try_keep_original2 ~default:instr call_exp (exec_exp pname call_exp) args
          (exec_args pname args) ~f:(fun converted_call_exp converted_args ->
            Call (ret_id_typ, converted_call_exp, converted_args, loc, call_flags) )
    | Prune (origin_exp, loc, is_true_branch, if_kind) ->
        try_keep_original ~default:instr origin_exp (exec_exp pname origin_exp)
          ~f:(fun converted_exp -> Prune (converted_exp, loc, is_true_branch, if_kind))
    | Metadata metadata ->
        try_keep_original ~default:instr metadata (exec_metadata pname metadata)
          ~f:(fun metadata' -> Metadata metadata')
end

let create_specialized_procdesc callee_pname callee_pdesc aliases =
  if List.is_empty aliases then None
  else
    let specialized_pname =
      let mangled_aliases = List.map aliases ~f:(fun alias -> List.map alias ~f:Pvar.get_name) in
      Procname.with_aliasing_parameters callee_pname mangled_aliases
    in
    let aliases =
      List.map aliases ~f:(fun alias ->
          List.map alias ~f:(fun pvar -> Pvar.specialize_pvar pvar specialized_pname) )
    in
    let new_attributes =
      let attributes = Procdesc.get_attributes callee_pdesc in
      let specialized_with_closures_info =
        (* because we relocalized the pvars in the instructions we need to relocalize
           those in the attributes as well to ensure they still match *)
        match attributes.specialized_with_closures_info with
        | None ->
            None
        | Some specialized_with_closures_info ->
            Some
              { specialized_with_closures_info with
                formals_to_closures=
                  Pvar.Map.fold
                    (fun pvar passed_closure map ->
                      Pvar.Map.add (Pvar.specialize_pvar pvar specialized_pname) passed_closure map
                      )
                    specialized_with_closures_info.formals_to_closures Pvar.Map.empty }
      in
      ProcAttributes.
        { attributes with
          specialized_with_aliasing_info= Some {orig_proc= callee_pname; aliases}
        ; specialized_with_closures_info
        ; proc_name= specialized_pname
        ; captured=
            List.map attributes.captured ~f:(fun (CapturedVar.{pvar} as captured_var) ->
                {captured_var with pvar= Pvar.specialize_pvar pvar specialized_pname} ) }
    in
    let specialized_pdesc = Procdesc.from_proc_attributes new_attributes in
    Procdesc.deep_copy_code_from_pdesc ~orig_pdesc:callee_pdesc ~dest_pdesc:specialized_pdesc ;
    let has_changed =
      Procdesc.replace_instrs specialized_pdesc ~f:(fun _node instr ->
          PvarSpecialization.exec_instr specialized_pname instr )
    in
    if has_changed then (
      IRAttributes.store ~proc_desc:(Some specialized_pdesc) new_attributes ;
      Some specialized_pname )
    else None


let make_formals_set callee_pname actual_args actual_captured_vars (attributes : ProcAttributes.t) =
  let open IOption.Let_syntax in
  let make_addr_to_formals_map actuals pvar_formals =
    match
      List.fold2 actuals pvar_formals ~init:AbstractValue.Map.empty ~f:(fun map (addr, _) formal ->
          match AbstractValue.Map.find_opt addr map with
          | None ->
              AbstractValue.Map.add addr [formal] map
          | Some formals ->
              AbstractValue.Map.add addr (formal :: formals) map )
    with
    | Unequal_lengths ->
        None
    | Ok map ->
        Some map
  in
  let* arg_addr_to_formals =
    let pvar_formals =
      List.map attributes.formals ~f:(fun (mangled, _, _) -> Pvar.mk mangled callee_pname)
    in
    make_addr_to_formals_map actual_args pvar_formals
  in
  let+ captured_addr_to_formals =
    let pvar_captured = List.map attributes.captured ~f:(fun CapturedVar.{pvar} -> pvar) in
    make_addr_to_formals_map actual_captured_vars pvar_captured
  in
  let addr_to_formals_map =
    AbstractValue.Map.merge
      (fun _ arg_formals captured_formals ->
        match (arg_formals, captured_formals) with
        | None, formals | formals, None ->
            formals
        | Some arg_formals, Some captured_formals ->
            Some (arg_formals @ captured_formals) )
      arg_addr_to_formals captured_addr_to_formals
  in
  AbstractValue.Map.fold
    (fun _ formals set -> match formals with [] | [_] -> set | formals -> formals :: set)
    addr_to_formals_map []


let get_actual_captured_vars callee_pname call_kind captured func_args path call_loc astate =
  let captured_formals =
    List.map captured ~f:(fun CapturedVar.{pvar; typ; capture_mode} ->
        (Var.of_pvar pvar, capture_mode, typ) )
  in
  let actuals = List.map func_args ~f:(fun FuncArg.{arg_payload; typ} -> (arg_payload, typ)) in
  let astate_captured_vars =
    PulseOperations.get_captured_actuals callee_pname path call_loc ~captured_formals ~call_kind
      ~actuals astate
  in
  let open IOption.Let_syntax in
  let+ astate, captured_vars = PulseOperationResult.sat_ok astate_captured_vars in
  (astate, List.map captured_vars ~f:fst)


let make_specialized_call_exp callee_pname func_args call_kind path call_loc astate =
  (*
     Create an identical copy of the callee except for its attributes which will indicate
     which captured variables and arguments are aliasing each other. This information can
     then be used when creating the pre of the callee to enforce aliasing variables to
     share the same memory.

     NOTE: the pvars in the copy are relocalized to belong to the copy when they belonged
     to the original callee
  *)
  let open IOption.Let_syntax in
  let* callee_pdesc = Procdesc.load callee_pname in
  let attributes = Procdesc.get_attributes callee_pdesc in
  let* astate, actual_captured_vars =
    get_actual_captured_vars callee_pname call_kind attributes.captured func_args path call_loc
      astate
  in
  let* formals_set =
    let actual_args = List.map func_args ~f:(fun FuncArg.{arg_payload} -> arg_payload) in
    make_formals_set callee_pname actual_args actual_captured_vars attributes
  in
  let+ specialized_pname = create_specialized_procdesc callee_pname callee_pdesc formals_set in
  let call_exp =
    match call_kind with
    | `Closure captured_vars ->
        let captured_vars =
          List.map captured_vars ~f:(fun (exp, pvar, typ, capture_mode) ->
              (exp, Pvar.specialize_pvar pvar specialized_pname, typ, capture_mode) )
        in
        Exp.Closure {name= specialized_pname; captured_vars}
    | _ ->
        Exp.Const (Cfun specialized_pname)
  in
  (specialized_pname, call_exp, astate)
