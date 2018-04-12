(*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd
module F = Format
module L = Logging

(** Forward analysis to compute uninitialized variables at each program point *)
module D =
UninitDomain.Domain
module UninitVars = AbstractDomain.FiniteSet (AccessPath)
module AliasedVars = AbstractDomain.FiniteSet (UninitDomain.VarPair)
module PrePost = AbstractDomain.Pair (D) (D)
module RecordDomain = UninitDomain.Record (UninitVars) (AliasedVars) (D)

module Summary = Summary.Make (struct
  type payload = UninitDomain.summary

  let update_payload sum (summary: Specs.summary) =
    {summary with payload= {summary.payload with uninit= Some sum}}


  let read_payload (summary: Specs.summary) = summary.payload.uninit
end)

let blacklisted_functions = [BuiltinDecl.__set_array_length]

let rec is_basic_type t =
  match t.Typ.desc with
  | Tint _ | Tfloat _ | Tvoid ->
      true
  | Tptr (t', _) ->
      is_basic_type t'
  | _ ->
      false


let is_blacklisted_function pname =
  List.exists ~f:(fun fname -> Typ.Procname.equal pname fname) blacklisted_functions


module TransferFunctions (CFG : ProcCfg.S) = struct
  module CFG = CFG
  module Domain = RecordDomain

  let report_intra ap loc summary =
    let message = F.asprintf "The value read from %a was never initialized" AccessPath.pp ap in
    let ltr = [Errlog.make_trace_element 0 loc "" []] in
    let exn =
      Exceptions.Checkers (IssueType.uninitialized_value, Localise.verbatim_desc message)
    in
    Reporting.log_error summary ~loc ~ltr exn


  type extras = FormalMap.t * Specs.summary

  let is_struct t = match t.Typ.desc with Typ.Tstruct _ -> true | _ -> false

  let get_formals call =
    match Ondemand.get_proc_desc call with
    | Some proc_desc ->
        Procdesc.get_formals proc_desc
    | _ ->
        []


  let should_report_var pdesc tenv uninit_vars ap =
    match (AccessPath.get_typ ap tenv, ap) with
    | Some typ, ((Var.ProgramVar pv, _), _) ->
        not (Pvar.is_frontend_tmp pv) && not (Procdesc.is_captured_var pdesc pv)
        && D.mem ap uninit_vars && is_basic_type typ
    | _, _ ->
        false


  let nth_formal_param callee_pname idx =
    let formals = get_formals callee_pname in
    List.nth formals idx


  let function_expects_a_pointer_as_nth_param callee_pname idx =
    match nth_formal_param callee_pname idx with Some (_, typ) -> Typ.is_pointer typ | _ -> false


  let is_struct_field_passed_by_ref call t al idx =
    is_struct t && List.length al > 0 && function_expects_a_pointer_as_nth_param call idx


  let report_on_function_params call pdesc tenv uninit_vars actuals loc extras =
    List.iteri
      ~f:(fun idx e ->
        match e with
        | HilExp.AccessExpression access_expr ->
            let (var, t), al = AccessExpression.to_access_path access_expr in
            if should_report_var pdesc tenv uninit_vars ((var, t), al) && not (Typ.is_pointer t)
               && not (is_struct_field_passed_by_ref call t al idx)
            then report_intra ((var, t), al) loc (snd extras)
            else ()
        | _ ->
            () )
      actuals


  let remove_all_fields tenv base uninit_vars =
    match base with
    | _, {Typ.desc= Tptr ({Typ.desc= Tstruct name_struct}, _)} | _, {Typ.desc= Tstruct name_struct}
          -> (
      match Tenv.lookup tenv name_struct with
      | Some {fields} ->
          List.fold
            ~f:(fun acc (fn, _, _) -> D.remove (base, [AccessPath.FieldAccess fn]) acc)
            fields ~init:uninit_vars
      | _ ->
          uninit_vars )
    | _ ->
        uninit_vars


  let remove_init_fields base formal_var uninit_vars init_fields =
    let subst_formal_actual_fields initialized_fields =
      D.map
        (fun ((v, t), a) ->
          let v' = if Var.equal v formal_var then fst base else v in
          let t' =
            match t.desc with
            | Typ.Tptr ({Typ.desc= Tstruct n}, _) ->
                (* a pointer to struct needs to be changed into struct
                       as the actual is just type struct and it would make it
                       equality fail. Not sure why the actual are type struct when
                      passed by reference *)
                {t with Typ.desc= Tstruct n}
            | _ ->
                t
          in
          ((v', t'), a) )
        initialized_fields
    in
    match base with
    | _, {Typ.desc= Tptr ({Typ.desc= Tstruct _}, _)} | _, {Typ.desc= Tstruct _} ->
        D.diff uninit_vars (subst_formal_actual_fields init_fields)
    | _ ->
        uninit_vars


  let is_dummy_constructor_of_a_struct call =
    let is_dummy_constructor_of_struct =
      match get_formals call with
      | [(_, {Typ.desc= Typ.Tptr ({Typ.desc= Tstruct _}, _)})] ->
          true
      | _ ->
          false
    in
    Typ.Procname.is_constructor call && is_dummy_constructor_of_struct


  let is_pointer_assignment tenv lhs rhs =
    HilExp.is_null_literal rhs
    (* the rhs has type int when assigning the lhs to null *)
    || Option.equal Typ.equal (AccessPath.get_typ lhs tenv) (HilExp.get_typ tenv rhs)
       && Typ.is_pointer (snd (fst lhs))


  (* checks that the set of initialized formal parameters defined in the precondition of
   the function (init_formal_params) contains the (base of) nth formal parameter of the function  *)
  let init_nth_actual_param callee_pname idx init_formal_params =
    match nth_formal_param callee_pname idx with
    | None ->
        None
    | Some (fparam, t) ->
        let var_fparam = Var.of_pvar (Pvar.mk fparam callee_pname) in
        if D.exists
             (fun (base, _) -> AccessPath.equal_base base (var_fparam, t))
             init_formal_params
        then Some var_fparam
        else None


  let remove_initialized_params pdesc call acc idx (base, al) remove_fields =
    match Summary.read_summary pdesc call with
    | Some {pre= initialized_formal_params; post= _} -> (
      match init_nth_actual_param call idx initialized_formal_params with
      | Some nth_formal ->
          let acc' = D.remove (base, al) acc in
          if remove_fields then remove_init_fields base nth_formal acc' initialized_formal_params
          else acc'
      | _ ->
          acc )
    | _ ->
        acc


  (* true if a function initializes at least a param or a field of a struct param *)
  let function_initializes_some_formal_params pdesc call =
    match Summary.read_summary pdesc call with
    | Some {pre= initialized_formal_params; post= _} ->
        not (D.is_empty initialized_formal_params)
    | _ ->
        false


  let exec_instr (astate: Domain.astate) {ProcData.pdesc; ProcData.extras; ProcData.tenv} _
      (instr: HilInstr.t) =
    let update_prepost (((_, lhs_typ), apl) as lhs_ap) rhs =
      if FormalMap.is_formal (fst lhs_ap) (fst extras) && Typ.is_pointer lhs_typ
         && (not (is_pointer_assignment tenv lhs_ap rhs) || List.length apl > 0)
      then
        let pre' = D.add lhs_ap (fst astate.prepost) in
        let post = snd astate.prepost in
        (pre', post)
      else astate.prepost
    in
    match instr with
    | Assign (lhs_access_expr, (HilExp.AccessExpression rhs_access_expr as rhs_expr), loc) ->
        let ((lhs_var, lhs_typ), apl) as lhs_ap =
          AccessExpression.to_access_path lhs_access_expr
        in
        let rhs_base, al = AccessExpression.to_access_path rhs_access_expr in
        let uninit_vars' = D.remove lhs_ap astate.uninit_vars in
        let uninit_vars =
          if Int.equal (List.length apl) 0 then
            (* if we assign to the root of a struct then we need to remove all the fields *)
            remove_all_fields tenv (lhs_var, lhs_typ) uninit_vars'
          else uninit_vars'
        in
        let prepost = update_prepost lhs_ap rhs_expr in
        (* check on lhs_typ to avoid false positive when assigning a pointer to another *)
        if should_report_var pdesc tenv uninit_vars (rhs_base, al) && not (Typ.is_pointer lhs_typ)
        then report_intra (rhs_base, al) loc (snd extras) ;
        {astate with uninit_vars; prepost}
    | Assign (lhs_access_expr, rhs, _) ->
        let (lhs_ap, apl) as lhs = AccessExpression.to_access_path lhs_access_expr in
        let uninit_vars = D.remove lhs astate.uninit_vars in
        let prepost = update_prepost (lhs_ap, apl) rhs in
        {astate with uninit_vars; prepost}
    | Call (_, Direct callee_pname, _, _, _)
      when Typ.Procname.equal callee_pname BuiltinDecl.objc_cpp_throw ->
        {astate with uninit_vars= D.empty}
    | Call (_, HilInstr.Direct call, [(HilExp.AccessExpression AddressOf Base base)], _, _)
      when is_dummy_constructor_of_a_struct call ->
        (* if it's a default constructor, we use the following heuristic: we assume that it initializes
    correctly all fields when there is an implementation of the constructor that initilizes at least one
    field. If there is no explicit implementation we cannot assume fields are initialized *)
        if function_initializes_some_formal_params pdesc call then
          let uninit_vars' =
            (* in HIL/SIL the default constructor has only one param: the struct *)
            remove_all_fields tenv base astate.uninit_vars
          in
          {astate with uninit_vars= uninit_vars'}
        else astate
    | Call (_, HilInstr.Direct call, actuals, _, loc) ->
        (* in case of intraprocedural only analysis we assume that parameters passed by reference
           to a function will be initialized inside that function *)
        let uninit_vars =
          List.foldi
            ~f:(fun idx acc actual_exp ->
              match actual_exp with
              | HilExp.AccessExpression access_expr -> (
                match AccessExpression.to_access_path access_expr with
                | ((_, {Typ.desc= Tarray _}) as base), al when is_blacklisted_function call ->
                    D.remove (base, al) acc
                | ((_, t) as base), al when is_struct_field_passed_by_ref call t al idx ->
                    (* Access to a field of a struct by reference *)
                    if Config.uninit_interproc then
                      remove_initialized_params pdesc call acc idx (base, al) false
                    else D.remove (base, al) acc
                | ap when Typ.Procname.is_constructor call ->
                    remove_all_fields tenv (fst ap) (D.remove ap acc)
                | ((_, {Typ.desc= Tptr _}) as base), al ->
                    if Config.uninit_interproc then
                      remove_initialized_params pdesc call acc idx (base, al) true
                    else
                      let acc' = D.remove (base, al) acc in
                      remove_all_fields tenv base acc'
                | _ ->
                    acc )
              | HilExp.Closure (_, apl) ->
                  (* remove the captured variables of a block/lambda *)
                  List.fold ~f:(fun acc' (base, _) -> D.remove (base, []) acc') ~init:acc apl
              | _ ->
                  acc )
            ~init:astate.uninit_vars actuals
        in
        report_on_function_params call pdesc tenv uninit_vars actuals loc extras ;
        {astate with uninit_vars}
    | Call _ | Assume _ ->
        astate
end

module CFG = ProcCfg.NormalOneInstrPerNode
module Analyzer =
  AbstractInterpreter.Make (CFG) (LowerHil.Make (TransferFunctions) (LowerHil.DefaultConfig))

let get_locals cfg tenv pdesc =
  List.fold
    ~f:(fun acc (var_data: ProcAttributes.var_data) ->
      let pvar = Pvar.mk var_data.name (Procdesc.get_proc_name pdesc) in
      let base_ap = ((Var.of_pvar pvar, var_data.typ), []) in
      match var_data.typ.Typ.desc with
      | Typ.Tstruct qual_name -> (
        match Tenv.lookup tenv qual_name with
        | Some {fields} ->
            let flist =
              List.fold
                ~f:(fun acc' (fn, _, _) -> (fst base_ap, [AccessPath.FieldAccess fn]) :: acc')
                ~init:acc fields
            in
            base_ap :: flist
            (* for struct we take the struct address, and the access_path
                                    to the fields one level down *)
        | _ ->
            acc )
      | Typ.Tarray {elt} ->
          (fst base_ap, [AccessPath.ArrayAccess (elt, [])]) :: acc
      | _ ->
          base_ap :: acc )
    ~init:[] (Procdesc.get_locals cfg)


let checker {Callbacks.tenv; summary; proc_desc} : Specs.summary =
  let cfg = CFG.from_pdesc proc_desc in
  (* start with empty set of uninit local vars and  empty set of init formal params *)
  let formal_map = FormalMap.make proc_desc in
  let uninit_vars = get_locals cfg tenv proc_desc in
  let init =
    ( { RecordDomain.uninit_vars= UninitVars.of_list uninit_vars
      ; RecordDomain.aliased_vars= AliasedVars.empty
      ; RecordDomain.prepost= (D.empty, D.empty) }
    , IdAccessPathMapDomain.empty )
  in
  let invariant_map =
    Analyzer.exec_cfg cfg
      (ProcData.make proc_desc tenv (formal_map, summary))
      ~initial:init ~debug:false
  in
  match Analyzer.extract_post (CFG.id (CFG.exit_node cfg)) invariant_map with
  | Some
      ( {RecordDomain.uninit_vars= _; RecordDomain.aliased_vars= _; RecordDomain.prepost= pre, post}
      , _ ) ->
      Summary.update_summary {pre; post} summary
  | None ->
      if Procdesc.Node.get_succs (Procdesc.get_start_node proc_desc) <> [] then (
        L.internal_error "Uninit analyzer failed to compute post for %a" Typ.Procname.pp
          (Procdesc.get_proc_name proc_desc) ;
        summary )
      else summary
