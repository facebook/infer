(*
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging

(** Forward analysis to compute uninitialized variables at each program point *)
module D = UninitDomain.Domain

module MaybeUninitVars = UninitDomain.MaybeUninitVars
module AliasedVars = AbstractDomain.FiniteSet (UninitDomain.VarPair)
module RecordDomain = UninitDomain.Record (MaybeUninitVars) (AliasedVars) (D)

module Payload = SummaryPayload.Make (struct
  type t = UninitDomain.Summary.t

  let update_payloads sum (payloads : Payloads.t) = {payloads with uninit= Some sum}

  let of_payloads (payloads : Payloads.t) = payloads.uninit
end)

module Models = struct
  let initializing_all_args = [BuiltinDecl.__set_array_length]

  let is_initializing_all_args pname =
    List.exists initializing_all_args ~f:(fun fname -> Typ.Procname.equal pname fname)
end

let should_report_on_type t =
  match t.Typ.desc with
  | Tptr (_, Pk_reference) ->
      false
  | Tint _ | Tfloat _ | Tvoid | Tptr _ ->
      true
  | _ ->
      false


type extras = {formals: FormalMap.t; summary: Summary.t}

module TransferFunctions (CFG : ProcCfg.S) = struct
  module CFG = CFG
  module Domain = RecordDomain

  type nonrec extras = extras

  let report_intra access_expr loc summary =
    let message =
      F.asprintf "The value read from %a was never initialized" HilExp.AccessExpression.pp
        access_expr
    in
    let ltr = [Errlog.make_trace_element 0 loc "" []] in
    Reporting.log_error summary ~loc ~ltr IssueType.uninitialized_value message


  let is_struct t = match t.Typ.desc with Typ.Tstruct _ -> true | _ -> false

  let is_array t = match t.Typ.desc with Typ.Tarray _ -> true | _ -> false

  let get_formals pname = Ondemand.get_proc_desc pname |> Option.map ~f:Procdesc.get_formals

  let should_report_var pdesc tenv maybe_uninit_vars access_expr =
    let base = HilExp.AccessExpression.get_base access_expr in
    match (HilExp.AccessExpression.get_typ access_expr tenv, base) with
    | Some typ, (Var.ProgramVar pv, _) ->
        (not (Pvar.is_frontend_tmp pv))
        && (not (Procdesc.is_captured_pvar pdesc pv))
        && MaybeUninitVars.mem access_expr maybe_uninit_vars
        && should_report_on_type typ
    | _, _ ->
        false


  let nth_formal_param callee_pname idx =
    get_formals callee_pname |> Option.bind ~f:(fun formals -> List.nth formals idx)


  let function_expects_a_pointer_as_nth_param callee_formals idx =
    match List.nth callee_formals idx with Some (_, typ) -> Typ.is_pointer typ | _ -> false


  let is_struct_field_passed_by_ref callee_formals t access_expr idx =
    is_struct t
    && (not (HilExp.AccessExpression.is_base access_expr))
    && function_expects_a_pointer_as_nth_param callee_formals idx


  let is_array_element_passed_by_ref callee_formals t access_expr idx =
    is_array t
    && (not (HilExp.AccessExpression.is_base access_expr))
    && function_expects_a_pointer_as_nth_param callee_formals idx


  let is_fld_or_array_elem_passed_by_ref t access_expr idx callee_formals =
    is_struct_field_passed_by_ref callee_formals t access_expr idx
    || is_array_element_passed_by_ref callee_formals t access_expr idx


  let report_on_function_params pdesc tenv maybe_uninit_vars actuals loc summary callee_formals_opt
      =
    List.iteri actuals ~f:(fun idx e ->
        match HilExp.ignore_cast e with
        | HilExp.AccessExpression access_expr ->
            let _, t = HilExp.AccessExpression.get_base access_expr in
            if
              should_report_var pdesc tenv maybe_uninit_vars access_expr
              && (not (Typ.is_pointer t))
              && not
                   (Option.exists callee_formals_opt ~f:(fun callee_formals ->
                        is_struct_field_passed_by_ref callee_formals t access_expr idx ))
            then report_intra access_expr loc summary
        | _ ->
            () )


  let is_dummy_constructor_of_a_struct call =
    let is_dummy_constructor_of_struct =
      match get_formals call with
      | Some [(_, {Typ.desc= Typ.Tptr ({Typ.desc= Tstruct _}, _)})] ->
          true
      | _ ->
          false
    in
    Typ.Procname.is_constructor call && is_dummy_constructor_of_struct


  let is_pointer_assignment tenv lhs rhs =
    let _, base_typ = HilExp.AccessExpression.get_base lhs in
    HilExp.is_null_literal rhs
    (* the rhs has type int when assigning the lhs to null *)
    || Option.equal Typ.equal (HilExp.AccessExpression.get_typ lhs tenv) (HilExp.get_typ tenv rhs)
       && Typ.is_pointer base_typ


  (* checks that the set of initialized formal parameters defined in the precondition of
   the function (init_formal_params) contains the (base of) nth formal parameter of the function  *)
  let init_nth_actual_param callee_pname idx init_formal_params =
    match nth_formal_param callee_pname idx with
    | None ->
        None
    | Some (fparam, t) ->
        let var_fparam = Var.of_pvar (Pvar.mk fparam callee_pname) in
        if
          D.exists
            (fun access_expr ->
              let base = HilExp.AccessExpression.get_base access_expr in
              AccessPath.equal_base base (var_fparam, t) )
            init_formal_params
        then Some var_fparam
        else None


  let remove_initialized_params pdesc call maybe_uninit_vars idx access_expr remove_fields =
    match Payload.read pdesc call with
    | Some {pre= init_formals; post= _} -> (
      match init_nth_actual_param call idx init_formals with
      | Some var_formal ->
          let maybe_uninit_vars = MaybeUninitVars.remove access_expr maybe_uninit_vars in
          if remove_fields then
            let base = HilExp.AccessExpression.get_base access_expr in
            let init_formals' = MaybeUninitVars.of_list (D.elements init_formals) in
            MaybeUninitVars.remove_init_fields base var_formal maybe_uninit_vars init_formals'
          else maybe_uninit_vars
      | _ ->
          maybe_uninit_vars )
    | _ ->
        maybe_uninit_vars


  (* true if a function initializes at least a param or a field of a struct param *)
  let function_initializes_some_formal_params pdesc call =
    match Payload.read pdesc call with
    | Some {pre= initialized_formal_params; post= _} ->
        not (D.is_empty initialized_formal_params)
    | _ ->
        false


  let exec_instr (astate : Domain.t) {ProcData.pdesc; extras= {formals; summary}; tenv} _
      (instr : HilInstr.t) =
    let check_access_expr ~loc rhs_access_expr =
      if should_report_var pdesc tenv astate.maybe_uninit_vars rhs_access_expr then
        report_intra rhs_access_expr loc summary
    in
    let rec check_hil_expr ~loc = function
      | HilExp.Cast (_, e) ->
          check_hil_expr ~loc e
      | HilExp.AccessExpression access_expr ->
          check_access_expr ~loc access_expr
      | _ ->
          ()
    in
    let update_prepost access_expr rhs =
      let lhs_base = HilExp.AccessExpression.get_base access_expr in
      if
        FormalMap.is_formal lhs_base formals
        && Typ.is_pointer (snd lhs_base)
        && ( (not (is_pointer_assignment tenv access_expr rhs))
           || not (HilExp.AccessExpression.is_base access_expr) )
      then
        let pre = D.add access_expr astate.prepost.UninitDomain.pre in
        {astate.prepost with pre}
      else astate.prepost
    in
    match instr with
    | Assign (lhs_access_expr, rhs_expr, loc) ->
        (* check on lhs_typ to avoid false positive when assigning a pointer to another *)
        ( match HilExp.AccessExpression.get_typ lhs_access_expr tenv with
        | Some lhs_typ when not (Typ.is_reference lhs_typ) ->
            check_hil_expr ~loc rhs_expr
        | _ ->
            () ) ;
        let maybe_uninit_vars = MaybeUninitVars.remove lhs_access_expr astate.maybe_uninit_vars in
        let maybe_uninit_vars =
          if HilExp.AccessExpression.is_base lhs_access_expr then
            (* if we assign to the root of a struct then we need to remove all the fields *)
            let lhs_base = HilExp.AccessExpression.get_base lhs_access_expr in
            MaybeUninitVars.remove_all_fields tenv lhs_base maybe_uninit_vars
            |> MaybeUninitVars.remove_dereference_access lhs_base
          else maybe_uninit_vars
        in
        let prepost = update_prepost lhs_access_expr rhs_expr in
        {astate with maybe_uninit_vars; prepost}
    | Call (_, Direct callee_pname, _, _, _)
      when Typ.Procname.equal callee_pname BuiltinDecl.objc_cpp_throw ->
        {astate with maybe_uninit_vars= MaybeUninitVars.empty}
    | Call (_, HilInstr.Direct call, [HilExp.AccessExpression (AddressOf (Base base))], _, _)
      when is_dummy_constructor_of_a_struct call ->
        (* if it's a default constructor, we use the following heuristic: we assume that it initializes
    correctly all fields when there is an implementation of the constructor that initilizes at least one
    field. If there is no explicit implementation we cannot assume fields are initialized *)
        if function_initializes_some_formal_params pdesc call then
          let maybe_uninit_vars =
            (* in HIL/SIL the default constructor has only one param: the struct *)
            MaybeUninitVars.remove_all_fields tenv base astate.maybe_uninit_vars
          in
          {astate with maybe_uninit_vars}
        else astate
    | Call (_, call, actuals, _, loc) ->
        (* in case of intraprocedural only analysis we assume that parameters passed by reference
           to a function will be initialized inside that function *)
        let pname_opt = match call with Direct pname -> Some pname | Indirect _ -> None in
        let callee_formals_opt = Option.bind pname_opt ~f:get_formals in
        let is_initializing_all_args =
          match call with
          | Direct pname ->
              Models.is_initializing_all_args pname
          | Indirect _ ->
              false
        in
        let maybe_uninit_vars =
          List.foldi ~init:astate.maybe_uninit_vars actuals ~f:(fun idx acc actual_exp ->
              match HilExp.ignore_cast actual_exp with
              | HilExp.AccessExpression access_expr -> (
                  let access_expr_to_remove =
                    match access_expr with AddressOf ae -> ae | _ -> access_expr
                  in
                  match HilExp.AccessExpression.get_base access_expr with
                  | _, {Typ.desc= Tarray _} when is_initializing_all_args ->
                      MaybeUninitVars.remove access_expr acc
                  | _, t
                  (* Access to a field of a struct or an element of an array by reference *)
                    when Option.exists callee_formals_opt
                           ~f:(is_fld_or_array_elem_passed_by_ref t access_expr idx) -> (
                    match pname_opt with
                    | Some pname when Config.uninit_interproc ->
                        remove_initialized_params pdesc pname acc idx access_expr_to_remove false
                    | _ ->
                        MaybeUninitVars.remove access_expr_to_remove acc )
                  | base when Option.exists pname_opt ~f:Typ.Procname.is_constructor ->
                      MaybeUninitVars.remove_all_fields tenv base
                        (MaybeUninitVars.remove access_expr_to_remove acc)
                  | _, {Typ.desc= Tptr _} -> (
                    match pname_opt with
                    | Some pname when Config.uninit_interproc ->
                        remove_initialized_params pdesc pname acc idx access_expr_to_remove true
                    | _ ->
                        MaybeUninitVars.remove_everything_under tenv access_expr_to_remove acc )
                  | _ ->
                      acc )
              | HilExp.Closure (_, apl) ->
                  (* remove the captured variables of a block/lambda *)
                  List.fold apl ~init:acc ~f:(fun acc (base, _) ->
                      MaybeUninitVars.remove (HilExp.AccessExpression.base base) acc )
              | _ ->
                  acc )
        in
        ( match call with
        | Direct _ ->
            report_on_function_params pdesc tenv maybe_uninit_vars actuals loc summary
              callee_formals_opt
        | Indirect _ ->
            () ) ;
        {astate with maybe_uninit_vars}
    | Assume (expr, _, _, loc) ->
        check_hil_expr expr ~loc ; astate
    | ExitScope _ ->
        astate


  let pp_session_name node fmt = F.fprintf fmt "uninit %a" CFG.Node.pp_id (CFG.Node.id node)
end

module CFG = ProcCfg.Normal
module Analyzer = LowerHil.MakeAbstractInterpreter (TransferFunctions (CFG))

module Initial = struct
  let get_locals tenv pdesc =
    List.fold (Procdesc.get_locals pdesc) ~init:[]
      ~f:(fun acc (var_data : ProcAttributes.var_data) ->
        let pvar = Pvar.mk var_data.name (Procdesc.get_proc_name pdesc) in
        let base_access_expr = HilExp.AccessExpression.base (Var.of_pvar pvar, var_data.typ) in
        match var_data.typ.Typ.desc with
        | Typ.Tstruct qual_name
        (* T30105165 remove filtering after we improve union translation *)
          when not (Typ.Name.is_union qual_name) -> (
          match Tenv.lookup tenv qual_name with
          | Some {fields} ->
              let flist =
                List.fold
                  ~f:(fun acc' (fn, _, _) ->
                    HilExp.AccessExpression.field_offset base_access_expr fn :: acc' )
                  ~init:acc fields
              in
              base_access_expr :: flist
              (* for struct we take the struct address, and the access_path
                                    to the fields one level down *)
          | _ ->
              acc )
        | Typ.Tarray {elt} ->
            HilExp.AccessExpression.array_offset base_access_expr elt None :: acc
        | Typ.Tptr _ ->
            base_access_expr :: HilExp.AccessExpression.dereference base_access_expr :: acc
        | _ ->
            base_access_expr :: acc )
end

let checker {Callbacks.tenv; summary; proc_desc} : Summary.t =
  (* start with empty set of uninit local vars and empty set of init formal params *)
  let maybe_uninit_vars = Initial.get_locals tenv proc_desc in
  let initial =
    { RecordDomain.maybe_uninit_vars= MaybeUninitVars.of_list maybe_uninit_vars
    ; aliased_vars= AliasedVars.empty
    ; prepost= {UninitDomain.pre= D.empty; post= D.empty} }
  in
  let proc_data =
    let formals = FormalMap.make proc_desc in
    ProcData.make proc_desc tenv {formals; summary}
  in
  match Analyzer.compute_post proc_data ~initial with
  | Some {RecordDomain.prepost} ->
      Payload.update_summary prepost summary
  | None ->
      if Procdesc.Node.get_succs (Procdesc.get_start_node proc_desc) <> [] then (
        L.internal_error "Uninit analyzer failed to compute post for %a" Typ.Procname.pp
          (Procdesc.get_proc_name proc_desc) ;
        summary )
      else summary
