(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format
module L = Logging
open Result.Monad_infix
module AbstractAddress = PulseDomain.AbstractAddress
module InterprocAction = PulseDomain.InterprocAction
module ValueHistory = PulseDomain.ValueHistory

include (* ocaml ignores the warning suppression at toplevel, hence the [include struct ... end] trick *)
  struct
  [@@@warning "-60"]

  (** Do not use {!PulseDomain} directly as it could result in bypassing abduction mechanisms in
     {!PulseOperations} and {!PulseAbductiveDomain} that take care of propagating facts to the
     precondition. *)
  module PulseDomain = struct end
  [@@deprecated "Use PulseAbductiveDomain or PulseOperations instead."]
end

let report summary diagnostic =
  let open PulseDiagnostic in
  Reporting.log_error summary ~loc:(get_location diagnostic) ~ltr:(get_trace diagnostic)
    (get_issue_type diagnostic) (get_message diagnostic)


let check_error summary = function
  | Ok ok ->
      ok
  | Error diagnostic ->
      report summary diagnostic ;
      (* We can also continue the analysis by returning {!PulseDomain.initial} here but there might
         be a risk we would get nonsense. This seems safer for now but TODO. *)
      raise_notrace AbstractDomain.Stop_analysis


module Payload = SummaryPayload.Make (struct
  type t = PulseSummary.t

  let field = Payloads.Fields.pulse
end)

module PulseTransferFunctions = struct
  module CFG = ProcCfg.Exceptional
  module Domain = PulseAbductiveDomain

  type extras = Summary.t

  let rec exec_assign summary (lhs_access : HilExp.AccessExpression.t) (rhs_exp : HilExp.t) loc
      astate =
    (* try to evaluate [rhs_exp] down to an abstract address or generate a fresh one *)
    let crumb = ValueHistory.Assignment {lhs= lhs_access; location= loc} in
    match rhs_exp with
    | AccessExpression rhs_access -> (
        PulseOperations.read loc rhs_access astate
        >>= fun (astate, (rhs_addr, rhs_trace)) ->
        let return_addr_trace = (rhs_addr, crumb :: rhs_trace) in
        PulseOperations.write loc lhs_access return_addr_trace astate
        >>= fun astate ->
        match lhs_access with
        | Base (var, _) when Var.is_return var ->
            PulseOperations.check_address_escape loc summary.Summary.proc_desc rhs_addr rhs_trace
              astate
        | _ ->
            Ok astate )
    | Closure (pname, captured) ->
        PulseOperations.Closures.record loc lhs_access pname captured astate
    | Cast (_, e) ->
        exec_assign summary lhs_access e loc astate
    | _ ->
        PulseOperations.read_all loc (HilExp.get_access_exprs rhs_exp) astate
        >>= PulseOperations.havoc [crumb] loc lhs_access


  let exec_unknown_call _summary ret (call : HilInstr.call) (actuals : HilExp.t list) _flags
      call_loc astate =
    let read_all args astate =
      PulseOperations.read_all call_loc (List.concat_map args ~f:HilExp.get_access_exprs) astate
    in
    let crumb = ValueHistory.Call {f= `HilCall call; actuals; location= call_loc} in
    let havoc_ret (ret, _) astate = PulseOperations.havoc_var [crumb] ret astate in
    let read_actuals_havoc_ret actuals ret astate = read_all actuals astate >>| havoc_ret ret in
    match call with
    | Direct callee_pname when Typ.Procname.is_constructor callee_pname -> (
        L.d_printfln "constructor call detected@." ;
        match actuals with
        | AccessExpression constructor_access :: rest ->
            let constructed_object = HilExp.AccessExpression.dereference constructor_access in
            PulseOperations.havoc [crumb] call_loc constructed_object astate
            >>= read_actuals_havoc_ret rest ret
        | _ ->
            Ok (havoc_ret ret astate) )
    | Direct (Typ.Procname.ObjC_Cpp callee_pname)
      when Typ.Procname.ObjC_Cpp.is_operator_equal callee_pname -> (
        L.d_printfln "operator= detected@." ;
        match actuals with
        (* copy assignment *)
        | [AccessExpression lhs; HilExp.AccessExpression rhs] ->
            let lhs_deref = HilExp.AccessExpression.dereference lhs in
            let rhs_deref = HilExp.AccessExpression.dereference rhs in
            PulseOperations.havoc [crumb] call_loc lhs_deref astate
            >>= fun astate ->
            PulseOperations.read call_loc rhs_deref astate >>| fst >>| havoc_ret ret
        | _ ->
            read_actuals_havoc_ret actuals ret astate )
    | _ ->
        L.d_printfln "skipping unknown procedure@." ;
        read_actuals_havoc_ret actuals ret astate


  let interprocedural_call caller_summary ret (call : HilInstr.call) (actuals : HilExp.t list)
      flags call_loc astate =
    let unknown_function () =
      exec_unknown_call caller_summary ret call actuals flags call_loc astate >>| List.return
    in
    match call with
    | Direct callee_pname -> (
      match Payload.read_full caller_summary.Summary.proc_desc callee_pname with
      | Some (callee_proc_desc, preposts) ->
          let formals =
            Procdesc.get_formals callee_proc_desc
            |> List.map ~f:(fun (mangled, _) -> Pvar.mk mangled callee_pname |> Var.of_pvar)
          in
          PulseOperations.Interproc.call callee_pname ~formals ~ret ~actuals flags call_loc astate
            preposts
      | None ->
          (* no spec found for some reason (unknown function, ...) *)
          L.d_printfln "No spec found for %a@\n" Typ.Procname.pp callee_pname ;
          unknown_function () )
    | Indirect access_expression ->
        L.d_printfln "Indirect call %a@\n" HilExp.AccessExpression.pp access_expression ;
        unknown_function ()


  (** has an object just gone out of scope? *)
  let get_out_of_scope_object (call : HilInstr.call) (actuals : HilExp.t list)
      (flags : CallFlags.t) =
    (* injected destructors are precisely inserted where an object goes out of scope *)
    if flags.cf_injected_destructor then
      match (call, actuals) with
      | ( Direct (Typ.Procname.ObjC_Cpp pname)
        , [AccessExpression (AddressOf (Base (ProgramVar pvar, typ)))] )
        when Pvar.is_local pvar && not (Typ.Procname.ObjC_Cpp.is_inner_destructor pname) ->
          (* ignore inner destructors, only trigger out of scope on the final destructor call *)
          Some (pvar, typ)
      | _ ->
          None
    else None


  (** [out_of_scope_access_expr] has just gone out of scope and in now invalid *)
  let exec_object_out_of_scope call_loc (pvar, typ) astate =
    (* invalidate both [&x] and [x]: reading either is now forbidden *)
    let invalidate pvar typ access astate =
      PulseOperations.invalidate
        (InterprocAction.Immediate {imm= GoneOutOfScope (pvar, typ); location= call_loc})
        call_loc access astate
    in
    let out_of_scope_base = HilExp.AccessExpression.base (Var.of_pvar pvar, typ) in
    invalidate pvar typ (HilExp.AccessExpression.dereference out_of_scope_base) astate
    >>= invalidate pvar typ out_of_scope_base


  let dispatch_call summary ret (call : HilInstr.call) (actuals : HilExp.t list) flags call_loc
      astate =
    let model =
      match call with
      | Indirect _ ->
          (* function pointer, etc.: skip for now *)
          None
      | Direct callee_pname ->
          PulseModels.dispatch callee_pname flags
    in
    match model with
    | Some model ->
        model call_loc ~ret ~actuals astate
    | None -> (
        (* do interprocedural call then destroy objects going out of scope *)
        PerfEvent.(log (fun logger -> log_begin_event logger ~name:"pulse interproc call" ())) ;
        let posts = interprocedural_call summary ret call actuals flags call_loc astate in
        PerfEvent.(log (fun logger -> log_end_event logger ())) ;
        match get_out_of_scope_object call actuals flags with
        | Some pvar_typ ->
            L.d_printfln "%a is going out of scope" Pvar.pp_value (fst pvar_typ) ;
            posts
            >>= fun posts ->
            List.map posts ~f:(fun astate -> exec_object_out_of_scope call_loc pvar_typ astate)
            |> Result.all
        | None ->
            posts )


  let hil_of_sil ?(add_deref = false) exp typ =
    HilExp.of_sil ~include_array_indexes:true ~f_resolve_id:(fun _ -> None) ~add_deref exp typ


  let exec_instr (astate : Domain.t) {ProcData.extras= summary} _cfg_node (instr : Sil.instr) =
    match instr with
    | Load (lhs_id, rhs_exp, typ, loc) ->
        let lhs_access = HilExp.AccessExpression.of_id lhs_id typ in
        let rhs_hexp = hil_of_sil ~add_deref:true rhs_exp typ in
        let post = exec_assign summary lhs_access rhs_hexp loc astate |> check_error summary in
        [post]
    | Store (lhs_exp, typ, rhs_exp, loc) ->
        let lhs_access_expr =
          match
            HilExp.access_expr_of_exp ~include_array_indexes:true
              ~f_resolve_id:(fun _ -> None)
              lhs_exp typ
          with
          | Some access_expr ->
              access_expr
          | None ->
              L.die InternalError "Non-assignable LHS expression %a at %a" Exp.pp lhs_exp
                Location.pp_file_pos loc
        in
        let rhs_hexp = hil_of_sil rhs_exp typ in
        let post =
          exec_assign summary lhs_access_expr rhs_hexp loc astate |> check_error summary
        in
        [post]
    | Prune (condition, loc, _is_then_branch, _if_kind) ->
        let condition_hexp = hil_of_sil condition (Typ.mk (Tint IBool)) in
        let post =
          PulseOperations.read_all loc (HilExp.get_access_exprs condition_hexp) astate
          |> check_error summary
        in
        [post]
    | Call ((ret_id, ret_typ), call_exp, actuals, loc, call_flags) ->
        let ret_hil = (Var.of_id ret_id, ret_typ) in
        let call_hil =
          match hil_of_sil call_exp (Typ.mk Tvoid) with
          | Constant (Cfun procname) | Closure (procname, _) ->
              HilInstr.Direct procname
          | HilExp.AccessExpression access_expr ->
              HilInstr.Indirect access_expr
          | call_exp ->
              L.(die InternalError) "Unexpected call expression %a" HilExp.pp call_exp
        in
        let actuals_hil = List.map ~f:(fun (exp, typ) -> hil_of_sil exp typ) actuals in
        let posts =
          dispatch_call summary ret_hil call_hil actuals_hil call_flags loc astate
          |> check_error summary
        in
        posts
    | Metadata (ExitScope (vars, _)) ->
        let post = PulseOperations.remove_vars vars astate in
        [post]
    | Metadata (VariableLifetimeBegins (pvar, _, location)) ->
        let var = Var.of_pvar pvar in
        let post = PulseOperations.realloc_var var location astate in
        [post]
    | Metadata (Abstract _ | Nullify _ | Skip) ->
        [astate]


  let pp_session_name _node fmt = F.pp_print_string fmt "Pulse"
end

module DisjunctiveTransferFunctions =
  TransferFunctions.MakeDisjunctive
    (PulseTransferFunctions)
    (struct
      let join_policy =
        match Config.pulse_max_disjuncts with 0 -> `NeverJoin | n -> `UnderApproximateAfter n


      let widen_policy = `UnderApproximateAfterNumIterations Config.pulse_widen_threshold
    end)

module DisjunctiveAnalyzer = AbstractInterpreter.MakeWTO (DisjunctiveTransferFunctions)

let checker {Callbacks.proc_desc; tenv; summary} =
  let proc_data = ProcData.make proc_desc tenv summary in
  AbstractAddress.init () ;
  let initial =
    DisjunctiveTransferFunctions.Disjuncts.singleton (PulseAbductiveDomain.mk_initial proc_desc)
  in
  match DisjunctiveAnalyzer.compute_post proc_data ~initial with
  | Some posts ->
      Payload.update_summary
        (PulseSummary.of_posts (DisjunctiveTransferFunctions.Disjuncts.elements posts))
        summary
  | None ->
      summary
  | exception AbstractDomain.Stop_analysis ->
      summary
