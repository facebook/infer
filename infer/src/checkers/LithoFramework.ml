(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module Domain = LithoDomain

(** return true if this function is part of the Litho framework code rather than client code *)
let is_function = function
  | Typ.Procname.Java java_procname -> (
    match Typ.Procname.Java.get_package java_procname with
    | Some "com.facebook.litho" ->
        true
    | _ ->
        false )
  | _ ->
      false


let is_component_builder procname tenv =
  match procname with
  | Typ.Procname.Java java_procname ->
      PatternMatch.is_subtype_of_str tenv
        (Typ.Procname.Java.get_class_type_name java_procname)
        "com.facebook.litho.Component$Builder"
  | _ ->
      false


let is_component procname tenv =
  match procname with
  | Typ.Procname.Java java_procname ->
      PatternMatch.is_subtype_of_str tenv
        (Typ.Procname.Java.get_class_type_name java_procname)
        "com.facebook.litho.Component"
  | _ ->
      false


let is_component_build_method procname tenv =
  match Typ.Procname.get_method procname with
  | "build" ->
      is_component_builder procname tenv
  | _ ->
      false


let is_component_create_method procname tenv =
  match Typ.Procname.get_method procname with "create" -> is_component procname tenv | _ -> false


let is_on_create_layout = function
  | Typ.Procname.Java java_pname -> (
    match Typ.Procname.Java.get_method java_pname with "onCreateLayout" -> true | _ -> false )
  | _ ->
      false


module type LithoContext = sig
  type t

  val field : (Payloads.t, t option) Field.t

  val check_callee : callee_pname:Typ.Procname.t -> tenv:Tenv.t -> t option -> bool

  val satisfies_heuristic :
    callee_pname:Typ.Procname.t -> callee_summary_opt:t option -> Tenv.t -> bool

  val should_report : Procdesc.t -> Tenv.t -> bool

  val report : t -> Tenv.t -> Summary.t -> unit

  val session_name : string
end

module TransferFunctions (CFG : ProcCfg.S) (LithoContext : LithoContext with type t = Domain.t) =
struct
  module CFG = CFG
  module Domain = Domain
  module Payload = SummaryPayload.Make (LithoContext)

  type extras = ProcData.no_extras

  let apply_callee_summary summary_opt caller_pname ret_id_typ actuals astate =
    match summary_opt with
    | Some summary ->
        (* TODO: append paths if the footprint access path is an actual path instead of a var *)
        let f_sub {Domain.LocalAccessPath.access_path= (var, _), _} =
          match Var.get_footprint_index var with
          | Some footprint_index -> (
            match List.nth actuals footprint_index with
            | Some (HilExp.AccessExpression actual_access_expr) ->
                Some
                  (Domain.LocalAccessPath.make
                     (HilExp.AccessExpression.to_access_path actual_access_expr)
                     caller_pname)
            | _ ->
                None )
          | None ->
              if Var.is_return var then
                Some (Domain.LocalAccessPath.make (ret_id_typ, []) caller_pname)
              else None
        in
        Domain.substitute ~f_sub summary |> Domain.join astate
    | None ->
        astate


  let exec_instr astate ProcData.{summary; tenv} _ (instr : HilInstr.t) : Domain.t =
    let caller_pname = Summary.get_proc_name summary in
    match instr with
    | Call
        ( return_base
        , Direct callee_pname
        , (HilExp.AccessExpression receiver_ae :: _ as actuals)
        , _
        , location ) ->
        let callee_summary_opt = Payload.read ~caller_summary:summary ~callee_pname in
        let receiver =
          Domain.LocalAccessPath.make
            (HilExp.AccessExpression.to_access_path receiver_ae)
            caller_pname
        in
        if
          ( LithoContext.check_callee ~callee_pname ~tenv callee_summary_opt
          || (* track callee in order to report respective errors *)
          Domain.mem receiver astate
          (* track anything called on a receiver we're already tracking *) )
          && LithoContext.satisfies_heuristic ~callee_pname ~callee_summary_opt tenv
        then
          let return_access_path = Domain.LocalAccessPath.make (return_base, []) caller_pname in
          let return_calls =
            (try Domain.find return_access_path astate with Caml.Not_found -> Domain.CallSet.empty)
            |> Domain.CallSet.add (Domain.MethodCall.make receiver callee_pname location)
          in
          Domain.add return_access_path return_calls astate
        else
          (* treat it like a normal call *)
          apply_callee_summary callee_summary_opt caller_pname return_base actuals astate
    | Call (ret_id_typ, Direct callee_procname, actuals, _, _) ->
        let callee_summary_opt =
          Payload.read ~caller_summary:summary ~callee_pname:callee_procname
        in
        apply_callee_summary callee_summary_opt caller_pname ret_id_typ actuals astate
    | Assign (lhs_ae, HilExp.AccessExpression rhs_ae, _) -> (
        (* creating an alias for the rhs binding; assume all reads will now occur through the
           alias. this helps us keep track of chains in cases like tmp = getFoo(); x = tmp;
           tmp.getBar() *)
        let lhs_access_path =
          Domain.LocalAccessPath.make (HilExp.AccessExpression.to_access_path lhs_ae) caller_pname
        in
        let rhs_access_path =
          Domain.LocalAccessPath.make (HilExp.AccessExpression.to_access_path rhs_ae) caller_pname
        in
        try
          let call_set = Domain.find rhs_access_path astate in
          Domain.remove rhs_access_path astate |> Domain.add lhs_access_path call_set
        with Caml.Not_found -> astate )
    | _ ->
        astate


  let pp_session_name _node fmt = F.pp_print_string fmt LithoContext.session_name
end

module MakeAnalyzer (LithoContext : LithoContext with type t = Domain.t) = struct
  module TF = TransferFunctions (ProcCfg.Exceptional) (LithoContext)
  module A = LowerHil.MakeAbstractInterpreter (TF)

  let checker {Callbacks.summary; exe_env} =
    let proc_desc = Summary.get_proc_desc summary in
    let tenv = Exe_env.get_tenv exe_env (Summary.get_proc_name summary) in
    let proc_data = ProcData.make_default summary tenv in
    match A.compute_post proc_data ~initial:Domain.empty with
    | Some post ->
        if LithoContext.should_report proc_desc tenv then LithoContext.report post tenv summary ;
        let postprocess astate formal_map : Domain.t =
          let f_sub access_path = Domain.LocalAccessPath.to_formal_option access_path formal_map in
          Domain.substitute ~f_sub astate
        in
        let payload = postprocess post (FormalMap.make proc_desc) in
        TF.Payload.update_summary payload summary
    | None ->
        summary
end
