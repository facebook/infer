(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging
open PulseBasicInterface
open PulseDomainInterface
open PulseOperationResult.Import

let mk_objc_self_pvar proc_name = Pvar.mk Mangled.self proc_name

let init_fields_zero tenv path location ~zero addr typ astate =
  let get_fields typ =
    match typ.Typ.desc with
    | Tstruct struct_name ->
        Tenv.lookup tenv struct_name |> Option.map ~f:(fun {Struct.fields} -> fields)
    | _ ->
        None
  in
  let rec init_fields_zero_helper addr typ astate =
    match get_fields typ with
    | Some fields ->
        List.fold fields ~init:(Ok astate) ~f:(fun acc (field, field_typ, _) ->
            let* acc in
            let acc, field_addr = Memory.eval_edge addr (FieldAccess field) acc in
            init_fields_zero_helper field_addr field_typ acc )
    | None ->
        PulseOperations.write_deref path location ~ref:addr ~obj:zero astate
  in
  init_fields_zero_helper addr typ astate


(** Constructs summary [{self = 0} {return = self}] when [proc_desc] returns a POD type. This allows
    us to connect invalidation with invalid access in the trace *)
let mk_nil_messaging_summary_aux tenv proc_name (proc_attrs : ProcAttributes.t) =
  let path = PathContext.initial in
  let t0 = path.PathContext.timestamp in
  let self = mk_objc_self_pvar proc_name in
  let astate = AbductiveDomain.mk_initial tenv proc_name proc_attrs in
  let** astate, (self_value, self_history) =
    PulseOperations.eval_deref path proc_attrs.loc (Lvar self) astate
  in
  let** astate = PulseArithmetic.prune_eq_zero self_value astate in
  let event = ValueHistory.NilMessaging (proc_attrs.loc, t0) in
  let updated_self_value_hist = (self_value, ValueHistory.sequence event self_history) in
  match List.last proc_attrs.formals with
  | Some (last_formal, {desc= Tptr (typ, _)}, _) when Mangled.is_return_param last_formal ->
      let ret_param_var = Pvar.get_ret_param_pvar proc_name in
      let** astate, ret_param_var_addr_hist =
        PulseOperations.eval_deref path proc_attrs.loc (Lvar ret_param_var) astate
      in
      Sat
        (init_fields_zero tenv path proc_attrs.loc ~zero:updated_self_value_hist
           ret_param_var_addr_hist typ astate )
  | _ ->
      let ret_var = Pvar.get_ret_pvar proc_name in
      let** astate, ret_var_addr_hist =
        PulseOperations.eval path Write proc_attrs.loc (Lvar ret_var) astate
      in
      Sat
        (PulseOperations.write_deref path proc_attrs.loc ~ref:ret_var_addr_hist
           ~obj:updated_self_value_hist astate )


let mk_latent_non_POD_nil_messaging tenv proc_name (proc_attrs : ProcAttributes.t) =
  let path = PathContext.initial in
  let self = mk_objc_self_pvar proc_name in
  let astate = AbductiveDomain.mk_initial tenv proc_name proc_attrs in
  let** astate, (self_value, _self_history) =
    PulseOperations.eval_deref path proc_attrs.loc (Lvar self) astate
  in
  let trace = Trace.Immediate {location= proc_attrs.loc; history= ValueHistory.epoch} in
  let** astate = PulseArithmetic.prune_eq_zero self_value astate in
  let++ summary =
    let open SatUnsat.Import in
    AbductiveDomain.Summary.of_post tenv proc_name proc_attrs proc_attrs.loc astate
    >>| AccessResult.ignore_leaks >>| AccessResult.of_abductive_summary_result
    >>| AccessResult.with_summary
  in
  ExecutionDomain.LatentInvalidAccess
    { astate= summary
    ; address= Decompiler.find self_value astate
    ; must_be_valid=
        (trace, Some (SelfOfNonPODReturnMethod (ProcAttributes.to_return_type proc_attrs)))
    ; calling_context= [] }


let mk_nil_messaging_summary tenv proc_name (proc_attrs : ProcAttributes.t) =
  if Procname.is_objc_instance_method proc_name then (
    if proc_attrs.is_ret_type_pod then (
      (* In ObjC, when a method is called on nil, there is no NPE, the method is actually not called
         and the return value is 0/false/nil.  We create a nil summary to avoid reporting NPE in
         this case.  However, there is an exception in the case where the return type is non-POD.
         In that case it's UB and we want to report an error. *)
      match
        (let** astate = mk_nil_messaging_summary_aux tenv proc_name proc_attrs in
         let open SatUnsat.Import in
         AbductiveDomain.Summary.of_post tenv proc_name proc_attrs proc_attrs.loc astate
         >>| AccessResult.ignore_leaks >>| AccessResult.of_abductive_summary_result
         >>| AccessResult.with_summary )
        |> PulseOperationResult.sat_ok
      with
      | Some summary ->
          Some (ContinueProgram summary)
      | None ->
          L.internal_error
            "mk_nil_messaging_summary_aux for %a resulted in an error or an unsat state" Procname.pp
            proc_name ;
          None )
    else
      match
        mk_latent_non_POD_nil_messaging tenv proc_name proc_attrs |> PulseOperationResult.sat_ok
      with
      | Some _ as some_summary ->
          some_summary
      | None ->
          L.internal_error
            "mk_latent_non_POD_nil_messaging for %a resulted in an error or an unsat state"
            Procname.pp proc_name ;
          None )
  else None


let prune_positive_allocated_self location self_address astate =
  (* it's important to do the prune *first* before the dereference to detect contradictions if the
     address is equal to 0 *)
  PulseArithmetic.prune_positive (fst self_address) astate
  >>|= PulseOperations.eval_access PathContext.initial Read location self_address Dereference
  >>|| fst


let initial_with_positive_self proc_name (proc_attrs : ProcAttributes.t) initial_astate =
  let self_var = mk_objc_self_pvar proc_name in
  if Procname.is_objc_instance_method proc_name then (
    let result =
      let** astate, self_address =
        PulseOperations.eval_deref PathContext.initial proc_attrs.loc (Lvar self_var) initial_astate
      in
      prune_positive_allocated_self proc_attrs.loc self_address astate
    in
    match PulseOperationResult.sat_ok result with
    | Some astate ->
        astate
    | None ->
        L.internal_error
          "found an error or an unsat state when adding [self > 0] to %a's initial state %a"
          AbductiveDomain.pp initial_astate Procname.pp proc_name ;
        initial_astate )
  else initial_astate


let append_objc_actual_self_positive proc_name (proc_attrs : ProcAttributes.t) self_actual astate =
  match self_actual with
  | Some (self, _) when Procname.is_objc_instance_method proc_name ->
      prune_positive_allocated_self proc_attrs.loc self astate
  | _ ->
      Sat (Ok astate)
