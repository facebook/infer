(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PulseDomainInterface
open PulseOperations.Import
open PulseBasicInterface

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
  (* HACK: we are operating on an "empty" initial state and do not expect to create any alarms
     (nothing is Invalid in the initial state) or unsatisfiability (we won't create arithmetic
     contradictions) so we make liberal use of [PulseResult.ok_exn] *)
  let astate, (self_value, self_history) =
    PulseOperations.eval_deref path proc_attrs.loc (Lvar self) astate |> PulseResult.ok_exn
  in
  let astate = PulseArithmetic.prune_eq_zero self_value astate |> PulseResult.ok_exn in
  let event = ValueHistory.NilMessaging (proc_attrs.loc, t0) in
  let updated_self_value_hist = (self_value, ValueHistory.sequence event self_history) in
  match List.last proc_attrs.formals with
  | Some (last_formal, {desc= Tptr (typ, _)}, _) when Mangled.is_return_param last_formal ->
      let ret_param_var = Pvar.get_ret_param_pvar proc_name in
      let astate, ret_param_var_addr_hist =
        PulseOperations.eval_deref path proc_attrs.loc (Lvar ret_param_var) astate
        |> PulseResult.ok_exn
      in
      init_fields_zero tenv path proc_attrs.loc ~zero:updated_self_value_hist
        ret_param_var_addr_hist typ astate
      |> PulseResult.ok_exn
  | _ ->
      let ret_var = Pvar.get_ret_pvar proc_name in
      let astate, ret_var_addr_hist =
        PulseOperations.eval path Write proc_attrs.loc (Lvar ret_var) astate |> PulseResult.ok_exn
      in
      PulseOperations.write_deref path proc_attrs.loc ~ref:ret_var_addr_hist
        ~obj:updated_self_value_hist astate
      |> PulseResult.ok_exn


let mk_latent_non_POD_nil_messaging tenv proc_name (proc_attrs : ProcAttributes.t) =
  let path = PathContext.initial in
  let self = mk_objc_self_pvar proc_name in
  let astate = AbductiveDomain.mk_initial tenv proc_name proc_attrs in
  (* same HACK as above with respect to [PulseResult.ok_exn] *)
  let astate, (self_value, _self_history) =
    PulseOperations.eval_deref path proc_attrs.loc (Lvar self) astate |> PulseResult.ok_exn
  in
  let trace = Trace.Immediate {location= proc_attrs.loc; history= ValueHistory.epoch} in
  let astate = PulseArithmetic.prune_eq_zero self_value astate |> PulseResult.ok_exn in
  match AbductiveDomain.summary_of_post tenv proc_name proc_attrs proc_attrs.loc astate with
  | Unsat | Sat (Error _) ->
      assert false
  | Sat (Ok summary) ->
      ExecutionDomain.LatentInvalidAccess
        { astate= summary
        ; address= Decompiler.find self_value astate
        ; must_be_valid=
            (trace, Some (SelfOfNonPODReturnMethod (ProcAttributes.to_return_type proc_attrs)))
        ; calling_context= [] }


let mk_nil_messaging_summary tenv proc_name (proc_attrs : ProcAttributes.t) =
  if Procname.is_objc_instance_method proc_name then
    if proc_attrs.is_ret_type_pod then
      (* In ObjC, when a method is called on nil, there is no NPE,
         the method is actually not called and the return value is 0/false/nil.
         We create a nil summary to avoid reporting NPE in this case.
         However, there is an exception in the case where the return type is non-POD.
         In that case it's UB and we want to report an error. *)
      let astate = mk_nil_messaging_summary_aux tenv proc_name proc_attrs in
      Some (ContinueProgram astate)
    else
      let summary = mk_latent_non_POD_nil_messaging tenv proc_name proc_attrs in
      Some summary
  else None


let prune_positive_allocated_self location self_address astate =
  (* it's important to do the prune *first* before the dereference to detect contradictions if the
     address is equal to 0 *)
  PulseArithmetic.prune_positive (fst self_address) astate
  >>= PulseOperations.eval_access PathContext.initial Read location self_address Dereference
  >>| fst


let initial_with_positive_self proc_name (proc_attrs : ProcAttributes.t) initial_astate =
  let self_var = mk_objc_self_pvar proc_name in
  (* same HACK as above with respect to [PulseResult.ok_exn] *)
  if Procname.is_objc_instance_method proc_name then
    let astate, self_address =
      PulseOperations.eval_deref PathContext.initial proc_attrs.loc (Lvar self_var) initial_astate
      |> PulseResult.ok_exn
    in
    prune_positive_allocated_self proc_attrs.loc self_address astate |> PulseResult.ok_exn
  else initial_astate


let append_objc_actual_self_positive proc_name (proc_attrs : ProcAttributes.t) self_actual astate =
  if Procname.is_objc_instance_method proc_name then
    Option.value_map self_actual ~default:(Ok astate) ~f:(fun (self, _) ->
        prune_positive_allocated_self proc_attrs.loc self astate )
  else Ok astate
