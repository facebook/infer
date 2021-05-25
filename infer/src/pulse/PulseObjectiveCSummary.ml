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

let mk_objc_self_pvar proc_desc =
  let proc_name = Procdesc.get_proc_name proc_desc in
  Pvar.mk Mangled.self proc_name


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
            let* acc = acc in
            let acc, field_addr = Memory.eval_edge addr (FieldAccess field) acc in
            init_fields_zero_helper field_addr field_typ acc )
    | None ->
        PulseOperations.write_deref path location ~ref:addr ~obj:zero astate
  in
  init_fields_zero_helper addr typ astate


let mk_objc_method_nil_summary_aux tenv proc_desc astate =
  (* Constructs summary {self = 0} {return = self}.
     This allows us to connect invalidation with invalid access in the trace *)
  let path = PathContext.initial in
  let location = Procdesc.get_loc proc_desc in
  let self = mk_objc_self_pvar proc_desc in
  let* astate, (self_value, self_history) =
    PulseOperations.eval_deref path location (Lvar self) astate
  in
  let* astate = PulseArithmetic.prune_eq_zero self_value astate in
  let event = ValueHistory.NilMessaging location in
  let updated_self_value_hist = (self_value, event :: self_history) in
  match List.last (Procdesc.get_formals proc_desc) with
  | Some (last_formal, {desc= Tptr (typ, _)}) when Mangled.is_return_param last_formal ->
      let ret_param_var = Procdesc.get_ret_param_var proc_desc in
      let* astate, ret_param_var_addr_hist =
        PulseOperations.eval_deref path location (Lvar ret_param_var) astate
      in
      init_fields_zero tenv path location ~zero:updated_self_value_hist ret_param_var_addr_hist typ
        astate
  | _ ->
      let ret_var = Procdesc.get_ret_var proc_desc in
      let* astate, ret_var_addr_hist =
        PulseOperations.eval path Write location (Lvar ret_var) astate
      in
      PulseOperations.write_deref path location ~ref:ret_var_addr_hist ~obj:updated_self_value_hist
        astate


let mk_objc_method_nil_summary tenv proc_desc initial =
  let proc_name = Procdesc.get_proc_name proc_desc in
  match (initial, proc_name) with
  | ContinueProgram astate, Procname.ObjC_Cpp {kind= ObjCInstanceMethod}
    when Procdesc.is_ret_type_pod proc_desc ->
      (* In ObjC, when a method is called on nil, there is no NPE,
         the method is actually not called and the return value is 0/false/nil.
         We create a nil summary to avoid reporting NPE in this case.
         However, there is an exception in the case where the return type is non-POD.
         In that case it's UB and we want to report an error. *)
      Some (mk_objc_method_nil_summary_aux tenv proc_desc astate)
  | ContinueProgram _, _
  | ExitProgram _, _
  | AbortProgram _, _
  | LatentAbortProgram _, _
  | LatentInvalidAccess _, _
  | ISLLatentMemoryError _, _ ->
      None


let append_objc_self_positive {InterproceduralAnalysis.tenv; proc_desc; err_log} astate =
  let location = Procdesc.get_loc proc_desc in
  let self = mk_objc_self_pvar proc_desc in
  match astate with
  | ContinueProgram astate ->
      let result =
        let* astate, value =
          PulseOperations.eval_deref PathContext.initial location (Lvar self) astate
        in
        PulseArithmetic.prune_positive (fst value) astate
      in
      PulseReport.report_result tenv proc_desc err_log location result
  | ExitProgram _
  | AbortProgram _
  | LatentAbortProgram _
  | LatentInvalidAccess _
  | ISLLatentMemoryError _ ->
      [astate]


let append_objc_actual_self_positive procdesc self_actual astate =
  let procname = Procdesc.get_proc_name procdesc in
  match procname with
  | Procname.ObjC_Cpp {kind= ObjCInstanceMethod} when Procdesc.is_ret_type_pod procdesc ->
      Option.value_map self_actual ~default:(Ok astate) ~f:(fun ((self, _), _) ->
          PulseArithmetic.prune_positive self astate )
  | _ ->
      Ok astate


let update_must_be_valid_reason {InterproceduralAnalysis.tenv; proc_desc; err_log} astate =
  let path = PathContext.initial in
  let location = Procdesc.get_loc proc_desc in
  let self = mk_objc_self_pvar proc_desc in
  let proc_name = Procdesc.get_proc_name proc_desc in
  match (astate, proc_name) with
  | ContinueProgram astate, Procname.ObjC_Cpp {kind= ObjCInstanceMethod}
    when not (Procdesc.is_ret_type_pod proc_desc) ->
      let result =
        (* add reason for must be valid to be because the return type is non pod *)
        let+ astate, value = PulseOperations.eval_deref path location (Lvar self) astate in
        AddressAttributes.replace_must_be_valid_reason path SelfOfNonPODReturnMethod (fst value)
          astate
      in
      PulseReport.report_result tenv proc_desc err_log location result
  | ContinueProgram _, _
  | ExitProgram _, _
  | AbortProgram _, _
  | LatentAbortProgram _, _
  | LatentInvalidAccess _, _
  | ISLLatentMemoryError _, _ ->
      [astate]


let update_objc_method_posts ({InterproceduralAnalysis.tenv; proc_desc; err_log} as analysis_data)
    ~initial_astate ~posts =
  match mk_objc_method_nil_summary tenv proc_desc initial_astate with
  | None ->
      List.concat_map ~f:(update_must_be_valid_reason analysis_data) posts
  | Some result ->
      let location = Procdesc.get_loc proc_desc in
      let nil_summary = PulseReport.report_result tenv proc_desc err_log location result in
      let posts = List.concat_map ~f:(append_objc_self_positive analysis_data) posts in
      nil_summary @ posts
