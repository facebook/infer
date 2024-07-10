(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging
open PulseBasicInterface
open PulseDomainInterface
open PulseOperationResult.Import

type pre_post_list = ExecutionDomain.summary list [@@deriving yojson_of]

type summary = {pre_post_list: pre_post_list; non_disj: (NonDisjDomain.Summary.t[@yojson.opaque])}
[@@deriving yojson_of]

type t = {main: summary; specialized: (summary Specialization.Pulse.Map.t[@yojson.opaque])}
[@@deriving yojson_of]

let pp_pre_post_list fmt ~pp_kind pre_posts =
  F.open_vbox 0 ;
  F.fprintf fmt "%d%t pre/post(s)@;" (List.length pre_posts) pp_kind ;
  List.iteri pre_posts ~f:(fun i (pre_post : ExecutionDomain.summary) ->
      F.fprintf fmt "#%d: @[%a@]@;" i ExecutionDomain.pp_summary pre_post ) ;
  F.close_box ()


let pp_summary fmt ~pp_kind {pre_post_list; non_disj} =
  F.fprintf fmt "@[<hov>pre/posts:%a@\nnon_disj:%a@]" (pp_pre_post_list ~pp_kind) pre_post_list
    NonDisjDomain.Summary.pp non_disj


let pp fmt {main; specialized} =
  if Specialization.Pulse.Map.is_empty specialized then
    pp_summary fmt ~pp_kind:(fun _fmt -> ()) main
  else
    let pp_kind fmt = F.pp_print_string fmt " main" in
    F.open_hvbox 0 ;
    pp_summary fmt ~pp_kind main ;
    Specialization.Pulse.Map.iter
      (fun specialization pre_posts ->
        F.fprintf fmt "@\n" ;
        let pp_kind fmt =
          F.fprintf fmt " specialized with %a" Specialization.Pulse.pp specialization
        in
        pp_summary fmt ~pp_kind pre_posts )
      specialized ;
    F.close_box ()


let add_disjunctive_pre_post pre_post {pre_post_list; non_disj} =
  {pre_post_list= pre_post :: pre_post_list; non_disj}


let empty = {pre_post_list= []; non_disj= NonDisjDomain.Summary.bottom}

let join summary1 summary2 =
  let pre_post_list = summary1.pre_post_list @ summary2.pre_post_list in
  let non_disj = NonDisjDomain.Summary.join summary1.non_disj summary2.non_disj in
  {pre_post_list; non_disj}


let exec_summary_of_post_common ({InterproceduralAnalysis.proc_desc} as analysis_data)
    ~continue_program ~exception_raised specialization location (exec_astate : ExecutionDomain.t) :
    _ ExecutionDomain.base_t SatUnsat.t =
  let summarize (astate : AbductiveDomain.t)
      ~(exec_domain_of_summary : AbductiveDomain.Summary.summary -> 'a ExecutionDomain.base_t)
      ~(is_exceptional_state : bool) : _ ExecutionDomain.base_t SatUnsat.t =
    let open SatUnsat.Import in
    let+ summary_result =
      AbductiveDomain.Summary.of_post
        (Procdesc.get_proc_name proc_desc)
        (Procdesc.get_attributes proc_desc)
        location astate
    in
    match (summary_result : _ result) with
    | Ok summary ->
        exec_domain_of_summary summary
    | Error (`MemoryLeak (summary, astate, allocator, allocation_trace, location)) ->
        PulseReport.report_summary_error analysis_data
          ( ReportableError {astate; diagnostic= MemoryLeak {allocator; allocation_trace; location}}
          , summary )
        |> Option.value ~default:(exec_domain_of_summary summary)
    | Error (`JavaResourceLeak (summary, astate, class_name, allocation_trace, location)) ->
        PulseReport.report_summary_error analysis_data
          ( ReportableError
              {astate; diagnostic= JavaResourceLeak {class_name; allocation_trace; location}}
          , summary )
        |> Option.value ~default:(exec_domain_of_summary summary)
    | Error (`HackUnawaitedAwaitable (summary, astate, allocation_trace, location)) ->
        (* suppress unawaited awaitable reporting in the case that we're throwing an exception because it leads to
           too many true-but-unhelpful positives. TODO: reinstate reporting in the case that the exception is caught *)
        if is_exceptional_state then (
          L.d_printfln "Suppressing Unawaited Awaitable report because exception thown" ;
          exec_domain_of_summary summary )
        else
          PulseReport.report_summary_error analysis_data
            ( ReportableError
                {astate; diagnostic= HackUnawaitedAwaitable {allocation_trace; location}}
            , summary )
          |> Option.value ~default:(exec_domain_of_summary summary)
    | Error (`HackUnfinishedBuilder (summary, astate, allocation_trace, location, builder_type)) ->
        if is_exceptional_state then (
          L.d_printfln "Suppressing Unfinished Builder report because exception thown" ;
          exec_domain_of_summary summary )
        else
          PulseReport.report_summary_error analysis_data
            ( ReportableError
                { astate
                ; diagnostic= HackUnfinishedBuilder {builder_type; allocation_trace; location} }
            , summary )
          |> Option.value ~default:(exec_domain_of_summary summary)
    | Error (`CSharpResourceLeak (summary, astate, class_name, allocation_trace, location)) ->
        PulseReport.report_summary_error analysis_data
          ( ReportableError
              {astate; diagnostic= CSharpResourceLeak {class_name; allocation_trace; location}}
          , summary )
        |> Option.value ~default:(exec_domain_of_summary summary)
    | Error (`PotentialInvalidAccessSummary (summary, astate, address, must_be_valid)) -> (
      match
        let open IOption.Let_syntax in
        let* addr = DecompilerExpr.abstract_value_of_expr address in
        let* _, attrs = AbductiveDomain.find_post_cell_opt addr (astate :> AbductiveDomain.t) in
        Attributes.get_invalid attrs
      with
      | None ->
          ExecutionDomain.LatentInvalidAccess
            {astate= summary; address; must_be_valid; calling_context= []}
      | Some (invalidation, invalidation_trace) ->
          (* NOTE: this probably leads to the error being dropped as the access trace is unlikely to
             contain the reason for invalidation and thus we will filter out the report. TODO:
             figure out if that's a problem. *)
          PulseReport.report_summary_error analysis_data
            ( ReportableError
                { diagnostic=
                    AccessToInvalidAddress
                      { calling_context= []
                      ; invalid_address= address
                      ; invalidation
                      ; invalidation_trace
                      ; access_trace= fst must_be_valid
                      ; must_be_valid_reason= snd must_be_valid }
                ; astate }
            , summary )
          |> Option.value ~default:(exec_domain_of_summary summary) )
  in
  match exec_astate with
  | ExceptionRaised astate ->
      summarize astate ~exec_domain_of_summary:exception_raised ~is_exceptional_state:true
  | ContinueProgram astate ->
      summarize astate ~exec_domain_of_summary:continue_program ~is_exceptional_state:false
  (* already a summary but need to reconstruct the variants to make the type system happy :( *)
  | AbortProgram astate ->
      Sat (AbortProgram astate)
  | ExitProgram astate ->
      Sat (ExitProgram astate)
  | LatentAbortProgram {astate; latent_issue} ->
      Sat (LatentAbortProgram {astate; latent_issue})
  | LatentInvalidAccess {astate; address; must_be_valid; calling_context} ->
      Sat (LatentInvalidAccess {astate; address; must_be_valid; calling_context})
  | LatentSpecializedTypeIssue {astate; specialized_type; trace} -> (
    match specialization with
    | Some specialization
      when Specialization.Pulse.has_type_in_specialization specialization specialized_type ->
        Sat (LatentSpecializedTypeIssue {astate; specialized_type; trace})
    | _ ->
        let diagnostic =
          Diagnostic.HackCannotInstantiateAbstractClass {type_name= specialized_type; trace}
        in
        PulseReport.report analysis_data ~is_suppressed:false ~latent:false diagnostic ;
        Sat (AbortProgram astate) )


let force_exit_program analysis_data post =
  exec_summary_of_post_common analysis_data None post
    ~continue_program:(fun astate -> ExitProgram astate)
    ~exception_raised:(fun astate -> ExitProgram astate)


let of_posts analysis_data specialization location posts non_disj =
  let pre_post_list =
    List.filter_mapi posts ~f:(fun i exec_state ->
        L.d_printfln "Creating spec out of state #%d:@\n%a" i
          (ExecutionDomain.pp_with_kind HTML None)
          exec_state ;
        exec_summary_of_post_common analysis_data specialization location exec_state
          ~continue_program:(fun astate -> ContinueProgram astate)
          ~exception_raised:(fun astate -> ExceptionRaised astate)
        |> SatUnsat.sat )
  in
  {pre_post_list; non_disj= NonDisjDomain.make_summary non_disj}


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
        List.fold fields ~init:(Ok astate) ~f:(fun acc {Struct.name= field; typ= field_typ} ->
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
  let astate = AbductiveDomain.mk_initial tenv proc_attrs in
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
  let astate = AbductiveDomain.mk_initial tenv proc_attrs in
  let** astate, (self_value, _self_history) =
    PulseOperations.eval_deref path proc_attrs.loc (Lvar self) astate
  in
  let trace = Trace.Immediate {location= proc_attrs.loc; history= ValueHistory.epoch} in
  let** astate = PulseArithmetic.prune_eq_zero self_value astate in
  let++ summary =
    let open SatUnsat.Import in
    AbductiveDomain.Summary.of_post proc_name proc_attrs proc_attrs.loc astate
    >>| AccessResult.ignore_leaks >>| AccessResult.of_abductive_summary_result
    >>| AccessResult.with_summary
  in
  ExecutionDomain.LatentInvalidAccess
    { astate= summary
    ; address= Decompiler.find self_value astate
    ; must_be_valid=
        (trace, Some (SelfOfNonPODReturnMethod (ProcAttributes.to_return_type proc_attrs)))
    ; calling_context= [] }


let mk_objc_nil_messaging_summary tenv (proc_attrs : ProcAttributes.t) =
  let proc_name = proc_attrs.proc_name in
  if Procname.is_objc_instance_method proc_name then (
    if proc_attrs.is_ret_type_pod then (
      (* In ObjC, when a method is called on nil, there is no NPE, the method is actually not called
         and the return value is 0/false/nil.  We create a nil summary to avoid reporting NPE in
         this case.  However, there is an exception in the case where the return type is non-POD.
         In that case it's UB and we want to report an error. *)
      match
        (let** astate = mk_nil_messaging_summary_aux tenv proc_name proc_attrs in
         let open SatUnsat.Import in
         AbductiveDomain.Summary.of_post proc_name proc_attrs proc_attrs.loc astate
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


let positive_allocated_self proc_name location self_address astate =
  (* For objc the method is called only if self>0, so we use [prune_positive] to send [self>0] to
     the precondition instead of relying on the fact that [self] is allocated (which is done in
     [AbductiveDomain.mk_initial]).

     It's important to do the prune *first* before the dereference to detect contradictions if the
     address is equal to 0 *)
  let astate =
    if Procname.is_objc_method proc_name then
      PulseArithmetic.prune_positive (fst self_address) astate
    else Sat (Ok astate)
  in
  astate
  >>|= PulseOperations.eval_access PathContext.initial Read location self_address Dereference
  >>|| fst


let initial_with_positive_self (proc_attrs : ProcAttributes.t) initial_astate =
  match ProcAttributes.get_this proc_attrs with
  | Some self_var -> (
      let result =
        let** astate, self_address =
          PulseOperations.eval_deref PathContext.initial proc_attrs.loc (Lvar self_var)
            initial_astate
        in
        positive_allocated_self proc_attrs.proc_name proc_attrs.loc self_address astate
      in
      match PulseOperationResult.sat_ok result with
      | Some astate ->
          astate
      | None ->
          L.internal_error
            "found an error or an unsat state when adding [self > 0] to %a's initial state %a"
            AbductiveDomain.pp initial_astate Procname.pp proc_attrs.proc_name ;
          initial_astate )
  | None ->
      initial_astate


let append_objc_actual_self_positive proc_name (proc_attrs : ProcAttributes.t) self_actual astate =
  match self_actual with
  | Some (self, _) when Procname.is_objc_instance_method proc_name ->
      positive_allocated_self proc_name proc_attrs.loc self astate
  | _ ->
      Sat (Ok astate)


let merge x y =
  let merged_is_same_to_x = ref true in
  let merged_is_same_to_y = ref true in
  let merged =
    Specialization.Pulse.Map.merge
      (fun _ x y ->
        match (x, y) with
        | None, None | Some _, Some _ ->
            x
        | Some _, None ->
            merged_is_same_to_y := false ;
            x
        | None, Some _ ->
            merged_is_same_to_x := false ;
            y )
      x.specialized y.specialized
  in
  if !merged_is_same_to_x then x
  else if !merged_is_same_to_y then y
  else {x with specialized= merged}


let get_missed_captures ~get_summary procnames =
  let module MissedCaptures = TransitiveInfo.MissedCaptures in
  let from_execution = function
    | ExecutionDomain.ContinueProgram summary
    | ExceptionRaised summary
    | ExitProgram summary
    | AbortProgram summary ->
        AbductiveDomain.Summary.get_transitive_info summary
    | LatentAbortProgram {astate}
    | LatentInvalidAccess {astate}
    | LatentSpecializedTypeIssue {astate} ->
        AbductiveDomain.Summary.get_transitive_info astate
  in
  let from_pre_post_list pre_post_list =
    List.map pre_post_list ~f:(fun exec ->
        (from_execution exec).PulseTransitiveInfo.missed_captures )
    |> List.reduce ~f:MissedCaptures.join
    |> Option.value ~default:MissedCaptures.bottom
  in
  let from_simple_summary {pre_post_list} = from_pre_post_list pre_post_list in
  let from_summary summary =
    Specialization.Pulse.Map.fold
      (fun _ summary -> MissedCaptures.join (from_simple_summary summary))
      summary.specialized
      (from_simple_summary summary.main)
  in
  List.map procnames ~f:(fun procname ->
      get_summary procname |> Option.value_map ~default:MissedCaptures.bottom ~f:from_summary )
  |> List.reduce ~f:MissedCaptures.join
  |> Option.value ~default:MissedCaptures.bottom
