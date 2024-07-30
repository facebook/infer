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

let debug fmt = L.debug Analysis Verbose fmt

let get_matching_dest_addr_opt ~edges_pre ~edges_post : (Access.t * AbstractValue.t) list option =
  match
    List.fold2 ~init:(Some [])
      ~f:(fun acc (access, (addr_dest_pre, _)) (_, (addr_dest_post, _)) ->
        if AbstractValue.equal addr_dest_pre addr_dest_post then
          Option.map acc ~f:(fun acc -> (access, addr_dest_pre) :: acc)
        else None )
      (UnsafeMemory.Edges.bindings edges_pre |> List.sort ~compare:[%compare: Access.t * _])
      (UnsafeMemory.Edges.bindings edges_post |> List.sort ~compare:[%compare: Access.t * _])
  with
  | Unequal_lengths ->
      debug "Mismatch in pre and post.\n" ;
      None
  | Ok x ->
      x


let ignore_array_index (access : Access.t) : unit Access.access =
  match access with
  | ArrayAccess (typ, _) ->
      ArrayAccess (typ, ())
  | FieldAccess fname ->
      FieldAccess fname
  | Dereference ->
      Dereference


let add_invalid_and_modified ~pvar ~access ~check_empty attrs access_list acc =
  let modified =
    Attributes.get_written_to attrs
    |> Option.value_map ~default:[] ~f:(fun (_, modified) -> [ImpurityDomain.WrittenTo modified])
  in
  let invalid_and_modified =
    match Attributes.get_invalid attrs with
    | None | Some (Invalidation.ConstantDereference _, _) ->
        modified
    | Some (invalidation, trace) ->
        ImpurityDomain.Invalid (invalidation, trace) :: modified
  in
  if check_empty && List.is_empty invalid_and_modified then
    L.(die InternalError) "Address is modified without being written to or invalidated."
  else
    let access_list = ignore_array_index access :: access_list in
    ( access_list
    , List.fold_left ~init:acc
        ~f:(fun acc trace ->
          ImpurityDomain.ModifiedVarMap.add pvar
            {ordered_access_list= List.rev access_list; trace}
            acc )
        invalid_and_modified )


let add_to_modified pname ~pvar ~access ~addr pre_heap post modified_vars =
  let rec aux (access_list, modified_vars) ~addr_to_explore ~visited =
    match addr_to_explore with
    | [] ->
        modified_vars
    | (access, addr) :: addr_to_explore -> (
        if AbstractValue.Set.mem addr visited then
          aux (access_list, modified_vars) ~addr_to_explore ~visited
        else
          let edges_pre_opt = UnsafeMemory.find_opt addr pre_heap in
          let edges_post_opt = UnsafeMemory.find_opt addr post.BaseDomain.heap in
          let attrs_post_opt = UnsafeAttributes.find_opt addr post.BaseDomain.attrs in
          let visited = AbstractValue.Set.add addr visited in
          match (edges_pre_opt, edges_post_opt, attrs_post_opt) with
          | None, None, None ->
              aux (access_list, modified_vars) ~addr_to_explore ~visited
          | Some _, None, None ->
              L.debug Analysis Verbose
                "%a is in the pre but not the post of the call to %a@\n\
                 callee heap pre: @[%a@]@\n\
                 callee post: @[%a@]@\n"
                AbstractValue.pp addr Procname.pp pname UnsafeMemory.pp pre_heap BaseDomain.pp post ;
              aux (access_list, modified_vars) ~addr_to_explore ~visited
          | None, _, attrs_post_opt ->
              let attrs_post = Option.value ~default:Attributes.empty attrs_post_opt in
              aux
                (add_invalid_and_modified ~pvar ~access ~check_empty:false attrs_post access_list
                   modified_vars )
                ~addr_to_explore ~visited
          | Some edges_pre, edges_post_opt, attrs_post_opt -> (
              let attrs_post = Option.value ~default:Attributes.empty attrs_post_opt in
              let edges_post = Option.value ~default:UnsafeMemory.Edges.empty edges_post_opt in
              match get_matching_dest_addr_opt ~edges_pre ~edges_post with
              | Some addr_list ->
                  (* we have multiple accesses to check here up to
                     currently accumulated point. We need to check them
                     one by one rather than all at once to ensure that
                     accesses are not collapsed together but kept
                     separately. *)
                  List.fold ~init:modified_vars
                    ~f:(fun acc addr ->
                      aux
                        (add_invalid_and_modified ~pvar ~access attrs_post ~check_empty:false
                           access_list acc )
                        ~addr_to_explore:(addr :: addr_to_explore) ~visited )
                    addr_list
              | None ->
                  aux
                    (add_invalid_and_modified ~pvar ~access ~check_empty:false attrs_post
                       access_list modified_vars )
                    ~addr_to_explore ~visited ) )
  in
  aux ([], modified_vars) ~addr_to_explore:[(access, addr)] ~visited:AbstractValue.Set.empty


let get_modified_params pname (summary : AbductiveDomain.Summary.t) post pre_heap formals =
  List.fold_left formals ~init:ImpurityDomain.ModifiedVarMap.bottom ~f:(fun acc (name, typ, _) ->
      let pvar = Pvar.mk name pname in
      match Stack.find_opt (Var.of_pvar pvar) (summary :> AbductiveDomain.t) with
      | Some (addr, _) when Typ.is_pointer typ -> (
        match UnsafeMemory.find_opt addr pre_heap with
        | Some edges_pre ->
            UnsafeMemory.Edges.fold edges_pre ~init:acc ~f:(fun acc (access, (addr, _)) ->
                add_to_modified pname ~pvar ~access ~addr pre_heap post acc )
        | None ->
            debug "The address %a is not materialized in pre-heap.\n" AbstractValue.pp addr ;
            acc )
      | _ ->
          acc )


let get_modified_globals pname (summary : AbductiveDomain.Summary.t) pre_heap post =
  Stack.fold
    (fun var (addr, _) modified_globals ->
      if Var.is_global var then
        (* since global vars are rooted in the stack, we don't have
           access here but we still want to pick up changes to
           globals. *)
        add_to_modified pname
          ~pvar:(Option.value_exn (Var.get_pvar var))
          ~access:Access.Dereference ~addr pre_heap post modified_globals
      else modified_globals )
    (summary :> AbductiveDomain.t)
    ImpurityDomain.ModifiedVarMap.bottom


let is_modeled_pure tenv pname =
  PurityModels.ProcName.dispatch tenv pname |> Option.exists ~f:PurityDomain.is_pure


(** Given Pulse summary, extract impurity info, i.e. parameters and global variables that are
    modified by the function and skipped functions. *)
let extract_impurity tenv pname formals (exec_state : ExecutionDomain.summary) : ImpurityDomain.t =
  let astate, exited =
    match exec_state with
    | ExitProgram astate ->
        (astate, true)
    | ContinueProgram astate | ExceptionRaised astate ->
        (astate, false)
    | AbortProgram astate
    | LatentAbortProgram {astate}
    | LatentInvalidAccess {astate}
    | LatentSpecializedTypeIssue {astate} ->
        (astate, false)
  in
  let pre_heap = (AbductiveDomain.Summary.get_pre astate).BaseDomain.heap in
  let post = AbductiveDomain.Summary.get_post astate in
  let modified_params = get_modified_params pname astate post pre_heap formals in
  let modified_globals = get_modified_globals pname astate pre_heap post in
  let skipped_calls =
    (* In hack we still have many unmodelled unknown hhbc instructions and other functions.
       Also, due to the depth-based capture implementation it is impossible to fully avoid unknown calls. *)
    if Language.curr_language_is Hack then SkippedCalls.empty
    else
      SkippedCalls.filter
        (fun proc_name _ ->
          PurityChecker.should_report proc_name && not (is_modeled_pure tenv proc_name) )
        (AbductiveDomain.Summary.get_skipped_calls astate)
  in
  {modified_globals; modified_params; skipped_calls; exited}


let add_modified_ltr param_source set acc =
  ImpurityDomain.ModifiedVarMap.fold (ImpurityDomain.add_to_errlog ~nesting:1 param_source) set acc


let report_immutable_field_modifications tenv impurity_astate proc_desc err_log =
  let proc_name = Procdesc.get_proc_name proc_desc in
  let loc = Procdesc.get_loc proc_desc in
  ImpurityDomain.get_modified_immutables_opt tenv impurity_astate
  |> Option.iter ~f:(fun (modified_params, modified_globals) ->
         let immutable_fun_desc =
           F.asprintf "Function %a modifies immutable fields" Procname.pp proc_name
         in
         let immutable_fun_ltr = Errlog.make_trace_element 0 loc immutable_fun_desc [] in
         let ltr =
           immutable_fun_ltr
           :: add_modified_ltr Formal modified_params (add_modified_ltr Global modified_globals [])
         in
         Reporting.log_issue proc_desc err_log ~loc ~ltr Impurity IssueType.modifies_immutable
           immutable_fun_desc )


let report_impure_pulse proc_desc err_log ~desc =
  let proc_name = Procdesc.get_proc_name proc_desc in
  let loc = Procdesc.get_loc proc_desc in
  let impure_fun_desc =
    F.asprintf "Impure function %a with %s pulse summary" Procname.pp proc_name desc
  in
  let impure_fun_ltr = Errlog.make_trace_element 0 loc impure_fun_desc [] in
  Reporting.log_issue proc_desc err_log ~loc ~ltr:[impure_fun_ltr] Impurity
    IssueType.impure_function impure_fun_desc


let report_impure_all proc_desc err_log
    ImpurityDomain.{modified_globals; modified_params; skipped_calls} =
  let proc_name = Procdesc.get_proc_name proc_desc in
  let loc = Procdesc.get_loc proc_desc in
  let skipped_functions =
    SkippedCalls.fold
      (fun proc_name trace acc ->
        Trace.add_to_errlog ~nesting:1 ~include_value_history:false
          ~pp_immediate:(fun fmt ->
            F.fprintf fmt "call to skipped function %a occurs here" Procname.pp proc_name )
          trace acc )
      skipped_calls []
  in
  let impure_fun_desc = F.asprintf "Impure function %a" Procname.pp proc_name in
  let impure_fun_ltr = Errlog.make_trace_element 0 loc impure_fun_desc [] in
  let ltr =
    impure_fun_ltr
    :: add_modified_ltr Formal modified_params
         (add_modified_ltr Global modified_globals skipped_functions)
  in
  Reporting.log_issue proc_desc err_log ~loc ~ltr Impurity IssueType.impure_function impure_fun_desc


let checker {IntraproceduralAnalysis.proc_desc; tenv; err_log}
    (pulse_summary_opt : PulseSummary.t option) =
  match pulse_summary_opt with
  | None ->
      report_impure_pulse proc_desc err_log ~desc:"no"
  | Some {main= {pre_post_list= []}} ->
      report_impure_pulse proc_desc err_log ~desc:"empty"
  | Some {main= {pre_post_list}} ->
      let formals = Procdesc.get_formals proc_desc in
      let proc_name = Procdesc.get_proc_name proc_desc in
      let impurity_astate =
        List.fold pre_post_list ~init:ImpurityDomain.pure ~f:(fun acc exec_state ->
            let impurity_astate = extract_impurity tenv proc_name formals exec_state in
            ImpurityDomain.join acc impurity_astate )
      in
      if PurityChecker.should_report proc_name && not (ImpurityDomain.is_pure impurity_astate) then (
        if Config.impurity_report_immutable_modifications then
          report_immutable_field_modifications tenv impurity_astate proc_desc err_log ;
        report_impure_all proc_desc err_log impurity_astate )
