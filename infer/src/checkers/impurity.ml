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

let get_matching_dest_addr_opt ~edges_pre ~edges_post :
    (BaseMemory.Access.t * AbstractValue.t) list option =
  match
    List.fold2 ~init:(Some [])
      ~f:(fun acc (access, (addr_dest_pre, _)) (_, (addr_dest_post, _)) ->
        if AbstractValue.equal addr_dest_pre addr_dest_post then
          Option.map acc ~f:(fun acc -> (access, addr_dest_pre) :: acc)
        else None )
      (BaseMemory.Edges.bindings edges_pre |> List.sort ~compare:[%compare: BaseMemory.Access.t * _])
      ( BaseMemory.Edges.bindings edges_post
      |> List.sort ~compare:[%compare: BaseMemory.Access.t * _] )
  with
  | Unequal_lengths ->
      debug "Mismatch in pre and post.\n" ;
      None
  | Ok x ->
      x


let ignore_array_index (access : BaseMemory.Access.t) : unit HilExp.Access.t =
  match access with
  | ArrayAccess (typ, _) ->
      ArrayAccess (typ, ())
  | FieldAccess fname ->
      FieldAccess fname
  | Dereference ->
      Dereference
  | TakeAddress ->
      TakeAddress


let add_invalid_and_modified ~pvar ~access ~check_empty attrs access_list acc =
  let modified =
    Attributes.get_written_to attrs
    |> Option.value_map ~default:[] ~f:(fun modified -> [ImpurityDomain.WrittenTo modified])
  in
  let invalid_and_modified =
    match Attributes.get_invalid attrs with
    | None | Some (Invalidation.ConstantDereference _, _) ->
        modified
    | Some invalid ->
        ImpurityDomain.Invalid invalid :: modified
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
          let edges_pre_opt = BaseMemory.find_opt addr pre_heap in
          let cell_post_opt = BaseDomain.find_cell_opt addr post in
          let visited = AbstractValue.Set.add addr visited in
          match (edges_pre_opt, cell_post_opt) with
          | None, None ->
              aux (access_list, modified_vars) ~addr_to_explore ~visited
          | Some _, None ->
              L.debug Analysis Verbose
                "%a is in the pre but not the post of the call to %a@\n\
                 callee heap pre: @[%a@]@\n\
                 callee post: @[%a@]@\n"
                AbstractValue.pp addr Procname.pp pname BaseMemory.pp pre_heap BaseDomain.pp post ;
              aux (access_list, modified_vars) ~addr_to_explore ~visited
          | None, Some (_, attrs_post) ->
              aux
                (add_invalid_and_modified ~pvar ~access ~check_empty:false attrs_post access_list
                   modified_vars)
                ~addr_to_explore ~visited
          | Some edges_pre, Some (edges_post, attrs_post) -> (
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
                         access_list acc)
                      ~addr_to_explore:(addr :: addr_to_explore) ~visited )
                  addr_list
            | None ->
                aux
                  (add_invalid_and_modified ~pvar ~access ~check_empty:false attrs_post access_list
                     modified_vars)
                  ~addr_to_explore ~visited ) )
  in
  aux ([], modified_vars) ~addr_to_explore:[(access, addr)] ~visited:AbstractValue.Set.empty


let get_modified_params pname post_stack pre_heap post formals =
  List.fold_left formals ~init:ImpurityDomain.ModifiedVarMap.bottom ~f:(fun acc (name, typ) ->
      let pvar = Pvar.mk name pname in
      match BaseStack.find_opt (Var.of_pvar pvar) post_stack with
      | Some (addr, _) when Typ.is_pointer typ -> (
        match BaseMemory.find_opt addr pre_heap with
        | Some edges_pre ->
            BaseMemory.Edges.fold edges_pre ~init:acc ~f:(fun acc (access, (addr, _)) ->
                add_to_modified pname ~pvar ~access ~addr pre_heap post acc )
        | None ->
            debug "The address is not materialized in in pre-heap." ;
            acc )
      | _ ->
          acc )


let get_modified_globals pname pre_heap post post_stack =
  BaseStack.fold
    (fun var (addr, _) modified_globals ->
      if Var.is_global var then
        (* since global vars are rooted in the stack, we don't have
           access here but we still want to pick up changes to
           globals. *)
        add_to_modified pname
          ~pvar:(Option.value_exn (Var.get_pvar var))
          ~access:HilExp.Access.Dereference ~addr pre_heap post modified_globals
      else modified_globals )
    post_stack ImpurityDomain.ModifiedVarMap.bottom


let is_modeled_pure tenv pname =
  match PurityModels.ProcName.dispatch tenv pname with
  | Some callee_summary ->
      PurityDomain.is_pure callee_summary
  | None ->
      false


(** Given Pulse summary, extract impurity info, i.e. parameters and global variables that are
    modified by the function and skipped functions. *)
let extract_impurity tenv pname formals (exec_state : ExecutionDomain.t) : ImpurityDomain.t =
  let astate, exited =
    match exec_state with
    | ExitProgram astate ->
        ((astate :> AbductiveDomain.t), true)
    | ContinueProgram astate | ISLLatentMemoryError astate ->
        (astate, false)
    | AbortProgram astate | LatentAbortProgram {astate} ->
        ((astate :> AbductiveDomain.t), false)
  in
  let pre_heap = (AbductiveDomain.get_pre astate).BaseDomain.heap in
  let post = AbductiveDomain.get_post astate in
  let post_stack = post.BaseDomain.stack in
  let modified_params = get_modified_params pname post_stack pre_heap post formals in
  let modified_globals = get_modified_globals pname pre_heap post post_stack in
  let skipped_calls =
    SkippedCalls.filter
      (fun proc_name _ ->
        PurityChecker.should_report proc_name && not (is_modeled_pure tenv proc_name) )
      astate.AbductiveDomain.skipped_calls
  in
  {modified_globals; modified_params; skipped_calls; exited}


let add_modified_ltr param_source set acc =
  ImpurityDomain.ModifiedVarMap.fold (ImpurityDomain.add_to_errlog ~nesting:1 param_source) set acc


let checker {IntraproceduralAnalysis.proc_desc; tenv; err_log}
    (pulse_summary_opt : PulseSummary.t option) =
  let proc_name = Procdesc.get_proc_name proc_desc in
  let pname_loc = Procdesc.get_loc proc_desc in
  match pulse_summary_opt with
  | None ->
      let impure_fun_desc =
        F.asprintf "Impure function %a with no pulse summary" Procname.pp proc_name
      in
      let impure_fun_ltr = Errlog.make_trace_element 0 pname_loc impure_fun_desc [] in
      Reporting.log_issue proc_desc err_log ~loc:pname_loc ~ltr:[impure_fun_ltr] Impurity
        IssueType.impure_function impure_fun_desc
  | Some [] ->
      let impure_fun_desc =
        F.asprintf "Impure function %a with empty pulse summary" Procname.pp proc_name
      in
      let impure_fun_ltr = Errlog.make_trace_element 0 pname_loc impure_fun_desc [] in
      Reporting.log_issue proc_desc err_log ~loc:pname_loc ~ltr:[impure_fun_ltr] Impurity
        IssueType.impure_function impure_fun_desc
  | Some pre_posts ->
      let formals = Procdesc.get_formals proc_desc in
      let (ImpurityDomain.{modified_globals; modified_params; skipped_calls} as impurity_astate) =
        List.fold pre_posts ~init:ImpurityDomain.pure
          ~f:(fun acc (exec_state : ExecutionDomain.summary) ->
            let modified =
              extract_impurity tenv proc_name formals (exec_state :> ExecutionDomain.t)
            in
            ImpurityDomain.join acc modified )
      in
      if PurityChecker.should_report proc_name then (
        if Config.report_immutable_modifications then
          ImpurityDomain.get_modified_immutables_opt tenv impurity_astate
          |> Option.iter ~f:(fun (modified_params, modified_globals) ->
                 let immutable_fun_desc =
                   F.asprintf "Function %a modifies immutable fields" Procname.pp proc_name
                 in
                 let immutable_fun_ltr =
                   Errlog.make_trace_element 0 pname_loc immutable_fun_desc []
                 in
                 let ltr =
                   immutable_fun_ltr
                   :: add_modified_ltr Formal modified_params
                        (add_modified_ltr Global modified_globals [])
                 in
                 Reporting.log_issue proc_desc err_log ~loc:pname_loc ~ltr Impurity
                   IssueType.modifies_immutable immutable_fun_desc ) ;
        if not (ImpurityDomain.is_pure impurity_astate) then
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
          let impure_fun_ltr = Errlog.make_trace_element 0 pname_loc impure_fun_desc [] in
          let ltr =
            impure_fun_ltr
            :: add_modified_ltr Formal modified_params
                 (add_modified_ltr Global modified_globals skipped_functions)
          in
          Reporting.log_issue proc_desc err_log ~loc:pname_loc ~ltr Impurity
            IssueType.impure_function impure_fun_desc )
