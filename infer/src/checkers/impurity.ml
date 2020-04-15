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

let debug fmt = L.(debug Analysis Verbose fmt)

let get_matching_dest_addr_opt ~edges_pre ~edges_post :
    (BaseMemory.Access.t * AbstractValue.t) list option =
  match
    List.fold2 ~init:(Some [])
      ~f:(fun acc (access, (addr_dest_pre, _)) (_, (addr_dest_post, _)) ->
        if AbstractValue.equal addr_dest_pre addr_dest_post then
          Option.map acc ~f:(fun acc -> (access, addr_dest_pre) :: acc)
        else None )
      (BaseMemory.Edges.bindings edges_pre)
      (BaseMemory.Edges.bindings edges_post)
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


let add_invalid_and_modified ~var ~access ~check_empty attrs acc : ImpurityDomain.ModifiedVarSet.t =
  let modified =
    Attributes.get_written_to attrs
    |> Option.value_map ~default:[] ~f:(fun modified -> [ImpurityDomain.WrittenTo modified])
  in
  let invalid_and_modified =
    match Attributes.get_invalid attrs with
    | None | Some (PulseInvalidation.ConstantDereference _, _) ->
        modified
    | Some invalid ->
        ImpurityDomain.Invalid invalid :: modified
  in
  if check_empty && List.is_empty invalid_and_modified then
    L.(die InternalError) "Address is modified without being written to or invalidated."
  else
    List.fold_left ~init:acc
      ~f:(fun acc trace ->
        ImpurityDomain.ModifiedVarSet.add {var; access= ignore_array_index access; trace} acc )
      invalid_and_modified


let add_to_modified ~var ~access ~addr pre_heap post modified_vars =
  let rec aux modified_vars ~addr_to_explore ~visited : ImpurityDomain.ModifiedVarSet.t =
    match addr_to_explore with
    | [] ->
        modified_vars
    | (access, addr) :: addr_to_explore -> (
        if AbstractValue.Set.mem addr visited then aux modified_vars ~addr_to_explore ~visited
        else
          let edges_pre_opt = BaseMemory.find_opt addr pre_heap in
          let cell_post_opt = BaseDomain.find_cell_opt addr post in
          let visited = AbstractValue.Set.add addr visited in
          match (edges_pre_opt, cell_post_opt) with
          | None, None ->
              aux modified_vars ~addr_to_explore ~visited
          | Some _, None ->
              L.(die InternalError)
                "It is unexpected to have an address which has a binding in pre but not in post!"
          | None, Some (_, attrs_post) ->
              aux
                (add_invalid_and_modified ~var ~access ~check_empty:false attrs_post modified_vars)
                ~addr_to_explore ~visited
          | Some edges_pre, Some (edges_post, attrs_post) -> (
            match get_matching_dest_addr_opt ~edges_pre ~edges_post with
            | Some addr_list ->
                aux
                  (add_invalid_and_modified ~var ~access attrs_post ~check_empty:false
                     modified_vars)
                  ~addr_to_explore:(addr_list @ addr_to_explore) ~visited
            | None ->
                aux
                  (add_invalid_and_modified ~var ~access ~check_empty:true attrs_post modified_vars)
                  ~addr_to_explore ~visited ) )
  in
  aux modified_vars ~addr_to_explore:[(access, addr)] ~visited:AbstractValue.Set.empty


let get_modified_params pname post_stack pre_heap post formals =
  List.fold_left formals ~init:ImpurityDomain.ModifiedVarSet.empty ~f:(fun acc (name, typ) ->
      let var = Var.of_pvar (Pvar.mk name pname) in
      match BaseStack.find_opt var post_stack with
      | Some (addr, _) when Typ.is_pointer typ -> (
        match BaseMemory.find_opt addr pre_heap with
        | Some edges_pre ->
            BaseMemory.Edges.fold
              (fun access (addr, _) acc -> add_to_modified ~var ~access ~addr pre_heap post acc)
              edges_pre acc
        | None ->
            debug "The address is not materialized in in pre-heap." ;
            acc )
      | _ ->
          acc )


let get_modified_globals pre_heap post post_stack =
  BaseStack.fold
    (fun var (addr, _) modified_globals ->
      if Var.is_global var then
        (* since global vars are rooted in the stack, we don't have
           access here but we still want to pick up changes to
           globals. *)
        add_to_modified ~var ~access:HilExp.Access.Dereference ~addr pre_heap post modified_globals
      else modified_globals )
    post_stack ImpurityDomain.ModifiedVarSet.empty


let is_modeled_pure tenv pname =
  match PurityModels.ProcName.dispatch tenv pname with
  | Some callee_summary ->
      PurityDomain.is_pure callee_summary
  | None ->
      false


(** Given Pulse summary, extract impurity info, i.e. parameters and global variables that are
    modified by the function and skipped functions. *)
let extract_impurity tenv pdesc (exec_state : PulseExecutionState.t) : ImpurityDomain.t =
  let astate, exited =
    match exec_state with
    | ExitProgram astate ->
        (astate, true)
    | AbortProgram astate | ContinueProgram astate ->
        (astate, false)
  in
  let pre_heap = (PulseAbductiveDomain.get_pre astate).BaseDomain.heap in
  let post = PulseAbductiveDomain.get_post astate in
  let post_stack = post.BaseDomain.stack in
  let pname = Procdesc.get_proc_name pdesc in
  let modified_params =
    Procdesc.get_formals pdesc |> get_modified_params pname post_stack pre_heap post
  in
  let modified_globals = get_modified_globals pre_heap post post_stack in
  let skipped_calls =
    PulseAbductiveDomain.get_skipped_calls astate
    |> PulseSkippedCalls.filter (fun proc_name _ ->
           Purity.should_report proc_name && not (is_modeled_pure tenv proc_name) )
  in
  {modified_globals; modified_params; skipped_calls; exited}


let checker {exe_env; Callbacks.summary} : Summary.t =
  let proc_name = Summary.get_proc_name summary in
  let pdesc = Summary.get_proc_desc summary in
  let pname_loc = Procdesc.get_loc pdesc in
  let tenv = Exe_env.get_tenv exe_env proc_name in
  ( match summary.payloads.pulse with
  | None ->
      let impure_fun_desc =
        F.asprintf "Impure function %a with no pulse summary" Procname.pp proc_name
      in
      let impure_fun_ltr = Errlog.make_trace_element 0 pname_loc impure_fun_desc [] in
      Reporting.log_error summary ~loc:pname_loc ~ltr:[impure_fun_ltr] IssueType.impure_function
        impure_fun_desc
  | Some [] ->
      let impure_fun_desc =
        F.asprintf "Impure function %a with empty pulse summary" Procname.pp proc_name
      in
      let impure_fun_ltr = Errlog.make_trace_element 0 pname_loc impure_fun_desc [] in
      Reporting.log_error summary ~loc:pname_loc ~ltr:[impure_fun_ltr] IssueType.impure_function
        impure_fun_desc
  | Some pre_posts ->
      let (ImpurityDomain.{modified_globals; modified_params; skipped_calls} as impurity_astate) =
        List.fold pre_posts ~init:ImpurityDomain.pure ~f:(fun acc exec_state ->
            let modified = extract_impurity tenv pdesc exec_state in
            ImpurityDomain.join acc modified )
      in
      if Purity.should_report proc_name && not (ImpurityDomain.is_pure impurity_astate) then
        let modified_ltr param_source set acc =
          ImpurityDomain.ModifiedVarSet.fold
            (ImpurityDomain.add_to_errlog ~nesting:1 param_source)
            set acc
        in
        let skipped_functions =
          PulseSkippedCalls.fold
            (fun proc_name trace acc ->
              PulseTrace.add_to_errlog ~nesting:1 ~include_value_history:false
                ~pp_immediate:(fun fmt ->
                  F.fprintf fmt "call to skipped function %a occurs here" Procname.pp proc_name )
                trace acc )
            skipped_calls []
        in
        let impure_fun_desc = F.asprintf "Impure function %a" Procname.pp proc_name in
        let impure_fun_ltr = Errlog.make_trace_element 0 pname_loc impure_fun_desc [] in
        let ltr =
          impure_fun_ltr
          :: modified_ltr Formal modified_params
               (modified_ltr Global modified_globals skipped_functions)
        in
        Reporting.log_error summary ~loc:pname_loc ~ltr IssueType.impure_function impure_fun_desc ) ;
  summary
