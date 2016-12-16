(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

module F = Format
module L = Logging


module Summary = Summary.Make (struct
    type summary = ThreadSafetyDomain.summary

    let update_payload summary payload =
      { payload with Specs.threadsafety = Some summary }

    let read_from_payload payload =
      payload.Specs.threadsafety
  end)

(* we want to consider Builder classes and other safe immutablility-ensuring patterns as
   thread-safe. we are overly friendly about this for now; any class whose name ends with `Builder`
   is assumed to be thread-safe. in the future, we can ask for builder classes to be annotated with
   @Builder and verify that annotated classes satisfy the expected invariants. *)
let is_builder_class class_name =
  String.is_suffix ~suffix:"Builder" class_name

(* similarly, we assume that immutable classes safely encapsulate their state *)
let is_immutable_collection_class class_name tenv =
  let immutable_collections = [
    "com.google.common.collect.ImmutableCollection";
    "com.google.common.collect.ImmutableMap";
    "com.google.common.collect.ImmutableTable";
  ] in
  PatternMatch.supertype_exists
    tenv (fun typename _ -> IList.mem (=) (Typename.name typename) immutable_collections) class_name

let is_call_to_builder_class_method = function
  | Procname.Java java_pname -> is_builder_class (Procname.java_get_class_name java_pname)
  | _ -> false

let is_call_to_immutable_collection_method tenv = function
  | Procname.Java java_pname ->
      is_immutable_collection_class (Procname.java_get_class_type_name java_pname) tenv
  | _ ->
      false

let is_initializer tenv proc_name =
  Procname.is_constructor proc_name ||
  Procname.is_class_initializer proc_name ||
  FbThreadSafety.is_custom_init tenv proc_name

module TransferFunctions (CFG : ProcCfg.S) = struct
  module CFG = CFG
  module Domain = ThreadSafetyDomain
  type extras = ProcData.no_extras

  type lock_model =
    | Lock
    | Unlock
    | NoEffect

  let get_lock_model = function
    | Procname.Java java_pname ->
        begin
          match Procname.java_get_class_name java_pname, Procname.java_get_method java_pname with
          | "java.util.concurrent.locks.Lock", "lock" ->
              Lock
          | ("java.util.concurrent.locks.ReentrantLock"
            | "java.util.concurrent.locks.ReentrantReadWriteLock$ReadLock"
            | "java.util.concurrent.locks.ReentrantReadWriteLock$WriteLock"),
            ("lock" | "tryLock" | "lockInterruptibly") ->
              Lock
          | ("java.util.concurrent.locks.Lock"
            |"java.util.concurrent.locks.ReentrantLock"
            | "java.util.concurrent.locks.ReentrantReadWriteLock$ReadLock"
            | "java.util.concurrent.locks.ReentrantReadWriteLock$WriteLock"),
            "unlock" ->
              Unlock
          | _ ->
              NoEffect
        end
    | pname when Procname.equal pname BuiltinDecl.__set_locked_attribute ->
        Lock
    | pname when Procname.equal pname BuiltinDecl.__delete_locked_attribute ->
        Unlock
    | _ ->
        NoEffect

  let resolve_id (id_map : IdAccessPathMapDomain.astate) id =
    try Some (IdAccessPathMapDomain.find id id_map)
    with Not_found -> None

  let add_path_to_state exp typ loc path_state id_map owned =
    (* remove the last field of the access path, if it has any *)
    let truncate = function
      | base, []
      | base, _ :: [] -> base, []
      | base, accesses -> base, IList.rev (IList.tl (IList.rev accesses)) in
    let f_resolve_id = resolve_id id_map in

    IList.fold_left
      (fun acc rawpath ->
         let base_path = truncate rawpath in
         if not (ThreadSafetyDomain.OwnershipDomain.mem base_path owned)
         then
           ThreadSafetyDomain.PathDomain.add_sink (ThreadSafetyDomain.make_access rawpath loc) acc
         else
           acc)
      path_state
      (AccessPath.of_exp exp typ ~f_resolve_id)

  let analyze_id_assignment lhs_id rhs_exp rhs_typ { ThreadSafetyDomain.id_map; } =
    let f_resolve_id = resolve_id id_map in
    match AccessPath.of_lhs_exp rhs_exp rhs_typ ~f_resolve_id with
    | Some rhs_access_path -> IdAccessPathMapDomain.add lhs_id rhs_access_path id_map
    | None -> id_map

  let exec_instr
      ({ ThreadSafetyDomain.locks; reads; writes; id_map; owned; } as astate)
      { ProcData.pdesc; tenv; } _ =

    let is_allocation pn =
      Procname.equal pn BuiltinDecl.__new ||
      Procname.equal pn BuiltinDecl.__new_array in
    let is_unprotected is_locked =
      not is_locked && not (Procdesc.is_java_synchronized pdesc) in
    let f_resolve_id = resolve_id id_map in
    function
    | Sil.Call (Some (lhs_id, lhs_typ), Const (Cfun pn), _, _, _) when is_allocation pn ->
        begin
          match AccessPath.of_lhs_exp (Exp.Var lhs_id) lhs_typ ~f_resolve_id with
          | Some lhs_access_path ->
              let owned' = ThreadSafetyDomain.OwnershipDomain.add lhs_access_path owned in
              { astate with owned = owned'; }
          | None ->
              astate
        end
    | Sil.Call (_, Const (Cfun pn), _, loc, _) ->
        begin
          (* assuming that modeled procedures do not have useful summaries *)
          match get_lock_model pn with
          | Lock ->
              { astate with locks = true; }
          | Unlock ->
              { astate with locks = false; }
          | NoEffect ->
              begin
                match Summary.read_summary pdesc pn with
                | Some (callee_locks, callee_reads, callee_writes) ->
                    let locks' = callee_locks || locks in
                    let astate' =
                      (* TODO (14842325): report on constructors that aren't threadsafe
                         (e.g., constructors that access static fields) *)
                      if is_unprotected locks' &&
                         not (is_initializer tenv pn) &&
                         not (is_call_to_builder_class_method pn) &&
                         not (is_call_to_immutable_collection_method tenv pn)
                      then
                        let call_site = CallSite.make pn loc in
                        let reads' =
                          ThreadSafetyDomain.PathDomain.with_callsite callee_reads call_site
                          |> ThreadSafetyDomain.PathDomain.join reads in
                        let writes' =
                          ThreadSafetyDomain.PathDomain.with_callsite callee_writes call_site
                          |> ThreadSafetyDomain.PathDomain.join writes in
                        { astate with reads = reads'; writes = writes'; }
                      else
                        astate in
                    { astate' with locks = locks'; }
                | None ->
                    astate
              end
        end

    | Sil.Store (Exp.Lvar lhs_pvar, lhs_typ, rhs_exp, _) when Pvar.is_frontend_tmp lhs_pvar ->
        let id_map' = analyze_id_assignment (Var.of_pvar lhs_pvar) rhs_exp lhs_typ astate in
        { astate with id_map = id_map'; }

    | Sil.Store (lhs_exp, lhs_typ, rhs_exp, loc) ->
        let writes' =
          match lhs_exp with
          | Lfield ( _, _, typ) when is_unprotected locks -> (* abstracts no lock being held *)
              add_path_to_state lhs_exp typ loc writes id_map owned
          | _ -> writes in
        (* if rhs is owned, propagate ownership to lhs. otherwise, remove lhs from ownerhsip set
           (since it may have previously held an owned memory loc and is now being reassigned *)
        let owned' =
          match AccessPath.of_lhs_exp lhs_exp lhs_typ ~f_resolve_id,
                AccessPath.of_lhs_exp rhs_exp lhs_typ ~f_resolve_id with
          | Some lhs_access_path, Some rhs_access_path ->
              if ThreadSafetyDomain.OwnershipDomain.mem rhs_access_path owned
              then ThreadSafetyDomain.OwnershipDomain.add lhs_access_path owned
              else ThreadSafetyDomain.OwnershipDomain.remove lhs_access_path owned
          | Some lhs_access_path, None ->
              ThreadSafetyDomain.OwnershipDomain.remove lhs_access_path owned
          | _ -> owned in
        { astate with writes = writes'; owned = owned'; }

    | Sil.Load (lhs_id, rhs_exp, rhs_typ, loc) ->
        let id_map' = analyze_id_assignment (Var.of_id lhs_id) rhs_exp rhs_typ astate in
        let reads' =
          match rhs_exp with
          | Lfield ( _, _, typ) when is_unprotected locks ->
              add_path_to_state rhs_exp typ loc reads id_map owned
          | _ ->
              reads in
        (* if rhs is owned, propagate ownership to lhs *)
        let owned' =
          match AccessPath.of_lhs_exp rhs_exp rhs_typ ~f_resolve_id with
          | Some rhs_access_path
            when ThreadSafetyDomain.OwnershipDomain.mem rhs_access_path owned ->
              ThreadSafetyDomain.OwnershipDomain.add (AccessPath.of_id lhs_id rhs_typ) owned
          | _ ->
              owned in
        { astate with Domain.reads = reads'; id_map = id_map'; owned = owned'; }
    |  _  ->
        astate
end

module Analyzer =
  AbstractInterpreter.Make
    (ProcCfg.Normal)
    (Scheduler.ReversePostorder)
    (TransferFunctions)

module Interprocedural = AbstractInterpreter.Interprocedural (Summary)

(* convert the abstract state to a summary by dropping the id map *)
let compute_post pdesc =
  match Analyzer.compute_post pdesc with
  | Some { locks; reads; writes; } -> Some (locks, reads, writes)
  | None -> None

(*This is a "checker"*)
let method_analysis callback =
  Interprocedural.compute_and_store_post
    ~compute_post
    ~make_extras:ProcData.make_empty_extras
    callback
  |> ignore

(* a results table is a Map where a key is an a procedure environment,
   i.e., something of type Idenv.t * Tenv.t * Procname.t * Procdesc.t
*)
module ResultsTableType = Caml.Map.Make (struct
    type t = Idenv.t * Tenv.t * Procname.t * Procdesc.t
    let compare (_, _, pn1, _) (_,_,pn2,_) =  Procname.compare pn1 pn2
  end)

let should_report_on_proc (_,tenv,proc_name,proc_desc) =
  not (is_initializer tenv proc_name) &&
  not (Procname.java_is_autogen_method proc_name) &&
  Procdesc.get_access proc_desc <> PredSymb.Private

(* creates a map from proc_envs to postconditions *)
let make_results_table get_proc_desc file_env =
  (* make a Map sending each element e of list l to (f e) *)
  let map_post_computation_over_procs f l =
    IList.fold_left (fun m p -> ResultsTableType.add p (f p) m
                    ) ResultsTableType.empty l
  in
  let compute_post_for_procedure = (* takes proc_env as arg *)
    fun (idenv, tenv, proc_name, proc_desc) ->
      let callback_arg =
        {Callbacks.get_proc_desc; get_procs_in_file = (fun _ -> []);
         idenv; tenv; proc_name; proc_desc} in
      match
        Interprocedural.compute_and_store_post
          ~compute_post
          ~make_extras:ProcData.make_empty_extras
          callback_arg with
      | Some post -> post
      | None ->
          ThreadSafetyDomain.LocksDomain.initial,
          ThreadSafetyDomain.PathDomain.initial,
          ThreadSafetyDomain.PathDomain.initial
  in
  map_post_computation_over_procs compute_post_for_procedure file_env

let get_current_class_and_threadsafe_superclasses tenv pname =
  match pname with
  | Procname.Java java_pname ->
      let current_class = Procname.java_get_class_type_name java_pname in
      let thread_safe_annotated_classes = PatternMatch.find_superclasses_with_attributes
          Annotations.ia_is_thread_safe tenv current_class
      in
      Some (current_class,thread_safe_annotated_classes)
  | _ -> None  (*shouldn't happen*)

(** The addendum message says that a superclass is marked @ThreadSafe,
    when the current class is not so marked*)
let calculate_addendum_message tenv pname =
  match get_current_class_and_threadsafe_superclasses tenv pname with
  | Some (current_class,thread_safe_annotated_classes) ->
      if not (IList.mem Typename.equal current_class thread_safe_annotated_classes) then
        match thread_safe_annotated_classes with
        | hd::_ -> F.asprintf "\n Note: Superclass %a is marked @ThreadSafe." Typename.pp hd
        | [] -> ""
      else ""
  | _ -> ""

let report_thread_safety_errors ( _, tenv, pname, pdesc) trace =
  let open ThreadSafetyDomain in
  let trace_of_pname callee_pname =
    match Summary.read_summary pdesc callee_pname with
    | Some (_, _, writes) -> writes
    | _ -> PathDomain.initial in
  let report_one_path ((_, sinks) as path) =
    let pp_accesses fmt sink =
      let _, accesses = PathDomain.Sink.kind sink in
      AccessPath.pp_access_list fmt accesses in
    let initial_sink, _ = IList.hd (IList.rev sinks) in
    let final_sink, _ = IList.hd sinks in
    let initial_sink_site = PathDomain.Sink.call_site initial_sink in
    let final_sink_site = PathDomain.Sink.call_site final_sink in
    let desc_of_sink sink =
      if CallSite.equal (PathDomain.Sink.call_site sink) final_sink_site
      then
        Format.asprintf "access to %a" pp_accesses sink
      else
        Format.asprintf
          "call to %a" Procname.pp (CallSite.pname (PathDomain.Sink.call_site sink)) in
    let loc = CallSite.loc (PathDomain.Sink.call_site initial_sink) in
    let ltr = PathDomain.to_sink_loc_trace ~desc_of_sink path in
    let msg = Localise.to_string Localise.thread_safety_error in
    let description =
      Format.asprintf "Public method %a%s writes to field %a outside of synchronization.%s"
        Procname.pp pname
        (if CallSite.equal final_sink_site initial_sink_site then "" else " indirectly")
        pp_accesses final_sink
        (calculate_addendum_message tenv pname) in
    let exn = Exceptions.Checkers (msg, Localise.verbatim_desc description) in
    Reporting.log_error pname ~loc ~ltr exn in

  IList.iter
    report_one_path
    (PathDomain.get_reportable_sink_paths trace ~trace_of_pname)

(* For now, just checks if there is one active element amongst the posts of the analyzed methods.
   This indicates that the method races with itself. To be refined later. *)
let process_results_table tab =
  ResultsTableType.iter   (* report errors for each method *)
    (fun proc_env (_, _, writes) ->
       if should_report_on_proc proc_env then
         report_thread_safety_errors proc_env writes
       else ()
    )
    tab

(* Currently we analyze if there is an @ThreadSafe annotation on at least one of
   the classes in a file. This might be tightened in future or even broadened in future
   based on other criteria *)
let should_report_on_file file_env =
  let current_class_or_super_marked_threadsafe =
    fun (_, tenv, pname, _) ->
      match get_current_class_and_threadsafe_superclasses tenv pname with
      | Some (_, thread_safe_annotated_classes) ->
          not (thread_safe_annotated_classes = [])
      | _ -> false
  in
  let current_class_marked_not_threadsafe =
    fun (_, tenv, pname, _) ->
      PatternMatch.check_current_class_attributes Annotations.ia_is_not_thread_safe tenv pname
  in
  not (IList.exists current_class_marked_not_threadsafe file_env) &&
  IList.exists current_class_or_super_marked_threadsafe file_env

(*This is a "cluster checker" *)
(*Gathers results by analyzing all the methods in a file, then post-processes
  the results to check (approximation of) thread safety *)
(* file_env: (Idenv.t * Tenv.t * Procname.t * Procdesc.t) list *)
let file_analysis _ _ get_procdesc file_env =
  let tab = make_results_table get_procdesc file_env in
  if should_report_on_file file_env then
    process_results_table tab
  else ()

      (*
    Todo:
    0. Refactor abstract domain to use records rather than tuples
    1. Track line numbers of accesses
    2. Track protected writes and reads, too; if we have a write and a read where
    one is non-protected then we have potential race (including protected write, unprotected read
    3. Lotsa other stuff
      *)
