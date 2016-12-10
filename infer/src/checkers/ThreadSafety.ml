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
    type summary = ThreadSafetyDomain.astate

    let update_payload astate payload =
      { payload with Specs.threadsafety = Some astate }

    let read_from_payload payload =
      payload.Specs.threadsafety
  end)

(* we want to consider Builder classes and other safe immutablility-ensuring patterns as
   thread-safe. we are overly friendly about this for now; any class whose name ends with `Builder`
   is assumed to be thread-safe. in the future, we can ask for builder classes to be annotated with
   @Builder and verify that annotated classes satisfy the expected invariants. *)
let is_builder_class class_name =
  String.is_suffix ~suffix:"Builder" class_name

let is_call_to_builder_class_method = function
  | Procname.Java java_pname -> is_builder_class (Procname.java_get_class_name java_pname)
  | _ -> false

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
    | None

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
              None
        end
    | pname when Procname.equal pname BuiltinDecl.__set_locked_attribute ->
        Lock
    | pname when Procname.equal pname BuiltinDecl.__delete_locked_attribute ->
        Unlock
    | _ ->
        None

  let add_path_to_state exp typ loc path_state =
    IList.fold_left
      (fun acc rawpath ->
         ThreadSafetyDomain.PathDomain.add_sink (ThreadSafetyDomain.make_access rawpath loc) acc)
      path_state
      (AccessPath.of_exp exp typ ~f_resolve_id:(fun _ -> None))

  let exec_instr ((lockstate, (readstate,writestate)) as astate) { ProcData.pdesc; tenv; } _ =
    let is_unprotected is_locked =
      not is_locked && not (Procdesc.is_java_synchronized pdesc) in
    function
    | Sil.Call (_, Const (Cfun pn), _, loc, _) ->
        begin
          (* assuming that modeled procedures do not have useful summaries *)
          match get_lock_model pn with
          | Lock ->
              true, snd astate
          | Unlock ->
              false, snd astate
          | None ->
              begin
                match Summary.read_summary pdesc pn with
                | Some (callee_lockstate, (callee_reads, callee_writes)) ->
                    let lockstate' = callee_lockstate || lockstate in
                    let _, read_writestate' =
                      (* TODO (14842325): report on constructors that aren't threadsafe
                         (e.g., constructors that access static fields) *)
                      if is_unprotected lockstate' &&
                         not (is_initializer tenv pn) &&
                         not (is_call_to_builder_class_method pn)
                      then
                        let call_site = CallSite.make pn loc in
                        let callee_readstate =
                          ThreadSafetyDomain.PathDomain.with_callsite callee_reads call_site in
                        let callee_writestate =
                          ThreadSafetyDomain.PathDomain.with_callsite callee_writes call_site in
                        let callee_astate =
                          callee_lockstate, (callee_readstate, callee_writestate) in
                        ThreadSafetyDomain.join callee_astate astate
                      else
                        astate in
                    lockstate', read_writestate'
                | None ->
                    astate
              end
        end

    | Sil.Store ((Lfield ( _, _, typ) as lhsfield) , _, _, loc) ->
        if is_unprotected lockstate then (* abstracts no lock being held*)
          (lockstate, (readstate, add_path_to_state lhsfield typ loc writestate))
        else astate

    (* Note: it appears that the third arg of Store is never an Lfield in the targets of,
       our translations, which is why we have not covered that case. *)
    | Sil.Store (_, _, Lfield _, _) ->
        failwith "Unexpected store instruction with rhs field"

    | Sil.Load (_, (Lfield ( _, _, typ) as rhsfield) , _, loc) ->
        if is_unprotected lockstate then (* abstracts no lock being held*)
          (lockstate, (add_path_to_state rhsfield typ loc readstate, writestate))
        else astate

    |  _  ->
        astate
end

module Analyzer =
  AbstractInterpreter.Make
    (ProcCfg.Normal)
    (Scheduler.ReversePostorder)
    (TransferFunctions)

module Interprocedural = Analyzer.Interprocedural (Summary)

(*This is a "checker"*)
let method_analysis callback =
  let proc_desc = callback.Callbacks.proc_desc in
  let opost = Interprocedural.checker callback ProcData.empty_extras in
  match opost with
  | Some post ->  (* I am printing to commandline and out to cater to javac and buck*)
      (L.stdout  "\n Procedure: %s@ "
         (Procname.to_string (Procdesc.get_proc_name proc_desc) )
      );
      L.stdout "\n POST: %a\n" ThreadSafetyDomain.pp post;
      (L.out  "\n Procedure: %s@ "
         (Procname.to_string (Procdesc.get_proc_name proc_desc) )
      );
      L.out "\n POST: %a\n" ThreadSafetyDomain.pp post
  | None -> ()


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
      match Interprocedural.checker callback_arg ProcData.empty_extras with
      | Some post -> post
      | None -> ThreadSafetyDomain.initial
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
    | Some (_, (_, callee_trace)) -> callee_trace
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
    (fun proc_env ( _,( _, writestate)) ->
       if should_report_on_proc proc_env then
         report_thread_safety_errors proc_env writestate
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
