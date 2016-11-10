(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils

module F = Format
module L = Logging

module PPrawpath = PrettyPrintable.MakePPSet(struct
    type t = AccessPath.raw
    let compare = AccessPath.raw_compare
    let pp_element = AccessPath.pp_raw
  end)

module LocksDomain =  AbstractDomain.FiniteSet(StringPPSet)

module PathDomain =  AbstractDomain.FiniteSet(PPrawpath)

module ReadWriteDomain = AbstractDomain.Pair (PathDomain) (PathDomain)

module CombinedDomain = AbstractDomain.Pair (LocksDomain) (ReadWriteDomain)
(* a typical element is (){locked}, {vars and fields})  *)


module TransferFunctions (CFG : ProcCfg.S) = struct
  module CFG = CFG
  module Domain = CombinedDomain
  type extras = ProcData.no_extras

  let is_lock_procedure pn = Procname.equal pn BuiltinDecl.__set_locked_attribute

  let is_unlock_procedure pn = Procname.equal pn BuiltinDecl.__delete_locked_attribute

  let add_path_to_state exp typ pathdomainstate =
    IList.fold_left
      (fun pathdomainstate_acc rawpath -> PathDomain.add rawpath pathdomainstate_acc)
      pathdomainstate
      (AccessPath.of_exp exp typ ~f_resolve_id:(fun _ -> None))

  let exec_instr ((lockstate,(readstate,writestate)) as astate) { ProcData.pdesc; } _ =
    let is_unprotected lockstate =
      (not (Procdesc.is_java_synchronized pdesc)) && (LocksDomain.is_empty lockstate)
    in
    function
    | Sil.Call (_, Const (Cfun pn), _, _, _) ->
        if is_lock_procedure pn
        then
          ((LocksDomain.add "locked" lockstate), (readstate,writestate))
        else if is_unlock_procedure pn
        then
          ((LocksDomain.remove "locked" lockstate) , (readstate,writestate))
        else
          astate

    | Sil.Store ((Lfield ( _, _, typ) as lhsfield) , _, _, _) ->
        if is_unprotected lockstate then (* abstracts no lock being held*)
          (lockstate, (readstate, add_path_to_state lhsfield typ writestate))
        else astate

    (* Note: it appears that the third arg of Store is never an Lfield in the targets of,
       our translations, which is why we have not covered that case. *)
    | Sil.Store (_, _, Lfield _, _) ->
        failwith "Unexpected store instruction with rhs field"

    | Sil.Load (_, (Lfield ( _, _, typ) as rhsfield) , _, _) ->
        if is_unprotected lockstate then (* abstracts no lock being held*)
          (lockstate, (add_path_to_state rhsfield typ readstate, writestate))
        else astate

    |  _  ->
        astate
end

module Analyzer =
  AbstractInterpreter.Make
    (ProcCfg.Normal)
    (Scheduler.ReversePostorder)
    (TransferFunctions)

let method_analysis { Callbacks.proc_desc; tenv; } =
  match Analyzer.compute_post (ProcData.make_default proc_desc tenv) with
  | Some post ->  (* I am printing to commandline and out to cater to javac and buck*)
      (L.stdout  "\n Procedure: %s@ "
         (Procname.to_string (Procdesc.get_proc_name proc_desc) )
      );
      L.stdout "\n POST: %a\n" CombinedDomain.pp post;
      (L.out  "\n Procedure: %s@ "
         (Procname.to_string (Procdesc.get_proc_name proc_desc) )
      );
      L.out "\n POST: %a\n" CombinedDomain.pp post
  | None -> ()

(* a results table is a Map where a key is an a procedure environment,
   i.e., something of type Idenv.t * Tenv.t * Procname.t * Procdesc.t
*)
module ResultsTableType = Map.Make (struct
    type t = Idenv.t * Tenv.t * Procname.t * Procdesc.t
    let compare (_, _, pn1, _) (_,_,pn2,_) =  Procname.compare pn1 pn2
  end)

let should_analyze_proc (_,tenv,proc_name,proc_desc) =
  not (FbThreadSafety.is_custom_init tenv proc_name) &&
  not (Procname.java_is_autogen_method proc_name) &&
  not (Procname.is_constructor proc_name) &&
  not (Procname.is_class_initializer proc_name) &&
  Procdesc.get_access proc_desc <> PredSymb.Private

(* creates a map from proc_envs to postconditions *)
let make_results_table file_env =
  let procs_to_analyze = IList.filter should_analyze_proc file_env
  in
  (* make a Map sending each element e of list l to (f e) *)
  let map_post_computation_over_procs f l =
    IList.fold_left (fun m p -> ResultsTableType.add p (f p) m
                    ) ResultsTableType.empty l
  in
  let compute_post_for_procedure = (* takes proc_env as arg *)
    fun (_, tenv, _, proc_desc) ->
      match Analyzer.compute_post (ProcData.make_default proc_desc tenv) with
      | Some post -> post
      | None -> CombinedDomain.initial
  in
  map_post_computation_over_procs compute_post_for_procedure procs_to_analyze

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

let report_thread_safety_errors ( _, tenv, pname, pdesc) writestate =
  let report_one_error access_path =
    let description =
      F.asprintf "Method %a writes to field %a outside of synchronization. %s"
        Procname.pp pname
        AccessPath.pp_access_list access_path
        (calculate_addendum_message tenv pname)
    in
    Checkers.ST.report_error tenv
      pname
      pdesc
      "CHECKERS_THREAD_SAFETY_WARNING"
      (Procdesc.get_loc pdesc)
      description
  in
  IList.iter report_one_error (IList.map snd (PathDomain.elements writestate))


(* For now, just checks if there is one active element amongst the posts of the analyzed methods.
   This indicates that the method races with itself. To be refined later. *)
let process_results_table tab =
  ResultsTableType.iter   (* report errors for each method *)
    (fun proc_env ( _,( _, writestate)) -> report_thread_safety_errors proc_env writestate)
    tab

(* Currently we analyze if there is an @ThreadSafe annotation on at least one of
   the classes in a file. This might be tightened in future or even broadened in future
   based on other criteria *)
let should_analyze_file file_env =
  let current_class_or_super_marked_threadsafe =
    fun (_, tenv, pname, _) ->
      match get_current_class_and_threadsafe_superclasses tenv pname with
      | Some (_, thread_safe_annotated_classes) ->
          not (thread_safe_annotated_classes = [])
      | _ -> false
  in
  IList.exists current_class_or_super_marked_threadsafe file_env

(*Gathers results by analyzing all the methods in a file, then post-processes
  the results to check (approximation of) thread safety *)
(* file_env: (Idenv.t * Tenv.t * Procname.t * Procdesc.t) list *)
let file_analysis _ _ _ file_env =
  if should_analyze_file file_env then
    process_results_table
      (make_results_table file_env)

      (*
    Todo:
    0. Refactor abstract domain to use records rather than tuples
    1. Track line numbers of accesses
    2. Track protected writes and reads, too; if we have a write and a read where
    one is non-protected then we have potential race (including protected write, unprotected read
    3. Lotsa other stuff
      *)
