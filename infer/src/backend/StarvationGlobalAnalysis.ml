(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging
module Domain = StarvationDomain

let analysis_req = AnalysisRequest.one Starvation

let iter_scheduled_pair (work_item : Domain.ScheduledWorkItem.t) f =
  let open Domain in
  let callsite = CallSite.make work_item.procname work_item.loc in
  fun pair ->
    CriticalPair.with_callsite pair callsite
    |> CriticalPair.apply_caller_thread work_item.thread
    |> Option.iter ~f


let iter_critical_pairs_of_summary f summary =
  Domain.fold_critical_pairs_of_summary (fun pair () -> f pair) summary ()


let iter_critical_pairs_of_scheduled_work f (work_item : Domain.ScheduledWorkItem.t) =
  Summary.OnDisk.get ~lazy_payloads:true analysis_req work_item.procname
  |> Option.bind ~f:(fun (summary : Summary.t) -> ILazy.force_option summary.payloads.starvation)
  |> Option.iter ~f:(iter_critical_pairs_of_summary (iter_scheduled_pair work_item f))


let should_report tenv procname =
  match (procname : Procname.t) with
  | Java _ ->
      StarvationModels.is_java_main_method procname
      || ConcurrencyModels.is_android_lifecycle_method tenv procname
  | C _ ->
      true
  | Block _ | CSharp _ | Erlang _ | Hack _ | ObjC_Cpp _ | Python _ ->
      false


let iter_summary ~f exe_env ({payloads; proc_name} : Summary.t) =
  let open Domain in
  Payloads.starvation payloads |> ILazy.force_option
  |> Option.iter ~f:(fun (payload : summary) ->
         let tenv = Exe_env.get_proc_tenv exe_env proc_name in
         if should_report tenv proc_name then iter_critical_pairs_of_summary (f proc_name) payload ;
         ScheduledWorkDomain.iter
           (iter_critical_pairs_of_scheduled_work (f proc_name))
           payload.scheduled_work )


module WorkHashSet = struct
  module T = struct
    type t = Procname.t * Domain.CriticalPair.t

    (* [compare] for critical pairs ignore various fields, so using a generated equality here would
       break the polymorphic hash function.  We use [phys_equal] instead and rely on the clients to
       not add duplicate items. *)
    let equal = phys_equal

    let hash = Hashtbl.hash
  end

  include HashSet.Make (T)

  let add_pair work_set caller pair = add (caller, pair) work_set
end

let report exe_env work_set =
  let open Domain in
  let task_bar = TaskBar.create ~jobs:1 in
  let to_do_items = ref (WorkHashSet.length work_set) in
  TaskBar.set_tasks_total task_bar !to_do_items ;
  let wrap_report (procname, (pair : CriticalPair.t)) init =
    to_do_items := !to_do_items - 1 ;
    TaskBar.set_remaining_tasks task_bar !to_do_items ;
    TaskBar.update_status task_bar ~slot:0 (Mtime_clock.now ()) (Procname.to_string procname) ;
    TaskBar.refresh task_bar ;
    Summary.OnDisk.get ~lazy_payloads:true analysis_req procname
    |> Option.fold ~init ~f:(fun acc summary ->
           let pattrs = Attributes.load_exn procname in
           let tenv = Exe_env.get_proc_tenv exe_env procname in
           let acc =
             Starvation.report_on_pair
               ~analyze_ondemand:(fun pname ->
                 Ondemand.analyze_proc_name exe_env analysis_req ~caller_summary:summary pname
                 |> AnalysisResult.to_option
                 |> Option.bind ~f:(fun summary ->
                        ILazy.force_option summary.Summary.payloads.starvation ) )
               tenv pattrs pair acc
           in
           Event.get_acquired_locks pair.elem.event
           |> List.fold ~init:acc ~f:(fun acc lock ->
                  let should_report_starvation =
                    CriticalPair.is_uithread pair && not (Procname.is_constructor procname)
                  in
                  WorkHashSet.fold
                    (fun (other_procname, (other_pair : CriticalPair.t)) acc ->
                      Starvation.report_on_parallel_composition ~should_report_starvation tenv
                        pattrs pair lock other_procname other_pair acc )
                    work_set acc ) )
  in
  WorkHashSet.fold wrap_report work_set Starvation.ReportMap.empty
  |> Starvation.ReportMap.store_multi_file ;
  TaskBar.finish task_bar


let whole_program_analysis () =
  L.progress "Starvation whole program analysis starts.@." ;
  let work_set = WorkHashSet.create 1 in
  let exe_env = Exe_env.mk () in
  L.progress "Processing on-disk summaries...@." ;
  Summary.OnDisk.iter_specs ~f:(iter_summary exe_env ~f:(WorkHashSet.add_pair work_set)) ;
  let num_pairs = WorkHashSet.length work_set in
  L.progress "Loaded %d pairs@." num_pairs ;
  L.progress "Reporting on processed summaries...@." ;
  report exe_env work_set
