(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging
module Domain = StarvationDomain

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
  Summary.OnDisk.get work_item.procname
  |> Option.bind ~f:(fun (summary : Summary.t) -> summary.payloads.starvation)
  |> Option.iter ~f:(iter_critical_pairs_of_summary (iter_scheduled_pair work_item f))


let iter_summary ~f exe_env (summary : Summary.t) =
  let open Domain in
  Payloads.starvation summary.payloads
  |> Option.iter ~f:(fun (payload : summary) ->
         let pname = Summary.get_proc_name summary in
         let tenv = Exe_env.get_proc_tenv exe_env pname in
         if
           StarvationModels.is_java_main_method pname
           || ConcurrencyModels.is_android_lifecycle_method tenv pname
         then iter_critical_pairs_of_summary (f pname) payload ;
         ScheduledWorkDomain.iter
           (iter_critical_pairs_of_scheduled_work (f pname))
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

  include Caml.Hashtbl.Make (T)

  let add_pair work_set caller pair = replace work_set (caller, pair) ()
end

let report exe_env work_set =
  let open Domain in
  let wrap_report (procname, (pair : CriticalPair.t)) () init =
    Summary.OnDisk.get procname
    |> Option.fold ~init ~f:(fun acc summary ->
           let pdesc = Summary.get_proc_desc summary in
           let pattrs = Procdesc.get_attributes pdesc in
           let tenv = Exe_env.get_proc_tenv exe_env procname in
           let acc =
             Starvation.report_on_pair
               ~analyze_ondemand:(fun pname ->
                 Ondemand.analyze_proc_name exe_env ~caller_summary:summary pname
                 |> Option.bind ~f:(fun summary ->
                        Option.map summary.Summary.payloads.starvation ~f:(fun starvation ->
                            (Summary.get_proc_desc summary, starvation) ) ) )
               tenv pattrs pair acc
           in
           Event.get_acquired_locks pair.elem.event
           |> List.fold ~init:acc ~f:(fun acc lock ->
                  let should_report_starvation =
                    CriticalPair.is_uithread pair && not (Procname.is_constructor procname)
                  in
                  WorkHashSet.fold
                    (fun (other_procname, (other_pair : CriticalPair.t)) () acc ->
                      Starvation.report_on_parallel_composition ~should_report_starvation tenv
                        pattrs pair lock other_procname other_pair acc )
                    work_set acc ) )
  in
  WorkHashSet.fold wrap_report work_set Starvation.ReportMap.empty
  |> Starvation.ReportMap.store_multi_file


let whole_program_analysis () =
  L.progress "Starvation whole program analysis starts.@." ;
  let work_set = WorkHashSet.create 1 in
  let exe_env = Exe_env.mk () in
  L.progress "Processing on-disk summaries...@." ;
  Summary.OnDisk.iter_specs ~f:(iter_summary exe_env ~f:(WorkHashSet.add_pair work_set)) ;
  L.progress "Loaded %d pairs@." (WorkHashSet.length work_set) ;
  L.progress "Reporting on processed summaries...@." ;
  report exe_env work_set
