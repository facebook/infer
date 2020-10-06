(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging
module Domain = StarvationDomain

(* given a scheduled-work item, read the summary of the scheduled method from the disk
   and adapt its contents to the thread it was scheduled too *)
let get_summary_of_scheduled_work (work_item : Domain.ScheduledWorkItem.t) =
  let astate = {Domain.initial with thread= work_item.thread} in
  let callsite = CallSite.make work_item.procname work_item.loc in
  let open IOption.Let_syntax in
  let* {Summary.payloads= {starvation}} = Summary.OnDisk.get work_item.procname in
  let+ callee_astate = starvation in
  let ({critical_pairs} : Domain.t) = Domain.integrate_summary callsite astate callee_astate in
  critical_pairs


(* given a summary, do [f work critical_pairs] for each [work] item scheduled in the summary,
   where [critical_pairs] are those of the method scheduled, adapted to the thread it's scheduled for *)
let iter_summary ~f exe_env (summary : Summary.t) =
  let open Domain in
  Payloads.starvation summary.payloads
  |> Option.iter ~f:(fun ({scheduled_work; critical_pairs} : summary) ->
         let pname = Summary.get_proc_name summary in
         let tenv = Exe_env.get_tenv exe_env pname in
         if
           StarvationModels.is_java_main_method pname
           || ConcurrencyModels.is_android_lifecycle_method tenv pname
         then f pname critical_pairs ;
         ScheduledWorkDomain.iter
           (fun work -> get_summary_of_scheduled_work work |> Option.iter ~f:(f pname))
           scheduled_work )


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

  let add_pairs work_set caller pairs =
    let open Domain in
    CriticalPairs.iter (fun pair -> replace work_set (caller, pair) ()) pairs
end

let report exe_env work_set =
  let open Domain in
  let wrap_report (procname, (pair : CriticalPair.t)) () init =
    Summary.OnDisk.get procname
    |> Option.fold ~init ~f:(fun acc summary ->
           let pdesc = Summary.get_proc_desc summary in
           let pattrs = Procdesc.get_attributes pdesc in
           let tenv = Exe_env.get_tenv exe_env procname in
           let acc =
             Starvation.report_on_pair
               ~analyze_ondemand:(fun pname ->
                 Ondemand.analyze_proc_name ~caller_summary:summary pname
                 |> Option.bind ~f:(fun summary ->
                        Option.map summary.Summary.payloads.starvation ~f:(fun starvation ->
                            (Summary.get_proc_desc summary, starvation) ) ) )
               tenv pattrs pair acc
           in
           match pair.elem.event with
           | LockAcquire {locks} ->
               List.fold locks ~init:acc ~f:(fun acc lock ->
                   let should_report_starvation =
                     CriticalPair.is_uithread pair && not (Procname.is_constructor procname)
                   in
                   WorkHashSet.fold
                     (fun (other_procname, (other_pair : CriticalPair.t)) () acc ->
                       Starvation.report_on_parallel_composition ~should_report_starvation tenv
                         pattrs pair lock other_procname other_pair acc )
                     work_set acc )
           | _ ->
               acc )
  in
  WorkHashSet.fold wrap_report work_set Starvation.ReportMap.empty
  |> Starvation.ReportMap.store_multi_file


let whole_program_analysis () =
  L.progress "Starvation whole program analysis starts.@." ;
  let work_set = WorkHashSet.create 1 in
  let exe_env = Exe_env.mk () in
  L.progress "Processing on-disk summaries...@." ;
  Summary.OnDisk.iter_specs ~f:(iter_summary exe_env ~f:(WorkHashSet.add_pairs work_set)) ;
  L.progress "Loaded %d pairs@." (WorkHashSet.length work_set) ;
  L.progress "Reporting on processed summaries...@." ;
  report exe_env work_set
