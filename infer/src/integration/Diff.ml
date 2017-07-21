(*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)
open! IStd
module F = Format
module L = Logging

type revision = Current | Previous

let string_of_revision = function Current -> "current" | Previous -> "previous"

let pp_revision f r = F.fprintf f "%s" (string_of_revision r)

let checkout revision =
  let script_opt =
    match revision with
    | Current
     -> Config.previous_to_current_script
    | Previous
     -> Config.current_to_previous_script
  in
  match script_opt with
  | None
   -> L.(die UserError)
        "Please specify a script to checkout the %a revision of your project using --checkout-%a <script>."
        pp_revision revision pp_revision revision
  | Some script
   -> L.progress "Checking out %a version:@\n  %s@\n" pp_revision revision script ;
      let (), exit_or_signal = Utils.with_process_in script Utils.consume_in in
      Result.iter_error exit_or_signal ~f:(fun _ ->
          L.(die ExternalError)
            "Failed to checkout %a revision: %s" pp_revision revision
            (Unix.Exit_or_signal.to_string_hum exit_or_signal) )

let save_report revision =
  let report_name = Config.results_dir ^/ F.asprintf "report-%a.json" pp_revision revision in
  Unix.rename ~src:Config.(results_dir ^/ report_json) ~dst:report_name ;
  L.progress "Results for the %a revision stored in %s@\n" pp_revision revision report_name ;
  report_name

let diff driver_mode =
  (* TODO(t15553258) run gen-build script if specified *)
  (* run capture *)
  Driver.capture driver_mode ~changed_files:None ;
  (* run analysis TODO(t15553258) add --reactive and --changed_files_index *)
  Driver.analyze_and_report driver_mode ~changed_files:None ;
  let current_report = Some (save_report Current) in
  (* TODO(t15553258) bail if nothing to analyze (configurable, some people might care about bugs
     fixed more than about time to analyze) *)
  checkout Previous ;
  (* TODO(t15553258) run gen-build script if specified *)
  (* run capture TODO(t15553258) add --reactive and --continue *)
  Driver.capture driver_mode ~changed_files:None ;
  (* run analysis TODO(t15553258) add --reactive and --changed_files_index *)
  Driver.analyze_and_report driver_mode ~changed_files:None ;
  checkout Current ;
  let previous_report = Some (save_report Previous) in
  (* compute differential *)
  ReportDiff.reportdiff ~current_report ~previous_report ; (* TODO(t15553258) report new bugs! *)
                                                           ()
