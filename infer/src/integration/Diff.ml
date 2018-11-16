(*
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format
module L = Logging

type revision = Current | Previous

let string_of_revision = function Current -> "current" | Previous -> "previous"

let pp_revision f r = F.pp_print_string f (string_of_revision r)

let checkout revision =
  let script_opt =
    match revision with
    | Current ->
        Config.previous_to_current_script
    | Previous ->
        Config.current_to_previous_script
  in
  match script_opt with
  | None ->
      L.(die UserError)
        "Please specify a script to checkout the %a revision of your project using --checkout-%a \
         <script>."
        pp_revision revision pp_revision revision
  | Some script ->
      L.progress "Checking out %a version:@\n  %s@\n" pp_revision revision script ;
      let (), exit_or_signal = Utils.with_process_in script Utils.consume_in in
      Result.iter_error exit_or_signal ~f:(fun _ ->
          L.(die ExternalError)
            "Failed to checkout %a revision: %s" pp_revision revision
            (Unix.Exit_or_signal.to_string_hum exit_or_signal) )


let save_report revision =
  let report_name = Config.results_dir ^/ F.asprintf "report-%a.json" pp_revision revision in
  Unix.rename ~src:Config.(results_dir ^/ report_json) ~dst:report_name ;
  let costs_report_name =
    Config.results_dir ^/ F.asprintf "costs-report-%a.json" pp_revision revision
  in
  Unix.rename ~src:Config.(results_dir ^/ costs_report_json) ~dst:costs_report_name ;
  L.progress "Results for the %a revision stored in %s@\n" pp_revision revision report_name ;
  L.progress "Costs data for the %a revision stored in %s@\n" pp_revision revision
    costs_report_name ;
  (report_name, costs_report_name)


let gen_previous_driver_mode script =
  let output, exit_or_signal = Utils.with_process_in script In_channel.input_lines in
  match exit_or_signal with
  | Error _ as status ->
      L.(die UserError)
        "*** command failed:@\n*** %s@\n*** %s@." script
        (Unix.Exit_or_signal.to_string_hum status)
  | Ok () ->
      (* FIXME(t15553258): this won't work if the build command has arguments that contain spaces. In that case the user should be able to use an argfile for the build command instead, so not critical to fix. *)
      let command = List.concat_map ~f:(String.split ~on:' ') output in
      L.environment_info "Build command for the previous project version: '%s'@\n%!"
        (String.concat ~sep:" " command) ;
      Driver.mode_of_build_command command


let diff driver_mode =
  Driver.run_prologue driver_mode ;
  let changed_files = Driver.read_config_changed_files () in
  Driver.capture driver_mode ~changed_files ;
  Driver.analyze_and_report ~suppress_console_report:true driver_mode ~changed_files ;
  let current_report, current_costs = save_report Current in
  (* Some files in the current checkout may be deleted in the old checkout. If we kept the results of the previous capture and analysis around, we would report issues on these files again in the previous checkout, which is wrong. Do not do anything too smart for now and just delete all results from the analysis of the current checkout. *)
  ResultsDir.delete_capture_and_analysis_data () ;
  if Config.memcached then ( Memcached.(connect () ; flush_all () ; disconnect ()) ) ;
  (* TODO(t15553258) bail if nothing to analyze (configurable, some people might care about bugs
     fixed more than about time to analyze) *)
  checkout Previous ;
  let previous_driver_mode =
    Option.value_map ~default:driver_mode ~f:gen_previous_driver_mode
      Config.gen_previous_build_command_script
  in
  Driver.capture previous_driver_mode ~changed_files ;
  Driver.analyze_and_report ~suppress_console_report:true previous_driver_mode ~changed_files ;
  checkout Current ;
  let previous_report, previous_costs = save_report Previous in
  (* compute differential *)
  ReportDiff.reportdiff ~current_report:(Some current_report)
    ~previous_report:(Some previous_report) ~current_costs:(Some current_costs)
    ~previous_costs:(Some previous_costs) ;
  Driver.run_epilogue driver_mode ;
  ()
