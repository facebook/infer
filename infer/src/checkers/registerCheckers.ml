(*
* Copyright (c) 2013 - Facebook. All rights reserved.
*)

(** Module for registering checkers. *)

module L = Logging
module F = Format
open Utils

(** Flags to activate checkers. *)
let active_procedure_checkers () =
  let checkers_enabled = Config.checkers_enabled () in

  let java_checkers =
    let l =
      [
      CallbackChecker.callback_checker_main, false;
      Checkers.callback_check_access, false;
      Checkers.callback_monitor_nullcheck, false;
      Checkers.callback_test_state , false;
      Checkers.callback_checkVisibleForTesting, false;
      Checkers.callback_check_write_to_parcel, false;
      Checkers.callback_find_deserialization, false;
      Dataflow.callback_test_dataflow, false;
      SqlChecker.callback_sql, false;
      Eradicate.callback_eradicate, !Config.eradicate;
      CodeQuery.code_query_callback, !CodeQuery.query <> None;
      Checkers.callback_check_field_access, false;
      ImmutableChecker.callback_check_immutable_cast, checkers_enabled;
      RepeatedCallsChecker.callback_check_repeated_calls, checkers_enabled;
      PrintfArgs.callback_printf_args, checkers_enabled;
      ] in
    list_map (fun (x, y) -> (x, y, Some Sil.Java)) l in
  let c_cpp_checkers =
    let l =
      [
      Checkers.callback_print_c_method_calls, false;
      CheckDeadCode.callback_check_dead_code, checkers_enabled;
      ] in
    list_map (fun (x, y) -> (x, y, Some Sil.C_CPP)) l in

  java_checkers @ c_cpp_checkers

let active_cluster_checkers () =
  [(Checkers.callback_check_cluster_access, false, Some Sil.Java)]

let register () =
  let register registry (callback, active, language_opt) =
    if active then registry language_opt callback in
  list_iter (register Callbacks.register_procedure_callback) (active_procedure_checkers ());
  list_iter (register Callbacks.register_cluster_callback) (active_cluster_checkers ())
