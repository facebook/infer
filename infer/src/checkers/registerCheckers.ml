(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

(** Module for registering checkers. *)

module L = Logging
module F = Format

(** Flags to activate checkers. *)
let active_procedure_checkers () =
  let checkers_enabled = Config.checkers_enabled in

  let java_checkers =
    let l =
      [
        Checkers.callback_check_access, false;
        Checkers.callback_monitor_nullcheck, false;
        Checkers.callback_test_state , false;
        Checkers.callback_checkVisibleForTesting, false;
        Checkers.callback_check_write_to_parcel, false;
        Checkers.callback_find_deserialization, false;
        CheckTraceCallSequence.callback_check_trace_call_sequence, false;
        Dataflow.callback_test_dataflow, false;
        FragmentRetainsViewChecker.callback_fragment_retains_view, checkers_enabled;
        SqlChecker.callback_sql, false;
        Eradicate.callback_eradicate, Config.eradicate;
        BoundedCallTree.checker, Config.crashcontext;
        JavaTaintAnalysis.checker, Config.quandary;
        Checkers.callback_check_field_access, false;
        ImmutableChecker.callback_check_immutable_cast, checkers_enabled;
        RepeatedCallsChecker.callback_check_repeated_calls, checkers_enabled;
        PrintfArgs.callback_printf_args, checkers_enabled;
        AnnotationReachability.Interprocedural.check_and_report, checkers_enabled;
        ThreadSafety.method_analysis, false;
      ] in
    (* make sure SimpleChecker.ml is not dead code *)
    if false then (let module SC = SimpleChecker.Make in ());
    IList.map (fun (x, y) -> (x, y, Some Config.Java)) l in
  let c_cpp_checkers =
    let l =
      [
        Checkers.callback_print_c_method_calls, false;
        CheckDeadCode.callback_check_dead_code, false;
        Checkers.callback_print_access_to_globals, false;
        CppTaintAnalysis.checker, Config.quandary;
        Siof.checker, checkers_enabled;
      ] in
    IList.map (fun (x, y) -> (x, y, Some Config.Clang)) l in

  java_checkers @ c_cpp_checkers

let active_cluster_checkers () =
  [(Checkers.callback_check_cluster_access, false, Some Config.Java);
   (ThreadSafety.file_analysis, Config.threadsafety, Some Config.Java)
  ]

let register () =
  let register registry (callback, active, language_opt) =
    if active then registry language_opt callback in
  IList.iter (register Callbacks.register_procedure_callback) (active_procedure_checkers ());
  IList.iter (register Callbacks.register_cluster_callback) (active_cluster_checkers ())
