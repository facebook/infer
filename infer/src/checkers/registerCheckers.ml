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

(* make sure SimpleChecker.ml is not dead code *)
let () = if false then let module SC = SimpleChecker.Make in ()

type callback = Procedure of Callbacks.proc_callback_t | Cluster of Callbacks.cluster_callback_t

let checkers =
  [ ( "annotation reachability"
    , Config.annotation_reachability
    , [(Procedure AnnotationReachability.checker, Config.Java)] )
  ; ( "biabduction"
    , Config.biabduction
    , [ (Procedure Interproc.analyze_procedure, Config.Clang)
      ; (Procedure Interproc.analyze_procedure, Config.Java) ] )
  ; ( "buffer overrun"
    , Config.bufferoverrun
    , [ (Procedure BufferOverrunChecker.checker, Config.Clang)
      ; (Procedure BufferOverrunChecker.checker, Config.Java) ] )
  ; ("crashcontext", Config.crashcontext, [(Procedure BoundedCallTree.checker, Config.Java)])
  ; ("eradicate", Config.eradicate, [(Procedure Eradicate.callback_eradicate, Config.Java)])
  ; ( "fragment retains view"
    , Config.fragment_retains_view
    , [(Procedure FragmentRetainsViewChecker.callback_fragment_retains_view, Config.Java)] )
  ; ( "immutable cast"
    , Config.immutable_cast
    , [(Procedure ImmutableChecker.callback_check_immutable_cast, Config.Java)] )
  ; ("printf args", Config.printf_args, [(Procedure PrintfArgs.callback_printf_args, Config.Java)])
  ; ( "nullable suggestion"
    , Config.suggest_nullable
    , [(Procedure NullabilitySuggest.checker, Config.Java)] )
  ; ( "quandary"
    , Config.quandary
    , [ (Procedure JavaTaintAnalysis.checker, Config.Java)
      ; (Procedure ClangTaintAnalysis.checker, Config.Clang) ] )
  ; ( "repeated calls"
    , Config.repeated_calls
    , [(Procedure RepeatedCallsChecker.callback_check_repeated_calls, Config.Java)] )
  ; ("resource leak", Config.resource_leak, [(Procedure ResourceLeaks.checker, Config.Java)])
  ; ("SIOF", Config.siof, [(Procedure Siof.checker, Config.Clang)])
  ; ( "thread safety"
    , Config.threadsafety
    , [ (Procedure ThreadSafety.analyze_procedure, Config.Clang)
      ; (Procedure ThreadSafety.analyze_procedure, Config.Java)
      ; (Cluster ThreadSafety.file_analysis, Config.Clang)
      ; (Cluster ThreadSafety.file_analysis, Config.Java) ] ) ]

let register () =
  let register_one (_, active, callbacks) =
    let register_callback (callback, language) =
      match callback with
      | Procedure procedure_cb
       -> Callbacks.register_procedure_callback (Some language) procedure_cb
      | Cluster cluster_cb
       -> Callbacks.register_cluster_callback (Some language) cluster_cb
    in
    if active then List.iter ~f:register_callback callbacks
  in
  List.iter ~f:register_one checkers

let pp_active_checkers fmt () =
  let has_active = ref false in
  List.iter checkers ~f:(fun (name, active, _) ->
      if active then (
        Format.fprintf fmt "%s%s" (if !has_active then ", " else "") name ;
        has_active := true ) ) ;
  if not !has_active then Format.fprintf fmt "none"
