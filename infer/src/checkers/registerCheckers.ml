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

type callback_fun =
  | Procedure of Callbacks.proc_callback_t
  | DynamicDispatch of Callbacks.proc_callback_t
  | Cluster of Callbacks.cluster_callback_t

type callback = callback_fun * Config.language

type checker = {name: string; active: bool; callbacks: callback list}

let all_checkers =
  [ { name= "annotation reachability"
    ; active= Config.annotation_reachability
    ; callbacks= [(Procedure AnnotationReachability.checker, Config.Java)] }
  ; { name= "biabduction"
    ; active= Config.biabduction
    ; callbacks=
        [ (Procedure Interproc.analyze_procedure, Config.Clang)
        ; (DynamicDispatch Interproc.analyze_procedure, Config.Java) ] }
  ; { name= "buffer overrun"
    ; active= Config.bufferoverrun
    ; callbacks=
        [ (Procedure BufferOverrunChecker.checker, Config.Clang)
        ; (Procedure BufferOverrunChecker.checker, Config.Java) ] }
  ; { name= "crashcontext"
    ; active= Config.crashcontext
    ; callbacks= [(Procedure BoundedCallTree.checker, Config.Java)] }
  ; { name= "eradicate"
    ; active= Config.eradicate
    ; callbacks= [(Procedure Eradicate.callback_eradicate, Config.Java)] }
  ; { name= "fragment retains view"
    ; active= Config.fragment_retains_view
    ; callbacks=
        [(Procedure FragmentRetainsViewChecker.callback_fragment_retains_view, Config.Java)] }
  ; { name= "immutable cast"
    ; active= Config.immutable_cast
    ; callbacks= [(Procedure ImmutableChecker.callback_check_immutable_cast, Config.Java)] }
  ; { name= "liveness"
    ; active= Config.liveness
    ; callbacks= [(Procedure Liveness.checker, Config.Clang)] }
  ; { name= "printf args"
    ; active= Config.printf_args
    ; callbacks= [(Procedure PrintfArgs.callback_printf_args, Config.Java)] }
  ; { name= "nullable checks"
    ; active= Config.check_nullable
    ; callbacks= [(Procedure NullabilityCheck.checker, Config.Clang)] }
  ; { name= "nullable suggestion"
    ; active= Config.suggest_nullable
    ; callbacks=
        [ (Procedure NullabilitySuggest.checker, Config.Java)
        ; (Procedure NullabilitySuggest.checker, Config.Clang) ] }
  ; { name= "quandary"
    ; active= Config.quandary
    ; callbacks=
        [ (Procedure JavaTaintAnalysis.checker, Config.Java)
        ; (Procedure ClangTaintAnalysis.checker, Config.Clang) ] }
  ; { name= "RacerD"
    ; active= Config.racerd
    ; callbacks=
        [ (Procedure RacerD.analyze_procedure, Config.Clang)
        ; (Procedure RacerD.analyze_procedure, Config.Java)
        ; (Cluster RacerD.file_analysis, Config.Clang)
        ; (Cluster RacerD.file_analysis, Config.Java) ] }
  ; { name= "repeated calls"
    ; active= Config.repeated_calls
    ; callbacks= [(Procedure RepeatedCallsChecker.callback_check_repeated_calls, Config.Java)] }
  ; { name=
        "resource leak"
        (** toy resource analysis to use in the infer lab, see the lab/ directory *)
    ; active= Config.resource_leak
    ; callbacks=
        [ ( (* the checked-in version is intraprocedural, but the lab asks to make it interprocedural later on *)
            Procedure ResourceLeaks.checker
          , Config.Java ) ] }
  ; {name= "SIOF"; active= Config.siof; callbacks= [(Procedure Siof.checker, Config.Clang)]}
  ; { name= "uninitialized variables"
    ; active= Config.uninit
    ; callbacks= [(Procedure Uninit.checker, Config.Clang)] } ]

let get_active_checkers () =
  let filter_checker {active} = active in
  List.filter ~f:filter_checker all_checkers

let register checkers =
  let register_one {callbacks} =
    let register_callback (callback, language) =
      match callback with
      | Procedure procedure_cb
       -> Callbacks.register_procedure_callback language procedure_cb
      | DynamicDispatch procedure_cb
       -> Callbacks.register_procedure_callback ~dynamic_dispath:true language procedure_cb
      | Cluster cluster_cb
       -> Callbacks.register_cluster_callback language cluster_cb
    in
    List.iter ~f:register_callback callbacks
  in
  List.iter ~f:register_one checkers

module LanguageSet = Caml.Set.Make (struct
  type t = Config.language

  let compare = Config.compare_language
end)

let pp_checker fmt {name; callbacks} =
  let langs_of_callbacks =
    List.fold_left callbacks ~init:LanguageSet.empty ~f:(fun langs (_, lang) ->
        LanguageSet.add lang langs )
    |> LanguageSet.elements
  in
  F.fprintf fmt "%s (%a)" name
    (Pp.seq ~sep:", " (Pp.to_string ~f:Config.string_of_language))
    langs_of_callbacks
