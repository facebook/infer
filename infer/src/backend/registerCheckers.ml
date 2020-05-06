(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Module for registering checkers. *)

module F = Format

(* make sure SimpleChecker.ml is not dead code *)
let () =
  if false then
    let module SC = SimpleChecker.Make in
    ()


type callback_fun =
  | Procedure of Callbacks.proc_callback_t
  | DynamicDispatch of Callbacks.proc_callback_t
  | File of {callback: Callbacks.file_callback_t; issue_dir: ResultsDirEntryName.id}

let interprocedural payload_field checker =
  Procedure (CallbackOfChecker.interprocedural_with_field payload_field checker)


let dynamic_dispatch payload_field checker =
  DynamicDispatch (CallbackOfChecker.interprocedural_with_field payload_field checker)


(** For checkers that read two separate payloads. Assumes that [checker] produces payloads for
    [payload_field1] *)
let interprocedural2 payload_field1 payload_field2 checker =
  Procedure
    (CallbackOfChecker.interprocedural
       ~f_analyze_dep:(fun proc_desc payloads -> Some (proc_desc, payloads))
       ~f_analyze_pdesc_dep:Option.some
       ~get_payload:(fun payloads ->
         (Field.get payload_field1 payloads, Field.get payload_field2 payloads) )
       ~set_payload:(fun payloads payload1 -> Field.fset payload_field1 payloads payload1)
       checker)


(** For checkers that read three separate payloads. *)
let interprocedural3 payload_field1 payload_field2 payload_field3 ~set_payload checker =
  Procedure
    (CallbackOfChecker.interprocedural
       ~f_analyze_dep:(fun proc_desc payloads -> Some (proc_desc, payloads))
       ~f_analyze_pdesc_dep:Option.some
       ~get_payload:(fun payloads ->
         ( Field.get payload_field1 payloads
         , Field.get payload_field2 payloads
         , Field.get payload_field3 payloads ) )
       ~set_payload checker)


let file issue_dir payload_field checker =
  File {callback= CallbackOfChecker.interprocedural_file payload_field checker; issue_dir}


let intraprocedural checker = Procedure (CallbackOfChecker.intraprocedural checker)

let intraprocedural_with_payload payload_field checker =
  Procedure (CallbackOfChecker.intraprocedural_with_field payload_field checker)


let intraprocedural_with_field_dependency payload_field checker =
  Procedure (CallbackOfChecker.intraprocedural_with_field_dependency payload_field checker)


type callback = callback_fun * Language.t

type checker = {name: string; active: bool; callbacks: callback list}

let all_checkers =
  (* The order of the list is important for those checkers that depend on other checkers having run
     before them. *)
  [ { name= "Self captured in block checker"
    ; active= Config.is_checker_enabled SelfInBlock
    ; callbacks= [(intraprocedural SelfInBlock.checker, Language.Clang)] }
  ; { name= "Class loading analysis"
    ; active= Config.is_checker_enabled ClassLoads
    ; callbacks=
        [(interprocedural Payloads.Fields.class_loads ClassLoads.analyze_procedure, Language.Java)]
    }
  ; { name= "purity"
    ; active= Config.(is_checker_enabled Purity || is_checker_enabled LoopHoisting)
    ; callbacks=
        (let purity =
           interprocedural2 Payloads.Fields.purity Payloads.Fields.buffer_overrun_analysis
             Purity.checker
         in
         [(purity, Language.Java); (purity, Language.Clang)] ) }
  ; { name= "Starvation analysis"
    ; active= Config.is_checker_enabled Starvation
    ; callbacks=
        [ (Procedure Starvation.analyze_procedure, Language.Java)
        ; (File {callback= Starvation.reporting; issue_dir= StarvationIssues}, Language.Java)
        ; (Procedure Starvation.analyze_procedure, Language.Clang)
        ; (File {callback= Starvation.reporting; issue_dir= StarvationIssues}, Language.Clang) ] }
  ; { name= "loop hoisting"
    ; active= Config.is_checker_enabled LoopHoisting
    ; callbacks=
        (let hoisting =
           interprocedural3
             ~set_payload:(fun payloads () ->
               (* this analysis doesn't produce additional payloads *) payloads )
             Payloads.Fields.buffer_overrun_analysis Payloads.Fields.purity Payloads.Fields.cost
             Hoisting.checker
         in
         [(hoisting, Language.Clang); (hoisting, Language.Java)] ) }
  ; { name= "cost analysis"
    ; active=
        Config.(
          is_checker_enabled Cost
          || (is_checker_enabled LoopHoisting && hoisting_report_only_expensive))
    ; callbacks=
        (let checker =
           interprocedural3 ~set_payload:(Field.fset Payloads.Fields.cost) Payloads.Fields.cost
             Payloads.Fields.buffer_overrun_analysis Payloads.Fields.purity Cost.checker
         in
         [(checker, Language.Clang); (checker, Language.Java)] ) }
  ; { name= "uninitialized variables"
    ; active= Config.is_checker_enabled Uninit
    ; callbacks= [(Procedure Uninit.checker, Language.Clang)] }
  ; { name= "SIOF"
    ; active= Config.is_checker_enabled SIOF
    ; callbacks= [(interprocedural Payloads.Fields.siof Siof.checker, Language.Clang)] }
  ; { name= "litho-required-props"
    ; active= Config.is_checker_enabled LithoRequiredProps
    ; callbacks=
        [(interprocedural Payloads.Fields.litho_required_props RequiredProps.checker, Language.Java)]
    }
  ; (* toy resource analysis to use in the infer lab, see the lab/ directory *)
    { name= "resource leak"
    ; active= Config.is_checker_enabled ResourceLeak
    ; callbacks=
        [ ( (* the checked-in version is intraprocedural, but the lab asks to make it
               interprocedural later on *)
            Procedure ResourceLeaks.checker
          , Language.Java ) ] }
  ; { name= "RacerD"
    ; active= Config.is_checker_enabled RacerD
    ; callbacks=
        [ (Procedure RacerD.analyze_procedure, Language.Clang)
        ; (Procedure RacerD.analyze_procedure, Language.Java)
        ; (File {callback= RacerD.file_analysis; issue_dir= RacerDIssues}, Language.Clang)
        ; (File {callback= RacerD.file_analysis; issue_dir= RacerDIssues}, Language.Java) ] }
  ; { name= "quandary"
    ; active= Config.(is_checker_enabled Quandary)
    ; callbacks=
        [ (Procedure JavaTaintAnalysis.checker, Language.Java)
        ; (Procedure ClangTaintAnalysis.checker, Language.Clang) ] }
  ; { name= "pulse"
    ; active= Config.(is_checker_enabled Pulse || is_checker_enabled Impurity)
    ; callbacks=
        (let pulse = interprocedural Payloads.Fields.pulse Pulse.checker in
         (pulse, Language.Clang)
         :: (if Config.is_checker_enabled Impurity then [(pulse, Language.Java)] else []) ) }
  ; { name= "impurity"
    ; active= Config.is_checker_enabled Impurity
    ; callbacks=
        (let impurity =
           intraprocedural_with_field_dependency Payloads.Fields.pulse Impurity.checker
         in
         [(impurity, Language.Java); (impurity, Language.Clang)] ) }
  ; { name= "printf args"
    ; active= Config.is_checker_enabled PrintfArgs
    ; callbacks= [(Procedure PrintfArgs.callback_printf_args, Language.Java)] }
  ; { name= "liveness"
    ; active= Config.is_checker_enabled Liveness
    ; callbacks= [(intraprocedural Liveness.checker, Language.Clang)] }
  ; { name= "inefficient keyset iterator"
    ; active= Config.is_checker_enabled InefficientKeysetIterator
    ; callbacks= [(intraprocedural InefficientKeysetIterator.checker, Language.Java)] }
  ; { name= "immutable cast"
    ; active= Config.is_checker_enabled ImmutableCast
    ; callbacks=
        [ ( intraprocedural_with_payload Payloads.Fields.nullsafe ImmutableChecker.analyze
          , Language.Java ) ] }
  ; { name= "fragment retains view"
    ; active= Config.is_checker_enabled FragmentRetainsView
    ; callbacks=
        [(intraprocedural FragmentRetainsViewChecker.callback_fragment_retains_view, Language.Java)]
    }
  ; { name= "eradicate"
    ; active= Config.is_checker_enabled Eradicate
    ; callbacks=
        [ ( intraprocedural_with_payload Payloads.Fields.nullsafe Eradicate.analyze_procedure
          , Language.Java )
        ; ( file NullsafeFileIssues Payloads.Fields.nullsafe FileLevelAnalysis.analyze_file
          , Language.Java ) ] }
  ; { name= "buffer overrun checker"
    ; active= Config.(is_checker_enabled BufferOverrun)
    ; callbacks=
        (let bo_checker =
           interprocedural2 Payloads.Fields.buffer_overrun_checker
             Payloads.Fields.buffer_overrun_analysis BufferOverrunChecker.checker
         in
         [(bo_checker, Language.Clang); (bo_checker, Language.Java)] ) }
  ; { name= "buffer overrun analysis"
    ; active=
        Config.(
          is_checker_enabled BufferOverrun || is_checker_enabled Cost
          || is_checker_enabled LoopHoisting || is_checker_enabled Purity)
    ; callbacks=
        (let bo_analysis =
           interprocedural Payloads.Fields.buffer_overrun_analysis
             BufferOverrunAnalysis.analyze_procedure
         in
         [(bo_analysis, Language.Clang); (bo_analysis, Language.Java)] ) }
  ; { name= "biabduction"
    ; active= Config.is_checker_enabled Biabduction
    ; callbacks=
        (let biabduction =
           dynamic_dispatch Payloads.Fields.biabduction
             (Topl.instrument_callback Interproc.analyze_procedure)
         in
         [(biabduction, Language.Clang); (biabduction, Language.Java)] ) }
  ; { name= "annotation reachability"
    ; active= Config.is_checker_enabled AnnotationReachability
    ; callbacks=
        (let annot_reach =
           interprocedural Payloads.Fields.annot_map AnnotationReachability.checker
         in
         [(annot_reach, Language.Java); (annot_reach, Language.Clang)] ) } ]


let get_active_checkers () =
  let filter_checker {active} = active in
  List.filter ~f:filter_checker all_checkers


let register checkers =
  let register_one {name; callbacks} =
    let register_callback (callback, language) =
      match callback with
      | Procedure procedure_cb ->
          Callbacks.register_procedure_callback ~checker_name:name language procedure_cb
      | DynamicDispatch procedure_cb ->
          Callbacks.register_procedure_callback ~checker_name:name ~dynamic_dispatch:true language
            procedure_cb
      | File {callback; issue_dir} ->
          Callbacks.register_file_callback ~checker_name:name language callback ~issue_dir
    in
    List.iter ~f:register_callback callbacks
  in
  List.iter ~f:register_one checkers


module LanguageSet = Caml.Set.Make (Language)

let pp_checker fmt {name; callbacks} =
  let langs_of_callbacks =
    List.fold_left callbacks ~init:LanguageSet.empty ~f:(fun langs (_, lang) ->
        LanguageSet.add lang langs )
    |> LanguageSet.elements
  in
  F.fprintf fmt "%s (%a)" name
    (Pp.seq ~sep:", " (Pp.of_string ~f:Language.to_string))
    langs_of_callbacks
