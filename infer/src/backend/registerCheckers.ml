(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Module for registering checkers. *)

module F = Format

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
       ~get_payload:(fun payloads ->
         (Field.get payload_field1 payloads, Field.get payload_field2 payloads) )
       ~set_payload:(fun payloads payload1 -> Field.fset payload_field1 payloads payload1)
       checker)


(** For checkers that read three separate payloads. *)
let interprocedural3 payload_field1 payload_field2 payload_field3 ~set_payload checker =
  Procedure
    (CallbackOfChecker.interprocedural
       ~f_analyze_dep:(fun proc_desc payloads -> Some (proc_desc, payloads))
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


type checker = {checker: Checker.t; callbacks: (callback_fun * Language.t) list}

let all_checkers =
  (* The order of the list is important for those checkers that depend on other checkers having run
     before them. *)
  [ {checker= SelfInBlock; callbacks= [(intraprocedural SelfInBlock.checker, Clang)]}
  ; { checker= BufferOverrunAnalysis
    ; callbacks=
        (let bo_analysis =
           interprocedural Payloads.Fields.buffer_overrun_analysis
             BufferOverrunAnalysis.analyze_procedure
         in
         [(bo_analysis, Clang); (bo_analysis, Java)] ) }
  ; { checker= BufferOverrunChecker
    ; callbacks=
        (let bo_checker =
           interprocedural2 Payloads.Fields.buffer_overrun_checker
             Payloads.Fields.buffer_overrun_analysis BufferOverrunChecker.checker
         in
         [(bo_checker, Clang); (bo_checker, Java)] ) }
  ; { checker= PurityAnalysis
    ; callbacks=
        (let purity =
           interprocedural2 Payloads.Fields.purity Payloads.Fields.buffer_overrun_analysis
             PurityAnalysis.checker
         in
         [(purity, Java); (purity, Clang)] ) }
  ; { checker= PurityChecker
    ; callbacks=
        (let purity =
           intraprocedural_with_field_dependency Payloads.Fields.purity PurityChecker.checker
         in
         [(purity, Java); (purity, Clang)] ) }
  ; { checker= Starvation
    ; callbacks=
        (let starvation = interprocedural Payloads.Fields.starvation Starvation.analyze_procedure in
         let starvation_file_reporting =
           file StarvationIssues Payloads.Fields.starvation Starvation.reporting
         in
         [ (starvation, Java)
         ; (starvation_file_reporting, Java)
         ; (starvation, Clang)
         ; (starvation_file_reporting, Clang) ] ) }
  ; { checker= LoopHoisting
    ; callbacks=
        (let hoisting =
           interprocedural3
             ~set_payload:(fun payloads () ->
               (* this analysis doesn't produce additional payloads *) payloads )
             Payloads.Fields.buffer_overrun_analysis Payloads.Fields.purity Payloads.Fields.cost
             Hoisting.checker
         in
         [(hoisting, Clang); (hoisting, Java)] ) }
  ; { checker= Cost
    ; callbacks=
        (let checker =
           interprocedural3 ~set_payload:(Field.fset Payloads.Fields.cost) Payloads.Fields.cost
             Payloads.Fields.buffer_overrun_analysis Payloads.Fields.purity Cost.checker
         in
         [(checker, Clang); (checker, Java)] ) }
  ; {checker= Uninit; callbacks= [(interprocedural Payloads.Fields.uninit Uninit.checker, Clang)]}
  ; {checker= SIOF; callbacks= [(interprocedural Payloads.Fields.siof Siof.checker, Clang)]}
  ; { checker= LithoRequiredProps
    ; callbacks= [(interprocedural Payloads.Fields.litho_required_props RequiredProps.checker, Java)]
    }
  ; (* toy resource analysis to use in the infer lab, see the lab/ directory *)
    { checker= ResourceLeakLabExercise
    ; callbacks=
        [ ( (* the checked-in version is intraprocedural, but the lab asks to make it
               interprocedural later on *)
            interprocedural Payloads.Fields.lab_resource_leaks ResourceLeaks.checker
          , Java ) ] }
  ; (* .NET resource analysis, based on the toy resource analysis in the infer lab *)
    { checker= DOTNETResourceLeaks
    ; callbacks=
        [(interprocedural Payloads.Fields.dotnet_resource_leaks ResourceLeaksCS.checker, CIL)] }
  ; { checker= RacerD
    ; callbacks=
        (let racerd_proc = interprocedural Payloads.Fields.racerd RacerDProcAnalysis.analyze in
         let racerd_file = file RacerDIssues Payloads.Fields.racerd RacerDFileAnalysis.analyze in
         [(racerd_proc, Clang); (racerd_proc, Java); (racerd_file, Clang); (racerd_file, Java)] ) }
  ; { checker= Quandary
    ; callbacks=
        [ (interprocedural Payloads.Fields.quandary JavaTaintAnalysis.checker, Java)
        ; (interprocedural Payloads.Fields.quandary ClangTaintAnalysis.checker, Clang) ] }
  ; { checker= Pulse
    ; callbacks=
        (let checker =
           if Config.is_checker_enabled ToplOnPulse then PulseToplShallow.analyze Pulse.checker
           else Pulse.checker
         in
         let pulse = interprocedural Payloads.Fields.pulse checker in
         [(pulse, Clang); (pulse, Java)] ) }
  ; { checker= Impurity
    ; callbacks=
        (let impurity =
           intraprocedural_with_field_dependency Payloads.Fields.pulse Impurity.checker
         in
         [(impurity, Java); (impurity, Clang)] ) }
  ; {checker= PrintfArgs; callbacks= [(intraprocedural PrintfArgs.checker, Java)]}
  ; {checker= Liveness; callbacks= [(intraprocedural Liveness.checker, Clang)]}
  ; { checker= InefficientKeysetIterator
    ; callbacks= [(intraprocedural InefficientKeysetIterator.checker, Java)] }
  ; { checker= ImmutableCast
    ; callbacks=
        [(intraprocedural_with_payload Payloads.Fields.nullsafe ImmutableChecker.analyze, Java)] }
  ; { checker= FragmentRetainsView
    ; callbacks= [(intraprocedural FragmentRetainsViewChecker.callback_fragment_retains_view, Java)]
    }
  ; { checker= Eradicate
    ; callbacks=
        [ (intraprocedural_with_payload Payloads.Fields.nullsafe Eradicate.analyze_procedure, Java)
        ; (file NullsafeFileIssues Payloads.Fields.nullsafe FileLevelAnalysis.analyze_file, Java) ]
    }
  ; { checker= Biabduction
    ; callbacks=
        (let biabduction =
           dynamic_dispatch Payloads.Fields.biabduction
             ( if Config.is_checker_enabled ToplOnBiabduction then
               Topl.analyze_with_biabduction Interproc.analyze_procedure
             else Interproc.analyze_procedure )
         in
         [(biabduction, Clang); (biabduction, Java); (biabduction, CIL)] ) }
  ; { checker= AnnotationReachability
    ; callbacks=
        (let annot_reach =
           interprocedural Payloads.Fields.annot_map AnnotationReachability.checker
         in
         [(annot_reach, Java); (annot_reach, Clang)] ) }
  ; { checker= ConfigChecksBetweenMarkers
    ; callbacks=
        (let checker =
           interprocedural Payloads.Fields.config_checks_between_markers
             ConfigChecksBetweenMarkers.checker
         in
         [(checker, Clang); (checker, Java)] ) }
  ; { checker= ConfigImpactAnalysis
    ; callbacks=
        (let checker =
           interprocedural Payloads.Fields.config_impact_analysis ConfigImpactAnalysis.checker
         in
         [(checker, Clang); (checker, Java)] ) } ]


let get_active_checkers () =
  let filter_checker {checker} = Config.is_checker_enabled checker in
  List.filter ~f:filter_checker all_checkers


let register checkers =
  let register_one {checker; callbacks} =
    let name = Checker.get_id checker in
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

let pp_checker fmt {checker; callbacks} =
  let langs_of_callbacks =
    List.fold_left callbacks ~init:LanguageSet.empty ~f:(fun langs (_, lang) ->
        LanguageSet.add lang langs )
    |> LanguageSet.elements
  in
  F.fprintf fmt "%s (%a)" (Checker.get_id checker)
    (Pp.seq ~sep:", " (Pp.of_string ~f:Language.to_string))
    langs_of_callbacks
