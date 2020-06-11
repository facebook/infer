(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Die

type t =
  | AnnotationReachability
  | Biabduction
  | BufferOverrunAnalysis
  | BufferOverrunChecker
  | ClassLoads
  | Cost
  | Eradicate
  | FragmentRetainsView
  | ImmutableCast
  | Impurity
  | InefficientKeysetIterator
  | Linters
  | LithoRequiredProps
  | Liveness
  | LoopHoisting
  | NullsafeDeprecated
  | PrintfArgs
  | Pulse
  | Purity
  | Quandary
  | RacerD
  | ResourceLeakLabExercise
  | SIOF
  | SelfInBlock
  | Starvation
  | TOPL
  | Uninit
[@@deriving equal, enumerate]

type support = NoSupport | Support | ExperimentalSupport | ToySupport

type cli_flags = {long: string; deprecated: string list; show_in_help: bool}

type config =
  { id: string
  ; support: Language.t -> support
  ; short_documentation: string
  ; cli_flags: cli_flags option
  ; enabled_by_default: bool
  ; activates: t list }

(* support for languages should be consistent with the corresponding
   callbacks registered. Or maybe with the issues reported in link
   with each analysis. Some runtime check probably needed. *)
let config_unsafe checker =
  let supports_clang_and_java _ = Support in
  let supports_clang_and_java_experimental _ = ExperimentalSupport in
  let supports_clang (language : Language.t) =
    match language with Clang -> Support | Java -> NoSupport
  in
  let supports_java (language : Language.t) =
    match language with Clang -> NoSupport | Java -> Support
  in
  let supports_java_experimental (language : Language.t) =
    match language with Clang -> NoSupport | Java -> ExperimentalSupport
  in
  match checker with
  | AnnotationReachability ->
      { id= "annotation-reachability"
      ; support= supports_clang_and_java
      ; short_documentation=
          "the annotation reachability checker. Given a pair of source and sink annotation, e.g. \
           @PerformanceCritical and @Expensive, this checker will warn whenever some method \
           annotated with @PerformanceCritical calls, directly or indirectly, another method \
           annotated with @Expensive"
      ; cli_flags= Some {long= "annotation-reachability"; deprecated= []; show_in_help= true}
      ; enabled_by_default= false
      ; activates= [] }
  | Biabduction ->
      { id= "biabduction"
      ; support= supports_clang_and_java
      ; short_documentation=
          "the separation logic based bi-abduction analysis using the checkers framework"
      ; cli_flags= Some {long= "biabduction"; deprecated= []; show_in_help= true}
      ; enabled_by_default= true
      ; activates= [] }
  | BufferOverrunAnalysis ->
      { id= "buffer-overrun-analysis"
      ; support= supports_clang_and_java
      ; short_documentation=
          "internal part of the buffer overrun analysis that computes values at each program \
           point, automatically triggered when analyses that depend on these are run"
      ; cli_flags= None
      ; enabled_by_default= false
      ; activates= [] }
  | BufferOverrunChecker ->
      { id= "buffer-overrun-checker"
      ; support= supports_clang_and_java
      ; short_documentation= "the buffer overrun analysis"
      ; cli_flags= Some {long= "bufferoverrun"; deprecated= []; show_in_help= true}
      ; enabled_by_default= false
      ; activates= [BufferOverrunAnalysis] }
  | ClassLoads ->
      { id= "class-loading-analysis"
      ; support= supports_java
      ; short_documentation= "Java class loading analysis"
      ; cli_flags= Some {long= "class-loads"; deprecated= []; show_in_help= true}
      ; enabled_by_default= false
      ; activates= [] }
  | Cost ->
      { id= "cost-analysis"
      ; support= supports_clang_and_java
      ; short_documentation= "checker for performance cost analysis"
      ; cli_flags= Some {long= "cost"; deprecated= []; show_in_help= true}
      ; enabled_by_default= false
      ; activates= [BufferOverrunAnalysis] }
  | Eradicate ->
      { id= "eradicate"
      ; support= supports_java
      ; short_documentation= "the eradicate @Nullable checker for Java annotations"
      ; cli_flags= Some {long= "eradicate"; deprecated= []; show_in_help= true}
      ; enabled_by_default= false
      ; activates= [] }
  | FragmentRetainsView ->
      { id= "fragment-retains-view"
      ; support= supports_java
      ; short_documentation=
          "detects when Android fragments are not explicitly nullified before becoming unreabable"
      ; cli_flags= Some {long= "fragment-retains-view"; deprecated= []; show_in_help= true}
      ; enabled_by_default= true
      ; activates= [] }
  | ImmutableCast ->
      { id= "immutable-cast"
      ; support= supports_java
      ; short_documentation=
          "the detection of object cast from immutable type to mutable type. For instance, it will \
           detect cast from ImmutableList to List, ImmutableMap to Map, and ImmutableSet to Set."
      ; cli_flags= Some {long= "immutable-cast"; deprecated= []; show_in_help= true}
      ; enabled_by_default= false
      ; activates= [] }
  | Impurity ->
      { id= "impurity"
      ; support= supports_clang_and_java_experimental
      ; short_documentation= "[EXPERIMENTAL] Impurity analysis"
      ; cli_flags= Some {long= "impurity"; deprecated= []; show_in_help= true}
      ; enabled_by_default= false
      ; activates= [Pulse] }
  | InefficientKeysetIterator ->
      { id= "inefficient-keyset-iterator"
      ; support= supports_java
      ; short_documentation=
          "Check for inefficient uses of keySet iterator that access both the key and the value."
      ; cli_flags= Some {long= "inefficient-keyset-iterator"; deprecated= []; show_in_help= true}
      ; enabled_by_default= true
      ; activates= [] }
  | Linters ->
      { id= "al-linters"
      ; support= supports_clang
      ; short_documentation= "syntactic linters"
      ; cli_flags= Some {long= "linters"; deprecated= []; show_in_help= true}
      ; enabled_by_default= true
      ; activates= [] }
  | LithoRequiredProps ->
      { id= "litho-required-props"
      ; support= supports_java_experimental
      ; short_documentation= "[EXPERIMENTAL] Required Prop check for Litho"
      ; cli_flags= Some {long= "litho-required-props"; deprecated= []; show_in_help= true}
      ; enabled_by_default= false
      ; activates= [] }
  | Liveness ->
      { id= "liveness"
      ; support= supports_clang
      ; short_documentation= "the detection of dead stores and unused variables"
      ; cli_flags= Some {long= "liveness"; deprecated= []; show_in_help= true}
      ; enabled_by_default= true
      ; activates= [] }
  | LoopHoisting ->
      { id= "loop-hoisting"
      ; support= supports_clang_and_java
      ; short_documentation= "checker for loop-hoisting"
      ; cli_flags= Some {long= "loop-hoisting"; deprecated= []; show_in_help= true}
      ; enabled_by_default= false
      ; activates= [BufferOverrunAnalysis; Purity] }
  | NullsafeDeprecated ->
      { id= "nullsafe"
      ; support= (fun _ -> NoSupport)
      ; short_documentation= "[RESERVED] Reserved for nullsafe typechecker, use --eradicate for now"
      ; cli_flags=
          Some
            { long= "nullsafe"
            ; deprecated= ["-check-nullable"; "-suggest-nullable"]
            ; show_in_help= false }
      ; enabled_by_default= false
      ; activates= [] }
  | PrintfArgs ->
      { id= "printf-args"
      ; support= supports_java
      ; short_documentation=
          "the detection of mismatch between the Java printf format strings and the argument types \
           For, example, this checker will warn about the type error in `printf(\"Hello %d\", \
           \"world\")`"
      ; cli_flags= Some {long= "printf-args"; deprecated= []; show_in_help= true}
      ; enabled_by_default= false
      ; activates= [] }
  | Pulse ->
      { id= "pulse"
      ; support= supports_clang_and_java_experimental
      ; short_documentation= "[EXPERIMENTAL] memory and lifetime analysis"
      ; cli_flags= Some {long= "pulse"; deprecated= ["-ownership"]; show_in_help= true}
      ; enabled_by_default= false
      ; activates= [] }
  | Purity ->
      { id= "purity"
      ; support= supports_clang_and_java_experimental
      ; short_documentation= "[EXPERIMENTAL] Purity analysis"
      ; cli_flags= Some {long= "purity"; deprecated= []; show_in_help= true}
      ; enabled_by_default= false
      ; activates= [BufferOverrunAnalysis] }
  | Quandary ->
      { id= "quandary"
      ; support= supports_clang_and_java
      ; short_documentation= "the quandary taint analysis"
      ; cli_flags= Some {long= "quandary"; deprecated= []; show_in_help= true}
      ; enabled_by_default= false
      ; activates= [] }
  | RacerD ->
      { id= "RacerD"
      ; support= supports_clang_and_java
      ; short_documentation= "the RacerD thread safety analysis"
      ; cli_flags= Some {long= "racerd"; deprecated= ["-threadsafety"]; show_in_help= true}
      ; enabled_by_default= true
      ; activates= [] }
  | ResourceLeakLabExercise ->
      { id= "resource-leak-lab"
      ; support= (fun _ -> ToySupport)
      ; short_documentation= ""
      ; cli_flags= Some {long= "resource-leak"; deprecated= []; show_in_help= false}
      ; enabled_by_default= false
      ; activates= [] }
  | SIOF ->
      { id= "SIOF"
      ; support= supports_clang
      ; short_documentation= "the Static Initialization Order Fiasco analysis (C++ only)"
      ; cli_flags= Some {long= "siof"; deprecated= []; show_in_help= true}
      ; enabled_by_default= true
      ; activates= [] }
  | SelfInBlock ->
      { id= "self-in-block"
      ; support= supports_clang
      ; short_documentation=
          "checker to flag incorrect uses of when Objective-C blocks capture self"
      ; cli_flags= Some {long= "self_in_block"; deprecated= []; show_in_help= true}
      ; enabled_by_default= true
      ; activates= [] }
  | Starvation ->
      { id= "starvation"
      ; support= supports_clang_and_java
      ; short_documentation= "starvation analysis"
      ; cli_flags= Some {long= "starvation"; deprecated= []; show_in_help= true}
      ; enabled_by_default= true
      ; activates= [] }
  | TOPL ->
      { id= "TOPL"
      ; support= supports_clang_and_java_experimental
      ; short_documentation= "TOPL"
      ; cli_flags= Some {long= "topl"; deprecated= []; show_in_help= true}
      ; enabled_by_default= false
      ; activates= [Biabduction] }
  | Uninit ->
      { id= "uninit"
      ; support= supports_clang
      ; short_documentation= "checker for use of uninitialized values"
      ; cli_flags= Some {long= "uninit"; deprecated= []; show_in_help= true}
      ; enabled_by_default= true
      ; activates= [] }


let config c =
  let config = config_unsafe c in
  let is_illegal_id_char c = match c with 'a' .. 'z' | 'A' .. 'Z' | '-' -> false | _ -> true in
  String.find config.id ~f:is_illegal_id_char
  |> Option.iter ~f:(fun c ->
         L.die InternalError
           "Illegal character '%c' in id: '%s'. Checker ids must be easy to pass on the command \
            line."
           c config.id ) ;
  config


let get_id c = (config c).id
