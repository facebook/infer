(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type t =
  | AnnotationReachability
  | Biabduction
  | BufferOverrun
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
  | ResourceLeak
  | SIOF
  | SelfInBlock
  | Starvation
  | Uninit
[@@deriving equal, enumerate]

type support = NoSupport | Support | ExperimentalSupport | ToySupport

type config =
  { support: Language.t -> support
  ; short_documentation: string
  ; cli_flag: string
  ; show_in_help: bool
  ; enabled_by_default: bool
  ; cli_deprecated_flags: string list }

(* support for languages should be consistent with the corresponding
   callbacks registered. Or maybe with the issues reported in link
   with each analysis. Some runtime check probably needed. *)
let config checker =
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
      { support= supports_clang_and_java
      ; short_documentation=
          "the annotation reachability checker. Given a pair of source and sink annotation, e.g. \
           @PerformanceCritical and @Expensive, this checker will warn whenever some method \
           annotated with @PerformanceCritical calls, directly or indirectly, another method \
           annotated with @Expensive"
      ; show_in_help= true
      ; cli_flag= "annotation-reachability"
      ; enabled_by_default= false
      ; cli_deprecated_flags= [] }
  | Biabduction ->
      { support= supports_clang_and_java
      ; short_documentation=
          "the separation logic based bi-abduction analysis using the checkers framework"
      ; show_in_help= true
      ; cli_flag= "biabduction"
      ; enabled_by_default= true
      ; cli_deprecated_flags= [] }
  | BufferOverrun ->
      { support= supports_clang_and_java
      ; short_documentation= "the buffer overrun analysis"
      ; show_in_help= true
      ; cli_flag= "bufferoverrun"
      ; enabled_by_default= false
      ; cli_deprecated_flags= [] }
  | ClassLoads ->
      { support= supports_java
      ; short_documentation= "Java class loading analysis"
      ; show_in_help= true
      ; cli_flag= "class-loads"
      ; enabled_by_default= false
      ; cli_deprecated_flags= [] }
  | Cost ->
      { support= supports_clang_and_java
      ; short_documentation= "checker for performance cost analysis"
      ; show_in_help= true
      ; cli_flag= "cost"
      ; enabled_by_default= false
      ; cli_deprecated_flags= [] }
  | Eradicate ->
      { support= supports_java
      ; short_documentation= "the eradicate @Nullable checker for Java annotations"
      ; show_in_help= true
      ; cli_flag= "eradicate"
      ; enabled_by_default= false
      ; cli_deprecated_flags= [] }
  | FragmentRetainsView ->
      { support= supports_java
      ; short_documentation=
          "detects when Android fragments are not explicitly nullified before becoming unreabable"
      ; show_in_help= true
      ; cli_flag= "fragment-retains-view"
      ; enabled_by_default= true
      ; cli_deprecated_flags= [] }
  | ImmutableCast ->
      { support= supports_java
      ; short_documentation=
          "the detection of object cast from immutable type to mutable type. For instance, it will \
           detect cast from ImmutableList to List, ImmutableMap to Map, and ImmutableSet to Set."
      ; show_in_help= true
      ; cli_flag= "immutable-cast"
      ; enabled_by_default= false
      ; cli_deprecated_flags= [] }
  | Impurity ->
      { support= supports_clang_and_java_experimental
      ; short_documentation= "[EXPERIMENTAL] Impurity analysis"
      ; show_in_help= true
      ; cli_flag= "impurity"
      ; enabled_by_default= false
      ; cli_deprecated_flags= [] }
  | InefficientKeysetIterator ->
      { support= supports_java
      ; short_documentation=
          "Check for inefficient uses of keySet iterator that access both the key and the value."
      ; show_in_help= true
      ; cli_flag= "inefficient-keyset-iterator"
      ; enabled_by_default= true
      ; cli_deprecated_flags= [] }
  | Linters ->
      { support= supports_clang
      ; short_documentation= "syntactic linters"
      ; show_in_help= true
      ; cli_flag= "linters"
      ; enabled_by_default= true
      ; cli_deprecated_flags= [] }
  | LithoRequiredProps ->
      { support= supports_java_experimental
      ; short_documentation= "[EXPERIMENTAL] Required Prop check for Litho"
      ; show_in_help= true
      ; cli_flag= "litho-required-props"
      ; enabled_by_default= false
      ; cli_deprecated_flags= [] }
  | Liveness ->
      { support= supports_clang
      ; short_documentation= "the detection of dead stores and unused variables"
      ; show_in_help= true
      ; cli_flag= "liveness"
      ; enabled_by_default= true
      ; cli_deprecated_flags= [] }
  | LoopHoisting ->
      { support= supports_clang_and_java
      ; short_documentation= "checker for loop-hoisting"
      ; show_in_help= true
      ; cli_flag= "loop-hoisting"
      ; enabled_by_default= false
      ; cli_deprecated_flags= [] }
  | NullsafeDeprecated ->
      { support= (fun _ -> NoSupport)
      ; short_documentation= "[RESERVED] Reserved for nullsafe typechecker, use --eradicate for now"
      ; show_in_help= false
      ; cli_flag= "nullsafe"
      ; enabled_by_default= false
      ; cli_deprecated_flags= ["-check-nullable"; "-suggest-nullable"] }
  | PrintfArgs ->
      { support= supports_java
      ; short_documentation=
          "the detection of mismatch between the Java printf format strings and the argument types \
           For, example, this checker will warn about the type error in `printf(\"Hello %d\", \
           \"world\")`"
      ; show_in_help= true
      ; cli_flag= "printf-args"
      ; enabled_by_default= false
      ; cli_deprecated_flags= [] }
  | Pulse ->
      { support= supports_clang_and_java_experimental
      ; short_documentation= "[EXPERIMENTAL] C++ lifetime analysis"
      ; show_in_help= true
      ; cli_flag= "pulse"
      ; enabled_by_default= false
      ; cli_deprecated_flags= ["-ownership"] }
  | Purity ->
      { support= supports_clang_and_java_experimental
      ; short_documentation= "[EXPERIMENTAL] Purity analysis"
      ; show_in_help= true
      ; cli_flag= "purity"
      ; enabled_by_default= false
      ; cli_deprecated_flags= [] }
  | Quandary ->
      { support= supports_clang_and_java
      ; short_documentation= "the quandary taint analysis"
      ; show_in_help= true
      ; cli_flag= "quandary"
      ; enabled_by_default= false
      ; cli_deprecated_flags= [] }
  | RacerD ->
      { support= supports_clang_and_java
      ; short_documentation= "the RacerD thread safety analysis"
      ; show_in_help= true
      ; cli_flag= "racerd"
      ; enabled_by_default= true
      ; cli_deprecated_flags= ["-threadsafety"] }
  | ResourceLeak ->
      { support= (fun _ -> ToySupport)
      ; short_documentation= ""
      ; show_in_help= false
      ; cli_flag= "resource-leak"
      ; enabled_by_default= false
      ; cli_deprecated_flags= [] }
  | SIOF ->
      { support= supports_clang
      ; short_documentation= "the Static Initialization Order Fiasco analysis (C++ only)"
      ; show_in_help= true
      ; cli_flag= "siof"
      ; enabled_by_default= true
      ; cli_deprecated_flags= [] }
  | SelfInBlock ->
      { support= supports_clang
      ; short_documentation=
          "checker to flag incorrect uses of when Objective-C blocks capture self"
      ; show_in_help= true
      ; cli_flag= "self_in_block"
      ; enabled_by_default= true
      ; cli_deprecated_flags= [] }
  | Starvation ->
      { support= supports_clang_and_java
      ; short_documentation= "starvation analysis"
      ; show_in_help= true
      ; cli_flag= "starvation"
      ; enabled_by_default= true
      ; cli_deprecated_flags= [] }
  | Uninit ->
      { support= supports_clang
      ; short_documentation= "checker for use of uninitialized values"
      ; show_in_help= true
      ; cli_flag= "uninit"
      ; enabled_by_default= true
      ; cli_deprecated_flags= [] }
