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
  | ConfigChecksBetweenMarkers
  | ConfigImpactAnalysis
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
  | PurityAnalysis
  | PurityChecker
  | Quandary
  | RacerD
  | ResourceLeakLabExercise
  | DOTNETResourceLeaks
  | SIOF
  | SelfInBlock
  | Starvation
  | ToplOnBiabduction
  | ToplOnPulse
  | Uninit
[@@deriving equal, enumerate]

type support = NoSupport | ExperimentalSupport | Support

(** see .mli for how to fill these *)
type kind =
  | UserFacing of {title: string; markdown_body: string}
  | UserFacingDeprecated of {title: string; markdown_body: string; deprecation_message: string}
  | Internal
  | Exercise

type cli_flags = {deprecated: string list; show_in_help: bool}

type config =
  { id: string
  ; kind: kind
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
    match language with Clang -> Support | Java -> NoSupport | CIL -> NoSupport
  in
  let supports_java (language : Language.t) =
    match language with Clang -> NoSupport | Java -> Support | CIL -> Support
  in
  match checker with
  | AnnotationReachability ->
      { id= "annotation-reachability"
      ; kind= UserFacing {title= "Annotation Reachability"; markdown_body= ""}
      ; support= supports_clang_and_java
      ; short_documentation=
          "Given a pair of source and sink annotation, e.g. `@PerformanceCritical` and \
           `@Expensive`, this checker will warn whenever some method annotated with \
           `@PerformanceCritical` calls, directly or indirectly, another method annotated with \
           `@Expensive`"
      ; cli_flags= Some {deprecated= []; show_in_help= true}
      ; enabled_by_default= false
      ; activates= [] }
  | Biabduction ->
      { id= "biabduction"
      ; kind=
          UserFacing
            { title= "Biabduction"
            ; markdown_body=
                "Read more about its foundations in the [Separation Logic and Biabduction \
                 page](separation-logic-and-bi-abduction)." }
      ; support= supports_clang_and_java
      ; short_documentation=
          "This analysis deals with a range of issues, many linked to memory safety."
      ; cli_flags= Some {deprecated= []; show_in_help= true}
      ; enabled_by_default= true
      ; activates= [] }
  | BufferOverrunAnalysis ->
      { id= "bufferoverrun-analysis"
      ; kind= Internal
      ; support= supports_clang_and_java
      ; short_documentation=
          "Internal part of the buffer overrun analysis that computes values at each program \
           point, automatically triggered when analyses that depend on these are run."
      ; cli_flags= None
      ; enabled_by_default= false
      ; activates= [] }
  | BufferOverrunChecker ->
      { id= "bufferoverrun"
      ; kind=
          UserFacing
            { title= "Buffer Overrun Analysis (InferBO)"
            ; markdown_body=
                "You can read about its origins in this [blog \
                 post](https://research.fb.com/inferbo-infer-based-buffer-overrun-analyzer/)." }
      ; support= supports_clang_and_java
      ; short_documentation= "InferBO is a detector for out-of-bounds array accesses."
      ; cli_flags= Some {deprecated= []; show_in_help= true}
      ; enabled_by_default= false
      ; activates= [BufferOverrunAnalysis] }
  | ConfigChecksBetweenMarkers ->
      { id= "config-checks-between-markers"
      ; kind=
          UserFacing
            { title= "Config Checks between Markers"
            ; markdown_body=
                "This checker collects config checkings in some program regions determined by \
                 pairs of marker-starts and marker-ends. The set of config checking functions, \
                 marker-start functions, and marker-end functions is hardcoded and empty by \
                 default for now, so to use this checker, please modify the code directly in \
                 [FbGKInteraction.ml](https://github.com/facebook/infer/tree/master/infer/src/opensource)."
            }
      ; support= supports_clang_and_java_experimental
      ; short_documentation= "[EXPERIMENTAL] Collects config checks between marker start and end."
      ; cli_flags= Some {deprecated= []; show_in_help= true}
      ; enabled_by_default= false
      ; activates= [] }
  | ConfigImpactAnalysis ->
      { id= "config-impact-analysis"
      ; kind=
          UserFacing
            { title= "Config Impact Analysis"
            ; markdown_body=
                "This checker collects functions whose execution isn't gated by certain \
                 pre-defined gating functions. The set of gating functions is hardcoded and empty \
                 by default for now, so to use this checker, please modify the code directly in \
                 [FbGKInteraction.ml](https://github.com/facebook/infer/tree/master/infer/src/opensource)."
            }
      ; support= supports_clang_and_java_experimental
      ; short_documentation=
          "[EXPERIMENTAL] Collects function that are called without config checks."
      ; cli_flags= Some {deprecated= []; show_in_help= true}
      ; enabled_by_default= false
      ; activates= [] }
  | Cost ->
      { id= "cost"
      ; kind=
          UserFacing
            { title= "Cost: Runtime Complexity Analysis"
            ; markdown_body= [%blob "../../documentation/checkers/Cost.md"] }
      ; support= supports_clang_and_java
      ; short_documentation=
          "Computes the time complexity of functions and methods. Can be used to detect changes in \
           runtime complexity with `infer reportdiff`."
      ; cli_flags= Some {deprecated= []; show_in_help= true}
      ; enabled_by_default= false
      ; activates= [BufferOverrunAnalysis; PurityAnalysis] }
  | Eradicate ->
      { id= "eradicate"
      ; kind=
          UserFacing
            {title= "Eradicate"; markdown_body= [%blob "../../documentation/checkers/Eradicate.md"]}
      ; support= supports_java
      ; short_documentation= "The eradicate `@Nullable` checker for Java annotations."
      ; cli_flags= Some {deprecated= []; show_in_help= true}
      ; enabled_by_default= false
      ; activates= [] }
  | FragmentRetainsView ->
      { id= "fragment-retains-view"
      ; kind=
          UserFacingDeprecated
            { title= "Fragment Retains View"
            ; markdown_body= ""
            ; deprecation_message= "Unmaintained due to poor precision." }
      ; support= supports_java
      ; short_documentation=
          "Detects when Android fragments are not explicitly nullified before becoming unreachable."
      ; cli_flags= Some {deprecated= []; show_in_help= true}
      ; enabled_by_default= true
      ; activates= [] }
  | ImmutableCast ->
      { id= "immutable-cast"
      ; kind=
          UserFacingDeprecated
            { title= "Immutable Cast"
            ; markdown_body=
                "Casts flagged by this checker are unsafe because calling mutation operations on \
                 the cast objects will fail at runtime."
            ; deprecation_message= "Unmaintained due to poor actionability of the reports." }
      ; support= supports_java
      ; short_documentation=
          "Detection of object cast from immutable types to mutable types. For instance, it will \
           detect casts from `ImmutableList` to `List`, `ImmutableMap` to `Map`, and \
           `ImmutableSet` to `Set`."
      ; cli_flags= Some {deprecated= []; show_in_help= true}
      ; enabled_by_default= false
      ; activates= [] }
  | Impurity ->
      { id= "impurity"
      ; kind=
          UserFacing
            {title= "Impurity"; markdown_body= [%blob "../../documentation/checkers/Impurity.md"]}
      ; support= supports_clang_and_java_experimental
      ; short_documentation=
          "Detects functions with potential side-effects. Same as \"purity\", but implemented on \
           top of Pulse."
      ; cli_flags= Some {deprecated= []; show_in_help= true}
      ; enabled_by_default= false
      ; activates= [Pulse] }
  | InefficientKeysetIterator ->
      { id= "inefficient-keyset-iterator"
      ; kind= UserFacing {title= "Inefficient keySet Iterator"; markdown_body= ""}
      ; support= supports_java
      ; short_documentation=
          "Check for inefficient uses of iterators that iterate on keys then lookup their values, \
           instead of iterating on key-value pairs directly."
      ; cli_flags= Some {deprecated= []; show_in_help= true}
      ; enabled_by_default= true
      ; activates= [] }
  | Linters ->
      { id= "linters"
      ; kind=
          UserFacingDeprecated
            { title= "AST Language (AL)"
            ; markdown_body= [%blob "../../documentation/checkers/ASTLanguage.md"]
            ; deprecation_message= "On end-of-life support, may be removed in the future." }
      ; support= supports_clang
      ; short_documentation= "Declarative linting framework over the Clang AST."
      ; cli_flags= Some {deprecated= []; show_in_help= true}
      ; enabled_by_default= true
      ; activates= [] }
  | LithoRequiredProps ->
      { id= "litho-required-props"
      ; kind=
          UserFacing
            { title= "Litho \"Required Props\""
            ; markdown_body= [%blob "../../documentation/checkers/LithoRequiredProps.md"] }
      ; support= supports_java
      ; short_documentation=
          "Checks that all non-optional `@Prop`s have been specified when constructing Litho \
           components."
      ; cli_flags= Some {deprecated= []; show_in_help= true}
      ; enabled_by_default= false
      ; activates= [] }
  | Liveness ->
      { id= "liveness"
      ; kind= UserFacing {title= "Liveness"; markdown_body= ""}
      ; support= supports_clang
      ; short_documentation= "Detection of dead stores and unused variables."
      ; cli_flags= Some {deprecated= []; show_in_help= true}
      ; enabled_by_default= true
      ; activates= [] }
  | LoopHoisting ->
      { id= "loop-hoisting"
      ; kind=
          UserFacing
            { title= "Loop Hoisting"
            ; markdown_body= [%blob "../../documentation/checkers/LoopHoisting.md"] }
      ; support= supports_clang_and_java
      ; short_documentation=
          "Detect opportunities to hoist function calls that are invariant outside of loop bodies \
           for efficiency."
      ; cli_flags= Some {deprecated= []; show_in_help= true}
      ; enabled_by_default= false
      ; activates= [BufferOverrunAnalysis; PurityAnalysis] }
  | NullsafeDeprecated ->
      { id= "nullsafe"
      ; kind= Internal
      ; support= (fun _ -> NoSupport)
      ; short_documentation=
          "[RESERVED] Reserved for nullsafe typechecker, use `--eradicate` for now."
      ; cli_flags= Some {deprecated= ["-check-nullable"; "-suggest-nullable"]; show_in_help= false}
      ; enabled_by_default= false
      ; activates= [] }
  | PrintfArgs ->
      { id= "printf-args"
      ; kind=
          UserFacingDeprecated
            { title= "`printf()` Argument Types"
            ; markdown_body= ""
            ; deprecation_message= "Unmaintained." }
      ; support= supports_java
      ; short_documentation=
          "Detect mismatches between the Java `printf` format strings and the argument types For \
           example, this checker will warn about the type error in `printf(\"Hello %d\", \
           \"world\")`"
      ; cli_flags= Some {deprecated= []; show_in_help= true}
      ; enabled_by_default= false
      ; activates= [] }
  | Pulse ->
      { id= "pulse"
      ; kind= UserFacing {title= "Pulse"; markdown_body= ""}
      ; support= (function Clang -> Support | Java -> ExperimentalSupport | CIL -> NoSupport)
      ; short_documentation= "Memory and lifetime analysis."
      ; cli_flags= Some {deprecated= ["-ownership"]; show_in_help= true}
      ; enabled_by_default= false
      ; activates= [] }
  | PurityAnalysis ->
      { id= "purity-analysis"
      ; kind= Internal
      ; support= supports_clang_and_java_experimental
      ; short_documentation= "Internal part of the purity checker."
      ; cli_flags= None
      ; enabled_by_default= false
      ; activates= [BufferOverrunAnalysis] }
  | PurityChecker ->
      { id= "purity"
      ; kind=
          UserFacing
            {title= "Purity"; markdown_body= [%blob "../../documentation/checkers/Purity.md"]}
      ; support= supports_clang_and_java_experimental
      ; short_documentation=
          "Detects pure (side-effect-free) functions. A different implementation of \"impurity\"."
      ; cli_flags= Some {deprecated= []; show_in_help= true}
      ; enabled_by_default= false
      ; activates= [PurityAnalysis] }
  | Quandary ->
      { id= "quandary"
      ; kind=
          UserFacing
            {title= "Quandary"; markdown_body= [%blob "../../documentation/checkers/Quandary.md"]}
      ; support= supports_clang_and_java
      ; short_documentation=
          "The Quandary taint analysis detects flows of values between sources and sinks, except \
           if the value went through a \"sanitizer\". In addition to some defaults, users can \
           specify their own sources, sinks, and sanitizers functions."
      ; cli_flags= Some {deprecated= []; show_in_help= true}
      ; enabled_by_default= false
      ; activates= [] }
  | RacerD ->
      { id= "racerd"
      ; kind=
          UserFacing
            {title= "RacerD"; markdown_body= [%blob "../../documentation/checkers/RacerD.md"]}
      ; support= supports_clang_and_java
      ; short_documentation= "Thread safety analysis."
      ; cli_flags= Some {deprecated= ["-threadsafety"]; show_in_help= true}
      ; enabled_by_default= true
      ; activates= [] }
  | ResourceLeakLabExercise ->
      { id= "resource-leak-lab"
      ; kind=
          UserFacing
            { title= "Resource Leak Lab Exercise"
            ; markdown_body=
                "This toy checker does nothing by default. Hack on it to make it report resource \
                 leaks! See the [lab \
                 instructions](https://github.com/facebook/infer/blob/master/infer/src/labs/README.md)."
            }
      ; support= (function Clang -> NoSupport | Java -> Support | CIL -> Support)
      ; short_documentation=
          "Toy checker for the \"resource leak\" write-your-own-checker exercise."
      ; cli_flags= Some {deprecated= []; show_in_help= false}
      ; enabled_by_default= false
      ; activates= [] }
  | DOTNETResourceLeaks ->
      { id= "dotnet-resource-leak"
      ; kind= UserFacing {title= "Resource Leak checker for .NET"; markdown_body= ""}
      ; support= (function Clang -> NoSupport | Java -> NoSupport | CIL -> Support)
      ; short_documentation= "\"resource leak\" checker for .NET."
      ; cli_flags= Some {deprecated= []; show_in_help= false}
      ; enabled_by_default= true
      ; activates= [] }
  | SIOF ->
      { id= "siof"
      ; kind= UserFacing {title= "Static Initialization Order Fiasco"; markdown_body= ""}
      ; support= supports_clang
      ; short_documentation=
          "Catches Static Initialization Order Fiascos in C++, that can lead to subtle, \
           compiler-version-dependent errors."
      ; cli_flags= Some {deprecated= []; show_in_help= true}
      ; enabled_by_default= true
      ; activates= [] }
  | SelfInBlock ->
      { id= "self-in-block"
      ; kind= UserFacing {title= "Self in Block"; markdown_body= ""}
      ; support= supports_clang
      ; short_documentation=
          "An Objective-C-specific analysis to detect when a block captures `self`."
      ; cli_flags= Some {deprecated= ["-self_in_block"]; show_in_help= true}
      ; enabled_by_default= true
      ; activates= [] }
  | Starvation ->
      { id= "starvation"
      ; kind=
          UserFacing
            { title= "Starvation"
            ; markdown_body= [%blob "../../documentation/checkers/Starvation.md"] }
      ; support= supports_clang_and_java
      ; short_documentation=
          "Detect various kinds of situations when no progress is being made because of \
           concurrency errors."
      ; cli_flags= Some {deprecated= []; show_in_help= true}
      ; enabled_by_default= true
      ; activates= [] }
  | ToplOnBiabduction ->
      { id= "topl-biabd"
      ; kind= UserFacing {title= "TOPL"; markdown_body= ""}
      ; support= supports_clang_and_java_experimental
      ; short_documentation=
          "Detects errors based on user-provided state machines describing multi-object monitors."
      ; cli_flags= Some {deprecated= []; show_in_help= true}
      ; enabled_by_default= false
      ; activates= [Biabduction] }
  | ToplOnPulse ->
      { id= "topl-pulse"
      ; kind= UserFacing {title= "TOPL"; markdown_body= ""}
      ; support= supports_clang_and_java_experimental
      ; short_documentation=
          "Detects errors based on user-provided state machines describing multi-object monitors."
      ; cli_flags= Some {deprecated= []; show_in_help= true}
      ; enabled_by_default= false
      ; activates= [Pulse] }
  | Uninit ->
      { id= "uninit"
      ; kind= UserFacing {title= "Uninitialized Value"; markdown_body= ""}
      ; support= supports_clang
      ; short_documentation= "Warns when values are used before having been initialized."
      ; cli_flags= Some {deprecated= []; show_in_help= true}
      ; enabled_by_default= true
      ; activates= [] }


let config c =
  let config = config_unsafe c in
  let is_illegal_id_char c = match c with 'a' .. 'z' | '-' -> false | _ -> true in
  String.find config.id ~f:is_illegal_id_char
  |> Option.iter ~f:(fun c ->
         L.die InternalError
           "Illegal character '%c' in id: '%s'. Checker ids must be easy to pass on the command \
            line."
           c config.id ) ;
  config


let get_id c = (config c).id

let from_id id = List.find all ~f:(fun checker -> String.equal (get_id checker) id)
