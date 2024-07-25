(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Die

type t =
  | AnnotationReachability
  | Biabduction
  | BufferOverrunAnalysis
  | BufferOverrunChecker
  | ConfigImpactAnalysis
  | Cost
  | DisjunctiveDemo
  | FragmentRetainsView
  | Impurity
  | InefficientKeysetIterator
  | Lineage
  | LineageShape
  | LithoRequiredProps
  | Liveness
  | LoopHoisting
  | ParameterNotNullChecked
  | Pulse
  | PurityAnalysis
  | PurityChecker
  | RacerD
  | ResourceLeakLabExercise
  | SILValidation
  | SIOF
  | ScopeLeakage
  | SelfInBlock
  | Starvation
  | Topl
[@@deriving compare, equal, enumerate]

type support = NoSupport | ExperimentalSupport | Support

let mk_support_func ?(clang = NoSupport) ?(java = NoSupport) ?(csharp = NoSupport)
    ?(erlang = NoSupport) ?(hack = NoSupport) ?(python = NoSupport) () : Language.t -> support =
  function
  | Clang ->
      clang
  | Java ->
      java
  | CIL ->
      csharp
  | Erlang ->
      erlang
  | Hack ->
      hack
  | Python ->
      python


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
  match checker with
  | AnnotationReachability ->
      { id= "annotation-reachability"
      ; kind= UserFacing {title= "Annotation Reachability"; markdown_body= ""}
      ; support= mk_support_func ~java:Support ()
      ; short_documentation=
          "Given pairs of source and sink annotations, e.g. `@A` and `@B`, this checker will warn \
           whenever some method annotated with `@A` calls, directly or indirectly, another method \
           annotated with `@B`. Besides the custom pairs, it is also possible to enable some \
           built-in checks, such as `@PerformanceCritical` reaching `@Expensive` or \
           `@NoAllocation` reaching `new`. See flags starting with `--annotation-reachability`."
      ; cli_flags= Some {deprecated= []; show_in_help= true}
      ; enabled_by_default= false
      ; activates= [] }
  | Biabduction ->
      { id= "biabduction"
      ; kind=
          UserFacingDeprecated
            { title= "Biabduction"
            ; markdown_body=
                "Read more about its foundations in the [Separation Logic and Biabduction \
                 page](separation-logic-and-bi-abduction)."
            ; deprecation_message=
                "This has been replaced by Pulse and will be removed in the next release." }
      ; support= mk_support_func ~clang:Support ~java:Support ~csharp:Support ()
      ; short_documentation=
          "This analysis deals with a range of issues, many linked to memory safety."
      ; cli_flags= Some {deprecated= []; show_in_help= true}
      ; enabled_by_default= false
      ; activates= [] }
  | BufferOverrunAnalysis ->
      { id= "bufferoverrun-analysis"
      ; kind= Internal
      ; support= mk_support_func ~clang:Support ~java:Support ()
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
      ; support= mk_support_func ~clang:Support ~java:Support ()
      ; short_documentation= "InferBO is a detector for out-of-bounds array accesses."
      ; cli_flags= Some {deprecated= []; show_in_help= true}
      ; enabled_by_default= false
      ; activates= [BufferOverrunAnalysis] }
  | ConfigImpactAnalysis ->
      { id= "config-impact-analysis"
      ; kind=
          UserFacing
            { title= "Config Impact Analysis"
            ; markdown_body=
                "This checker collects functions whose execution isn't gated by certain \
                 pre-defined gating functions. The set of gating functions is hardcoded and empty \
                 by default for now, so to use this checker, please modify the code directly in \
                 [FbGKInteraction.ml](https://github.com/facebook/infer/tree/main/infer/src/opensource)."
            }
      ; support= mk_support_func ~clang:ExperimentalSupport ~java:ExperimentalSupport ()
      ; short_documentation=
          "[EXPERIMENTAL] Collects function that are called without config checks."
      ; cli_flags= Some {deprecated= []; show_in_help= true}
      ; enabled_by_default= false
      ; activates= [] }
  | Cost ->
      { id= "cost"
      ; kind=
          UserFacing
            { title= "Cost: Complexity Analysis"
            ; markdown_body= [%blob "./documentation/checkers/Cost.md"] }
      ; support= mk_support_func ~clang:Support ~java:Support ~hack:ExperimentalSupport ()
      ; short_documentation=
          "Computes the asymptotic complexity of functions with respect to execution cost or other \
           user defined resources. Can be used to detect changes in the complexity with `infer \
           reportdiff`."
      ; cli_flags= Some {deprecated= []; show_in_help= true}
      ; enabled_by_default= false
      ; activates= [BufferOverrunAnalysis; PurityAnalysis] }
  | DisjunctiveDemo ->
      { id= "disjunctive-demo"
      ; kind= Internal
      ; support= mk_support_func ~clang:Support ()
      ; short_documentation= "Demo of the disjunctive domain, used for testing."
      ; cli_flags= Some {deprecated= []; show_in_help= false}
      ; enabled_by_default= false
      ; activates= [] }
  | FragmentRetainsView ->
      { id= "fragment-retains-view"
      ; kind= UserFacing {title= "Fragment Retains View"; markdown_body= ""}
      ; support= mk_support_func ~java:Support ()
      ; short_documentation=
          "Detects when Android fragments are not explicitly nullified before becoming unreachable."
      ; cli_flags= Some {deprecated= []; show_in_help= true}
      ; enabled_by_default= true
      ; activates= [] }
  | Impurity ->
      { id= "impurity"
      ; kind=
          UserFacing
            {title= "Impurity"; markdown_body= [%blob "./documentation/checkers/Impurity.md"]}
      ; support=
          mk_support_func ~clang:ExperimentalSupport ~java:ExperimentalSupport
            ~hack:ExperimentalSupport ()
      ; short_documentation=
          "Detects functions with potential side-effects. Same as \"purity\", but implemented on \
           top of Pulse."
      ; cli_flags= Some {deprecated= []; show_in_help= true}
      ; enabled_by_default= false
      ; activates= [Pulse] }
  | InefficientKeysetIterator ->
      { id= "inefficient-keyset-iterator"
      ; kind= UserFacing {title= "Inefficient keySet Iterator"; markdown_body= ""}
      ; support= mk_support_func ~java:Support ()
      ; short_documentation=
          "Check for inefficient uses of iterators that iterate on keys then lookup their values, \
           instead of iterating on key-value pairs directly."
      ; cli_flags= Some {deprecated= []; show_in_help= true}
      ; enabled_by_default= true
      ; activates= [] }
  | LithoRequiredProps ->
      { id= "litho-required-props"
      ; kind=
          UserFacing
            { title= "Litho \"Required Props\""
            ; markdown_body= [%blob "./documentation/checkers/LithoRequiredProps.md"] }
      ; support= mk_support_func ~java:Support ()
      ; short_documentation=
          "Checks that all non-optional `@Prop`s have been specified when constructing Litho \
           components."
      ; cli_flags= Some {deprecated= []; show_in_help= true}
      ; enabled_by_default= false
      ; activates= [] }
  | Liveness ->
      { id= "liveness"
      ; kind= UserFacing {title= "Liveness"; markdown_body= ""}
      ; support= mk_support_func ~clang:Support ()
      ; short_documentation= "Detection of dead stores and unused variables."
      ; cli_flags= Some {deprecated= []; show_in_help= true}
      ; enabled_by_default= true
      ; activates= [] }
  | LoopHoisting ->
      { id= "loop-hoisting"
      ; kind=
          UserFacing
            { title= "Loop Hoisting"
            ; markdown_body= [%blob "./documentation/checkers/LoopHoisting.md"] }
      ; support= mk_support_func ~clang:Support ~java:Support ()
      ; short_documentation=
          "Detect opportunities to hoist function calls that are invariant outside of loop bodies \
           for efficiency."
      ; cli_flags= Some {deprecated= []; show_in_help= true}
      ; enabled_by_default= false
      ; activates= [BufferOverrunAnalysis; PurityAnalysis] }
  | ParameterNotNullChecked ->
      { id= "parameter-not-null-checked"
      ; kind=
          UserFacing
            { title= "Parameter Not Null Checked"
            ; markdown_body= [%blob "./documentation/checkers/ParameterNotNullChecked.md"] }
      ; support= mk_support_func ~clang:Support ()
      ; short_documentation=
          "An Objective-C-specific analysis to detect when a block parameter is used before being \
           checked for null first."
      ; cli_flags= Some {deprecated= []; show_in_help= true}
      ; enabled_by_default= true
      ; activates= [] }
  | Pulse ->
      { id= "pulse"
      ; kind= UserFacing {title= "Pulse"; markdown_body= [%blob "./documentation/checkers/Pulse.md"]}
      ; support=
          mk_support_func ~clang:Support ~java:Support ~erlang:ExperimentalSupport ~hack:Support ()
      ; short_documentation= "General-purpose memory and value analysis engine."
      ; cli_flags= Some {deprecated= []; show_in_help= true}
      ; enabled_by_default= true
      ; activates= [] }
  | PurityAnalysis ->
      { id= "purity-analysis"
      ; kind= Internal
      ; support= mk_support_func ~clang:ExperimentalSupport ~java:ExperimentalSupport ()
      ; short_documentation= "Internal part of the purity checker."
      ; cli_flags= None
      ; enabled_by_default= false
      ; activates= [BufferOverrunAnalysis] }
  | PurityChecker ->
      { id= "purity"
      ; kind=
          UserFacing {title= "Purity"; markdown_body= [%blob "./documentation/checkers/Purity.md"]}
      ; support= mk_support_func ~clang:ExperimentalSupport ~java:ExperimentalSupport ()
      ; short_documentation=
          "Detects pure (side-effect-free) functions. A different implementation of \"impurity\"."
      ; cli_flags= Some {deprecated= []; show_in_help= true}
      ; enabled_by_default= false
      ; activates= [PurityAnalysis] }
  | RacerD ->
      { id= "racerd"
      ; kind=
          UserFacing {title= "RacerD"; markdown_body= [%blob "./documentation/checkers/RacerD.md"]}
      ; support= mk_support_func ~clang:Support ~java:Support ~csharp:Support ()
      ; short_documentation= "Thread safety analysis."
      ; cli_flags= Some {deprecated= []; show_in_help= true}
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
                 instructions](https://github.com/facebook/infer/blob/main/infer/src/labs/README.md)."
            }
      ; support= mk_support_func ~java:Support ~csharp:Support ()
      ; short_documentation=
          "Toy checker for the \"resource leak\" write-your-own-checker exercise."
      ; cli_flags= Some {deprecated= []; show_in_help= false}
      ; enabled_by_default= false
      ; activates= [] }
  | ScopeLeakage ->
      { id= "scope-leakage"
      ; kind= UserFacing {title= "Scope Leakage"; markdown_body= ""}
      ; support= mk_support_func ~java:Support ()
      ; short_documentation=
          "The Java/Kotlin checker takes into account a set of \"scope\" annotations and a \
           must-not-hold relation over the scopes. The checker raises an alarm if there exists a \
           field access path from object A to object B, with respective scopes SA and SB, such \
           that must-not-hold(SA, SB)."
      ; cli_flags= Some {deprecated= []; show_in_help= true}
      ; enabled_by_default= false
      ; activates= [] }
  | SIOF ->
      { id= "siof"
      ; kind= UserFacing {title= "Static Initialization Order Fiasco"; markdown_body= ""}
      ; support= mk_support_func ~clang:Support ()
      ; short_documentation=
          "Catches Static Initialization Order Fiascos in C++, that can lead to subtle, \
           compiler-version-dependent errors."
      ; cli_flags= Some {deprecated= []; show_in_help= true}
      ; enabled_by_default= true
      ; activates= [] }
  | Lineage ->
      { id= "lineage"
      ; kind= UserFacing {title= "Lineage"; markdown_body= ""}
      ; support= mk_support_func ~erlang:Support ()
      ; short_documentation= "Computes a dataflow graph"
      ; cli_flags= Some {deprecated= []; show_in_help= true}
      ; enabled_by_default= false
      ; activates= [LineageShape] }
  | LineageShape ->
      { id= "lineage-shape"
      ; kind= Internal
      ; support= mk_support_func ~erlang:Support ()
      ; short_documentation= "Computes shape informations to be used in the Lineage analysis"
      ; cli_flags= Some {deprecated= []; show_in_help= false}
      ; enabled_by_default= false
      ; activates= [] }
  | SelfInBlock ->
      { id= "self-in-block"
      ; kind= UserFacing {title= "Self in Block"; markdown_body= ""}
      ; support= mk_support_func ~clang:Support ()
      ; short_documentation=
          "An Objective-C-specific analysis to detect when a block captures `self`."
      ; cli_flags= Some {deprecated= ["-self_in_block"]; show_in_help= true}
      ; enabled_by_default= true
      ; activates= [] }
  | Starvation ->
      { id= "starvation"
      ; kind=
          UserFacing
            {title= "Starvation"; markdown_body= [%blob "./documentation/checkers/Starvation.md"]}
      ; support= mk_support_func ~clang:Support ~java:Support ()
      ; short_documentation=
          "Detect various kinds of situations when no progress is being made because of \
           concurrency errors."
      ; cli_flags= Some {deprecated= []; show_in_help= true}
      ; enabled_by_default= true
      ; activates= [] }
  | Topl ->
      { id= "topl"
      ; kind= UserFacing {title= "Topl"; markdown_body= [%blob "./documentation/checkers/Topl.md"]}
      ; support=
          mk_support_func ~clang:ExperimentalSupport ~java:ExperimentalSupport
            ~erlang:ExperimentalSupport ()
      ; short_documentation=
          "Detect errors based on user-provided state machines describing temporal properties over \
           multiple objects."
      ; cli_flags= Some {deprecated= []; show_in_help= true}
      ; enabled_by_default= false
      ; activates= [Pulse] }
  | SILValidation ->
      { id= "sil-validation"
      ; kind= UserFacing {title= "SIL validation"; markdown_body= ""}
      ; support= mk_support_func ~java:Support ()
      ; short_documentation=
          "This checker validates that all SIL instructions in all procedure bodies conform to a \
           (front-end specific) subset of SIL."
      ; cli_flags= Some {deprecated= []; show_in_help= true}
      ; enabled_by_default= false
      ; activates= [] }


let sanity_check config =
  let is_illegal_id_char c = match c with 'a' .. 'z' | '-' -> false | _ -> true in
  String.find config.id ~f:is_illegal_id_char
  |> Option.iter ~f:(fun c ->
         L.die InternalError
           "Illegal character '%c' in id: '%s'. Checker ids must be easy to pass on the command \
            line."
           c config.id ) ;
  ( match config.kind with
  | UserFacingDeprecated _ when config.enabled_by_default ->
      L.die InternalError "Checker %s is both deprecated and enabled by default." config.id
  | _ ->
      () ) ;
  ()


let config checker =
  let config = config_unsafe checker in
  sanity_check config ;
  config


let get_id c = (config c).id

let from_id id = List.find all ~f:(fun checker -> String.equal (get_id checker) id)

let is_user_facing c =
  let {kind} = config c in
  match kind with UserFacing _ | UserFacingDeprecated _ -> true | Exercise | Internal -> false


let pp_manual fmt checker =
  let {kind; short_documentation; activates} = config checker in
  let pp_kind fmt = function
    | UserFacing _ | Exercise ->
        ()
    | Internal ->
        F.fprintf fmt
          "\n\
           $(b,NOTE): This is used internally by other checkers and shouldn't need to be activated \
           directly."
    | UserFacingDeprecated {deprecation_message} ->
        F.fprintf fmt "\n\n$(b,DEPRECATED): %s\n" deprecation_message
  in
  let pp_activates fmt activates =
    match (List.filter ~f:is_user_facing) activates with
    | [] ->
        ()
    | _ :: _ as activates ->
        let pp_checker fmt c =
          let {id} = config c in
          F.fprintf fmt "$(i,%s)" id
        in
        F.fprintf fmt "\n$(i,ACTIVATES): %a" (Pp.seq ~sep:"," pp_checker) activates
  in
  F.fprintf fmt "%s%a%a" short_documentation pp_activates activates pp_kind kind


module Key = struct
  type nonrec t = t [@@deriving compare]

  let pp f checker = F.pp_print_string f (get_id checker)
end

module Set = PrettyPrintable.MakePPSet (Key)
module DependencyMap = PrettyPrintable.MakePPMonoMap (Key) (Set)

let dependency_map =
  let rec init_dep checker map =
    match DependencyMap.find_opt checker map with
    | Some checkers ->
        (checkers, map)
    | None ->
        let {activates} = config_unsafe checker in
        let dependencies, map =
          List.fold activates
            ~init:(Set.singleton checker, map)
            ~f:(fun (acc_dependencies, map) checker ->
              let new_dependencies, map = init_dep checker map in
              (Set.union acc_dependencies new_dependencies, map) )
        in
        (dependencies, DependencyMap.add checker dependencies map)
  in
  List.fold all ~init:DependencyMap.empty ~f:(fun map checker -> init_dep checker map |> snd)


let get_dependencies checker = DependencyMap.find checker dependency_map
