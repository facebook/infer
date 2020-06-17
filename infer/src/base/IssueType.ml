(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Die

type visibility = User | Developer | Silent [@@deriving compare, equal]

let string_of_visibility = function User -> "User" | Developer -> "Developer" | Silent -> "Silent"

type severity = Like | Info | Advice | Warning | Error [@@deriving compare, equal, enumerate]

let string_of_severity = function
  | Advice ->
      "ADVICE"
  | Error ->
      "ERROR"
  | Info ->
      "INFO"
  | Like ->
      "LIKE"
  | Warning ->
      "WARNING"


(* Make sure we cannot create new issue types other than by calling [register_from_string]. This is because
     we want to keep track of the list of all the issues ever declared. *)
module Unsafe : sig
  type t = private
    { unique_id: string
    ; checker: Checker.t
    ; visibility: visibility
    ; user_documentation: string option
    ; mutable default_severity: severity
    ; mutable enabled: bool
    ; mutable hum: string
    ; mutable doc_url: string option
    ; mutable linters_def_file: string option }
  [@@deriving compare]

  val equal : t -> t -> bool

  val pp : F.formatter -> t -> unit

  val find_from_string : id:string -> t option

  val register_from_string :
       ?enabled:bool
    -> ?is_cost_issue:bool
    -> ?hum:string
    -> ?doc_url:string
    -> ?linters_def_file:string
    -> id:string
    -> ?visibility:visibility
    -> ?user_documentation:string
    -> severity
    -> Checker.t
    -> t

  val register_from_cost_string :
       ?enabled:bool
    -> ?is_on_ui_thread:bool
    -> kind:CostKind.t
    -> (string -> string, F.formatter, unit, string) format4
    -> t

  val all_issues : unit -> t list

  val set_enabled : t -> bool -> unit
end = struct
  module T = struct
    type t =
      { unique_id: string
      ; checker: Checker.t
      ; visibility: visibility
      ; user_documentation: string option
      ; mutable default_severity: severity
      ; mutable enabled: bool
      ; mutable hum: string
      ; mutable doc_url: string option
      ; mutable linters_def_file: string option }

    let compare {unique_id= id1} {unique_id= id2} = String.compare id1 id2

    let equal = [%compare.equal: t]

    type rank = string

    let to_rank {unique_id} = unique_id

    let pp fmt t = F.pp_print_string fmt t.hum
  end

  include T
  module IssueSet = PrettyPrintable.MakePPUniqRankSet (String) (T)

  (** keep track of the list of all declared issue types *)
  let all_issues = ref IssueSet.empty

  let prettify s =
    String.lowercase s |> String.split ~on:'_' |> List.map ~f:String.capitalize
    |> String.concat ~sep:" " |> String.strip


  let set_enabled issue b = issue.enabled <- b

  let find_from_string ~id:unique_id = IssueSet.find_rank !all_issues unique_id

  (** Avoid creating new issue types. The idea is that there are three types of issue types:

      + Statically pre-defined issue types, namely the ones in this module

      + Dynamically created ones, eg from custom errors defined in the models, or defined by the
        user in AL linters

      + Issue types created at command-line-parsing time. These can mention issues of type 1. or 2.,
        but issues of type 2. have not yet been defined. Thus, we record only there [enabled] status
        definitely. The [hum]an-readable description can be updated when we encounter the definition
        of the issue type, eg in AL. *)
  let register_from_string ?(enabled = true) ?(is_cost_issue = false) ?hum:hum0 ?doc_url
      ?linters_def_file ~id:unique_id ?(visibility = User) ?user_documentation default_severity
      checker =
    match find_from_string ~id:unique_id with
    | ((Some
         ( { unique_id= _ (* we know it has to be the same *)
           ; checker= checker_old
           ; visibility= visibility_old
           ; user_documentation= _ (* new one must be [None] for dynamic issue types *)
           ; default_severity= _ (* mutable field to update *)
           ; enabled= _ (* not touching this one since [Config] will have set it *)
           ; hum= _ (* mutable field to update *)
           ; doc_url= _ (* mutable field to update *)
           ; linters_def_file= _ (* mutable field to update *) } as issue ))[@warning "+9"]) ->
        (* update fields that were supplied this time around, but keep the previous values of others
           and assert that the immutable fields are the same (see doc comment) *)
        let die_of_mismatch ~what ~old ~new_ =
          L.die InternalError
            "%s for issue \"%s\" doesn't match: found new %s \"%s\" but %s \"%s\" was already \
             registered for this issue type"
            (String.capitalize what) unique_id what new_ what old
        in
        if not (Checker.equal checker checker_old) then
          die_of_mismatch ~what:"checker" ~old:(Checker.get_id checker_old)
            ~new_:(Checker.get_id checker) ;
        if not (equal_visibility visibility visibility_old) then
          die_of_mismatch ~what:"visibility"
            ~old:(string_of_visibility visibility_old)
            ~new_:(string_of_visibility visibility) ;
        ( match user_documentation with
        | Some user_documentation when not is_cost_issue ->
            L.die InternalError "Unexpected user documentation for issue type %s:@\n@\n%s@\n"
              unique_id user_documentation
        | _ ->
            () ) ;
        issue.default_severity <- default_severity ;
        Option.iter hum0 ~f:(fun hum -> issue.hum <- hum) ;
        if Option.is_some doc_url then issue.doc_url <- doc_url ;
        if Option.is_some linters_def_file then issue.linters_def_file <- linters_def_file ;
        issue
    | None ->
        let hum = match hum0 with Some str -> str | _ -> prettify unique_id in
        let issue =
          { unique_id
          ; visibility
          ; user_documentation
          ; default_severity
          ; checker
          ; enabled
          ; hum
          ; doc_url
          ; linters_def_file }
        in
        all_issues := IssueSet.add !all_issues issue ;
        issue


  let cost_issue_doc_list =
    [ ( "EXECUTION_TIME_COMPLEXITY_INCREASE"
      , [%blob "../../documentation/issues/EXECUTION_TIME_COMPLEXITY_INCREASE.md"] )
    ; ( "EXECUTION_TIME_COMPLEXITY_INCREASE_UI_THREAD"
      , [%blob "../../documentation/issues/EXECUTION_TIME_COMPLEXITY_INCREASE_UI_THREAD.md"] )
    ; ( "EXECUTION_TIME_UNREACHABLE_AT_EXIT"
      , [%blob "../../documentation/issues/EXECUTION_TIME_UNREACHABLE_AT_EXIT.md"] )
    ; ("INFINITE_EXECUTION_TIME", [%blob "../../documentation/issues/INFINITE_EXECUTION_TIME.md"])
    ]


  (** cost issues are already registered below.*)
  let register_from_cost_string ?(enabled = true) ?(is_on_ui_thread = false) ~(kind : CostKind.t) s
      =
    let issue_type_base = F.asprintf s (CostKind.to_issue_string kind) in
    let issue_type = if is_on_ui_thread then issue_type_base ^ "_UI_THREAD" else issue_type_base in
    let user_documentation =
      match List.find cost_issue_doc_list ~f:(fun (s, _doc) -> String.equal s issue_type) with
      | Some (_, doc) ->
          doc
      | None ->
          L.die InternalError
            "Unexpected cost issue %s: either the issue is not enabled or unknown." issue_type
    in
    register_from_string ~is_cost_issue:true ~enabled ~id:issue_type Error Cost ~user_documentation


  let all_issues () = IssueSet.elements !all_issues
end

include Unsafe

let checker_can_report reporting_checker {checker= allowed_checker} =
  Checker.equal reporting_checker allowed_checker


let abduction_case_not_implemented =
  register_from_string ~visibility:Developer ~id:"Abduction_case_not_implemented" Error Biabduction


let array_of_pointsto =
  register_from_string ~visibility:Developer ~id:"Array_of_pointsto" Error Biabduction


let array_out_of_bounds_l1 =
  register_from_string ~visibility:Developer ~enabled:false ~id:"ARRAY_OUT_OF_BOUNDS_L1" Error
    Biabduction


let array_out_of_bounds_l2 =
  register_from_string ~visibility:Developer ~enabled:false ~id:"ARRAY_OUT_OF_BOUNDS_L2" Warning
    Biabduction


let array_out_of_bounds_l3 =
  register_from_string ~visibility:Developer ~enabled:false ~id:"ARRAY_OUT_OF_BOUNDS_L3" Warning
    Biabduction


let assert_failure =
  register_from_string ~visibility:Developer ~id:"Assert_failure" Error Biabduction


let _assign_pointer_warning =
  register_from_string ~id:"ASSIGN_POINTER_WARNING" Warning Linters
    ~user_documentation:[%blob "../../documentation/issues/ASSIGN_POINTER_WARNING.md"]


let bad_footprint = register_from_string ~visibility:Developer ~id:"Bad_footprint" Error Biabduction

let _bad_pointer_comparison =
  register_from_string ~id:"BAD_POINTER_COMPARISON" Warning Linters
    ~user_documentation:[%blob "../../documentation/issues/BAD_POINTER_COMPARISON.md"]


let biabduction_analysis_stops =
  register_from_string ~visibility:Developer ~enabled:false ~id:"BIABDUCTION_ANALYSIS_STOPS" Warning
    Biabduction


let buffer_overrun_l1 =
  register_from_string ~id:"BUFFER_OVERRUN_L1" Error BufferOverrunChecker
    ~user_documentation:[%blob "../../documentation/issues/BUFFER_OVERRUN.md"]


let buffer_overrun_l2 =
  register_from_string ~id:"BUFFER_OVERRUN_L2" Error BufferOverrunChecker
    ~user_documentation:"See [BUFFER_OVERRUN_L1](#buffer_overrun_l1)"


let buffer_overrun_l3 =
  register_from_string ~id:"BUFFER_OVERRUN_L3" Error BufferOverrunChecker
    ~user_documentation:"See [BUFFER_OVERRUN_L1](#buffer_overrun_l1)"


let buffer_overrun_l4 =
  register_from_string ~enabled:false ~id:"BUFFER_OVERRUN_L4" Error BufferOverrunChecker
    ~user_documentation:"See [BUFFER_OVERRUN_L1](#buffer_overrun_l1)"


let buffer_overrun_l5 =
  register_from_string ~enabled:false ~id:"BUFFER_OVERRUN_L5" Error BufferOverrunChecker
    ~user_documentation:"See [BUFFER_OVERRUN_L1](#buffer_overrun_l1)"


let buffer_overrun_r2 =
  register_from_string ~id:"BUFFER_OVERRUN_R2" Error BufferOverrunChecker
    ~user_documentation:"See [BUFFER_OVERRUN_L1](#buffer_overrun_l1)"


let buffer_overrun_s2 =
  register_from_string ~id:"BUFFER_OVERRUN_S2" Error BufferOverrunChecker
    ~user_documentation:"See [BUFFER_OVERRUN_L1](#buffer_overrun_l1)"


let buffer_overrun_t1 =
  register_from_string ~id:"BUFFER_OVERRUN_T1" Error BufferOverrunChecker
    ~user_documentation:"See [BUFFER_OVERRUN_L1](#buffer_overrun_l1)"


let buffer_overrun_u5 =
  register_from_string ~enabled:false ~id:"BUFFER_OVERRUN_U5" Error BufferOverrunChecker
    ~user_documentation:"See [BUFFER_OVERRUN_L1](#buffer_overrun_l1)"


let cannot_star = register_from_string ~visibility:Developer ~id:"Cannot_star" Error Biabduction

let captured_strong_self =
  register_from_string ~id:"CAPTURED_STRONG_SELF" ~hum:"Captured strongSelf" Error SelfInBlock
    ~user_documentation:[%blob "../../documentation/issues/CAPTURED_STRONG_SELF.md"]


let checkers_allocates_memory =
  register_from_string ~id:"CHECKERS_ALLOCATES_MEMORY" ~hum:"Allocates Memory" Error
    AnnotationReachability
    ~user_documentation:[%blob "../../documentation/issues/CHECKERS_ALLOCATES_MEMORY.md"]


let checkers_annotation_reachability_error =
  register_from_string ~id:"CHECKERS_ANNOTATION_REACHABILITY_ERROR"
    ~hum:"Annotation Reachability Error" Error AnnotationReachability
    ~user_documentation:
      [%blob "../../documentation/issues/CHECKERS_ANNOTATION_REACHABILITY_ERROR.md"]


let checkers_calls_expensive_method =
  register_from_string ~id:"CHECKERS_CALLS_EXPENSIVE_METHOD" ~hum:"Expensive Method Called" Error
    AnnotationReachability
    ~user_documentation:[%blob "../../documentation/issues/CHECKERS_CALLS_EXPENSIVE_METHOD.md"]


let checkers_expensive_overrides_unexpensive =
  register_from_string ~id:"CHECKERS_EXPENSIVE_OVERRIDES_UNANNOTATED"
    ~hum:"Expensive Overrides Unannotated" Error AnnotationReachability
    ~user_documentation:
      [%blob "../../documentation/issues/CHECKERS_EXPENSIVE_OVERRIDES_UNANNOTATED.md"]


let checkers_fragment_retain_view =
  register_from_string ~id:"CHECKERS_FRAGMENT_RETAINS_VIEW" ~hum:"Fragment Retains View" Warning
    FragmentRetainsView
    ~user_documentation:[%blob "../../documentation/issues/CHECKERS_FRAGMENT_RETAINS_VIEW.md"]


let checkers_immutable_cast =
  register_from_string ~id:"CHECKERS_IMMUTABLE_CAST" Warning ImmutableCast
    ~user_documentation:[%blob "../../documentation/issues/CHECKERS_IMMUTABLE_CAST.md"]


let checkers_printf_args =
  register_from_string ~id:"CHECKERS_PRINTF_ARGS" Error PrintfArgs
    ~user_documentation:[%blob "../../documentation/issues/CHECKERS_PRINTF_ARGS.md"]


let class_cast_exception =
  register_from_string ~visibility:Developer ~enabled:false ~id:"CLASS_CAST_EXCEPTION" Error
    Biabduction


let component_factory_function =
  register_from_string ~id:"COMPONENT_FACTORY_FUNCTION" Advice Linters
    ~user_documentation:[%blob "../../documentation/issues/COMPONENT_FACTORY_FUNCTION.md"]


let component_file_cyclomatic_complexity =
  register_from_string ~id:"COMPONENT_FILE_CYCLOMATIC_COMPLEXITY" Info Linters


let component_file_line_count =
  register_from_string ~enabled:false ~id:"COMPONENT_FILE_LINE_COUNT" Info Linters


let component_initializer_with_side_effects =
  register_from_string ~id:"COMPONENT_INITIALIZER_WITH_SIDE_EFFECTS" Advice Linters
    ~user_documentation:
      [%blob "../../documentation/issues/COMPONENT_INITIALIZER_WITH_SIDE_EFFECTS.md"]


let component_with_multiple_factory_methods =
  register_from_string ~id:"COMPONENT_WITH_MULTIPLE_FACTORY_METHODS" Advice Linters
    ~user_documentation:
      [%blob "../../documentation/issues/COMPONENT_WITH_MULTIPLE_FACTORY_METHODS.md"]


let component_with_unconventional_superclass =
  register_from_string ~id:"COMPONENT_WITH_UNCONVENTIONAL_SUPERCLASS" Advice Linters
    ~user_documentation:
      [%blob "../../documentation/issues/COMPONENT_WITH_UNCONVENTIONAL_SUPERCLASS.md"]


let condition_always_false =
  register_from_string ~enabled:false ~id:"CONDITION_ALWAYS_FALSE" Warning BufferOverrunChecker
    ~user_documentation:"A condition expression is **always** evaluated to false."


let condition_always_true =
  register_from_string ~enabled:false ~id:"CONDITION_ALWAYS_TRUE" Warning BufferOverrunChecker
    ~user_documentation:"A condition expression is **always** evaluated to true."


(* ~user_documentation:"A config checking is done between a marker's start and end" *)
let config_checks_between_markers =
  register_from_string ~enabled:false ~id:"CONFIG_CHECKS_BETWEEN_MARKERS" Advice
    ConfigChecksBetweenMarkers


let constant_address_dereference =
  register_from_string ~enabled:false ~id:"CONSTANT_ADDRESS_DEREFERENCE" Warning Pulse
    ~user_documentation:[%blob "../../documentation/issues/CONSTANT_ADDRESS_DEREFERENCE.md"]


let create_intent_from_uri =
  register_from_string ~id:"CREATE_INTENT_FROM_URI" Error Quandary
    ~user_documentation:
      "Create an intent/start a component using a (possibly user-controlled) URI. may or may not \
       be an issue depending on where the URI comes from."


let cross_site_scripting =
  register_from_string ~id:"CROSS_SITE_SCRIPTING" Error Quandary
    ~user_documentation:"Untrusted data flows into HTML; XSS risk."


let _cxx_reference_captured_in_objc_block =
  register_from_string ~id:"CXX_REFERENCE_CAPTURED_IN_OBJC_BLOCK" Warning Linters
    ~user_documentation:[%blob "../../documentation/issues/CXX_REFERENCE_CAPTURED_IN_OBJC_BLOCK.md"]


let dangling_pointer_dereference =
  register_from_string ~enabled:false ~id:"DANGLING_POINTER_DEREFERENCE" Error Biabduction


let dangling_pointer_dereference_maybe =
  register_from_string ~visibility:Developer ~enabled:false ~id:"DANGLING_POINTER_DEREFERENCE_MAYBE"
    Warning Biabduction


let dead_store =
  register_from_string ~id:"DEAD_STORE" Error Liveness
    ~user_documentation:[%blob "../../documentation/issues/DEAD_STORE.md"]


let deadlock =
  register_from_string ~id:"DEADLOCK" Error Starvation
    ~user_documentation:[%blob "../../documentation/issues/DEADLOCK.md"]


let _direct_atomic_property_access =
  register_from_string ~id:"DIRECT_ATOMIC_PROPERTY_ACCESS" Warning Linters
    ~user_documentation:[%blob "../../documentation/issues/DIRECT_ATOMIC_PROPERTY_ACCESS.md"]


let _discouraged_weak_property_custom_setter =
  register_from_string ~id:"DISCOURAGED_WEAK_PROPERTY_CUSTOM_SETTER" Warning Linters
    ~user_documentation:
      [%blob "../../documentation/issues/DISCOURAGED_WEAK_PROPERTY_CUSTOM_SETTER.md"]


let divide_by_zero = register_from_string ~enabled:false ~id:"DIVIDE_BY_ZERO" Error Biabduction

let do_not_report = register_from_string ~id:"DO_NOT_REPORT" Error Quandary

let empty_vector_access =
  register_from_string ~id:"EMPTY_VECTOR_ACCESS" Error Biabduction
    ~user_documentation:[%blob "../../documentation/issues/EMPTY_VECTOR_ACCESS.md"]


(* Condition redundant is a very non-precise issue. Depending on the origin of what is compared with
   null, this can have a lot of reasons to be actually nullable.

   Until it is made non-precise, it is recommended to not turn this warning on.  But even when it is
   on, this should not be more than advice.  *)
let eradicate_condition_redundant =
  register_from_string ~id:"ERADICATE_CONDITION_REDUNDANT" ~hum:"Condition Redundant" Advice
    Eradicate
    ~user_documentation:[%blob "../../documentation/issues/ERADICATE_CONDITION_REDUNDANT.md"]


(* TODO(T54070503) remove condition redundant nonnull *)
let _ =
  register_from_string ~id:"ERADICATE_CONDITION_REDUNDANT_NONNULL"
    ~hum:"Condition Redundant Non-Null" Warning Eradicate


let eradicate_field_not_initialized =
  register_from_string ~id:"ERADICATE_FIELD_NOT_INITIALIZED" ~hum:"Field Not Initialized" Warning
    Eradicate
    ~user_documentation:[%blob "../../documentation/issues/ERADICATE_FIELD_NOT_INITIALIZED.md"]


let eradicate_field_not_nullable =
  register_from_string ~id:"ERADICATE_FIELD_NOT_NULLABLE" ~hum:"Field Not Nullable" Warning
    Eradicate
    ~user_documentation:[%blob "../../documentation/issues/ERADICATE_FIELD_NOT_NULLABLE.md"]


(* Very non-precise issue. Should be actually turned off unless for experimental purposes. *)
let eradicate_field_over_annotated =
  register_from_string ~id:"ERADICATE_FIELD_OVER_ANNOTATED" ~hum:"Field Over Annotated" Advice
    Eradicate


let eradicate_inconsistent_subclass_parameter_annotation =
  register_from_string ~id:"ERADICATE_INCONSISTENT_SUBCLASS_PARAMETER_ANNOTATION"
    ~hum:"Inconsistent Subclass Parameter Annotation" Warning Eradicate
    ~user_documentation:
      [%blob "../../documentation/issues/ERADICATE_INCONSISTENT_SUBCLASS_PARAMETER_ANNOTATION.md"]


let eradicate_inconsistent_subclass_return_annotation =
  register_from_string ~id:"ERADICATE_INCONSISTENT_SUBCLASS_RETURN_ANNOTATION"
    ~hum:"Inconsistent Subclass Return Annotation" Warning Eradicate
    ~user_documentation:
      [%blob "../../documentation/issues/ERADICATE_INCONSISTENT_SUBCLASS_RETURN_ANNOTATION.md"]


let eradicate_redundant_nested_class_annotation =
  register_from_string ~id:"ERADICATE_REDUNDANT_NESTED_CLASS_ANNOTATION"
    ~hum:"@Nullsafe annotation is redundant" Advice Eradicate


let eradicate_bad_nested_class_annotation =
  register_from_string ~id:"ERADICATE_BAD_NESTED_CLASS_ANNOTATION"
    ~hum:"@Nullsafe annotation is inconsistent with outer class" Warning Eradicate


let eradicate_nullable_dereference =
  register_from_string ~id:"ERADICATE_NULLABLE_DEREFERENCE" ~hum:"Nullable Dereference" Warning
    Eradicate


let eradicate_parameter_not_nullable =
  register_from_string ~id:"ERADICATE_PARAMETER_NOT_NULLABLE" ~hum:"Parameter Not Nullable" Warning
    Eradicate
    ~user_documentation:[%blob "../../documentation/issues/ERADICATE_PARAMETER_NOT_NULLABLE.md"]


let eradicate_return_not_nullable =
  register_from_string ~id:"ERADICATE_RETURN_NOT_NULLABLE" ~hum:"Return Not Nullable" Warning
    Eradicate
    ~user_documentation:[%blob "../../documentation/issues/ERADICATE_RETURN_NOT_NULLABLE.md"]


(* Very non-precise issue. Should be actually turned off unless for experimental purposes. *)
let eradicate_return_over_annotated =
  register_from_string ~id:"ERADICATE_RETURN_OVER_ANNOTATED" ~hum:"Return Over Annotated" Advice
    Eradicate
    ~user_documentation:[%blob "../../documentation/issues/ERADICATE_RETURN_OVER_ANNOTATED.md"]


let eradicate_unchecked_usage_in_nullsafe =
  register_from_string ~id:"ERADICATE_UNCHECKED_USAGE_IN_NULLSAFE"
    ~hum:"Nullsafe mode: unchecked usage of a value" Warning Eradicate


let eradicate_unvetted_third_party_in_nullsafe =
  register_from_string ~id:"ERADICATE_UNVETTED_THIRD_PARTY_IN_NULLSAFE"
    ~hum:"Nullsafe mode: unchecked usage of unvetted third-party" Warning Eradicate


(* Meta issues in eradicate are technical issues reflecting null-safety state of classes in general,
   in contrast with concrete nullability type violations *)

let eradicate_meta_class_is_nullsafe =
  register_from_string ~id:"ERADICATE_META_CLASS_IS_NULLSAFE"
    ~hum:
      "Class is marked @Nullsafe and has 0 issues" (* Should be enabled for special integrations *)
    ~enabled:false Info Eradicate


(* Class is either:
   - has at least one nullability issue.
   - has at least one (currently possibly hidden) issue in order to be marked as @Nullsafe.
 *)
let eradicate_meta_class_needs_improvement =
  register_from_string ~id:"ERADICATE_META_CLASS_NEEDS_IMPROVEMENT"
    ~hum:
      "Class needs improvement to become @Nullsafe" (* Should be enabled for special integrations *)
    ~enabled:false Info Eradicate


let eradicate_meta_class_can_be_nullsafe =
  register_from_string ~id:"ERADICATE_META_CLASS_CAN_BE_NULLSAFE"
    ~hum:
      "Class has 0 issues and can be marked @Nullsafe"
      (* Should be enabled for special integrations *) ~enabled:false Advice Eradicate


let exposed_insecure_intent_handling =
  register_from_string ~id:"EXPOSED_INSECURE_INTENT_HANDLING" Error Quandary
    ~user_documentation:"Undocumented."


let failure_exe = register_from_string ~visibility:Silent ~id:"Failure_exe" Info Biabduction

let field_not_null_checked =
  register_from_string ~id:"IVAR_NOT_NULL_CHECKED" Warning Biabduction
    ~user_documentation:[%blob "../../documentation/issues/IVAR_NOT_NULL_CHECKED.md"]


(* from AL default linters *)
let _global_variable_initialized_with_function_or_method_call =
  register_from_string ~enabled:false ~id:"GLOBAL_VARIABLE_INITIALIZED_WITH_FUNCTION_OR_METHOD_CALL"
    Warning Linters
    ~user_documentation:
      [%blob
        "../../documentation/issues/GLOBAL_VARIABLE_INITIALIZED_WITH_FUNCTION_OR_METHOD_CALL.md"]


let guardedby_violation_racerd =
  register_from_string Warning ~id:"GUARDEDBY_VIOLATION" ~hum:"GuardedBy Violation" RacerD
    ~user_documentation:[%blob "../../documentation/issues/GUARDEDBY_VIOLATION.md"]


let impure_function = register_from_string ~id:"IMPURE_FUNCTION" Error Impurity

let inefficient_keyset_iterator =
  register_from_string ~id:"INEFFICIENT_KEYSET_ITERATOR" Error InefficientKeysetIterator
    ~user_documentation:[%blob "../../documentation/issues/INEFFICIENT_KEYSET_ITERATOR.md"]


let inferbo_alloc_is_big =
  register_from_string ~id:"INFERBO_ALLOC_IS_BIG" Error BufferOverrunChecker
    ~user_documentation:"`malloc` is passed a large constant value."


let inferbo_alloc_is_negative =
  register_from_string ~id:"INFERBO_ALLOC_IS_NEGATIVE" Error BufferOverrunChecker
    ~user_documentation:"`malloc` is called with a negative size."


let inferbo_alloc_is_zero =
  register_from_string ~id:"INFERBO_ALLOC_IS_ZERO" Error BufferOverrunChecker
    ~user_documentation:"`malloc` is called with a zero size."


let inferbo_alloc_may_be_big =
  register_from_string ~id:"INFERBO_ALLOC_MAY_BE_BIG" Error BufferOverrunChecker
    ~user_documentation:"`malloc` *may* be called with a large value."


let inferbo_alloc_may_be_negative =
  register_from_string ~id:"INFERBO_ALLOC_MAY_BE_NEGATIVE" Error BufferOverrunChecker
    ~user_documentation:"`malloc` *may* be called with a negative value."


let inferbo_alloc_may_be_tainted =
  register_from_string ~id:"INFERBO_ALLOC_MAY_BE_TAINTED" Error BufferOverrunChecker
    ~user_documentation:
      "`malloc` *may* be called with a tainted value from external sources.  This is experimental \
       and will be removed sooner or later."


let infinite_cost_call ~kind = register_from_cost_string ~enabled:false "INFINITE_%s" ~kind

let inherently_dangerous_function =
  register_from_string ~visibility:Developer ~id:"INHERENTLY_DANGEROUS_FUNCTION" Warning Biabduction


let insecure_intent_handling =
  register_from_string ~id:"INSECURE_INTENT_HANDLING" Error Quandary
    ~user_documentation:"Undocumented."


let integer_overflow_l1 =
  register_from_string ~id:"INTEGER_OVERFLOW_L1" Error BufferOverrunChecker
    ~user_documentation:[%blob "../../documentation/issues/INTEGER_OVERFLOW.md"]


let integer_overflow_l2 =
  register_from_string ~id:"INTEGER_OVERFLOW_L2" Error BufferOverrunChecker
    ~user_documentation:"See [INTEGER_OVERFLOW_L1](#integer_overflow_l1)"


let integer_overflow_l5 =
  register_from_string ~enabled:false ~id:"INTEGER_OVERFLOW_L5" Error BufferOverrunChecker
    ~user_documentation:"See [INTEGER_OVERFLOW_L1](#integer_overflow_l1)"


let integer_overflow_r2 =
  register_from_string ~id:"INTEGER_OVERFLOW_R2" Error BufferOverrunChecker
    ~user_documentation:"See [INTEGER_OVERFLOW_L1](#integer_overflow_l1)"


let integer_overflow_u5 =
  register_from_string ~enabled:false ~id:"INTEGER_OVERFLOW_U5" Error BufferOverrunChecker
    ~user_documentation:"See [INTEGER_OVERFLOW_L1](#integer_overflow_l1)"


let interface_not_thread_safe =
  register_from_string Warning ~id:"INTERFACE_NOT_THREAD_SAFE" RacerD
    ~user_documentation:[%blob "../../documentation/issues/INTERFACE_NOT_THREAD_SAFE.md"]


let internal_error =
  register_from_string ~visibility:Developer ~id:"Internal_error" Error Biabduction


let invariant_call = register_from_string ~enabled:false ~id:"INVARIANT_CALL" Error LoopHoisting

let javascript_injection =
  register_from_string ~id:"JAVASCRIPT_INJECTION" Error Quandary
    ~user_documentation:"Untrusted data flows into JavaScript."


let lab_resource_leak = register_from_string ~id:"LAB_RESOURCE_LEAK" Error ResourceLeakLabExercise

let leak_after_array_abstraction =
  register_from_string ~visibility:Developer ~id:"Leak_after_array_abstraction" Error Biabduction


let leak_in_footprint =
  register_from_string ~visibility:Developer ~id:"Leak_in_footprint" Error Biabduction


let leak_unknown_origin =
  register_from_string ~visibility:Developer ~enabled:false ~id:"Leak_unknown_origin" Error
    Biabduction


let lock_consistency_violation =
  register_from_string Warning ~id:"LOCK_CONSISTENCY_VIOLATION" RacerD
    ~user_documentation:[%blob "../../documentation/issues/LOCK_CONSISTENCY_VIOLATION.md"]


let lockless_violation =
  register_from_string ~id:"LOCKLESS_VIOLATION" Error Starvation
    ~user_documentation:[%blob "../../documentation/issues/LOCKLESS_VIOLATION.md"]


let logging_private_data =
  register_from_string ~id:"LOGGING_PRIVATE_DATA" Error Quandary ~user_documentation:"Undocumented."


let expensive_loop_invariant_call =
  register_from_string ~id:"EXPENSIVE_LOOP_INVARIANT_CALL" Error LoopHoisting


let memory_leak =
  register_from_string ~enabled:false ~id:"BIABDUCTION_MEMORY_LEAK" ~hum:"Memory Leak" Error
    Biabduction


let missing_fld =
  register_from_string ~visibility:Developer ~id:"Missing_fld" ~hum:"Missing Field" Error
    Biabduction


let missing_required_prop =
  register_from_string ~id:"MISSING_REQUIRED_PROP" Error LithoRequiredProps


let mixed_self_weakself =
  register_from_string ~id:"MIXED_SELF_WEAKSELF" ~hum:"Mixed Self WeakSelf" Error SelfInBlock
    ~user_documentation:[%blob "../../documentation/issues/MIXED_SELF_WEAKSELF.md"]


let multiple_weakself =
  register_from_string ~id:"MULTIPLE_WEAKSELF" ~hum:"Multiple WeakSelf Use" Error SelfInBlock
    ~user_documentation:[%blob "../../documentation/issues/MULTIPLE_WEAKSELF.md"]


let mutable_local_variable_in_component_file =
  register_from_string ~id:"MUTABLE_LOCAL_VARIABLE_IN_COMPONENT_FILE" Advice Linters
    ~user_documentation:
      [%blob "../../documentation/issues/MUTABLE_LOCAL_VARIABLE_IN_COMPONENT_FILE.md"]


let null_dereference =
  register_from_string ~id:"NULL_DEREFERENCE" Error Biabduction
    ~user_documentation:[%blob "../../documentation/issues/NULL_DEREFERENCE.md"]


let null_test_after_dereference =
  register_from_string ~enabled:false ~id:"NULL_TEST_AFTER_DEREFERENCE" Warning Biabduction


let nullptr_dereference =
  register_from_string ~enabled:false ~id:"NULLPTR_DEREFERENCE" Error Pulse
    ~user_documentation:"See [NULL_DEREFERENCE](#null_dereference)."


let parameter_not_null_checked =
  register_from_string ~id:"PARAMETER_NOT_NULL_CHECKED" Warning Biabduction
    ~user_documentation:[%blob "../../documentation/issues/PARAMETER_NOT_NULL_CHECKED.md"]


let pointer_size_mismatch = register_from_string ~id:"POINTER_SIZE_MISMATCH" Error Biabduction

let _pointer_to_const_objc_class =
  register_from_string ~id:"POINTER_TO_CONST_OBJC_CLASS" Warning Linters
    ~user_documentation:[%blob "../../documentation/issues/POINTER_TO_CONST_OBJC_CLASS.md"]


let precondition_not_found =
  register_from_string ~visibility:Developer ~id:"PRECONDITION_NOT_FOUND" Error Biabduction


let precondition_not_met =
  register_from_string ~visibility:Developer ~id:"PRECONDITION_NOT_MET" Warning Biabduction


let premature_nil_termination =
  register_from_string ~id:"PREMATURE_NIL_TERMINATION_ARGUMENT" Warning Biabduction
    ~user_documentation:[%blob "../../documentation/issues/PREMATURE_NIL_TERMINATION_ARGUMENT.md"]


let pulse_memory_leak =
  register_from_string ~id:"MEMORY_LEAK" Error Pulse
    ~user_documentation:[%blob "../../documentation/issues/MEMORY_LEAK.md"]


let pure_function = register_from_string ~id:"PURE_FUNCTION" Error Purity

let quandary_taint_error =
  register_from_string ~hum:"Taint Error" ~id:"QUANDARY_TAINT_ERROR" Error Quandary
    ~user_documentation:"Generic taint error when nothing else fits."


let _registered_observer_being_deallocated =
  register_from_string ~id:"REGISTERED_OBSERVER_BEING_DEALLOCATED" Warning Linters
    ~user_documentation:
      [%blob "../../documentation/issues/REGISTERED_OBSERVER_BEING_DEALLOCATED.md"]


let resource_leak =
  register_from_string ~id:"RESOURCE_LEAK" Error Biabduction
    ~user_documentation:[%blob "../../documentation/issues/RESOURCE_LEAK.md"]


let retain_cycle =
  register_from_string ~enabled:true ~id:"RETAIN_CYCLE" Error Biabduction
    ~user_documentation:[%blob "../../documentation/issues/RETAIN_CYCLE.md"]


let skip_function =
  register_from_string ~visibility:Developer ~enabled:false ~id:"SKIP_FUNCTION" Info Biabduction


let skip_pointer_dereference =
  register_from_string ~enabled:false ~id:"SKIP_POINTER_DEREFERENCE" Info Biabduction


let shell_injection =
  register_from_string ~id:"SHELL_INJECTION" Error Quandary
    ~user_documentation:"Environment variable or file data flowing to shell."


let shell_injection_risk =
  register_from_string ~id:"SHELL_INJECTION_RISK" Error Quandary
    ~user_documentation:"Code injection if the caller of the endpoint doesn't sanitize on its end."


let sql_injection =
  register_from_string ~id:"SQL_INJECTION" Error Quandary
    ~user_documentation:"Untrusted and unescaped data flows to SQL."


let sql_injection_risk =
  register_from_string ~id:"SQL_INJECTION_RISK" Error Quandary
    ~user_documentation:"Untrusted and unescaped data flows to SQL."


let stack_variable_address_escape =
  register_from_string ~id:"STACK_VARIABLE_ADDRESS_ESCAPE" Error Pulse
    ~user_documentation:[%blob "../../documentation/issues/STACK_VARIABLE_ADDRESS_ESCAPE.md"]


let starvation =
  register_from_string ~id:"STARVATION" ~hum:"UI Thread Starvation" Error Starvation
    ~user_documentation:[%blob "../../documentation/issues/STARVATION.md"]


let static_initialization_order_fiasco =
  register_from_string ~id:"STATIC_INITIALIZATION_ORDER_FIASCO" Error SIOF
    ~user_documentation:[%blob "../../documentation/issues/STATIC_INITIALIZATION_ORDER_FIASCO.md"]


let strict_mode_violation =
  register_from_string ~id:"STRICT_MODE_VIOLATION" ~hum:"Strict Mode Violation" Error Starvation
    ~user_documentation:[%blob "../../documentation/issues/STRICT_MODE_VIOLATION.md"]


let _strong_delegate_warning =
  register_from_string ~id:"STRONG_DELEGATE_WARNING" Warning Linters
    ~user_documentation:[%blob "../../documentation/issues/STRONG_DELEGATE_WARNING.md"]


let strong_self_not_checked =
  register_from_string ~id:"STRONG_SELF_NOT_CHECKED" ~hum:"StrongSelf Not Checked" Error SelfInBlock
    ~user_documentation:[%blob "../../documentation/issues/STRONG_SELF_NOT_CHECKED.md"]


let symexec_memory_error =
  register_from_string ~visibility:Developer ~id:"Symexec_memory_error"
    ~hum:"Symbolic Execution Memory Error" Error Biabduction


let thread_safety_violation =
  register_from_string Warning ~id:"THREAD_SAFETY_VIOLATION" RacerD
    ~user_documentation:[%blob "../../documentation/issues/THREAD_SAFETY_VIOLATION.md"]


let complexity_increase ~kind ~is_on_ui_thread =
  register_from_cost_string ~kind ~is_on_ui_thread "%s_COMPLEXITY_INCREASE"


let topl_error = register_from_string ~id:"TOPL_ERROR" Error TOPL

let unary_minus_applied_to_unsigned_expression =
  register_from_string ~enabled:false ~id:"UNARY_MINUS_APPLIED_TO_UNSIGNED_EXPRESSION" Warning
    Biabduction


let _unavailable_api_in_supported_ios_sdk =
  register_from_string ~id:"UNAVAILABLE_API_IN_SUPPORTED_IOS_SDK" Warning Linters
    ~user_documentation:[%blob "../../documentation/issues/UNAVAILABLE_API_IN_SUPPORTED_IOS_SDK.md"]


let uninitialized_value =
  register_from_string ~id:"UNINITIALIZED_VALUE" Error Uninit
    ~user_documentation:[%blob "../../documentation/issues/UNINITIALIZED_VALUE.md"]


let unreachable_code_after =
  register_from_string ~id:"UNREACHABLE_CODE" Error BufferOverrunChecker
    ~user_documentation:"A program point is unreachable."


let use_after_delete =
  register_from_string ~id:"USE_AFTER_DELETE" Error Pulse
    ~user_documentation:[%blob "../../documentation/issues/USE_AFTER_DELETE.md"]


let use_after_free =
  register_from_string ~id:"USE_AFTER_FREE" Error Pulse
    ~user_documentation:[%blob "../../documentation/issues/USE_AFTER_FREE.md"]


let use_after_lifetime =
  register_from_string ~id:"USE_AFTER_LIFETIME" Error Pulse
    ~user_documentation:[%blob "../../documentation/issues/USE_AFTER_LIFETIME.md"]


let user_controlled_sql_risk =
  register_from_string ~id:"USER_CONTROLLED_SQL_RISK" Error Quandary
    ~user_documentation:"Untrusted data flows to SQL (no injection risk)."


let untrusted_buffer_access =
  register_from_string ~enabled:false ~id:"UNTRUSTED_BUFFER_ACCESS" Error Quandary
    ~user_documentation:"Untrusted data of any kind flowing to buffer."


let untrusted_deserialization =
  register_from_string ~id:"UNTRUSTED_DESERIALIZATION" Error Quandary
    ~user_documentation:"User-controlled deserialization."


let untrusted_deserialization_risk =
  register_from_string ~id:"UNTRUSTED_DESERIALIZATION_RISK" Error Quandary
    ~user_documentation:"User-controlled deserialization"


let untrusted_environment_change_risk =
  register_from_string ~id:"UNTRUSTED_ENVIRONMENT_CHANGE_RISK" Error Quandary
    ~user_documentation:"User-controlled environment mutation."


let untrusted_file =
  register_from_string ~id:"UNTRUSTED_FILE" Error Quandary
    ~user_documentation:
      "User-controlled file creation; may be vulnerable to path traversal and more."


let untrusted_file_risk =
  register_from_string ~id:"UNTRUSTED_FILE_RISK" Error Quandary
    ~user_documentation:
      "User-controlled file creation; may be vulnerable to path traversal and more."


let untrusted_heap_allocation =
  register_from_string ~enabled:false ~id:"UNTRUSTED_HEAP_ALLOCATION" Error Quandary
    ~user_documentation:
      "Untrusted data of any kind flowing to heap allocation. this can cause crashes or DOS."


let untrusted_intent_creation =
  register_from_string ~id:"UNTRUSTED_INTENT_CREATION" Error Quandary
    ~user_documentation:"Creating an Intent from user-controlled data."


let untrusted_url_risk =
  register_from_string ~id:"UNTRUSTED_URL_RISK" Error Quandary
    ~user_documentation:"Untrusted flag, environment variable, or file data flowing to URL."


let untrusted_variable_length_array =
  register_from_string ~id:"UNTRUSTED_VARIABLE_LENGTH_ARRAY" Error Quandary
    ~user_documentation:
      "Untrusted data of any kind flowing to stack buffer allocation. Trying to allocate a stack \
       buffer that's too large will cause a stack overflow."


let vector_invalidation = register_from_string ~id:"VECTOR_INVALIDATION" Error Pulse

let weak_self_in_noescape_block =
  register_from_string ~id:"WEAK_SELF_IN_NO_ESCAPE_BLOCK" Error SelfInBlock
    ~user_documentation:[%blob "../../documentation/issues/WEAK_SELF_IN_NO_ESCAPE_BLOCK.md"]


let wrong_argument_number =
  register_from_string ~visibility:Developer ~id:"Wrong_argument_number"
    ~hum:"Wrong Argument Number" Error Biabduction


let unreachable_cost_call ~kind =
  register_from_cost_string ~enabled:false ~kind "%s_UNREACHABLE_AT_EXIT"


(* register enabled cost issues *)
let () =
  List.iter CostKind.enabled_cost_kinds ~f:(fun CostKind.{kind} ->
      List.iter [true; false] ~f:(fun is_on_ui_thread ->
          ignore (unreachable_cost_call ~kind) ;
          ignore (infinite_cost_call ~kind) ;
          ignore (complexity_increase ~kind ~is_on_ui_thread) ;
          () ) )
