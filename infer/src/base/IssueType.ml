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

type severity = Info | Advice | Warning | Error [@@deriving compare, equal, enumerate]

let string_of_severity = function
  | Advice ->
      "ADVICE"
  | Error ->
      "ERROR"
  | Info ->
      "INFO"
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
    ; mutable hum: string }
  [@@deriving compare, equal]

  val pp : F.formatter -> t -> unit

  val find_from_string : id:string -> t option

  val register :
       ?enabled:bool
    -> ?hum:string
    -> id:string
    -> user_documentation:string
    -> severity
    -> Checker.t
    -> t

  val register_hidden :
       ?is_silent:bool
    -> ?enabled:bool
    -> ?hum:string
    -> id:string
    -> ?user_documentation:string
    -> severity
    -> Checker.t
    -> t

  val register_dynamic :
       ?enabled:bool
    -> ?hum:string
    -> id:string
    -> ?user_documentation:string
    -> severity
    -> Checker.t
    -> t

  val register_cost :
       ?enabled:bool
    -> ?is_on_ui_thread:bool
    -> kind:CostKind.t
    -> (string -> string, F.formatter, unit, string) format4
    -> t

  val register_with_latent :
       ?enabled:bool
    -> ?hum:string
    -> id:string
    -> user_documentation:string
    -> severity
    -> Checker.t
    -> latent:bool
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
      ; mutable hum: string }
    [@@deriving equal]

    let compare {unique_id= id1} {unique_id= id2} = String.compare id1 id2

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

      + Dynamically created ones, eg from custom errors defined in the models or by annotation
        reachability

      + Issue types created at command-line-parsing time. These can mention issues of type 1. or 2.,
        but issues of type 2. have not yet been defined. Thus, we record only there [enabled] status
        definitely. The [hum]an-readable description can be updated when we encounter the definition
        of the issue type. *)
  let register_static_or_dynamic ?(enabled = true) ~is_cost_issue ?hum:hum0 ~id:unique_id
      ~visibility ~user_documentation default_severity checker =
    match find_from_string ~id:unique_id with
    | ((Some
         ( { unique_id= _ (* we know it has to be the same *)
           ; checker= checker_old
           ; visibility= visibility_old
           ; user_documentation= _ (* new one must be [None] for dynamic issue types *)
           ; default_severity= _ (* mutable field to update *)
           ; enabled= _ (* not touching this one since [Config] will have set it *)
           ; hum= _ (* mutable field to update *) } as issue ) )
    [@warning "+missing-record-field-pattern"] ) ->
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
        issue
    | None ->
        let hum = match hum0 with Some str -> str | _ -> prettify unique_id in
        let issue =
          {unique_id; visibility; user_documentation; default_severity; checker; enabled; hum}
        in
        all_issues := IssueSet.add !all_issues issue ;
        issue


  let register ?enabled ?hum ~id ~user_documentation default_severity checker =
    register_static_or_dynamic ?enabled ~is_cost_issue:false ?hum ~id ~visibility:User
      ~user_documentation:(Some user_documentation) default_severity checker


  let register_hidden ?(is_silent = false) ?enabled ?hum ~id ?user_documentation default_severity
      checker =
    register_static_or_dynamic ?enabled ~is_cost_issue:false ?hum ~id
      ~visibility:(if is_silent then Silent else Developer)
      ~user_documentation default_severity checker


  let register_dynamic ?enabled ?hum ~id ?user_documentation default_severity checker =
    register_static_or_dynamic ?enabled ~is_cost_issue:false ?hum ~id ~visibility:User
      ~user_documentation default_severity checker


  let cost_issue_doc_list =
    [ ( "EXECUTION_TIME_COMPLEXITY_INCREASE"
      , [%blob "./documentation/issues/EXECUTION_TIME_COMPLEXITY_INCREASE.md"] )
    ; ( "EXECUTION_TIME_COMPLEXITY_INCREASE_UI_THREAD"
      , [%blob "./documentation/issues/EXECUTION_TIME_COMPLEXITY_INCREASE_UI_THREAD.md"] )
    ; ( "EXECUTION_TIME_UNREACHABLE_AT_EXIT"
      , [%blob "./documentation/issues/EXECUTION_TIME_UNREACHABLE_AT_EXIT.md"] )
    ; ("INFINITE_EXECUTION_TIME", [%blob "./documentation/issues/INFINITE_EXECUTION_TIME.md"])
    ; ("EXPENSIVE_EXECUTION_TIME", [%blob "./documentation/issues/EXPENSIVE_EXECUTION_TIME.md"]) ]


  (** cost issues are already registered below.*)
  let register_cost ?(enabled = true) ?(is_on_ui_thread = false) ~(kind : CostKind.t) s =
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
    register_static_or_dynamic ~is_cost_issue:true ~enabled ~id:issue_type ~visibility:User Error
      Cost ~user_documentation:(Some user_documentation)


  let register_with_latent ?enabled ?hum ~id ~user_documentation default_severity checker =
    let issue = register ?enabled ?hum ~id ~user_documentation default_severity checker in
    let user_documentation =
      Printf.sprintf
        "A latent [%s](#%s). See the [documentation on Pulse latent \
         issues](/docs/next/checker-pulse#latent-issues)."
        id (String.lowercase id)
    in
    let latent_issue =
      register ~enabled:false ?hum ~id:(id ^ "_LATENT") ~user_documentation default_severity checker
    in
    fun ~latent -> if latent then latent_issue else issue


  let all_issues () = IssueSet.elements !all_issues
end

include Unsafe

let checker_can_report reporting_checker {checker= allowed_checker} =
  Checker.equal reporting_checker allowed_checker


let abduction_case_not_implemented =
  register_hidden ~id:"Abduction_case_not_implemented" Error Biabduction


let arbitrary_code_execution_under_lock =
  register ~id:"ARBITRARY_CODE_EXECUTION_UNDER_LOCK" ~hum:"Arbitrary Code Execution Under lock"
    Error Starvation
    ~user_documentation:[%blob "./documentation/issues/ARBITRARY_CODE_EXECUTION_UNDER_LOCK.md"]


let array_of_pointsto = register_hidden ~id:"Array_of_pointsto" Error Biabduction

let array_out_of_bounds_l1 =
  register_hidden ~enabled:false ~id:"ARRAY_OUT_OF_BOUNDS_L1" Error Biabduction


let array_out_of_bounds_l2 =
  register_hidden ~enabled:false ~id:"ARRAY_OUT_OF_BOUNDS_L2" Warning Biabduction


let array_out_of_bounds_l3 =
  register_hidden ~enabled:false ~id:"ARRAY_OUT_OF_BOUNDS_L3" Warning Biabduction


let assert_failure = register_hidden ~id:"Assert_failure" Error Biabduction

let bad_footprint = register_hidden ~id:"Bad_footprint" Error Biabduction

let bad_arg =
  register_with_latent ~id:"BAD_ARG" Error Pulse
    ~user_documentation:[%blob "./documentation/issues/BAD_ARG.md"]


let bad_key =
  register_with_latent ~id:"BAD_KEY" Error Pulse
    ~user_documentation:[%blob "./documentation/issues/BAD_KEY.md"]


let bad_map =
  register_with_latent ~id:"BAD_MAP" Error Pulse
    ~user_documentation:[%blob "./documentation/issues/BAD_MAP.md"]


let bad_record =
  register_with_latent ~id:"BAD_RECORD" Error Pulse
    ~user_documentation:[%blob "./documentation/issues/BAD_RECORD.md"]


let bad_return =
  register_with_latent ~id:"BAD_RETURN" Error Pulse
    ~user_documentation:[%blob "./documentation/issues/BAD_RETURN.md"]


let block_parameter_not_null_checked =
  register ~id:"BLOCK_PARAMETER_NOT_NULL_CHECKED" Warning ParameterNotNullChecked
    ~user_documentation:[%blob "./documentation/issues/BLOCK_PARAMETER_NOT_NULL_CHECKED.md"]


let biabduction_analysis_stops =
  register_hidden ~enabled:false ~id:"BIABDUCTION_ANALYSIS_STOPS" Warning Biabduction


let biabduction_retain_cycle =
  register ~enabled:true ~id:"BIABDUCTION_RETAIN_CYCLE" Error Biabduction
    ~user_documentation:"See [RETAIN_CYCLE](#retain_cycle)."


let buffer_overrun_l1 =
  register ~id:"BUFFER_OVERRUN_L1" Error BufferOverrunChecker
    ~user_documentation:[%blob "./documentation/issues/BUFFER_OVERRUN.md"]


let buffer_overrun_l2 =
  register ~id:"BUFFER_OVERRUN_L2" Error BufferOverrunChecker
    ~user_documentation:"See [BUFFER_OVERRUN_L1](#buffer_overrun_l1)"


let buffer_overrun_l3 =
  register ~id:"BUFFER_OVERRUN_L3" Error BufferOverrunChecker
    ~user_documentation:"See [BUFFER_OVERRUN_L1](#buffer_overrun_l1)"


let buffer_overrun_l4 =
  register ~enabled:false ~id:"BUFFER_OVERRUN_L4" Error BufferOverrunChecker
    ~user_documentation:"See [BUFFER_OVERRUN_L1](#buffer_overrun_l1)"


let buffer_overrun_l5 =
  register ~enabled:false ~id:"BUFFER_OVERRUN_L5" Error BufferOverrunChecker
    ~user_documentation:"See [BUFFER_OVERRUN_L1](#buffer_overrun_l1)"


let buffer_overrun_s2 =
  register ~id:"BUFFER_OVERRUN_S2" Error BufferOverrunChecker
    ~user_documentation:"See [BUFFER_OVERRUN_L1](#buffer_overrun_l1)"


let buffer_overrun_u5 =
  register ~enabled:false ~id:"BUFFER_OVERRUN_U5" Error BufferOverrunChecker
    ~user_documentation:"See [BUFFER_OVERRUN_L1](#buffer_overrun_l1)"


let cannot_star = register_hidden ~id:"Cannot_star" Error Biabduction

let captured_strong_self =
  register ~id:"CAPTURED_STRONG_SELF" ~hum:"Captured strongSelf" Error SelfInBlock
    ~user_documentation:[%blob "./documentation/issues/CAPTURED_STRONG_SELF.md"]


let checkers_allocates_memory =
  register ~id:"CHECKERS_ALLOCATES_MEMORY" ~hum:"Allocates Memory" Error AnnotationReachability
    ~user_documentation:[%blob "./documentation/issues/CHECKERS_ALLOCATES_MEMORY.md"]


let checkers_annotation_reachability_error =
  register ~id:"CHECKERS_ANNOTATION_REACHABILITY_ERROR" ~hum:"Annotation Reachability Error" Error
    AnnotationReachability
    ~user_documentation:[%blob "./documentation/issues/CHECKERS_ANNOTATION_REACHABILITY_ERROR.md"]


let checkers_calls_expensive_method =
  register ~id:"CHECKERS_CALLS_EXPENSIVE_METHOD" ~hum:"Expensive Method Called" Error
    AnnotationReachability
    ~user_documentation:[%blob "./documentation/issues/CHECKERS_CALLS_EXPENSIVE_METHOD.md"]


let checkers_expensive_overrides_unexpensive =
  register ~id:"CHECKERS_EXPENSIVE_OVERRIDES_UNANNOTATED" ~hum:"Expensive Overrides Unannotated"
    Error AnnotationReachability
    ~user_documentation:[%blob "./documentation/issues/CHECKERS_EXPENSIVE_OVERRIDES_UNANNOTATED.md"]


let checkers_fragment_retain_view =
  register ~id:"CHECKERS_FRAGMENT_RETAINS_VIEW" ~hum:"Fragment Retains View" Warning
    FragmentRetainsView
    ~user_documentation:[%blob "./documentation/issues/CHECKERS_FRAGMENT_RETAINS_VIEW.md"]


let checkers_printf_args =
  register ~id:"CHECKERS_PRINTF_ARGS" Error PrintfArgs
    ~user_documentation:[%blob "./documentation/issues/CHECKERS_PRINTF_ARGS.md"]


let class_cast_exception =
  register_hidden ~enabled:false ~id:"CLASS_CAST_EXCEPTION" Error Biabduction


let condition_always_false =
  register_hidden ~enabled:false ~id:"CONDITION_ALWAYS_FALSE" Warning BufferOverrunChecker


let condition_always_true =
  register_hidden ~enabled:false ~id:"CONDITION_ALWAYS_TRUE" Warning BufferOverrunChecker


let config_impact_analysis =
  register ~enabled:false ~id:"CONFIG_IMPACT" Advice ConfigImpactAnalysis
    ~user_documentation:[%blob "./documentation/issues/CONFIG_IMPACT.md"]


let config_impact_analysis_strict =
  register ~enabled:false ~id:"CONFIG_IMPACT_STRICT" Advice ConfigImpactAnalysis
    ~user_documentation:[%blob "./documentation/issues/CONFIG_IMPACT_STRICT.md"]


let pulse_config_usage =
  register ~enabled:false ~id:"CONFIG_USAGE" Info Pulse
    ~user_documentation:[%blob "./documentation/issues/CONFIG_USAGE.md"]


let pulse_const_refable =
  register ~id:"PULSE_CONST_REFABLE" Error Pulse ~hum:"Const Refable Parameter"
    ~user_documentation:[%blob "./documentation/issues/PULSE_CONST_REFABLE.md"]


let constant_address_dereference =
  register_with_latent ~enabled:false ~id:"CONSTANT_ADDRESS_DEREFERENCE" Warning Pulse
    ~user_documentation:[%blob "./documentation/issues/CONSTANT_ADDRESS_DEREFERENCE.md"]


let create_intent_from_uri =
  register ~id:"CREATE_INTENT_FROM_URI" Error Quandary
    ~user_documentation:
      "Create an intent/start a component using a (possibly user-controlled) URI. may or may not \
       be an issue depending on where the URI comes from."


let cross_site_scripting =
  register ~id:"CROSS_SITE_SCRIPTING" Error Quandary
    ~user_documentation:"Untrusted data flows into HTML; XSS risk."


let dangling_pointer_dereference =
  register ~enabled:false ~id:"DANGLING_POINTER_DEREFERENCE" Error Biabduction (* TODO *)
    ~user_documentation:""


let dangling_pointer_dereference_maybe =
  register_hidden ~enabled:false ~id:"DANGLING_POINTER_DEREFERENCE_MAYBE" Warning Biabduction


let dead_store =
  register ~id:"DEAD_STORE" Error Liveness
    ~user_documentation:[%blob "./documentation/issues/DEAD_STORE.md"]


let deadlock =
  register ~id:"DEADLOCK" Error Starvation
    ~user_documentation:[%blob "./documentation/issues/DEADLOCK.md"]


let divide_by_zero =
  register ~enabled:false ~id:"DIVIDE_BY_ZERO" Error Biabduction (* TODO *) ~user_documentation:""


let do_not_report = register_hidden ~id:"DO_NOT_REPORT" Error Quandary

let empty_vector_access =
  register ~id:"EMPTY_VECTOR_ACCESS" Error Biabduction
    ~user_documentation:[%blob "./documentation/issues/EMPTY_VECTOR_ACCESS.md"]


let exposed_insecure_intent_handling =
  register ~id:"EXPOSED_INSECURE_INTENT_HANDLING" Error Quandary ~user_documentation:"Undocumented."


let expensive_cost_call ~kind = register_cost ~enabled:false "EXPENSIVE_%s" ~kind

let failure_exe = register_hidden ~is_silent:true ~id:"Failure_exe" Info Biabduction

let guardedby_violation =
  register Warning ~id:"GUARDEDBY_VIOLATION" ~hum:"GuardedBy Violation" RacerD
    ~user_documentation:[%blob "./documentation/issues/GUARDEDBY_VIOLATION.md"]


let impure_function =
  register ~id:"IMPURE_FUNCTION" Error Impurity
    ~user_documentation:[%blob "./documentation/issues/IMPURE_FUNCTION.md"]


let inefficient_keyset_iterator =
  register ~id:"INEFFICIENT_KEYSET_ITERATOR" Warning InefficientKeysetIterator
    ~user_documentation:[%blob "./documentation/issues/INEFFICIENT_KEYSET_ITERATOR.md"]


let inferbo_alloc_is_big =
  register ~id:"INFERBO_ALLOC_IS_BIG" Error BufferOverrunChecker
    ~user_documentation:
      "`malloc` is passed a large constant value (>=10^6). For example, `int n = 1000000; \
       malloc(n);` generates `INFERBO_ALLOC_IS_BIG` on `malloc(n)`.\n\n\
       Action: Fix the size argument or make sure it is really needed."


let inferbo_alloc_is_negative =
  register ~id:"INFERBO_ALLOC_IS_NEGATIVE" Error BufferOverrunChecker
    ~user_documentation:
      "`malloc` is called with a negative size. For example, `int n = 3 - 5; malloc(n);` generates \
       `INFERBO_ALLOC_IS_NEGATIVE` on `malloc(n)`.\n\n\
       Action: Fix the size argument."


let inferbo_alloc_is_zero =
  register ~id:"INFERBO_ALLOC_IS_ZERO" Error BufferOverrunChecker
    ~user_documentation:
      "`malloc` is called with a zero size. For example, `int n = 3 - 3; malloc(n);` generates \
       `INFERBO_ALLOC_IS_ZERO` on `malloc(n)`.\n\n\
       Action: Fix the size argument."


let inferbo_alloc_may_be_big =
  register ~id:"INFERBO_ALLOC_MAY_BE_BIG" Error BufferOverrunChecker
    ~user_documentation:
      "`malloc` *may* be called with a large value. For example, `int n = b ? 3 : 1000000; \
       malloc(n);` generates `INFERBO_ALLOC_MAY_BE_BIG` on `malloc(n)`.\n\n\
       Action: Fix the size argument or add a bound checking, e.g. `if (n < A_SMALL_NUMBER) { \
       malloc(n); }`."


let inferbo_alloc_may_be_negative =
  register ~id:"INFERBO_ALLOC_MAY_BE_NEGATIVE" Error BufferOverrunChecker
    ~user_documentation:
      "`malloc` *may* be called with a negative value. For example, `int n = b ? 3 : -5; \
       malloc(n);` generates `INFERBO_ALLOC_MAY_BE_NEGATIVE` on `malloc(n)`.\n\n\
       Action: Fix the size argument or add a bound checking, e.g. `if (n > 0) { malloc(n); }`."


let infinite_cost_call ~kind = register_cost ~enabled:false "INFINITE_%s" ~kind

let inherently_dangerous_function =
  register_hidden ~id:"INHERENTLY_DANGEROUS_FUNCTION" Warning Biabduction


let insecure_intent_handling =
  register ~id:"INSECURE_INTENT_HANDLING" Error Quandary ~user_documentation:"Undocumented."


let integer_overflow_l1 =
  register ~id:"INTEGER_OVERFLOW_L1" Error BufferOverrunChecker
    ~user_documentation:[%blob "./documentation/issues/INTEGER_OVERFLOW.md"]


let integer_overflow_l2 =
  register ~id:"INTEGER_OVERFLOW_L2" Error BufferOverrunChecker
    ~user_documentation:"See [INTEGER_OVERFLOW_L1](#integer_overflow_l1)"


let integer_overflow_l5 =
  register ~enabled:false ~id:"INTEGER_OVERFLOW_L5" Error BufferOverrunChecker
    ~user_documentation:"See [INTEGER_OVERFLOW_L1](#integer_overflow_l1)"


let integer_overflow_u5 =
  register ~enabled:false ~id:"INTEGER_OVERFLOW_U5" Error BufferOverrunChecker
    ~user_documentation:"See [INTEGER_OVERFLOW_L1](#integer_overflow_l1)"


let interface_not_thread_safe =
  register Warning ~id:"INTERFACE_NOT_THREAD_SAFE" RacerD
    ~user_documentation:[%blob "./documentation/issues/INTERFACE_NOT_THREAD_SAFE.md"]


let internal_error = register_hidden ~id:"Internal_error" Error Biabduction

let invalid_sil =
  register ~enabled:true ~id:"INVALID_SIL" Error SILValidation
    ~user_documentation:[%blob "./documentation/issues/INVALID_SIL.md"]


let invariant_call =
  register ~enabled:false ~id:"INVARIANT_CALL" Error LoopHoisting
    ~user_documentation:[%blob "./documentation/issues/INVARIANT_CALL.md"]


let ipc_on_ui_thread =
  register Warning ~id:"IPC_ON_UI_THREAD" Starvation
    ~user_documentation:"A blocking `Binder` IPC call occurs on the UI thread."


let javascript_injection =
  register ~id:"JAVASCRIPT_INJECTION" Error Quandary
    ~user_documentation:"Untrusted data flows into JavaScript."


let lab_resource_leak =
  register ~id:"LAB_RESOURCE_LEAK" Error ResourceLeakLabExercise ~user_documentation:"Toy issue."


let leak_after_array_abstraction =
  register_hidden ~id:"Leak_after_array_abstraction" Error Biabduction


let leak_in_footprint = register_hidden ~id:"Leak_in_footprint" Error Biabduction

let leak_unknown_origin = register_hidden ~enabled:false ~id:"Leak_unknown_origin" Error Biabduction

let lock_consistency_violation =
  register Warning ~id:"LOCK_CONSISTENCY_VIOLATION" RacerD
    ~user_documentation:[%blob "./documentation/issues/LOCK_CONSISTENCY_VIOLATION.md"]


let lockless_violation =
  register ~id:"LOCKLESS_VIOLATION" Error Starvation
    ~user_documentation:[%blob "./documentation/issues/LOCKLESS_VIOLATION.md"]


let logging_private_data =
  register ~id:"LOGGING_PRIVATE_DATA" Error Quandary ~user_documentation:"Undocumented."


let expensive_loop_invariant_call =
  register ~id:"EXPENSIVE_LOOP_INVARIANT_CALL" Error LoopHoisting
    ~user_documentation:[%blob "./documentation/issues/EXPENSIVE_LOOP_INVARIANT_CALL.md"]


let memory_leak =
  register ~enabled:false ~id:"BIABDUCTION_MEMORY_LEAK" ~hum:"Memory Leak" Error Biabduction
    ~user_documentation:"See [MEMORY_LEAK](#memory_leak)."


let missing_fld = register_hidden ~id:"Missing_fld" ~hum:"Missing Field" Error Biabduction

let missing_required_prop =
  register ~id:"MISSING_REQUIRED_PROP" ~hum:"Missing Required Prop" Error LithoRequiredProps
    ~user_documentation:[%blob "./documentation/issues/MISSING_REQUIRED_PROP.md"]


let mixed_self_weakself =
  register ~id:"MIXED_SELF_WEAKSELF" ~hum:"Mixed Self WeakSelf" Error SelfInBlock
    ~user_documentation:[%blob "./documentation/issues/MIXED_SELF_WEAKSELF.md"]


let modifies_immutable =
  register ~id:"MODIFIES_IMMUTABLE" Error Impurity
    ~user_documentation:[%blob "./documentation/issues/MODIFIES_IMMUTABLE.md"]


let multiple_weakself =
  register ~id:"MULTIPLE_WEAKSELF" ~hum:"Multiple WeakSelf Use" Error SelfInBlock
    ~user_documentation:[%blob "./documentation/issues/MULTIPLE_WEAKSELF.md"]


let nil_block_call =
  register_with_latent ~id:"NIL_BLOCK_CALL" Error Pulse
    ~user_documentation:[%blob "./documentation/issues/NIL_BLOCK_CALL.md"]


let nil_insertion_into_collection =
  register_with_latent ~id:"NIL_INSERTION_INTO_COLLECTION" Error Pulse
    ~hum:"Nil Insertion Into Collection"
    ~user_documentation:[%blob "./documentation/issues/NIL_INSERTION_INTO_COLLECTION.md"]


let nil_messaging_to_non_pod =
  register_with_latent ~id:"NIL_MESSAGING_TO_NON_POD" Error Pulse
    ~user_documentation:[%blob "./documentation/issues/NIL_MESSAGING_TO_NON_POD.md"]


let no_match_of_rhs =
  register_with_latent ~id:"NO_MATCH_OF_RHS" Error Pulse
    ~user_documentation:[%blob "./documentation/issues/NO_MATCH_OF_RHS.md"]


let no_matching_case_clause =
  register_with_latent ~id:"NO_MATCHING_CASE_CLAUSE" Error Pulse
    ~user_documentation:[%blob "./documentation/issues/NO_MATCHING_CASE_CLAUSE.md"]


let no_matching_function_clause =
  register_with_latent ~id:"NO_MATCHING_FUNCTION_CLAUSE" Error Pulse
    ~user_documentation:[%blob "./documentation/issues/NO_MATCHING_FUNCTION_CLAUSE.md"]


let no_true_branch_in_if =
  register_with_latent ~id:"NO_TRUE_BRANCH_IN_IF" Error Pulse
    ~user_documentation:[%blob "./documentation/issues/NO_TRUE_BRANCH_IN_IF.md"]


let no_matching_branch_in_try =
  register_with_latent ~id:"NO_MATCHING_BRANCH_IN_TRY" Error Pulse
    ~user_documentation:[%blob "./documentation/issues/NO_MATCHING_BRANCH_IN_TRY.md"]


let null_argument =
  register_with_latent ~id:"NULL_ARGUMENT" Error Pulse
    ~user_documentation:[%blob "./documentation/issues/NULL_ARGUMENT.md"]


let null_dereference =
  register ~id:"NULL_DEREFERENCE" Error Biabduction
    ~user_documentation:"See [NULLPTR_DEREFERENCE](#nullptr_dereference)."


let nullptr_dereference =
  register_with_latent ~id:"NULLPTR_DEREFERENCE" ~hum:"Null Dereference" Error Pulse
    ~user_documentation:[%blob "./documentation/issues/NULLPTR_DEREFERENCE.md"]


let nullptr_dereference_in_nullsafe_class =
  register_with_latent ~id:"NULLPTR_DEREFERENCE_IN_NULLSAFE_CLASS" ~hum:"Null Dereference" Error
    Pulse ~user_documentation:[%blob "./documentation/issues/NULLPTR_DEREFERENCE.md"]


let optional_empty_access =
  register_with_latent ~id:"OPTIONAL_EMPTY_ACCESS" Error Pulse
    ~user_documentation:[%blob "./documentation/issues/OPTIONAL_EMPTY_ACCESS.md"]


let precondition_not_found = register_hidden ~id:"PRECONDITION_NOT_FOUND" Error Biabduction

let precondition_not_met = register_hidden ~id:"PRECONDITION_NOT_MET" Warning Biabduction

let premature_nil_termination =
  register ~id:"PREMATURE_NIL_TERMINATION_ARGUMENT" Warning Biabduction
    ~user_documentation:[%blob "./documentation/issues/PREMATURE_NIL_TERMINATION_ARGUMENT.md"]


let pulse_transitive_access =
  register ~enabled:true ~id:"PULSE_TRANSITIVE_ACCESS" Error Pulse
    ~user_documentation:[%blob "./documentation/issues/PULSE_TRANSITIVE_ACCESS.md"]


let pulse_memory_leak_c =
  register ~id:"MEMORY_LEAK_C" ~hum:"Memory Leak" Error Pulse
    ~user_documentation:[%blob "./documentation/issues/MEMORY_LEAK_C.md"]


let pulse_memory_leak_cpp =
  register ~id:"MEMORY_LEAK_CPP" ~hum:"Memory Leak" ~enabled:false Error Pulse
    ~user_documentation:"See [MEMORY_LEAK_C](#memory_leak_c)"


let pulse_resource_leak =
  register ~enabled:true ~id:"PULSE_RESOURCE_LEAK" Error Pulse
    ~user_documentation:"See [RESOURCE_LEAK](#resource_leak)"


let pulse_unawaited_awaitable =
  register ~enabled:true ~id:"PULSE_UNAWAITED_AWAITABLE" Error Pulse ~hum:"Unawaited Awaitable"
    ~user_documentation:[%blob "./documentation/issues/PULSE_UNAWAITED_AWAITABLE.md"]


let pulse_uninitialized_const =
  register ~enabled:false ~id:"PULSE_UNINITIALIZED_CONST" Error Pulse ~hum:"Uninitialized Const"
    ~user_documentation:[%blob "./documentation/issues/PULSE_UNINITIALIZED_CONST.md"]


let pure_function =
  register ~id:"PURE_FUNCTION" Error PurityChecker
    ~user_documentation:[%blob "./documentation/issues/PURE_FUNCTION.md"]


let quandary_taint_error =
  register ~hum:"Taint Error" ~id:"QUANDARY_TAINT_ERROR" Error Quandary
    ~user_documentation:"Generic taint error when nothing else fits."


let readonly_shared_ptr_param =
  register ~id:"PULSE_READONLY_SHARED_PTR_PARAM" Error Pulse ~hum:"Read-only Shared Parameter"
    ~user_documentation:[%blob "./documentation/issues/PULSE_READONLY_SHARED_PTR_PARAM.md"]


let taint_error =
  register ~hum:"Taint Error" ~id:"TAINT_ERROR" Error Pulse
    ~user_documentation:"A taint flow was detected from a source to a sink"


let sensitive_data_flow =
  register ~enabled:false ~hum:"Sensitive Data Flow" ~id:"SENSITIVE_DATA_FLOW" Advice Pulse
    ~user_documentation:"A flow of sensitive data was detected from a source."


let data_flow_to_sink =
  register ~enabled:false ~hum:"Data Flow to Sink" ~id:"DATA_FLOW_TO_SINK" Advice Pulse
    ~user_documentation:"A flow of data was detected to a sink."


let datalog_fact =
  register ~id:"DATALOG_FACT" Info Datalog
    ~user_documentation:"Datalog fact used as input for a datalog solver."


let regex_op_on_ui_thread =
  register Warning ~id:"REGEX_OP_ON_UI_THREAD" Starvation
    ~user_documentation:
      "A potentially costly operation on a regular expression occurs on the UI thread."


let resource_leak =
  register ~id:"RESOURCE_LEAK" Error Biabduction
    ~user_documentation:[%blob "./documentation/issues/RESOURCE_LEAK.md"]


let retain_cycle =
  register ~enabled:false ~id:"RETAIN_CYCLE" Error Pulse
    ~user_documentation:[%blob "./documentation/issues/RETAIN_CYCLE.md"]


let scope_leakage =
  register ~enabled:true ~id:"SCOPE_LEAKAGE" Error ScopeLeakage
    ~user_documentation:[%blob "./documentation/issues/SCOPE_LEAKAGE.md"]


let skip_function = register_hidden ~enabled:false ~id:"SKIP_FUNCTION" Info Biabduction

let shell_injection =
  register ~id:"SHELL_INJECTION" Error Quandary
    ~user_documentation:"Environment variable or file data flowing to shell."


let shell_injection_risk =
  register ~id:"SHELL_INJECTION_RISK" Error Quandary
    ~user_documentation:"Code injection if the caller of the endpoint doesn't sanitize on its end."


let sql_injection =
  register ~id:"SQL_INJECTION" Error Quandary
    ~user_documentation:"Untrusted and unescaped data flows to SQL."


let sql_injection_risk =
  register ~id:"SQL_INJECTION_RISK" Error Quandary
    ~user_documentation:"Untrusted and unescaped data flows to SQL."


let stack_variable_address_escape =
  register ~id:"STACK_VARIABLE_ADDRESS_ESCAPE" Error Pulse
    ~user_documentation:[%blob "./documentation/issues/STACK_VARIABLE_ADDRESS_ESCAPE.md"]


let starvation =
  register ~id:"STARVATION" ~hum:"UI Thread Starvation" Error Starvation
    ~user_documentation:[%blob "./documentation/issues/STARVATION.md"]


let static_initialization_order_fiasco =
  register ~id:"STATIC_INITIALIZATION_ORDER_FIASCO" Error SIOF
    ~user_documentation:[%blob "./documentation/issues/STATIC_INITIALIZATION_ORDER_FIASCO.md"]


let strict_mode_violation =
  register ~id:"STRICT_MODE_VIOLATION" ~hum:"Strict Mode Violation" Error Starvation
    ~user_documentation:[%blob "./documentation/issues/STRICT_MODE_VIOLATION.md"]


let strong_self_not_checked =
  register ~id:"STRONG_SELF_NOT_CHECKED" ~hum:"StrongSelf Not Checked" Error SelfInBlock
    ~user_documentation:[%blob "./documentation/issues/STRONG_SELF_NOT_CHECKED.md"]


let symexec_memory_error =
  register_hidden ~id:"Symexec_memory_error" ~hum:"Symbolic Execution Memory Error" Error
    Biabduction


let thread_safety_violation =
  register Warning ~id:"THREAD_SAFETY_VIOLATION" RacerD
    ~user_documentation:[%blob "./documentation/issues/THREAD_SAFETY_VIOLATION.md"]


let complexity_increase ~kind ~is_on_ui_thread =
  register_cost ~kind ~is_on_ui_thread "%s_COMPLEXITY_INCREASE"


let topl_error =
  register_with_latent ~id:"TOPL_ERROR" Error Topl
    ~user_documentation:[%blob "./documentation/issues/TOPL_ERROR.md"]


let uninitialized_value =
  register ~id:"UNINITIALIZED_VALUE" Error Uninit
    ~user_documentation:[%blob "./documentation/issues/UNINITIALIZED_VALUE.md"]


let uninitialized_value_pulse =
  register_with_latent ~id:"PULSE_UNINITIALIZED_VALUE" Error Pulse ~hum:"Uninitialized Value"
    ~user_documentation:
      "See [UNINITIALIZED_VALUE](#uninitialized_value). Re-implemented using Pulse."


let unnecessary_copy_pulse =
  register ~id:"PULSE_UNNECESSARY_COPY" Error Pulse ~hum:"Unnecessary Copy"
    ~user_documentation:[%blob "./documentation/issues/PULSE_UNNECESSARY_COPY.md"]


let unnecessary_copy_assignment_pulse =
  register ~id:"PULSE_UNNECESSARY_COPY_ASSIGNMENT" Error Pulse ~hum:"Unnecessary Copy Assignment"
    ~user_documentation:"See [PULSE_UNNECESSARY_COPY](#pulse_unnecessary_copy)."


let unnecessary_copy_assignment_const_pulse =
  register ~enabled:false ~id:"PULSE_UNNECESSARY_COPY_ASSIGNMENT_CONST" Error Pulse
    ~hum:"Unnecessary Copy Assignment from Const"
    ~user_documentation:"See [PULSE_UNNECESSARY_COPY](#pulse_unnecessary_copy)."


let unnecessary_copy_assignment_movable_pulse =
  register ~id:"PULSE_UNNECESSARY_COPY_ASSIGNMENT_MOVABLE" Error Pulse
    ~hum:"Unnecessary Copy Assignment Movable"
    ~user_documentation:"See [PULSE_UNNECESSARY_COPY_MOVABLE](#pulse_unnecessary_copy_movable)."


let unnecessary_copy_intermediate_pulse =
  register ~id:"PULSE_UNNECESSARY_COPY_INTERMEDIATE" Error Pulse
    ~hum:"Unnecessary Copy Intermediate"
    ~user_documentation:[%blob "./documentation/issues/PULSE_UNNECESSARY_COPY_INTERMEDIATE.md"]


let unnecessary_copy_intermediate_const_pulse =
  register ~enabled:false ~id:"PULSE_UNNECESSARY_COPY_INTERMEDIATE_CONST" Error Pulse
    ~hum:"Unnecessary Copy Intermediate from Const"
    ~user_documentation:"See [PULSE_UNNECESSARY_COPY](#pulse_unnecessary_copy)."


let unnecessary_copy_movable_pulse =
  register ~id:"PULSE_UNNECESSARY_COPY_MOVABLE" Error Pulse ~hum:"Unnecessary Copy Movable"
    ~user_documentation:[%blob "./documentation/issues/PULSE_UNNECESSARY_COPY_MOVABLE.md"]


let unnecessary_copy_optional_pulse =
  register ~id:"PULSE_UNNECESSARY_COPY_OPTIONAL" Error Pulse ~hum:"Unnecessary Copy to Optional"
    ~user_documentation:[%blob "./documentation/issues/PULSE_UNNECESSARY_COPY_OPTIONAL.md"]


let unnecessary_copy_optional_const_pulse =
  register ~enabled:false ~id:"PULSE_UNNECESSARY_COPY_OPTIONAL_CONST" Error Pulse
    ~hum:"Unnecessary Copy to Optional from Const"
    ~user_documentation:"See [PULSE_UNNECESSARY_COPY_OPTIONAL](#pulse_unnecessary_copy_optional)."


let unnecessary_copy_return_pulse =
  register ~enabled:false ~id:"PULSE_UNNECESSARY_COPY_RETURN" Error Pulse
    ~hum:"Unnecessary Copy Return"
    ~user_documentation:[%blob "./documentation/issues/PULSE_UNNECESSARY_COPY_RETURN.md"]


let unreachable_code_after = register_hidden ~id:"UNREACHABLE_CODE" Error BufferOverrunChecker

let use_after_delete =
  register_with_latent ~id:"USE_AFTER_DELETE" Error Pulse
    ~user_documentation:[%blob "./documentation/issues/USE_AFTER_DELETE.md"]


let use_after_free =
  register_with_latent ~id:"USE_AFTER_FREE" Error Pulse
    ~user_documentation:[%blob "./documentation/issues/USE_AFTER_FREE.md"]


let use_after_lifetime =
  register_with_latent ~id:"USE_AFTER_LIFETIME" Error Pulse
    ~user_documentation:[%blob "./documentation/issues/USE_AFTER_LIFETIME.md"]


let user_controlled_sql_risk =
  register ~id:"USER_CONTROLLED_SQL_RISK" Error Quandary
    ~user_documentation:"Untrusted data flows to SQL (no injection risk)."


let untrusted_buffer_access =
  register ~enabled:false ~id:"UNTRUSTED_BUFFER_ACCESS" Error Quandary
    ~user_documentation:"Untrusted data of any kind flowing to buffer."


let untrusted_deserialization =
  register ~id:"UNTRUSTED_DESERIALIZATION" Error Quandary
    ~user_documentation:"User-controlled deserialization."


let untrusted_deserialization_risk =
  register ~id:"UNTRUSTED_DESERIALIZATION_RISK" Error Quandary
    ~user_documentation:"User-controlled deserialization"


let untrusted_environment_change_risk =
  register ~id:"UNTRUSTED_ENVIRONMENT_CHANGE_RISK" Error Quandary
    ~user_documentation:"User-controlled environment mutation."


let untrusted_file =
  register ~id:"UNTRUSTED_FILE" Error Quandary
    ~user_documentation:
      "User-controlled file creation; may be vulnerable to path traversal and more."


let untrusted_file_risk =
  register ~id:"UNTRUSTED_FILE_RISK" Error Quandary
    ~user_documentation:
      "User-controlled file creation; may be vulnerable to path traversal and more."


let untrusted_heap_allocation =
  register ~enabled:false ~id:"UNTRUSTED_HEAP_ALLOCATION" Error Quandary
    ~user_documentation:
      "Untrusted data of any kind flowing to heap allocation. this can cause crashes or DOS."


let untrusted_intent_creation =
  register ~id:"UNTRUSTED_INTENT_CREATION" Error Quandary
    ~user_documentation:"Creating an Intent from user-controlled data."


let untrusted_url_risk =
  register ~id:"UNTRUSTED_URL_RISK" Error Quandary
    ~user_documentation:"Untrusted flag, environment variable, or file data flowing to URL."


let untrusted_variable_length_array =
  register ~id:"UNTRUSTED_VARIABLE_LENGTH_ARRAY" Error Quandary
    ~user_documentation:
      "Untrusted data of any kind flowing to stack buffer allocation. Trying to allocate a stack \
       buffer that's too large will cause a stack overflow."


let vector_invalidation =
  register_with_latent ~id:"VECTOR_INVALIDATION" Error Pulse
    ~user_documentation:[%blob "./documentation/issues/VECTOR_INVALIDATION.md"]


let pulse_reference_stability =
  register ~id:"PULSE_REFERENCE_STABILITY" ~enabled:false Error Pulse
    ~user_documentation:[%blob "./documentation/issues/PULSE_REFERENCE_STABILITY.md"]


let weak_self_in_noescape_block =
  register ~id:"WEAK_SELF_IN_NO_ESCAPE_BLOCK" Error SelfInBlock
    ~user_documentation:[%blob "./documentation/issues/WEAK_SELF_IN_NO_ESCAPE_BLOCK.md"]


let wrong_argument_number =
  register_hidden ~id:"Wrong_argument_number" ~hum:"Wrong Argument Number" Error Biabduction


let unreachable_cost_call ~kind = register_cost ~enabled:false ~kind "%s_UNREACHABLE_AT_EXIT"

(* register enabled cost issues *)
let () =
  List.iter CostKind.enabled_cost_kinds ~f:(fun CostKind.{kind} ->
      List.iter [true; false] ~f:(fun is_on_ui_thread ->
          ignore (unreachable_cost_call ~kind) ;
          ignore (infinite_cost_call ~kind) ;
          ignore (expensive_cost_call ~kind) ;
          ignore (complexity_increase ~kind ~is_on_ui_thread) ;
          () ) )


module Map = PrettyPrintable.MakePPMap (struct
  type nonrec t = t [@@deriving compare]

  let pp = pp
end)
