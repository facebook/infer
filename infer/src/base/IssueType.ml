(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Die

(* Make sure we cannot create new issue types other than by calling [register_from_string]. This is because
     we want to keep track of the list of all the issues ever declared. *)
module Unsafe : sig
  type t = private
    { unique_id: string
    ; checker: Checker.t
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
    -> ?hum:string
    -> ?doc_url:string
    -> ?linters_def_file:string
    -> id:string
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
  let register_from_string ?(enabled = true) ?hum:hum0 ?doc_url ?linters_def_file ~id:unique_id
      checker =
    match find_from_string ~id:unique_id with
    | ((Some
         ( { unique_id= _ (* we know it has to be the same *)
           ; checker= checker_old
           ; enabled= _ (* not touching this one since [Config] will have set it *)
           ; hum= _ (* mutable field to update *)
           ; doc_url= _ (* mutable field to update *)
           ; linters_def_file= _ (* mutable field to update *) } as issue ))[@warning "+9"]) ->
        (* update fields that were supplied this time around, but keep the previous values of others
           and assert that the immutable fields are the same (see doc comment) *)
        if not (Checker.equal checker checker_old) then
          L.die InternalError
            "Checker definition for issue \"%s\" doesn't match: found new checker \"%s\" but \
             checker \"%s\" was already registered for this issue type"
            unique_id (Checker.get_name checker) (Checker.get_name checker_old) ;
        Option.iter hum0 ~f:(fun hum -> issue.hum <- hum) ;
        if Option.is_some doc_url then issue.doc_url <- doc_url ;
        if Option.is_some linters_def_file then issue.linters_def_file <- linters_def_file ;
        issue
    | None ->
        let hum = match hum0 with Some str -> str | _ -> prettify unique_id in
        let issue = {unique_id; checker; enabled; hum; doc_url; linters_def_file} in
        all_issues := IssueSet.add !all_issues issue ;
        issue


  (** cost issues are already registered below.*)
  let register_from_cost_string ?(enabled = true) ?(is_on_ui_thread = false) ~(kind : CostKind.t) s
      =
    let issue_type_base = F.asprintf s (CostKind.to_issue_string kind) in
    let issue_type = if is_on_ui_thread then issue_type_base ^ "_UI_THREAD" else issue_type_base in
    register_from_string ~enabled ~id:issue_type Cost


  let all_issues () = IssueSet.elements !all_issues
end

include Unsafe

let checker_can_report reporting_checker {checker= allowed_checker} =
  Checker.equal reporting_checker allowed_checker


let abduction_case_not_implemented =
  register_from_string ~id:"Abduction_case_not_implemented" Biabduction


let array_of_pointsto = register_from_string ~id:"Array_of_pointsto" Biabduction

let array_out_of_bounds_l1 =
  register_from_string ~enabled:false ~id:"ARRAY_OUT_OF_BOUNDS_L1" Biabduction


let array_out_of_bounds_l2 =
  register_from_string ~enabled:false ~id:"ARRAY_OUT_OF_BOUNDS_L2" Biabduction


let array_out_of_bounds_l3 =
  register_from_string ~enabled:false ~id:"ARRAY_OUT_OF_BOUNDS_L3" Biabduction


let assert_failure = register_from_string ~id:"Assert_failure" Biabduction

let bad_footprint = register_from_string ~id:"Bad_footprint" Biabduction

let biabduction_analysis_stops =
  register_from_string ~enabled:false ~id:"BIABDUCTION_ANALYSIS_STOPS" Biabduction


let biabd_condition_always_false =
  register_from_string ~enabled:false ~hum:"Condition Always False"
    ~id:"BIABD_CONDITION_ALWAYS_FALSE" Biabduction


let biabd_condition_always_true =
  register_from_string ~enabled:false ~hum:"Condition Always True" ~id:"BIABD_CONDITION_ALWAYS_TRUE"
    Biabduction


let biabd_registered_observer_being_deallocated =
  register_from_string ~hum:"Registered Observer Being Deallocated"
    ~id:"BIABD_REGISTERED_OBSERVER_BEING_DEALLOCATED" Biabduction


let biabd_stack_variable_address_escape =
  register_from_string ~enabled:false ~hum:"Stack Variable Address Escape"
    ~id:"BIABD_STACK_VARIABLE_ADDRESS_ESCAPE" Biabduction


let biabd_use_after_free =
  register_from_string ~hum:"Use After Free" ~id:"BIABD_USE_AFTER_FREE" Biabduction


let buffer_overrun_l1 = register_from_string ~id:"BUFFER_OVERRUN_L1" BufferOverrunChecker

let buffer_overrun_l2 = register_from_string ~id:"BUFFER_OVERRUN_L2" BufferOverrunChecker

let buffer_overrun_l3 = register_from_string ~id:"BUFFER_OVERRUN_L3" BufferOverrunChecker

let buffer_overrun_l4 =
  register_from_string ~enabled:false ~id:"BUFFER_OVERRUN_L4" BufferOverrunChecker


let buffer_overrun_l5 =
  register_from_string ~enabled:false ~id:"BUFFER_OVERRUN_L5" BufferOverrunChecker


let buffer_overrun_r2 = register_from_string ~id:"BUFFER_OVERRUN_R2" BufferOverrunChecker

let buffer_overrun_s2 = register_from_string ~id:"BUFFER_OVERRUN_S2" BufferOverrunChecker

let buffer_overrun_t1 = register_from_string ~id:"BUFFER_OVERRUN_T1" BufferOverrunChecker

let buffer_overrun_u5 =
  register_from_string ~enabled:false ~id:"BUFFER_OVERRUN_U5" BufferOverrunChecker


let cannot_star = register_from_string ~id:"Cannot_star" Biabduction

let captured_strong_self =
  register_from_string ~id:"CAPTURED_STRONG_SELF" ~hum:"Captured strongSelf" SelfInBlock


let checkers_allocates_memory =
  register_from_string ~id:"CHECKERS_ALLOCATES_MEMORY" ~hum:"Allocates Memory"
    AnnotationReachability


let checkers_annotation_reachability_error =
  register_from_string ~id:"CHECKERS_ANNOTATION_REACHABILITY_ERROR"
    ~hum:"Annotation Reachability Error" AnnotationReachability


let checkers_calls_expensive_method =
  register_from_string ~id:"CHECKERS_CALLS_EXPENSIVE_METHOD" ~hum:"Expensive Method Called"
    AnnotationReachability


let checkers_expensive_overrides_unexpensive =
  register_from_string ~id:"CHECKERS_EXPENSIVE_OVERRIDES_UNANNOTATED"
    ~hum:"Expensive Overrides Unannotated" AnnotationReachability


let checkers_fragment_retain_view =
  register_from_string ~id:"CHECKERS_FRAGMENT_RETAINS_VIEW" ~hum:"Fragment Retains View"
    FragmentRetainsView


let checkers_immutable_cast = register_from_string ~id:"CHECKERS_IMMUTABLE_CAST" ImmutableCast

let checkers_printf_args = register_from_string ~id:"CHECKERS_PRINTF_ARGS" PrintfArgs

let class_cast_exception =
  register_from_string ~enabled:false ~id:"CLASS_CAST_EXCEPTION" Biabduction


let class_load = register_from_string ~id:"CLASS_LOAD" ClassLoads

let component_factory_function = register_from_string ~id:"COMPONENT_FACTORY_FUNCTION" Linters

let component_file_cyclomatic_complexity =
  register_from_string ~id:"COMPONENT_FILE_CYCLOMATIC_COMPLEXITY" Linters


let component_file_line_count =
  register_from_string ~enabled:false ~id:"COMPONENT_FILE_LINE_COUNT" Linters


let component_initializer_with_side_effects =
  register_from_string ~id:"COMPONENT_INITIALIZER_WITH_SIDE_EFFECTS" Linters


let component_with_multiple_factory_methods =
  register_from_string ~id:"COMPONENT_WITH_MULTIPLE_FACTORY_METHODS" Linters


let component_with_unconventional_superclass =
  register_from_string ~id:"COMPONENT_WITH_UNCONVENTIONAL_SUPERCLASS" Linters


let condition_always_false =
  register_from_string ~enabled:false ~id:"CONDITION_ALWAYS_FALSE" BufferOverrunChecker


let condition_always_true =
  register_from_string ~enabled:false ~id:"CONDITION_ALWAYS_TRUE" BufferOverrunChecker


let constant_address_dereference =
  register_from_string ~enabled:false ~id:"CONSTANT_ADDRESS_DEREFERENCE" Pulse


let create_intent_from_uri = register_from_string ~id:"CREATE_INTENT_FROM_URI" Quandary

let cross_site_scripting = register_from_string ~id:"CROSS_SITE_SCRIPTING" Quandary

let dangling_pointer_dereference =
  register_from_string ~enabled:false ~id:"DANGLING_POINTER_DEREFERENCE" Biabduction


let dangling_pointer_dereference_maybe =
  register_from_string ~enabled:false ~id:"DANGLING_POINTER_DEREFERENCE_MAYBE" Biabduction


let dead_store = register_from_string ~id:"DEAD_STORE" Liveness

let deadlock = register_from_string ~id:"DEADLOCK" Starvation

let deallocate_stack_variable = register_from_string ~id:"DEALLOCATE_STACK_VARIABLE" Biabduction

let deallocate_static_memory = register_from_string ~id:"DEALLOCATE_STATIC_MEMORY" Biabduction

let deallocation_mismatch = register_from_string ~id:"DEALLOCATION_MISMATCH" Biabduction

let divide_by_zero = register_from_string ~enabled:false ~id:"DIVIDE_BY_ZERO" Biabduction

let do_not_report = register_from_string ~id:"DO_NOT_REPORT" Quandary

let empty_vector_access = register_from_string ~id:"EMPTY_VECTOR_ACCESS" Biabduction

let eradicate_condition_redundant =
  register_from_string ~id:"ERADICATE_CONDITION_REDUNDANT" ~hum:"Condition Redundant" Eradicate


(* TODO(T54070503) remove condition redundant nonnull *)
let _ =
  register_from_string ~id:"ERADICATE_CONDITION_REDUNDANT_NONNULL"
    ~hum:"Condition Redundant Non-Null" Eradicate


let eradicate_field_not_initialized =
  register_from_string ~id:"ERADICATE_FIELD_NOT_INITIALIZED" ~hum:"Field Not Initialized" Eradicate


let eradicate_field_not_nullable =
  register_from_string ~id:"ERADICATE_FIELD_NOT_NULLABLE" ~hum:"Field Not Nullable" Eradicate


let eradicate_field_over_annotated =
  register_from_string ~id:"ERADICATE_FIELD_OVER_ANNOTATED" ~hum:"Field Over Annotated" Eradicate


let eradicate_inconsistent_subclass_parameter_annotation =
  register_from_string ~id:"ERADICATE_INCONSISTENT_SUBCLASS_PARAMETER_ANNOTATION"
    ~hum:"Inconsistent Subclass Parameter Annotation" Eradicate


let eradicate_inconsistent_subclass_return_annotation =
  register_from_string ~id:"ERADICATE_INCONSISTENT_SUBCLASS_RETURN_ANNOTATION"
    ~hum:"Inconsistent Subclass Return Annotation" Eradicate


let eradicate_redundant_nested_class_annotation =
  register_from_string ~id:"ERADICATE_REDUNDANT_NESTED_CLASS_ANNOTATION"
    ~hum:"@Nullsafe annotation is redundant" Eradicate


let eradicate_bad_nested_class_annotation =
  register_from_string ~id:"ERADICATE_BAD_NESTED_CLASS_ANNOTATION"
    ~hum:"@Nullsafe annotation is inconsistent with outer class" Eradicate


let eradicate_nullable_dereference =
  register_from_string ~id:"ERADICATE_NULLABLE_DEREFERENCE" ~hum:"Nullable Dereference" Eradicate


let eradicate_parameter_not_nullable =
  register_from_string ~id:"ERADICATE_PARAMETER_NOT_NULLABLE" ~hum:"Parameter Not Nullable"
    Eradicate


let eradicate_return_not_nullable =
  register_from_string ~id:"ERADICATE_RETURN_NOT_NULLABLE" ~hum:"Return Not Nullable" Eradicate


let eradicate_return_over_annotated =
  register_from_string ~id:"ERADICATE_RETURN_OVER_ANNOTATED" ~hum:"Return Over Annotated" Eradicate


let eradicate_unchecked_usage_in_nullsafe =
  register_from_string ~id:"ERADICATE_UNCHECKED_USAGE_IN_NULLSAFE"
    ~hum:"Nullsafe mode: unchecked usage of a value" Eradicate


let eradicate_unvetted_third_party_in_nullsafe =
  register_from_string ~id:"ERADICATE_UNVETTED_THIRD_PARTY_IN_NULLSAFE"
    ~hum:"Nullsafe mode: unchecked usage of unvetted third-party" Eradicate


(* Meta issues in eradicate are technical issues reflecting null-safety state of classes in general,
   in contrast with concrete nullability type violations *)

let eradicate_meta_class_is_nullsafe =
  register_from_string ~id:"ERADICATE_META_CLASS_IS_NULLSAFE"
    ~hum:
      "Class is marked @Nullsafe and has 0 issues" (* Should be enabled for special integrations *)
    ~enabled:false Eradicate


(* Class is either:
   - has at least one nullability issue.
   - has at least one (currently possibly hidden) issue in order to be marked as @Nullsafe.
 *)
let eradicate_meta_class_needs_improvement =
  register_from_string ~id:"ERADICATE_META_CLASS_NEEDS_IMPROVEMENT"
    ~hum:
      "Class needs improvement to become @Nullsafe" (* Should be enabled for special integrations *)
    ~enabled:false Eradicate


let eradicate_meta_class_can_be_nullsafe =
  register_from_string ~id:"ERADICATE_META_CLASS_CAN_BE_NULLSAFE"
    ~hum:
      "Class has 0 issues and can be marked @Nullsafe"
      (* Should be enabled for special integrations *) ~enabled:false Eradicate


let expensive_cost_call ~kind ~is_on_ui_thread =
  register_from_cost_string ~enabled:false ~kind ~is_on_ui_thread "EXPENSIVE_%s"


let exposed_insecure_intent_handling =
  register_from_string ~id:"EXPOSED_INSECURE_INTENT_HANDLING" Quandary


let failure_exe = register_from_string ~id:"Failure_exe" Biabduction

let field_not_null_checked = register_from_string ~id:"IVAR_NOT_NULL_CHECKED" Biabduction

(* from AL default linters *)
let _global_variable_initialized_with_function_or_method_call =
  register_from_string ~enabled:false ~id:"GLOBAL_VARIABLE_INITIALIZED_WITH_FUNCTION_OR_METHOD_CALL"
    Linters


let guardedby_violation_racerd =
  register_from_string ~id:"GUARDEDBY_VIOLATION" ~hum:"GuardedBy Violation" RacerD


let impure_function = register_from_string ~id:"IMPURE_FUNCTION" Impurity

let inefficient_keyset_iterator =
  register_from_string ~id:"INEFFICIENT_KEYSET_ITERATOR" InefficientKeysetIterator


let inferbo_alloc_is_big = register_from_string ~id:"INFERBO_ALLOC_IS_BIG" BufferOverrunChecker

let inferbo_alloc_is_negative =
  register_from_string ~id:"INFERBO_ALLOC_IS_NEGATIVE" BufferOverrunChecker


let inferbo_alloc_is_zero = register_from_string ~id:"INFERBO_ALLOC_IS_ZERO" BufferOverrunChecker

let inferbo_alloc_may_be_big =
  register_from_string ~id:"INFERBO_ALLOC_MAY_BE_BIG" BufferOverrunChecker


let inferbo_alloc_may_be_negative =
  register_from_string ~id:"INFERBO_ALLOC_MAY_BE_NEGATIVE" BufferOverrunChecker


let inferbo_alloc_may_be_tainted =
  register_from_string ~id:"INFERBO_ALLOC_MAY_BE_TAINTED" BufferOverrunChecker


let infinite_cost_call ~kind = register_from_cost_string ~enabled:false "INFINITE_%s" ~kind

let inherently_dangerous_function =
  register_from_string ~id:"INHERENTLY_DANGEROUS_FUNCTION" Biabduction


let insecure_intent_handling = register_from_string ~id:"INSECURE_INTENT_HANDLING" Quandary

let integer_overflow_l1 = register_from_string ~id:"INTEGER_OVERFLOW_L1" BufferOverrunChecker

let integer_overflow_l2 = register_from_string ~id:"INTEGER_OVERFLOW_L2" BufferOverrunChecker

let integer_overflow_l5 =
  register_from_string ~enabled:false ~id:"INTEGER_OVERFLOW_L5" BufferOverrunChecker


let integer_overflow_r2 = register_from_string ~id:"INTEGER_OVERFLOW_R2" BufferOverrunChecker

let integer_overflow_u5 =
  register_from_string ~enabled:false ~id:"INTEGER_OVERFLOW_U5" BufferOverrunChecker


let interface_not_thread_safe = register_from_string ~id:"INTERFACE_NOT_THREAD_SAFE" RacerD

let internal_error = register_from_string ~id:"Internal_error" Biabduction

let invariant_call = register_from_string ~enabled:false ~id:"INVARIANT_CALL" LoopHoisting

let javascript_injection = register_from_string ~id:"JAVASCRIPT_INJECTION" Quandary

let lab_resource_leak = register_from_string ~id:"LAB_RESOURCE_LEAK" ResourceLeakLabExercise

let leak_after_array_abstraction =
  register_from_string ~id:"Leak_after_array_abstraction" Biabduction


let leak_in_footprint = register_from_string ~id:"Leak_in_footprint" Biabduction

let leak_unknown_origin = register_from_string ~enabled:false ~id:"Leak_unknown_origin" Biabduction

let lock_consistency_violation = register_from_string ~id:"LOCK_CONSISTENCY_VIOLATION" RacerD

let lockless_violation = register_from_string ~id:"LOCKLESS_VIOLATION" Starvation

let logging_private_data = register_from_string ~id:"LOGGING_PRIVATE_DATA" Quandary

let expensive_loop_invariant_call =
  register_from_string ~id:"EXPENSIVE_LOOP_INVARIANT_CALL" LoopHoisting


let memory_leak = register_from_string ~id:"MEMORY_LEAK" Biabduction

let missing_fld = register_from_string ~id:"Missing_fld" ~hum:"Missing Field" Biabduction

let missing_required_prop = register_from_string ~id:"MISSING_REQUIRED_PROP" LithoRequiredProps

let mixed_self_weakself =
  register_from_string ~id:"MIXED_SELF_WEAKSELF" ~hum:"Mixed Self WeakSelf" SelfInBlock


let multiple_weakself =
  register_from_string ~id:"MULTIPLE_WEAKSELF" ~hum:"Multiple WeakSelf Use" SelfInBlock


let mutable_local_variable_in_component_file =
  register_from_string ~id:"MUTABLE_LOCAL_VARIABLE_IN_COMPONENT_FILE" Linters


let null_dereference = register_from_string ~id:"NULL_DEREFERENCE" Biabduction

let null_test_after_dereference =
  register_from_string ~enabled:false ~id:"NULL_TEST_AFTER_DEREFERENCE" Biabduction


let nullptr_dereference = register_from_string ~enabled:false ~id:"NULLPTR_DEREFERENCE" Pulse

let parameter_not_null_checked = register_from_string ~id:"PARAMETER_NOT_NULL_CHECKED" Biabduction

let pointer_size_mismatch = register_from_string ~id:"POINTER_SIZE_MISMATCH" Biabduction

let precondition_not_found = register_from_string ~id:"PRECONDITION_NOT_FOUND" Biabduction

let precondition_not_met = register_from_string ~id:"PRECONDITION_NOT_MET" Biabduction

let premature_nil_termination =
  register_from_string ~id:"PREMATURE_NIL_TERMINATION_ARGUMENT" Biabduction


let pulse_memory_leak = register_from_string ~enabled:false ~id:"PULSE_MEMORY_LEAK" Pulse

let pure_function = register_from_string ~id:"PURE_FUNCTION" Purity

let quandary_taint_error =
  register_from_string ~hum:"Taint Error" ~id:"QUANDARY_TAINT_ERROR" Quandary


let resource_leak = register_from_string ~id:"RESOURCE_LEAK" Biabduction

let retain_cycle = register_from_string ~enabled:true ~id:"RETAIN_CYCLE" Biabduction

let skip_function = register_from_string ~enabled:false ~id:"SKIP_FUNCTION" Biabduction

let skip_pointer_dereference =
  register_from_string ~enabled:false ~id:"SKIP_POINTER_DEREFERENCE" Biabduction


let shell_injection = register_from_string ~id:"SHELL_INJECTION" Quandary

let shell_injection_risk = register_from_string ~id:"SHELL_INJECTION_RISK" Quandary

let sql_injection = register_from_string ~id:"SQL_INJECTION" Quandary

let sql_injection_risk = register_from_string ~id:"SQL_INJECTION_RISK" Quandary

let stack_variable_address_escape = register_from_string ~id:"STACK_VARIABLE_ADDRESS_ESCAPE" Pulse

let starvation = register_from_string ~id:"STARVATION" ~hum:"UI Thread Starvation" Starvation

let static_initialization_order_fiasco =
  register_from_string ~id:"STATIC_INITIALIZATION_ORDER_FIASCO" SIOF


let strict_mode_violation =
  register_from_string ~id:"STRICT_MODE_VIOLATION" ~hum:"Strict Mode Violation" Starvation


let strong_self_not_checked =
  register_from_string ~id:"STRONG_SELF_NOT_CHECKED" ~hum:"StrongSelf Not Checked" SelfInBlock


let symexec_memory_error =
  register_from_string ~id:"Symexec_memory_error" ~hum:"Symbolic Execution Memory Error" Biabduction


let thread_safety_violation = register_from_string ~id:"THREAD_SAFETY_VIOLATION" RacerD

let complexity_increase ~kind ~is_on_ui_thread =
  register_from_cost_string ~kind ~is_on_ui_thread "%s_COMPLEXITY_INCREASE"


let topl_error = register_from_string ~id:"TOPL_ERROR" TOPL

let unary_minus_applied_to_unsigned_expression =
  register_from_string ~enabled:false ~id:"UNARY_MINUS_APPLIED_TO_UNSIGNED_EXPRESSION" Biabduction


let uninitialized_value = register_from_string ~id:"UNINITIALIZED_VALUE" Uninit

let unreachable_code_after = register_from_string ~id:"UNREACHABLE_CODE" BufferOverrunChecker

let use_after_delete = register_from_string ~id:"USE_AFTER_DELETE" Pulse

let use_after_free = register_from_string ~id:"USE_AFTER_FREE" Pulse

let use_after_lifetime = register_from_string ~id:"USE_AFTER_LIFETIME" Pulse

let user_controlled_sql_risk = register_from_string ~id:"USER_CONTROLLED_SQL_RISK" Quandary

let untrusted_buffer_access =
  register_from_string ~enabled:false ~id:"UNTRUSTED_BUFFER_ACCESS" Quandary


let untrusted_deserialization = register_from_string ~id:"UNTRUSTED_DESERIALIZATION" Quandary

let untrusted_deserialization_risk =
  register_from_string ~id:"UNTRUSTED_DESERIALIZATION_RISK" Quandary


let untrusted_environment_change_risk =
  register_from_string ~id:"UNTRUSTED_ENVIRONMENT_CHANGE_RISK" Quandary


let untrusted_file = register_from_string ~id:"UNTRUSTED_FILE" Quandary

let untrusted_file_risk = register_from_string ~id:"UNTRUSTED_FILE_RISK" Quandary

let untrusted_heap_allocation =
  register_from_string ~enabled:false ~id:"UNTRUSTED_HEAP_ALLOCATION" Quandary


let untrusted_intent_creation = register_from_string ~id:"UNTRUSTED_INTENT_CREATION" Quandary

let untrusted_url_risk = register_from_string ~id:"UNTRUSTED_URL_RISK" Quandary

let untrusted_variable_length_array =
  register_from_string ~id:"UNTRUSTED_VARIABLE_LENGTH_ARRAY" Quandary


let vector_invalidation = register_from_string ~id:"VECTOR_INVALIDATION" Pulse

let weak_self_in_noescape_block =
  register_from_string ~id:"WEAK_SELF_IN_NO_ESCAPE_BLOCK" SelfInBlock


let wrong_argument_number =
  register_from_string ~id:"Wrong_argument_number" ~hum:"Wrong Argument Number" Biabduction


let unreachable_cost_call ~kind =
  register_from_cost_string ~enabled:false ~kind "%s_UNREACHABLE_AT_EXIT"


(* register enabled cost issues *)
let () =
  List.iter CostKind.enabled_cost_kinds ~f:(fun CostKind.{kind} ->
      List.iter [true; false] ~f:(fun is_on_ui_thread ->
          ignore (unreachable_cost_call ~kind) ;
          ignore (expensive_cost_call ~kind ~is_on_ui_thread) ;
          ignore (infinite_cost_call ~kind) ;
          ignore (complexity_increase ~kind ~is_on_ui_thread) ;
          () ) )
