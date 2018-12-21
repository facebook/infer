(*
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

(* Make sure we cannot create new issue types other than by calling [from_string]. This is because
     we want to keep track of the list of all the issues ever declared. *)
module Unsafe : sig
  type t = private
    { unique_id: string
    ; mutable enabled: bool
    ; mutable hum: string
    ; mutable doc_url: string option
    ; mutable linters_def_file: string option }
  [@@deriving compare]

  val equal : t -> t -> bool

  val from_string :
    ?enabled:bool -> ?hum:string -> ?doc_url:string -> ?linters_def_file:string -> string -> t

  val all_issues : unit -> t list

  val set_enabled : t -> bool -> unit
end = struct
  module T = struct
    type t =
      { unique_id: string
      ; mutable enabled: bool
      ; mutable hum: string
      ; mutable doc_url: string option
      ; mutable linters_def_file: string option }

    let compare {unique_id= id1} {unique_id= id2} = String.compare id1 id2

    let equal = [%compare.equal: t]
  end

  include T
  module IssueSet = Caml.Set.Make (T)

  (** keep track of the list of all declared issue types *)
  let all_issues = ref IssueSet.empty

  let prettify s =
    String.lowercase s |> String.split ~on:'_' |> List.map ~f:String.capitalize
    |> String.concat ~sep:" " |> String.strip


  let set_enabled issue b = issue.enabled <- b

  (** avoid creating new issue types. The idea is that there are three types of issue types:
        1. Statically pre-defined issue types, namely the ones in this module

        2. Dynamically created ones, eg from custom errors defined in the models, or defined by the
        user in AL linters

        3. Issue types created at command-line-parsing time. These can mention issues of type 1. or
        2., but issues of type 2. have not yet been defined. Thus, we record only there [enabled]
        status definitely. The [hum]an-readable description can be updated when we encounter the
        definition of the issue type, eg in AL. *)
  let from_string ?(enabled = true) ?hum:hum0 ?doc_url ?linters_def_file unique_id =
    let hum = match hum0 with Some str -> str | _ -> prettify unique_id in
    let issue = {unique_id; enabled; hum; doc_url; linters_def_file} in
    try
      let old = IssueSet.find issue !all_issues in
      (* update human-readable string in case it was supplied this time, but keep the previous
           value of enabled (see doc comment) *)
      if Option.is_some hum0 then old.hum <- hum ;
      if Option.is_some doc_url then old.doc_url <- doc_url ;
      if Option.is_some linters_def_file then old.linters_def_file <- linters_def_file ;
      old
    with Caml.Not_found ->
      all_issues := IssueSet.add issue !all_issues ;
      issue


  let all_issues () = IssueSet.elements !all_issues
end

include Unsafe

(** pretty print a localised string *)
let pp fmt t = Format.pp_print_string fmt t.unique_id

let abduction_case_not_implemented = from_string "Abduction_case_not_implemented"

let analysis_stops = from_string ~enabled:false "ANALYSIS_STOPS"

let array_of_pointsto = from_string "Array_of_pointsto"

let array_out_of_bounds_l1 = from_string ~enabled:false "ARRAY_OUT_OF_BOUNDS_L1"

let array_out_of_bounds_l2 = from_string ~enabled:false "ARRAY_OUT_OF_BOUNDS_L2"

let array_out_of_bounds_l3 = from_string ~enabled:false "ARRAY_OUT_OF_BOUNDS_L3"

let assert_failure = from_string "Assert_failure"

let bad_footprint = from_string "Bad_footprint"

let buffer_overrun_l1 = from_string "BUFFER_OVERRUN_L1"

let buffer_overrun_l2 = from_string "BUFFER_OVERRUN_L2"

let buffer_overrun_l3 = from_string "BUFFER_OVERRUN_L3"

let buffer_overrun_l4 = from_string ~enabled:false "BUFFER_OVERRUN_L4"

let buffer_overrun_l5 = from_string ~enabled:false "BUFFER_OVERRUN_L5"

let buffer_overrun_r2 = from_string "BUFFER_OVERRUN_R2"

let buffer_overrun_s2 = from_string "BUFFER_OVERRUN_S2"

let buffer_overrun_u5 = from_string ~enabled:false "BUFFER_OVERRUN_U5"

let cannot_star = from_string "Cannot_star"

let checkers_allocates_memory = from_string "CHECKERS_ALLOCATES_MEMORY"

let checkers_annotation_reachability_error = from_string "CHECKERS_ANNOTATION_REACHABILITY_ERROR"

let checkers_calls_expensive_method = from_string "CHECKERS_CALLS_EXPENSIVE_METHOD"

let checkers_expensive_overrides_unexpensive =
  from_string "CHECKERS_EXPENSIVE_OVERRIDES_UNANNOTATED"


let checkers_fragment_retain_view = from_string "CHECKERS_FRAGMENT_RETAINS_VIEW"

let checkers_immutable_cast = from_string "CHECKERS_IMMUTABLE_CAST"

let checkers_printf_args = from_string "CHECKERS_PRINTF_ARGS"

let class_cast_exception = from_string ~enabled:false "CLASS_CAST_EXCEPTION"

let class_load = from_string "CLASS_LOAD"

let codequery = from_string "Codequery"

let comparing_floats_for_equality = from_string "COMPARING_FLOAT_FOR_EQUALITY"

let component_factory_function = from_string "COMPONENT_FACTORY_FUNCTION"

let component_file_cyclomatic_complexity = from_string "COMPONENT_FILE_CYCLOMATIC_COMPLEXITY"

let component_file_line_count = from_string "COMPONENT_FILE_LINE_COUNT"

let component_initializer_with_side_effects = from_string "COMPONENT_INITIALIZER_WITH_SIDE_EFFECTS"

let component_with_multiple_factory_methods = from_string "COMPONENT_WITH_MULTIPLE_FACTORY_METHODS"

let component_with_unconventional_superclass =
  from_string "COMPONENT_WITH_UNCONVENTIONAL_SUPERCLASS"


let condition_always_false = from_string ~enabled:false "CONDITION_ALWAYS_FALSE"

let condition_always_true = from_string ~enabled:false "CONDITION_ALWAYS_TRUE"

let create_intent_from_uri = from_string "CREATE_INTENT_FROM_URI"

let cross_site_scripting = from_string "CROSS_SITE_SCRIPTING"

let dangling_pointer_dereference = from_string ~enabled:false "DANGLING_POINTER_DEREFERENCE"

let dead_store = from_string "DEAD_STORE"

let deadlock = from_string "DEADLOCK"

let deallocate_stack_variable = from_string "DEALLOCATE_STACK_VARIABLE"

let deallocate_static_memory = from_string "DEALLOCATE_STATIC_MEMORY"

let deallocation_mismatch = from_string "DEALLOCATION_MISMATCH"

let divide_by_zero = from_string ~enabled:false "DIVIDE_BY_ZERO"

let do_not_report = from_string "DO_NOT_REPORT"

let empty_vector_access = from_string "EMPTY_VECTOR_ACCESS"

let eradicate_condition_redundant =
  from_string "ERADICATE_CONDITION_REDUNDANT" ~hum:"Condition Redundant"


let eradicate_condition_redundant_nonnull =
  from_string "ERADICATE_CONDITION_REDUNDANT_NONNULL" ~hum:"Condition Redundant Non-Null"


let eradicate_field_not_initialized =
  from_string "ERADICATE_FIELD_NOT_INITIALIZED" ~hum:"Field Not Initialized"


let eradicate_field_not_mutable =
  from_string "ERADICATE_FIELD_NOT_MUTABLE" ~hum:"Field Not Mutable"


let eradicate_field_not_nullable =
  from_string "ERADICATE_FIELD_NOT_NULLABLE" ~hum:"Field Not Nullable"


let eradicate_field_over_annotated =
  from_string "ERADICATE_FIELD_OVER_ANNOTATED" ~hum:"Field Over Annotated"


let eradicate_field_value_absent =
  from_string "ERADICATE_FIELD_VALUE_ABSENT" ~hum:"Field Value Absent"


let eradicate_inconsistent_subclass_parameter_annotation =
  from_string "ERADICATE_INCONSISTENT_SUBCLASS_PARAMETER_ANNOTATION"
    ~hum:"Inconsistent Subclass Parameter Annotation"


let eradicate_inconsistent_subclass_return_annotation =
  from_string "ERADICATE_INCONSISTENT_SUBCLASS_RETURN_ANNOTATION"
    ~hum:"Inconsistent Subclass Return Annotation"


let eradicate_null_field_access =
  from_string "ERADICATE_NULL_FIELD_ACCESS" ~hum:"Null Field Access"


let eradicate_null_method_call = from_string "ERADICATE_NULL_METHOD_CALL" ~hum:"Null Method Call"

let eradicate_parameter_not_nullable =
  from_string "ERADICATE_PARAMETER_NOT_NULLABLE" ~hum:"Parameter Not Nullable"


let eradicate_parameter_value_absent =
  from_string "ERADICATE_PARAMETER_VALUE_ABSENT" ~hum:"Parameter Value Absent"


let eradicate_return_not_nullable =
  from_string "ERADICATE_RETURN_NOT_NULLABLE" ~hum:"Return Not Nullable"


let eradicate_return_over_annotated =
  from_string "ERADICATE_RETURN_OVER_ANNOTATED" ~hum:"Return Over Annotated"


let eradicate_return_value_not_present =
  from_string "ERADICATE_RETURN_VALUE_NOT_PRESENT" ~hum:"Return Value Not Present"


let eradicate_value_not_present =
  from_string "ERADICATE_VALUE_NOT_PRESENT" ~hum:"Value Not Present"


let expensive_execution_time_call = from_string ~enabled:false "EXPENSIVE_EXECUTION_TIME_CALL"

let exposed_insecure_intent_handling = from_string "EXPOSED_INSECURE_INTENT_HANDLING"

let failure_exe = from_string "Failure_exe"

let nullsafe_field_not_nullable =
  from_string "NULLSAFE_FIELD_NOT_NULLABLE" ~hum:"Field Not Nullable"


let field_not_null_checked = from_string "IVAR_NOT_NULL_CHECKED"

(* from AL default linters *)
let _global_variable_initialized_with_function_or_method_call =
  from_string ~enabled:false "GLOBAL_VARIABLE_INITIALIZED_WITH_FUNCTION_OR_METHOD_CALL"


let graphql_field_access = from_string "GRAPHQL_FIELD_ACCESS"

let inferbo_alloc_is_big = from_string "INFERBO_ALLOC_IS_BIG"

let inferbo_alloc_is_negative = from_string "INFERBO_ALLOC_IS_NEGATIVE"

let inferbo_alloc_is_zero = from_string "INFERBO_ALLOC_IS_ZERO"

let inferbo_alloc_may_be_big = from_string "INFERBO_ALLOC_MAY_BE_BIG"

let inferbo_alloc_may_be_negative = from_string "INFERBO_ALLOC_MAY_BE_NEGATIVE"

let infinite_execution_time_call = from_string ~enabled:false "INFINITE_EXECUTION_TIME_CALL"

let inherently_dangerous_function = from_string "INHERENTLY_DANGEROUS_FUNCTION"

let insecure_intent_handling = from_string "INSECURE_INTENT_HANDLING"

let integer_overflow_l1 = from_string "INTEGER_OVERFLOW_L1"

let integer_overflow_l2 = from_string "INTEGER_OVERFLOW_L2"

let integer_overflow_l5 = from_string ~enabled:false "INTEGER_OVERFLOW_L5"

let integer_overflow_r2 = from_string "INTEGER_OVERFLOW_R2"

let integer_overflow_u5 = from_string ~enabled:false "INTEGER_OVERFLOW_U5"

let interface_not_thread_safe = from_string "INTERFACE_NOT_THREAD_SAFE"

let internal_error = from_string "Internal_error"

let invariant_call = from_string "INVARIANT_CALL"

let javascript_injection = from_string "JAVASCRIPT_INJECTION"

let leak_after_array_abstraction = from_string "Leak_after_array_abstraction"

let leak_in_footprint = from_string "Leak_in_footprint"

let lock_consistency_violation = from_string "LOCK_CONSISTENCY_VIOLATION"

let logging_private_data = from_string "LOGGING_PRIVATE_DATA"

let loop_invariant_call = from_string "LOOP_INVARIANT_CALL"

let memory_leak = from_string "MEMORY_LEAK"

let missing_fld = from_string "Missing_fld" ~hum:"Missing Field"

let missing_required_prop = from_string "MISSING_REQUIRED_PROP"

let mutable_local_variable_in_component_file =
  from_string "MUTABLE_LOCAL_VARIABLE_IN_COMPONENT_FILE"


let null_dereference = from_string "NULL_DEREFERENCE"

let null_test_after_dereference = from_string ~enabled:false "NULL_TEST_AFTER_DEREFERENCE"

let nullable_dereference = from_string "NULLABLE_DEREFERENCE"

let parameter_not_null_checked = from_string "PARAMETER_NOT_NULL_CHECKED"

let performance_variation = from_string "PERFORMANCE_VARIATION"

let pointer_size_mismatch = from_string "POINTER_SIZE_MISMATCH"

let precondition_not_found = from_string "PRECONDITION_NOT_FOUND"

let precondition_not_met = from_string "PRECONDITION_NOT_MET"

let premature_nil_termination = from_string "PREMATURE_NIL_TERMINATION_ARGUMENT"

let pure_function = from_string "PURE_FUNCTION"

let quandary_taint_error = from_string "QUANDARY_TAINT_ERROR"

let registered_observer_being_deallocated = from_string "REGISTERED_OBSERVER_BEING_DEALLOCATED"

let resource_leak = from_string "RESOURCE_LEAK"

let retain_cycle = from_string ~enabled:true "RETAIN_CYCLE"

let return_expression_required = from_string "RETURN_EXPRESSION_REQUIRED"

let return_statement_missing = from_string "RETURN_STATEMENT_MISSING"

let return_value_ignored = from_string ~enabled:false "RETURN_VALUE_IGNORED"

let skip_function = from_string "SKIP_FUNCTION"

let skip_pointer_dereference = from_string "SKIP_POINTER_DEREFERENCE"

let shell_injection = from_string "SHELL_INJECTION"

let shell_injection_risk = from_string "SHELL_INJECTION_RISK"

let sql_injection = from_string "SQL_INJECTION"

let sql_injection_risk = from_string "SQL_INJECTION_RISK"

let stack_variable_address_escape = from_string ~enabled:false "STACK_VARIABLE_ADDRESS_ESCAPE"

let starvation = from_string "STARVATION" ~hum:"UI Thread Starvation"

let static_initialization_order_fiasco = from_string "STATIC_INITIALIZATION_ORDER_FIASCO"

let strict_mode_violation = from_string "STRICT_MODE_VIOLATION" ~hum:"Strict Mode Violation"

let symexec_memory_error =
  from_string "Symexec_memory_error" ~hum:"Symbolic Execution Memory Error"


let tainted_buffer_access = from_string "TAINTED_BUFFER_ACCESS"

let tainted_memory_allocation = from_string "TAINTED_MEMORY_ALLOCATION"

let thread_safety_violation = from_string "THREAD_SAFETY_VIOLATION"

let unary_minus_applied_to_unsigned_expression =
  from_string ~enabled:false "UNARY_MINUS_APPLIED_TO_UNSIGNED_EXPRESSION"


let uninitialized_value = from_string "UNINITIALIZED_VALUE"

let unknown_proc = from_string "Unknown_proc" ~hum:"Unknown Procedure"

let unreachable_code_after = from_string "UNREACHABLE_CODE"

let unsafe_guarded_by_access = from_string "UNSAFE_GUARDED_BY_ACCESS"

let use_after_delete = from_string "USE_AFTER_DELETE"

let use_after_destructor = from_string "USE_AFTER_DESTRUCTOR"

let use_after_free = from_string "USE_AFTER_FREE"

let use_after_lifetime = from_string "USE_AFTER_LIFETIME"

let user_controlled_sql_risk = from_string "USER_CONTROLLED_SQL_RISK"

let untrusted_buffer_access = from_string ~enabled:false "UNTRUSTED_BUFFER_ACCESS"

let untrusted_deserialization = from_string "UNTRUSTED_DESERIALIZATION"

let untrusted_deserialization_risk = from_string "UNTRUSTED_DESERIALIZATION_RISK"

let untrusted_environment_change_risk = from_string "UNTRUSTED_ENVIRONMENT_CHANGE_RISK"

let untrusted_file = from_string "UNTRUSTED_FILE"

let untrusted_file_risk = from_string "UNTRUSTED_FILE_RISK"

let untrusted_heap_allocation = from_string ~enabled:false "UNTRUSTED_HEAP_ALLOCATION"

let untrusted_intent_creation = from_string "UNTRUSTED_INTENT_CREATION"

let untrusted_url_risk = from_string "UNTRUSTED_URL_RISK"

let untrusted_variable_length_array = from_string "UNTRUSTED_VARIABLE_LENGTH_ARRAY"

let vector_invalidation = from_string "VECTOR_INVALIDATION"

let wrong_argument_number = from_string "Wrong_argument_number" ~hum:"Wrong Argument Number"

let zero_execution_time_call = from_string ~enabled:false "ZERO_EXECUTION_TIME_CALL"
