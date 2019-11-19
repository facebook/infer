(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

(* Make sure we cannot create new issue types other than by calling [register_from_string]. This is because
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

  val register_from_string :
    ?enabled:bool -> ?hum:string -> ?doc_url:string -> ?linters_def_file:string -> string -> t

  val register_from_cost_string :
       ?enabled:bool
    -> ?is_on_cold_start:bool
    -> ?is_on_ui_thread:bool
    -> kind:CostKind.t
    -> (string -> string, Format.formatter, unit, string) format4
    -> t

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

  (** Avoid creating new issue types. The idea is that there are three types of issue types:
        1. Statically pre-defined issue types, namely the ones in this module

        2. Dynamically created ones, eg from custom errors defined in the models, or defined by the
        user in AL linters

        3. Issue types created at command-line-parsing time. These can mention issues of type 1. or
        2., but issues of type 2. have not yet been defined. Thus, we record only there [enabled]
        status definitely. The [hum]an-readable description can be updated when we encounter the
        definition of the issue type, eg in AL. *)
  let register_from_string ?(enabled = true) ?hum:hum0 ?doc_url ?linters_def_file unique_id =
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


  (** cost issues are already registered below.*)
  let register_from_cost_string ?(enabled = true) ?(is_on_cold_start = false)
      ?(is_on_ui_thread = false) ~(kind : CostKind.t) s =
    let issue_type_base = Format.asprintf s (CostKind.to_issue_string kind) in
    let issue_type =
      if is_on_ui_thread then issue_type_base ^ "_UI_THREAD"
      else if is_on_cold_start then issue_type_base ^ "_COLD_START"
      else issue_type_base
    in
    register_from_string ~enabled issue_type


  let all_issues () = IssueSet.elements !all_issues
end

include Unsafe

(** pretty print a localised string *)
let pp fmt t = Format.pp_print_string fmt t.unique_id

let abduction_case_not_implemented = register_from_string "Abduction_case_not_implemented"

let analysis_stops = register_from_string ~enabled:false "ANALYSIS_STOPS"

let array_of_pointsto = register_from_string "Array_of_pointsto"

let array_out_of_bounds_l1 = register_from_string ~enabled:false "ARRAY_OUT_OF_BOUNDS_L1"

let array_out_of_bounds_l2 = register_from_string ~enabled:false "ARRAY_OUT_OF_BOUNDS_L2"

let array_out_of_bounds_l3 = register_from_string ~enabled:false "ARRAY_OUT_OF_BOUNDS_L3"

let assert_failure = register_from_string "Assert_failure"

let bad_footprint = register_from_string "Bad_footprint"

let buffer_overrun_l1 = register_from_string "BUFFER_OVERRUN_L1"

let buffer_overrun_l2 = register_from_string "BUFFER_OVERRUN_L2"

let buffer_overrun_l3 = register_from_string "BUFFER_OVERRUN_L3"

let buffer_overrun_l4 = register_from_string ~enabled:false "BUFFER_OVERRUN_L4"

let buffer_overrun_l5 = register_from_string ~enabled:false "BUFFER_OVERRUN_L5"

let buffer_overrun_r2 = register_from_string "BUFFER_OVERRUN_R2"

let buffer_overrun_s2 = register_from_string "BUFFER_OVERRUN_S2"

let buffer_overrun_u5 = register_from_string ~enabled:false "BUFFER_OVERRUN_U5"

let cannot_star = register_from_string "Cannot_star"

let checkers_allocates_memory =
  register_from_string "CHECKERS_ALLOCATES_MEMORY" ~hum:"Allocates Memory"


let checkers_annotation_reachability_error =
  register_from_string "CHECKERS_ANNOTATION_REACHABILITY_ERROR" ~hum:"Annotation Reachability Error"


let checkers_calls_expensive_method =
  register_from_string "CHECKERS_CALLS_EXPENSIVE_METHOD" ~hum:"Expensive Method Called"


let checkers_expensive_overrides_unexpensive =
  register_from_string "CHECKERS_EXPENSIVE_OVERRIDES_UNANNOTATED"
    ~hum:"Expensive Overrides Unannotated"


let checkers_fragment_retain_view =
  register_from_string "CHECKERS_FRAGMENT_RETAINS_VIEW" ~hum:"Fragment Retains View"


let checkers_immutable_cast = register_from_string "CHECKERS_IMMUTABLE_CAST"

let checkers_printf_args = register_from_string "CHECKERS_PRINTF_ARGS"

let class_cast_exception = register_from_string ~enabled:false "CLASS_CAST_EXCEPTION"

let class_load = register_from_string "CLASS_LOAD"

let codequery = register_from_string "Codequery"

let comparing_floats_for_equality = register_from_string "COMPARING_FLOAT_FOR_EQUALITY"

let component_factory_function = register_from_string "COMPONENT_FACTORY_FUNCTION"

let component_file_cyclomatic_complexity =
  register_from_string "COMPONENT_FILE_CYCLOMATIC_COMPLEXITY"


let component_file_line_count = register_from_string "COMPONENT_FILE_LINE_COUNT"

let component_initializer_with_side_effects =
  register_from_string "COMPONENT_INITIALIZER_WITH_SIDE_EFFECTS"


let component_with_multiple_factory_methods =
  register_from_string "COMPONENT_WITH_MULTIPLE_FACTORY_METHODS"


let component_with_unconventional_superclass =
  register_from_string "COMPONENT_WITH_UNCONVENTIONAL_SUPERCLASS"


let condition_always_false = register_from_string ~enabled:false "CONDITION_ALWAYS_FALSE"

let condition_always_true = register_from_string ~enabled:false "CONDITION_ALWAYS_TRUE"

let constant_address_dereference =
  register_from_string ~enabled:false "CONSTANT_ADDRESS_DEREFERENCE"


let create_intent_from_uri = register_from_string "CREATE_INTENT_FROM_URI"

let cross_site_scripting = register_from_string "CROSS_SITE_SCRIPTING"

let dangling_pointer_dereference =
  register_from_string ~enabled:false "DANGLING_POINTER_DEREFERENCE"


let dead_store = register_from_string "DEAD_STORE"

let deadlock = register_from_string "DEADLOCK"

let deallocate_stack_variable = register_from_string "DEALLOCATE_STACK_VARIABLE"

let deallocate_static_memory = register_from_string "DEALLOCATE_STATIC_MEMORY"

let deallocation_mismatch = register_from_string "DEALLOCATION_MISMATCH"

let divide_by_zero = register_from_string ~enabled:false "DIVIDE_BY_ZERO"

let do_not_report = register_from_string "DO_NOT_REPORT"

let empty_vector_access = register_from_string "EMPTY_VECTOR_ACCESS"

let eradicate_condition_redundant =
  register_from_string "ERADICATE_CONDITION_REDUNDANT" ~hum:"Condition Redundant"


(* TODO(T54070503) remove condition redundant nonnull *)
let _ =
  register_from_string "ERADICATE_CONDITION_REDUNDANT_NONNULL" ~hum:"Condition Redundant Non-Null"


let eradicate_field_not_initialized =
  register_from_string "ERADICATE_FIELD_NOT_INITIALIZED" ~hum:"Field Not Initialized"


let eradicate_field_not_nullable =
  register_from_string "ERADICATE_FIELD_NOT_NULLABLE" ~hum:"Field Not Nullable"


let eradicate_field_over_annotated =
  register_from_string "ERADICATE_FIELD_OVER_ANNOTATED" ~hum:"Field Over Annotated"


let eradicate_inconsistent_subclass_parameter_annotation =
  register_from_string "ERADICATE_INCONSISTENT_SUBCLASS_PARAMETER_ANNOTATION"
    ~hum:"Inconsistent Subclass Parameter Annotation"


let eradicate_inconsistent_subclass_return_annotation =
  register_from_string "ERADICATE_INCONSISTENT_SUBCLASS_RETURN_ANNOTATION"
    ~hum:"Inconsistent Subclass Return Annotation"


let eradicate_nullable_dereference =
  register_from_string "ERADICATE_NULLABLE_DEREFERENCE" ~hum:"Nullable Dereference"


let eradicate_parameter_not_nullable =
  register_from_string "ERADICATE_PARAMETER_NOT_NULLABLE" ~hum:"Parameter Not Nullable"


let eradicate_return_not_nullable =
  register_from_string "ERADICATE_RETURN_NOT_NULLABLE" ~hum:"Return Not Nullable"


let eradicate_return_over_annotated =
  register_from_string "ERADICATE_RETURN_OVER_ANNOTATED" ~hum:"Return Over Annotated"


let eradicate_forbidden_non_strict_in_strict =
  register_from_string "ERADICATE_UNCHECKED_NONSTRICT_FROM_STRICT"
    ~hum:"Strict mode: unchecked usage of a value from non-strict code"


let eradicate_unvetted_third_party_in_strict =
  register_from_string "ERADICATE_UNVETTED_THIRD_PARTY_IN_STRICT"
    ~hum:"Strict mode: unchecked usage of unvetted third-party"


let expensive_cost_call ~kind ~is_on_cold_start ~is_on_ui_thread =
  register_from_cost_string ~enabled:false ~kind ~is_on_cold_start ~is_on_ui_thread "EXPENSIVE_%s"


let exposed_insecure_intent_handling = register_from_string "EXPOSED_INSECURE_INTENT_HANDLING"

let failure_exe = register_from_string "Failure_exe"

let field_not_null_checked = register_from_string "IVAR_NOT_NULL_CHECKED"

(* from AL default linters *)
let _global_variable_initialized_with_function_or_method_call =
  register_from_string ~enabled:false "GLOBAL_VARIABLE_INITIALIZED_WITH_FUNCTION_OR_METHOD_CALL"


let graphql_field_access = register_from_string "GRAPHQL_FIELD_ACCESS"

let guardedby_violation_racerd =
  register_from_string "GUARDEDBY_VIOLATION" ~hum:"GuardedBy Violation"


let impure_function = register_from_string "IMPURE_FUNCTION"

let inefficient_keyset_iterator = register_from_string "INEFFICIENT_KEYSET_ITERATOR"

let inferbo_alloc_is_big = register_from_string "INFERBO_ALLOC_IS_BIG"

let inferbo_alloc_is_negative = register_from_string "INFERBO_ALLOC_IS_NEGATIVE"

let inferbo_alloc_is_zero = register_from_string "INFERBO_ALLOC_IS_ZERO"

let inferbo_alloc_may_be_big = register_from_string "INFERBO_ALLOC_MAY_BE_BIG"

let inferbo_alloc_may_be_negative = register_from_string "INFERBO_ALLOC_MAY_BE_NEGATIVE"

let infinite_cost_call ~kind = register_from_cost_string ~enabled:false "INFINITE_%s" ~kind

let inherently_dangerous_function = register_from_string "INHERENTLY_DANGEROUS_FUNCTION"

let insecure_intent_handling = register_from_string "INSECURE_INTENT_HANDLING"

let integer_overflow_l1 = register_from_string "INTEGER_OVERFLOW_L1"

let integer_overflow_l2 = register_from_string "INTEGER_OVERFLOW_L2"

let integer_overflow_l5 = register_from_string ~enabled:false "INTEGER_OVERFLOW_L5"

let integer_overflow_r2 = register_from_string "INTEGER_OVERFLOW_R2"

let integer_overflow_u5 = register_from_string ~enabled:false "INTEGER_OVERFLOW_U5"

let interface_not_thread_safe = register_from_string "INTERFACE_NOT_THREAD_SAFE"

let internal_error = register_from_string "Internal_error"

let invariant_call = register_from_string ~enabled:false "INVARIANT_CALL"

let javascript_injection = register_from_string "JAVASCRIPT_INJECTION"

let leak_after_array_abstraction = register_from_string "Leak_after_array_abstraction"

let leak_in_footprint = register_from_string "Leak_in_footprint"

let lock_consistency_violation = register_from_string "LOCK_CONSISTENCY_VIOLATION"

let lockless_violation = register_from_string "LOCKLESS_VIOLATION"

let logging_private_data = register_from_string "LOGGING_PRIVATE_DATA"

let expensive_loop_invariant_call = register_from_string "EXPENSIVE_LOOP_INVARIANT_CALL"

let memory_leak = register_from_string "MEMORY_LEAK"

let missing_fld = register_from_string "Missing_fld" ~hum:"Missing Field"

let missing_required_prop = register_from_string "MISSING_REQUIRED_PROP"

let mixed_self_weakself = register_from_string "MIXED_SELF_WEAKSELF" ~hum:"Mixed Self WeakSelf"

let mutable_local_variable_in_component_file =
  register_from_string "MUTABLE_LOCAL_VARIABLE_IN_COMPONENT_FILE"


let null_dereference = register_from_string "NULL_DEREFERENCE"

let null_test_after_dereference = register_from_string ~enabled:false "NULL_TEST_AFTER_DEREFERENCE"

let nullptr_dereference = register_from_string ~enabled:false "NULLPTR_DEREFERENCE"

let nullsafe_field_not_nullable =
  register_from_string "NULLSAFE_FIELD_NOT_NULLABLE" ~hum:"Field Not Nullable"


let nullsafe_nullable_dereference =
  register_from_string "NULLSAFE_NULLABLE_DEREFERENCE" ~hum:"Nullable Dereference"


let parameter_not_null_checked = register_from_string "PARAMETER_NOT_NULL_CHECKED"

let pointer_size_mismatch = register_from_string "POINTER_SIZE_MISMATCH"

let precondition_not_found = register_from_string "PRECONDITION_NOT_FOUND"

let precondition_not_met = register_from_string "PRECONDITION_NOT_MET"

let premature_nil_termination = register_from_string "PREMATURE_NIL_TERMINATION_ARGUMENT"

let pure_function = register_from_string "PURE_FUNCTION"

let quandary_taint_error = register_from_string "QUANDARY_TAINT_ERROR"

let registered_observer_being_deallocated =
  register_from_string "REGISTERED_OBSERVER_BEING_DEALLOCATED"


let resource_leak = register_from_string "RESOURCE_LEAK"

let retain_cycle = register_from_string ~enabled:true "RETAIN_CYCLE"

let return_expression_required = register_from_string "RETURN_EXPRESSION_REQUIRED"

let return_statement_missing = register_from_string "RETURN_STATEMENT_MISSING"

let return_value_ignored = register_from_string ~enabled:false "RETURN_VALUE_IGNORED"

let skip_function = register_from_string "SKIP_FUNCTION"

let skip_pointer_dereference = register_from_string "SKIP_POINTER_DEREFERENCE"

let shell_injection = register_from_string "SHELL_INJECTION"

let shell_injection_risk = register_from_string "SHELL_INJECTION_RISK"

let sql_injection = register_from_string "SQL_INJECTION"

let sql_injection_risk = register_from_string "SQL_INJECTION_RISK"

let stack_variable_address_escape =
  register_from_string ~enabled:false "STACK_VARIABLE_ADDRESS_ESCAPE"


let starvation = register_from_string "STARVATION" ~hum:"UI Thread Starvation"

let static_initialization_order_fiasco = register_from_string "STATIC_INITIALIZATION_ORDER_FIASCO"

let strict_mode_violation =
  register_from_string "STRICT_MODE_VIOLATION" ~hum:"Strict Mode Violation"


let strong_self_not_checked =
  register_from_string "STRONG_SELF_NOT_CHECKED" ~hum:"StrongSelf Not Checked"


let symexec_memory_error =
  register_from_string "Symexec_memory_error" ~hum:"Symbolic Execution Memory Error"


let tainted_buffer_access = register_from_string "TAINTED_BUFFER_ACCESS"

let tainted_memory_allocation = register_from_string "TAINTED_MEMORY_ALLOCATION"

let thread_safety_violation = register_from_string "THREAD_SAFETY_VIOLATION"

let complexity_increase ~kind ~is_on_cold_start ~is_on_ui_thread =
  register_from_cost_string ~kind ~is_on_cold_start ~is_on_ui_thread "%s_COMPLEXITY_INCREASE"


let topl_error = register_from_string "TOPL_ERROR"

let unary_minus_applied_to_unsigned_expression =
  register_from_string ~enabled:false "UNARY_MINUS_APPLIED_TO_UNSIGNED_EXPRESSION"


let uninitialized_value = register_from_string "UNINITIALIZED_VALUE"

let unknown_proc = register_from_string "Unknown_proc" ~hum:"Unknown Procedure"

let unreachable_code_after = register_from_string "UNREACHABLE_CODE"

let unsafe_guarded_by_access = register_from_string "UNSAFE_GUARDED_BY_ACCESS"

let use_after_delete = register_from_string "USE_AFTER_DELETE"

let use_after_free = register_from_string "USE_AFTER_FREE"

let biabd_use_after_free = register_from_string "BIABD_USE_AFTER_FREE"

let use_after_lifetime = register_from_string "USE_AFTER_LIFETIME"

let user_controlled_sql_risk = register_from_string "USER_CONTROLLED_SQL_RISK"

let untrusted_buffer_access = register_from_string ~enabled:false "UNTRUSTED_BUFFER_ACCESS"

let untrusted_deserialization = register_from_string "UNTRUSTED_DESERIALIZATION"

let untrusted_deserialization_risk = register_from_string "UNTRUSTED_DESERIALIZATION_RISK"

let untrusted_environment_change_risk = register_from_string "UNTRUSTED_ENVIRONMENT_CHANGE_RISK"

let untrusted_file = register_from_string "UNTRUSTED_FILE"

let untrusted_file_risk = register_from_string "UNTRUSTED_FILE_RISK"

let untrusted_heap_allocation = register_from_string ~enabled:false "UNTRUSTED_HEAP_ALLOCATION"

let untrusted_intent_creation = register_from_string "UNTRUSTED_INTENT_CREATION"

let untrusted_url_risk = register_from_string "UNTRUSTED_URL_RISK"

let untrusted_variable_length_array = register_from_string "UNTRUSTED_VARIABLE_LENGTH_ARRAY"

let vector_invalidation = register_from_string "VECTOR_INVALIDATION"

let wrong_argument_number =
  register_from_string "Wrong_argument_number" ~hum:"Wrong Argument Number"


let zero_cost_call ~kind = register_from_cost_string ~enabled:false ~kind "ZERO_%s"

(* register enabled cost issues *)
let () =
  List.iter CostKind.enabled_cost_kinds ~f:(fun CostKind.{kind} ->
      List.iter [true; false] ~f:(fun is_on_cold_start ->
          List.iter [true; false] ~f:(fun is_on_ui_thread ->
              let _ = zero_cost_call ~kind in
              let _ = expensive_cost_call ~kind ~is_on_cold_start ~is_on_ui_thread in
              let _ = infinite_cost_call ~kind in
              let _ = complexity_increase ~kind ~is_on_cold_start ~is_on_ui_thread in
              () ) ) )
