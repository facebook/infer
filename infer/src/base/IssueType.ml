(*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)
open! IStd

(* Make sure we cannot create new issue types other than by calling [from_string]. This is because
     we want to keep track of the list of all the issues ever declared. *)
module Unsafe : sig
  type t = private
    {unique_id: string; mutable enabled: bool; mutable hum: string}
    [@@deriving compare]

  val equal : t -> t -> bool

  val from_string : ?enabled:bool -> ?hum:string -> string -> t

  val all_issues : unit -> t list

  val set_enabled : t -> bool -> unit
end = struct
  module T = struct
    type t = {unique_id: string; mutable enabled: bool; mutable hum: string}

    let compare {unique_id= id1} {unique_id= id2} = String.compare id1 id2

    let equal = [%compare.equal : t]
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
  let from_string ?(enabled= true) ?hum:hum0 unique_id =
    let hum = match hum0 with Some str -> str | _ -> prettify unique_id in
    let issue = {unique_id; enabled; hum} in
    try
      let old = IssueSet.find issue !all_issues in
      (* update human-readable string in case it was supplied this time, but keep the previous
           value of enabled (see doc comment) *)
      if Option.is_some hum0 then old.hum <- hum ;
      old
    with Not_found ->
      all_issues := IssueSet.add issue !all_issues ;
      issue

  let all_issues () = IssueSet.elements !all_issues
end

include Unsafe

(** pretty print a localised string *)
let pp fmt t = Format.fprintf fmt "%s" t.unique_id

let abduction_case_not_implemented = from_string "Abduction_case_not_implemented"

let analysis_stops = from_string ~enabled:false "ANALYSIS_STOPS"

let array_of_pointsto = from_string "Array_of_pointsto"

let array_out_of_bounds_l1 = from_string ~enabled:false "ARRAY_OUT_OF_BOUNDS_L1"

let array_out_of_bounds_l2 = from_string ~enabled:false "ARRAY_OUT_OF_BOUNDS_L2"

let array_out_of_bounds_l3 = from_string ~enabled:false "ARRAY_OUT_OF_BOUNDS_L3"

let assert_failure = from_string "Assert_failure"

let bad_footprint = from_string "Bad_footprint"

let buffer_overrun = from_string "BUFFER_OVERRUN"

let cannot_star = from_string "Cannot_star"

let checkers_access_global = from_string "CHECKERS_ACCESS_GLOBAL"

let checkers_immutable_cast = from_string "CHECKERS_IMMUTABLE_CAST"

let checkers_print_c_call = from_string "CHECKERS_PRINT_C_CALL"

let checkers_print_objc_method_calls = from_string "CHECKERS_PRINT_OBJC_METHOD_CALLS"

let checkers_printf_args = from_string "CHECKERS_PRINTF_ARGS"

let checkers_repeated_calls = from_string "CHECKERS_REPEATED_CALLS"

let checkers_trace_calls_sequence = from_string "CHECKERS_TRACE_CALLS_SEQUENCE"

let class_cast_exception = from_string ~enabled:false "CLASS_CAST_EXCEPTION"

let cluster_callback = from_string "CLUSTER_CALLBACK"

let codequery = from_string "Codequery"

let comparing_floats_for_equality = from_string "COMPARING_FLOAT_FOR_EQUALITY"

let condition_always_false = from_string ~enabled:false "CONDITION_ALWAYS_FALSE"

let condition_always_true = from_string ~enabled:false "CONDITION_ALWAYS_TRUE"

let context_leak = from_string "CONTEXT_LEAK"

let dangling_pointer_dereference = from_string ~enabled:false "DANGLING_POINTER_DEREFERENCE"

let dead_store = from_string "DEAD_STORE"

let deallocate_stack_variable = from_string "DEALLOCATE_STACK_VARIABLE"

let deallocate_static_memory = from_string "DEALLOCATE_STATIC_MEMORY"

let deallocation_mismatch = from_string "DEALLOCATION_MISMATCH"

let divide_by_zero = from_string ~enabled:false "DIVIDE_BY_ZERO"

let double_lock = from_string "DOUBLE_LOCK"

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

let failure_exe = from_string "Failure_exe"

let field_should_be_nullable = from_string "FIELD_SHOULD_BE_NULLABLE"

let field_not_null_checked = from_string "IVAR_NOT_NULL_CHECKED"

(* from AL default linters *)
let _global_variable_initialized_with_function_or_method_call =
  from_string ~enabled:false "GLOBAL_VARIABLE_INITIALIZED_WITH_FUNCTION_OR_METHOD_CALL"

let inherently_dangerous_function = from_string "INHERENTLY_DANGEROUS_FUNCTION"

let internal_error = from_string "Internal_error"

let leak_after_array_abstraction = from_string "Leak_after_array_abstraction"

let leak_in_footprint = from_string "Leak_in_footprint"

let memory_leak = from_string "MEMORY_LEAK"

let missing_fld = from_string "Missing_fld" ~hum:"Missing Field"

let null_dereference = from_string "NULL_DEREFERENCE"

let null_test_after_dereference = from_string ~enabled:false "NULL_TEST_AFTER_DEREFERENCE"

let parameter_not_null_checked = from_string "PARAMETER_NOT_NULL_CHECKED"

let pointer_size_mismatch = from_string "POINTER_SIZE_MISMATCH"

let precondition_not_found = from_string "PRECONDITION_NOT_FOUND"

let precondition_not_met = from_string "PRECONDITION_NOT_MET"

let premature_nil_termination = from_string "PREMATURE_NIL_TERMINATION_ARGUMENT"

let proc_callback = from_string "PROC_CALLBACK" ~hum:"Procedure Callback"

let quandary_taint_error = from_string "QUANDARY_TAINT_ERROR"

let registered_observer_being_deallocated = from_string "REGISTERED_OBSERVER_BEING_DEALLOCATED"

let resource_leak = from_string "RESOURCE_LEAK"

let retain_cycle = from_string ~enabled:false "RETAIN_CYCLE"

let return_expression_required = from_string "RETURN_EXPRESSION_REQUIRED"

let return_statement_missing = from_string "RETURN_STATEMENT_MISSING"

let return_value_ignored = from_string ~enabled:false "RETURN_VALUE_IGNORED"

let skip_function = from_string "SKIP_FUNCTION"

let skip_pointer_dereference = from_string "SKIP_POINTER_DEREFERENCE"

let stack_variable_address_escape = from_string ~enabled:false "STACK_VARIABLE_ADDRESS_ESCAPE"

let static_initialization_order_fiasco = from_string "STATIC_INITIALIZATION_ORDER_FIASCO"

let symexec_memory_error =
  from_string "Symexec_memory_error" ~hum:"Symbolic Execution Memory Error"

let thread_safety_violation = from_string "THREAD_SAFETY_VIOLATION"

let unary_minus_applied_to_unsigned_expression =
  from_string ~enabled:false "UNARY_MINUS_APPLIED_TO_UNSIGNED_EXPRESSION"

let uninitialized_value = from_string ~enabled:false "UNINITIALIZED_VALUE"

let unknown_proc = from_string "Unknown_proc" ~hum:"Unknown Procedure"

let unreachable_code_after = from_string "UNREACHABLE_CODE"

let unsafe_guarded_by_access = from_string "UNSAFE_GUARDED_BY_ACCESS"

let use_after_free = from_string "USE_AFTER_FREE"

let wrong_argument_number = from_string "Wrong_argument_number" ~hum:"Wrong Argument Number"
