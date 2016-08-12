(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

type issue =
  | Assign_pointer_warning
  | Bad_pointer_comparison
  | Cxx_reference_captured_in_objc_block
  | Direct_atomic_property_access
  | Global_variable_initialized_with_function_or_method_call
  | Mutable_local_variable_in_component_file
  | Registered_observer_being_deallocated
  | Strong_delegate_warning

let to_string issue =
  match issue with
  | Assign_pointer_warning -> "ASSIGN_POINTER_WARNING"
  | Bad_pointer_comparison -> "BAD_POINTER_COMPARISON"
  | Cxx_reference_captured_in_objc_block ->
      "CXX_REFERENCE_CAPTURED_IN_OBJC_BLOCK"
  | Direct_atomic_property_access -> "DIRECT_ATOMIC_PROPERTY_ACCESS"
  | Global_variable_initialized_with_function_or_method_call ->
      "GLOBAL_VARIABLE_INITIALIZED_WITH_FUNCTION_OR_METHOD_CALL"
  | Mutable_local_variable_in_component_file -> "MUTABLE_LOCAL_VARIABLE_IN_COMPONENT_FILE"
  | Registered_observer_being_deallocated ->
      Localise.to_string (Localise.registered_observer_being_deallocated)
  | Strong_delegate_warning -> "STRONG_DELEGATE_WARNING"

let severity_of_issue issue =
  match issue with
  | Assign_pointer_warning
  | Bad_pointer_comparison
  | Cxx_reference_captured_in_objc_block
  | Direct_atomic_property_access
  | Global_variable_initialized_with_function_or_method_call
  | Mutable_local_variable_in_component_file -> Exceptions.Kadvice
  | Registered_observer_being_deallocated
  | Strong_delegate_warning -> Exceptions.Kwarning


type issue_desc = {
  issue : issue; (* issue *)
  description : string; (* Description in the error message *)
  suggestion : string option; (* an optional suggestion or correction *)
  loc : Location.t; (* location in the code *)
}
