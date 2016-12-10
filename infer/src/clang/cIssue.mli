(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

type issue =
  | Assign_pointer_warning
  | Bad_pointer_comparison
  | Component_factory_function
  | Component_file_cyclomatic_complexity
  | Component_file_line_count
  | Component_initializer_with_side_effects
  | Component_with_multiple_factory_methods
  | Component_with_unconventional_superclass
  | Cxx_reference_captured_in_objc_block
  | Direct_atomic_property_access
  | Global_variable_initialized_with_function_or_method_call
  | Mutable_local_variable_in_component_file
  | Registered_observer_being_deallocated
  | Strong_delegate_warning

val to_string : issue -> string

val severity_of_issue : issue -> Exceptions.err_kind

type issue_desc = {
  issue : issue; (* issue *)
  description : string; (* Description in the error message *)
  suggestion : string option; (* an optional suggestion or correction *)
  loc : Location.t; (* location in the code *)
}
