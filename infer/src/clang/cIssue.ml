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
  Localise.to_string
    (match issue with
     | Assign_pointer_warning ->
         Localise.assign_pointer_warning
     | Bad_pointer_comparison ->
         Localise.bad_pointer_comparison
     | Cxx_reference_captured_in_objc_block ->
         Localise.cxx_reference_captured_in_objc_block
     | Direct_atomic_property_access ->
         Localise.direct_atomic_property_access
     | Global_variable_initialized_with_function_or_method_call ->
         Localise.global_variable_initialized_with_function_or_method_call
     | Mutable_local_variable_in_component_file ->
         Localise.mutable_local_variable_in_component_file
     | Registered_observer_being_deallocated ->
         Localise.registered_observer_being_deallocated
     | Strong_delegate_warning ->
         Localise.strong_delegate_warning
    )

let severity_of_issue issue =
  match issue with
  | Assign_pointer_warning
  | Bad_pointer_comparison
  | Cxx_reference_captured_in_objc_block
  | Direct_atomic_property_access
  | Global_variable_initialized_with_function_or_method_call
  | Registered_observer_being_deallocated
  | Strong_delegate_warning -> Exceptions.Kwarning
  | Mutable_local_variable_in_component_file -> Exceptions.Kadvice


type issue_desc = {
  issue : issue; (* issue *)
  description : string; (* Description in the error message *)
  suggestion : string option; (* an optional suggestion or correction *)
  loc : Location.t; (* location in the code *)
}
