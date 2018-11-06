(*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** List of all builtins that are interpreted specially by the backend *)
module type S = sig
  type t

  val __array_access : t

  val __assert_fail : t

  val __builtin_va_arg : t

  val __builtin_va_copy : t

  val __builtin_va_end : t

  val __builtin_va_start : t

  val __cast : t
  (** [__cast(val,typ)] implements java's [typ(val)] *)

  val __cxx_typeid : t

  val __delete : t

  val __delete_array : t

  val __delete_locked_attribute : t

  val __exit : t

  val __free_cf : t

  val __get_array_length : t

  val __get_type_of : t

  val __global_access : t

  val __infer_assume : t

  val __infer_fail : t

  val __infer_skip : t
  (** used to represent behavior that is not modeled in infer *)

  val __instanceof : t
  (** [__instanceof(val,typ)] implements java's [val instanceof typ] *)

  val __method_set_ignore_attribute : t

  val __new : t

  val __new_array : t

  val __objc_alloc_no_fail : t

  val __objc_dictionary_literal : t

  val __placement_delete : t

  val __placement_new : t

  val __print_value : t

  val __require_allocated_array : t

  val __set_array_length : t

  val __set_file_attribute : t

  val __set_locked_attribute : t

  val __set_mem_attribute : t

  val __set_observer_attribute : t

  val __set_unsubscribed_observer_attribute : t

  val __set_wont_leak_attribute : t

  val __split_get_nth : t

  val __throw : t

  val __unwrap_exception : t

  val __variable_initialization : t
  (** produced by the clang frontend to denote that a variable is being initialized *)

  val abort : t

  val exit : t

  val free : t

  val fscanf : t

  val fwscanf : t

  val malloc : t

  val malloc_no_fail : t

  val nsArray_arrayWithObjects : t

  val nsArray_arrayWithObjectsCount : t

  val objc_cpp_throw : t

  val pthread_create : t

  val scanf : t

  val sscanf : t

  val swscanf : t

  val vfscanf : t

  val vfwscanf : t

  val vscanf : t

  val vsscanf : t

  val vswscanf : t

  val vwscanf : t

  val wscanf : t
end
