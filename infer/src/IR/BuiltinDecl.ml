(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type t = Procname.t

let builtin_decls = ref Procname.Set.empty

let register pname = builtin_decls := Procname.Set.add pname !builtin_decls

let create_procname name =
  let pname = Procname.from_string_c_fun name in
  register pname ;
  pname


let create_objc_class_method class_name method_name parameters =
  let method_kind = Procname.ObjC_Cpp.ObjCClassMethod in
  let tname = Typ.Name.Objc.from_string class_name in
  let pname =
    Procname.ObjC_Cpp
      (Procname.ObjC_Cpp.make tname method_name method_kind Typ.NoTemplate parameters)
  in
  register pname ;
  pname


let is_declared pname = Procname.Set.mem pname !builtin_decls

let __assert_fail = create_procname "__assert_fail"

let __atomic_fetch_max = create_procname "__atomic_fetch_max"

let __atomic_fetch_min = create_procname "__atomic_fetch_min"

let __atomic_fetch_nand = create_procname "__atomic_fetch_nand"

let __atomic_max_fetch = create_procname "__atomic_max_fetch"

let __atomic_min_fetch = create_procname "__atomic_min_fetch"

let __atomic_nand_fetch = create_procname "__atomic_nand_fetch"

let __c11_atomic_fetch_max = create_procname "__c11_atomic_fetch_max"

let __c11_atomic_fetch_min = create_procname "__c11_atomic_fetch_min"

let __opencl_atomic_fetch_max = create_procname "__opencl_atomic_fetch_max"

let __opencl_atomic_fetch_min = create_procname "__opencl_atomic_fetch_min"

let __builtin_add_overflow = create_procname "__builtin_add_overflow"

let __builtin_mul_overflow = create_procname "__builtin_mul_overflow"

let __builtin_sub_overflow = create_procname "__builtin_sub_overflow"

let __builtin_va_arg = create_procname "__builtin_va_arg"

let __builtin_va_copy = create_procname "__builtin_va_copy"

let __builtin_va_end = create_procname "__builtin_va_end"

let __builtin_va_start = create_procname "__builtin_va_start"

let __builtin_offsetof = create_procname "__builtin_offsetof"

let __cast = create_procname "__cast"

let __call_objc_block = create_procname "__call_objc_block"

let __call_c_function_ptr = create_procname "__call_c_function_ptr"

let __cxx_typeid = create_procname "__cxx_typeid"

let __delete = create_procname "__delete"

let __delete_array = create_procname "__delete_array"

let __delete_locked_attribute = create_procname "__delete_locked_attribute"

let __erlang_equal = create_procname "__erlang_equal"

let __erlang_exactly_equal = create_procname "__erlang_exactly_equal"

let __erlang_not_equal = create_procname "__erlang_not_equal"

let __erlang_exactly_not_equal = create_procname "__erlang_exactly_not_equal"

let __erlang_lesser = create_procname "__erlang_lesser"

let __erlang_lesser_or_equal = create_procname "__erlang_lesser_or_equal"

let __erlang_greater = create_procname "__erlang_greater"

let __erlang_greater_or_equal = create_procname "__erlang_greater_or_equal"

let __erlang_error_badgenerator = create_procname "__erlang_error_badgenerator"

let __erlang_error_badkey = create_procname "__erlang_error_badkey"

let __erlang_error_badmatch = create_procname "__erlang_error_badmatch"

let __erlang_error_badmap = create_procname "__erlang_error_badmap"

let __erlang_error_badrecord = create_procname "__erlang_error_badrecord"

let __erlang_error_badreturn = create_procname "__erlang_error_badreturn"

let __erlang_error_case_clause = create_procname "__erlang_error_case_clause"

let __erlang_error_else_clause = create_procname "__erlang_error_else_clause"

let __erlang_error_function_clause = create_procname "__erlang_error_function_clause"

let __erlang_error_if_clause = create_procname "__erlang_error_if_clause"

let __erlang_error_try_clause = create_procname "__erlang_error_try_clause"

let __erlang_make_cons = create_procname "__erlang_make_cons"

let __erlang_make_map = create_procname "__erlang_make_map"

let __erlang_make_nil = create_procname "__erlang_make_nil"

let __erlang_make_atom = create_procname "__erlang_make_atom"

let __erlang_make_integer = create_procname "__erlang_make_integer"

let __erlang_make_tuple = create_procname "__erlang_make_tuple"

let __erlang_make_str_const = create_procname "__erlang_make_str_const"

let __erlang_make_bitstring = create_procname "__erlang_make_bitstring"

let __erlang_map_to_list = create_procname "__erlang_map_to_list"

let __erlang_receive = create_procname "__erlang_receive"

let __erlang_str_equal = create_procname "__erlang_str_equal"

let __exit = create_procname "_exit"

let __objc_bridge_transfer = create_procname "__objc_bridge_transfer"

let __get_array_length = create_procname "__get_array_length"

let __get_hidden_field = create_procname "__get_hidden_field"

let __get_type_of = create_procname "__get_type_of"

let __infer_assume = create_procname "__infer_assume"

let __infer_fail = create_procname "__infer_fail"

let __infer_generic_selection_expr = Procname.from_string_c_fun "__infer_generic_selection_expr"

let __infer_initializer_list = create_procname "__infer_initializer_list"

let __infer_skip = create_procname "__infer_skip"

let __infer_skip_function = Procname.from_string_c_fun "__infer_skip_function"

let __infer_skip_gcc_asm_stmt = Procname.from_string_c_fun "__infer_skip_gcc_asm_stmt"

let __infer_structured_binding = create_procname "__infer_structured_binding"

let __instanceof = create_procname "__instanceof"

let __java_throw = create_procname "__java_throw"

let __hack_throw = create_procname "__hack_throw"

let __get_lazy_class = create_procname "__get_lazy_class"

let __lazy_class_initialize = create_procname "__lazy_class_initialize"

let __method_set_ignore_attribute = create_procname "__method_set_ignore_attribute"

let __new = create_procname "__new"

let __new_array = create_procname "__new_array"

let __objc_alloc_no_fail = create_procname "__objc_alloc_no_fail"

let __objc_cast = create_procname "__objc_cast"

let __objc_dictionary_literal =
  create_objc_class_method "NSDictionary" "dictionaryWithObjects:forKeys:count:" [None; None; None]


let __objc_get_ref_count = create_procname "__objc_get_ref_count"

let __objc_set_ref_count = create_procname "__objc_set_ref_count"

let __placement_delete = create_procname "__placement_delete"

let __placement_new = create_procname "__placement_new"

let __print_value = create_procname "__print_value"

let __require_allocated_array = create_procname "__require_allocated_array"

let __set_array_length = create_procname "__set_array_length"

let __set_autorelease_attribute = create_procname "__set_autorelease_attribute"

let __set_file_attribute = create_procname "__set_file_attribute"

let __set_hidden_field = create_procname "__set_hidden_field"

let __set_locked_attribute = create_procname "__set_locked_attribute"

let __set_mem_attribute = create_procname "__set_mem_attribute"

let __set_observer_attribute = create_procname "__set_observer_attribute"

let __set_unsubscribed_observer_attribute = create_procname "__set_unsubscribed_observer_attribute"

let __set_wont_leak_attribute = create_procname "__set_wont_leak_attribute"

let __split_get_nth = create_procname "__split_get_nth"

let __throw = create_procname "__throw"

let __unwrap_exception = create_procname "__unwrap_exception"

let __builtin_cxx_co_return = create_procname "__builtin_cxx_co_return"

let __builtin_cxx_co_await = create_procname "__builtin_cxx_co_await"

let abort = create_procname "abort"

let dispatch_sync = create_procname "dispatch_sync"

let exit = create_procname "exit"

let free = create_procname "free"

let fscanf = create_procname "fscanf"

let fwscanf = create_procname "fwscanf"

let malloc = create_procname "malloc"

let malloc_no_fail = create_procname "malloc_no_fail"

let nsArray_arrayWithObjects =
  let objc_object = Typ.Name.C.from_string "objc_object" in
  create_objc_class_method "NSArray" "arrayWithObjects:" [Some objc_object]


let nsArray_arrayWithObjectsCount =
  create_objc_class_method "NSArray" "arrayWithObjects:count:" [None; None]


let objc_insert_key = create_procname "_objc_insertKey"

let objc_insert_value = create_procname "_objc_insertValue"

let objc_autorelease_pool_pop = create_procname "_objc_autoreleasePoolPop"

let objc_autorelease_pool_push = create_procname "_objc_autoreleasePoolPush"

let objc_cpp_throw = create_procname "__infer_objc_cpp_throw"

let pthread_create = create_procname "pthread_create"

let scanf = create_procname "scanf"

let sscanf = create_procname "sscanf"

let swscanf = create_procname "swscanf"

let vfscanf = create_procname "vfscanf"

let vfwscanf = create_procname "vfwscanf"

let vscanf = create_procname "vscanf"

let vsscanf = create_procname "vsscanf"

let vswscanf = create_procname "vswscanf"

let vwscanf = create_procname "vwscanf"

let wscanf = create_procname "wscanf"

let zero_initialization = create_procname "__infer_zero_initialization"

let match_builtin builtin _ s = String.equal s (Procname.get_method builtin)
