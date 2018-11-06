(*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type t = Typ.Procname.t

let builtin_decls = ref Typ.Procname.Set.empty

let register pname = builtin_decls := Typ.Procname.Set.add pname !builtin_decls

let create_procname name =
  let pname = Typ.Procname.from_string_c_fun name in
  register pname ; pname


let create_objc_class_method class_name method_name parameters =
  let method_kind = Typ.Procname.ObjC_Cpp.ObjCClassMethod in
  let tname = Typ.Name.Objc.from_string class_name in
  let pname =
    Typ.Procname.ObjC_Cpp
      (Typ.Procname.ObjC_Cpp.make tname method_name method_kind Typ.NoTemplate parameters)
  in
  register pname ; pname


let is_declared pname = Typ.Procname.Set.mem pname !builtin_decls

let __array_access = create_procname "__array_access"

let __assert_fail = create_procname "__assert_fail"

let __builtin_va_arg = create_procname "__builtin_va_arg"

let __builtin_va_copy = create_procname "__builtin_va_copy"

let __builtin_va_end = create_procname "__builtin_va_end"

let __builtin_va_start = create_procname "__builtin_va_start"

let __cast = create_procname "__cast"

let __cxx_typeid = create_procname "__cxx_typeid"

let __delete = create_procname "__delete"

let __delete_array = create_procname "__delete_array"

let __delete_locked_attribute = create_procname "__delete_locked_attribute"

let __exit = create_procname "_exit"

let __free_cf = create_procname "__free_cf"

let __get_array_length = create_procname "__get_array_length"

let __get_hidden_field = create_procname "__get_hidden_field"

let __get_type_of = create_procname "__get_type_of"

let __global_access = create_procname "__global_access"

let __infer_assume = create_procname "__infer_assume"

let __infer_fail = create_procname "__infer_fail"

let __infer_skip = create_procname "__infer_skip"

let __instanceof = create_procname "__instanceof"

let __method_set_ignore_attribute = create_procname "__method_set_ignore_attribute"

let __new = create_procname "__new"

let __new_array = create_procname "__new_array"

let __objc_alloc = create_procname "__objc_alloc"

let __objc_alloc_no_fail = create_procname "__objc_alloc_no_fail"

let __objc_cast = create_procname "__objc_cast"

let __objc_dictionary_literal =
  create_objc_class_method "NSDictionary" "dictionaryWithObjects:forKeys:count:" [None; None; None]


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

let __variable_initialization = create_procname "__variable_initialization"

let abort = create_procname "abort"

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
