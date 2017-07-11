(*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd
open Javalib_pack

(** {2 Class names and types} *)

let builtins_package = "com.facebook.infer.builtins"

let infer_builtins_cl = builtins_package ^ ".InferBuiltins"

let infer_array_cl = builtins_package ^ ".InferArray"

let infer_undefined_cl = builtins_package ^ ".InferUndefined"

let obj_type = JBasics.TObject (JBasics.TClass JBasics.java_lang_object)

let bool_type = JBasics.TBasic `Bool

let string_cl = "java.lang.String"

let class_cl = "java.lang.Class"

let npe_cl = "java.lang.NullPointerException"

let cce_cl = "java.lang.ClassCastException"

let out_of_bound_cl = "java.lang.ArrayIndexOutOfBoundsException"

let reentrant_lock_cl = "java.util.concurrent.locks.ReentrantLock"

let lock_cl = "java.util.concurrent.locks.Lock"

let reentrant_rwlock_cl = "java.util.concurrent.locks.ReentrantReadWriteLock"

let reentrant_rlock_cl = reentrant_rwlock_cl ^ "$ReadLock"

let reentrant_wlock_cl = reentrant_rwlock_cl ^ "$WriteLock"

let thread_class = "java.lang.Thread"

let runnable_if = "java.lang.Runnable"

let lock_method = "lock"

let unlock_method = "unlock"

let try_lock_method = "tryLock"

let start_method = "start"

let run_method = "run"

(** {2 Names of special variables, constants and method names} *)

let this = Mangled.from_string "this"

let constructor_name = "<init>"

let clone_name = "clone"

let field_st = Mangled.from_string "field"

let field_cst = "<field>"

(** {2 Names of primitive types} *)

let void = "void"

let boolean_st = "boolean"

let byte_st = "byte"

let char_st = "char"

let double_st = "double"

let float_st = "float"

let int_st = "int"

let long_st = "long"

let short_st = "short"

(** {2 Encoding of primitive types when they are the element type of arrays } *)

let boolean_code = "Z"

let byte_code = "B"

let char_code = "C"

let double_code = "D"

let float_code = "F"

let int_code = "I"

let long_code = "J"

let short_code = "S"

let class_code cl = "L" ^ cl

let errors_db_file = "errors.db"

let main_errors_file = "Java_frontend.errors"

(** {2 Flags } *)

(* the Sawja representation of the Java Bytecode will be printed *)
let html_mode = ref false

(* The dynamic dispatch will be handled partially statically *)
let static_dispatch = ref false

(* counts the amount of initializer methods, in init-mode *)
let init_count = ref 0

let normalize_string s =
  let rexp = Str.regexp_string "$" in
  Str.global_replace rexp "_" s
