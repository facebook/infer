(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open Javalib_pack

(** {2 Class names and types} *)

let builtins_package = "com.facebook.infer.builtins"

let infer_builtins_cl = builtins_package ^ ".InferBuiltins"

let obj_type = JBasics.TObject (JBasics.TClass JBasics.java_lang_object)

let string_cl = "java.lang.String"

let class_cl = "java.lang.Class"

let npe_cl = "java.lang.NullPointerException"

let cce_cl = "java.lang.ClassCastException"

let out_of_bound_cl = "java.lang.ArrayIndexOutOfBoundsException"

(** {2 Names of special variables, constants and method names} *)

let this = Mangled.this

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
