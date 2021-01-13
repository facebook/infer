(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val boolean_code : string

val byte_code : string

val char_code : string

val double_code : string

val float_code : string

val int_code : string

val long_code : string

val short_code : string

val class_code : string -> string

val boolean_st : string

val byte_st : string

val char_st : string

val double_st : string

val float_st : string

val int_st : string

val long_st : string

val short_st : string

val constructor_name : string

val void : string

val this : Mangled.t

val clone_name : string

val field_cst : string

val field_st : Mangled.t

val infer_builtins_cl : string

val is_synthetic_name : string -> bool
