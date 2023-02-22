(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val is_modelled_static_function : string -> bool

val is_builtin_expect : Procname.t -> bool

val is_builtin_object_size : Procname.t -> bool

val is_std_addressof : Procname.t -> bool

val is_replace_with_deref_first_arg : Procname.t -> bool

val is_assert_log : Procname.t -> bool

val is_handleFailureInMethod : string -> bool

val is_modeled_builtin : string -> bool

val is_modeled_attribute : string -> bool

val get_predefined_model_method_signature :
     Typ.Name.t
  -> string
  -> (   Typ.Name.t
      -> string
      -> Procname.ObjC_Cpp.kind
      -> Procname.Parameter.clang_parameter list
      -> Procname.t )
  -> CMethodSignature.t option
