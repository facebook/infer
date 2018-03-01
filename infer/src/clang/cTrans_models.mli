(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

val is_modelled_static_function : string -> bool

val is_builtin_expect : Typ.Procname.t -> bool

val is_builtin_object_size : Typ.Procname.t -> bool

val is_std_addressof : Typ.Procname.t -> bool

val is_replace_with_deref_first_arg : Typ.Procname.t -> bool

val is_assert_log : Typ.Procname.t -> bool

val is_handleFailureInMethod : string -> bool

val is_modeled_builtin : string -> bool

val is_modeled_attribute : string -> bool

val get_predefined_model_method_signature :
  Typ.Name.t -> string -> (Typ.Name.t -> string -> Typ.Procname.ObjC_Cpp.kind -> Typ.Procname.t)
  -> CFrontend_config.clang_lang -> CMethodSignature.t option
