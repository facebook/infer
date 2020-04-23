(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Procnames for the builtin functions supported *)

include BUILTINS.S with type t = Procname.t

val is_declared : Procname.t -> bool

val __infer_initializer_list : Procname.t

val __infer_skip_function : Procname.t

val __infer_skip_gcc_asm_stmt : Procname.t

val __infer_generic_selection_expr : Procname.t
