(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type model_table_t = (string, bool * bool list) Caml.Hashtbl.t

(* The key is a unique string representation of a method.
   The value is nullability of its return value and params correspondingly.
   true corresponds to Nullable.
 *)

val annotated_table_nullability : model_table_t

val check_not_null_table : model_table_t
(** List of methods known to perform a non-nullable assertion *)

val check_not_null_parameter_table : (string, int) Caml.Hashtbl.t
(** The key is a string representation of a method known to perform a non-nullable assertion. The
    value is an index (starting from 1) of an argument which nullability is being asserted. *)

val check_state_table : model_table_t

val check_argument_table : model_table_t

val containsKey_table : model_table_t

val mapPut_table : model_table_t

val noreturn_table : model_table_t

val true_on_null_table : model_table_t
