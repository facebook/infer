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

(** Used to describe a method complementary to a given one. Contains information needed for
    reporting (hence does not describe the whole signature). *)
type nonnull_alternative_method = {package_name: string; class_name: string; method_name: string}

val nonnull_alternatives_table : (string, nonnull_alternative_method) Caml.Hashtbl.t
(** The key is a string representation of a [@Nullable] method. The value is the description of
    non-nullable alternative: a method does the same, but never returns null (does a null check
    inside). *)

val field_nullability_table : (string, bool) Caml.Hashtbl.t
(** Table of known fields whos nullability is explicitly modelled. Keys are field full names; value
    [true] means nullable *)
