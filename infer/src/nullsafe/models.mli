(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Methods dealing with specific knowledge about code in important third libraries, standard
    libraries, etc *)

val get_modelled_annotated_signature :
  is_callee_in_trust_list:bool -> Tenv.t -> ProcAttributes.t -> AnnotatedSignature.t
(** Return the annotated signature of the procedure, taking into account models. External models
    take precedence over internal ones. *)

val is_check_not_null : Procname.Java.t -> bool
(** Check if the procedure is one of the known methods asserting nullability of the object. Nullsafe
    should understand that both the argument and return value are non-nullable after the call. *)

val get_check_not_null_parameter : Procname.Java.t -> int option
(** Parameter number (starting from 1) for a procedure known to produce a non-nullable assertion.
    [None] if the function is not known to be an aseertion OR the parameter number is not known *)

val is_check_state : Procname.Java.t -> bool
(** Check if the procedure is one of the known Preconditions.checkState. *)

val is_check_argument : Procname.Java.t -> bool
(** Check if the procedure is one of the known Preconditions.checkArgument. *)

val is_noreturn : Procname.Java.t -> bool
(** Check if the procedure does not return. *)

val is_true_on_null : Procname.Java.t -> bool
(** Check if the procedure returns true on null. *)

val is_false_on_null : Procname.Java.t -> bool
(** Check if the procedure returns false on null. *)

val is_containsKey : Procname.Java.t -> bool
(** Check if the procedure is Map.containsKey(). *)

val is_mapPut : Procname.Java.t -> bool
(** Check if the procedure is Map.put(). *)

val find_nonnullable_alternative : Procname.Java.t -> ModelTables.nonnull_alternative_method option
(** Check if a (nullable) method has a non-nullable alternative: A method that does the same as
    [proc_name] but asserts the result is not null before returning to the caller. *)

val is_field_nonnullable : Fieldname.t -> bool
(** Check if a given field is known to be a non-nullable *)
