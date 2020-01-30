(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

(** Inheritance rule:

    + Return type for an overridden method is covariant: overridden method is allowed to narrow down
      the return value to a subtype of the one from the base method; this means it is OK to make the
      return value non-null when it was nullable in the base)
    + Parameter type for an overridden method is contravariant. It is OK for a derived method to
      accept nullable in the params even if the base does not accept nullable.

    NOTE: Rule 1) is based on Java covariance rule for the return type. In contrast, rule 2) is
    nullsafe specific as Java does not support type contravariance for method params. *)

type violation [@@deriving compare]

type violation_type =
  | InconsistentParam of {param_description: string; param_position: int}
  | InconsistentReturn
[@@deriving compare]

type type_role = Param | Ret

val check :
     nullsafe_mode:NullsafeMode.t
  -> type_role
  -> base:Nullability.t
  -> overridden:Nullability.t
  -> (unit, violation) result

val violation_description :
     violation
  -> violation_type
  -> base_proc_name:Procname.t
  -> overridden_proc_name:Procname.t
  -> string

val violation_severity : violation -> Exceptions.severity
