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

type type_role = Param | Ret

val check : type_role -> base:Nullability.t -> overridden:Nullability.t -> (unit, violation) result
(** See description of the rule in the header of the file. Note that formal fact of violation might
    or might not be reported to the user, depending on the mode. See [to_reportable_violation] *)

(** Violation that needs to be reported to the user. *)
module ReportableViolation : sig
  type t

  type violation_type =
    | InconsistentParam of {param_description: string; param_index: int}
    | InconsistentReturn
  [@@deriving compare]

  val from : NullsafeMode.t -> violation -> t option
  (** Depending on the mode, violation might or might not be important enough to be reported to the
      user. If it should NOT be reported for that mode, this function will return None. *)

  val make_nullsafe_issue :
       t
    -> violation_type
    -> nullsafe_mode:NullsafeMode.t
    -> loc:Location.t
    -> base_proc_name:Procname.Java.t
    -> overridden_proc_name:Procname.Java.t
    -> NullsafeIssue.t
  (** Given context around violation, return error message together with the info where to put this
      message *)
end
