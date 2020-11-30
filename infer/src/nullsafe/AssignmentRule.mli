(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

(** Assignment rule should be checked when a value is assigned to a location. Assignment can be
    explicit (lhs = rhs) or implicit (e.g. returning from a function). This rule checks if null can
    be passed to a place that does not expect null. *)

type violation [@@deriving compare]

val check : lhs:AnnotatedNullability.t -> rhs:InferredNullability.t -> (unit, violation) result
(** If `null` can leak from a "less strict" type to "more strict" type, this is an Assignment Rule
    violation. *)

(** Violation that will occur if the provisional annotation becomes real [@Nullable] *)
module ProvisionalViolation : sig
  type t

  val offending_annotations : t -> ProvisionalAnnotation.t list
  (** Non-empty list of corresponding provisional annotations (adding any of those will lead to an
      issue) *)

  val fix_annotation : t -> ProvisionalAnnotation.t option
  (** If there is a place such as adding [@Nullable] will fix the issue, this is the one. *)

  val from : violation -> t option
  (** If the violation is provisional (so is not real but will become real when the annotation is
      added), create it. *)
end

(** Violation that needs to be reported to the user. *)
module ReportableViolation : sig
  type t

  val from : NullsafeMode.t -> violation -> t option
  (** Depending on the mode, violation might or might not be important enough to be reported to the
      user. If it should NOT be reported for that mode, this function will return None. *)

  type assignment_type =
    | PassingParamToFunction of function_info
    | AssigningToField of Fieldname.t
    | ReturningFromFunction of Procname.Java.t
  [@@deriving compare]

  and function_info =
    { param_signature: AnnotatedSignature.param_signature
    ; actual_param_expression: string
    ; param_index: int
    ; annotated_signature: AnnotatedSignature.t
    ; procname: Procname.Java.t }

  val make_nullsafe_issue :
    assignment_location:Location.t -> assignment_type -> t -> NullsafeIssue.t
  (** Given context around violation, return error message together with the info where to put this
      message *)
end
