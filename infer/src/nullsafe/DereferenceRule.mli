(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

(** Dereference rule should be checked every type an object is dereferenced. The rule checks if the
    reference is nullable. *)

type violation [@@deriving compare]

val check : InferredNullability.t -> (unit, violation) result
(** violation of Dereference rule reflects possibility of dereferencing of `null`. Note that this
    might or might not be severe enough to be reported to the user, depending on the mode
    agreements. *)

(** Violation that will occur if the provisional annotation becomes real [@Nullable] *)
module ProvisionalViolation : sig
  type t

  val offending_annotations : t -> ProvisionalAnnotation.t list
  (** Non-empty list of corresponding provisional annotations (adding any of those will lead to an
      issue) *)

  val from : violation -> t option
  (** If the violation is provisional (so is not real but will become real when the annotation is
      added), create it. *)
end

(** Violation that needs to be reported to the user. *)
module ReportableViolation : sig
  type t

  type dereference_type =
    | MethodCall of Procname.Java.t
    | AccessToField of Fieldname.t
    | AccessByIndex of {index_desc: string}
    | ArrayLengthAccess
  [@@deriving compare]

  val from : NullsafeMode.t -> violation -> t option
  (** Depending on the mode, violation might or might not be important enough to be reported to the
      user. If it should NOT be reported for that mode, this function will return None. *)

  val make_nullsafe_issue :
       t
    -> dereference_location:Location.t
    -> dereference_type
    -> nullable_object_descr:string option
    -> NullsafeIssue.t
  (** Given context around violation, return error message together with the info where to put this
      message *)
end
