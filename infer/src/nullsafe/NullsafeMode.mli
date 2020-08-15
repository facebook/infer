(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Represents a type-checking mode of nullsafe. *)

module Trust : sig
  [@@@warning "-32"]

  type trust_list

  val is_trust_none : trust_list -> bool

  type t = All | Only of trust_list [@@deriving compare, equal]

  val none : t

  val of_annot : Annot.t -> t option
  (** Returns [Trust.t] when provided annotation matches the format of [@TrustList], otherwise
      [None]. *)

  val pp : Format.formatter -> t -> unit
end

type t = Default | Local of Trust.t | Strict [@@deriving compare, equal]

val of_annot : Annot.t -> t option
  [@@warning "-32"]
(** Returns [t] when provided annotation matches the format of [@Nullsafe], otherwise [None]. *)

val of_class : Tenv.t -> JavaClassName.t -> t
(** Extracts mode information from class annotations *)

val of_procname : Tenv.t -> Procname.t -> t
(** Extracts mode information from a class where procname is defined. Should be called for Java
    procnames only; throws otherwise *)

val of_java_procname : Tenv.t -> Procname.Java.t -> t
(** Extracts mode information from a class where procname is defined. Should be called for Java
    procnames only; throws otherwise *)

val is_in_trust_list : t -> JavaClassName.t -> bool
(** Check whether [JavaClassName.t] is in explicit trust list specified in the mode *)

val is_stricter_than : stricter:t -> weaker:t -> bool
(** Check whether [stricter] is (strongly) stricter than [weaker] *)

val severity : t -> IssueType.severity
(** Provides a default choice of issue severity for a particular mode. Rule is: severity should be
    ERROR if and only if it is enforced. *)

val pp : Format.formatter -> t -> unit

type nested_class_annotation_problem =
  | RedundantNestedClassAnnotation
      (** Nested mode is explicitly annotated exactly like the outer one. *)
  | NestedModeIsWeaker of weak_type  (** Attempt to relax the mode imposed in the outer class. *)

and weak_type =
  | ExtraTrustClass of JavaClassName.t list  (** Nested class has this extra list of classes *)
  | Other

val check_problematic_class_annotation :
  Tenv.t -> JavaClassName.t -> (unit, nested_class_annotation_problem) result
(** Given a (not anonymous) class name, check if there are semantic problems with [@Nullsafe] mode
    annotation for this class *)
