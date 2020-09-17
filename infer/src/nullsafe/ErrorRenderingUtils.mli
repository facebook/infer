(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Helper tools for nicer rendering nullsafe error. *)

open! IStd

(** "Effectively nullable values" from the user perspective. Depending on context, convention, and
    mode, Nullsafe treats such and such things as nullable or non-null. At some point this needs to
    be explain to the user. *)
module UserFriendlyNullable : sig
  type t =
    | ExplainablyNullable of explainably_nullable_kind
        (** Value that is nullable according to nullsafe semantics and conventions. It can be
            nullable because of an explicit annotation, models, default nullability conventions,
            etc. *)
    | UntrustedNonnull of untrusted_kind
        (** Value is not nullable per se, but we still can not treat it as non-null in current mode.
            From the user perspective, it is a very different case: violations of this type need to
            be explained in a way so that it is clear why exactly can not nullsafe trust it in this
            context. *)

  and explainably_nullable_kind = Nullable | Null

  and untrusted_kind = ThirdPartyNonnull | UncheckedNonnull | LocallyCheckedNonnull

  val from_nullability : Nullability.t -> t option
end

val is_object_nullability_self_explanatory : object_expression:string -> TypeOrigin.t -> bool
(** In order to understand why such and such object is nullable (or not nullable), we render its
    origin. In some cases this is redundant and adds extra noise for the user. *)

val mk_nullsafe_issue_for_untrusted_values :
     nullsafe_mode:NullsafeMode.t
  -> untrusted_kind:UserFriendlyNullable.untrusted_kind
  -> bad_usage_location:Location.t
  -> TypeOrigin.t
  -> NullsafeIssue.t
(** Situation when we tried to use nonnull values in a nullsafe mode that does not trust them to be
    non-nullable: [untrusted_kind]. From the user perspective, this case is different from normal
    nullable assignment or dereference violation: what needs to be described is why does not this
    mode trust this value (and what are possible actions). NOTE: Location of the error will be NOT
    in the place when the value is used (that is [bad_usage_location]), but where the value is first
    obtained from. *)

val find_alternative_nonnull_method_description : TypeOrigin.t -> string option
(** If type origin is the result of a nullable method call that have a known nonnullable alternative
    (the one that does the check inside), return the string representation of that alternative
    suitable for error messaging. *)
