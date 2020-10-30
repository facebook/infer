(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

(** Nullability is a central concept for Nullsafe type-checker. Informally, nullability is a "type"
    \- set of values together with some additional context. All nullsafe is interested about if
    whether a value can be null, and if it can, what are potential causes of leaking the null inside
    it. Formally, nullability values form a lattice with partial order and upper bound operations.
    Practically, most of nullsafe core should remain agnostic over exact values of nullability (e.g.
    pattern-mathching over them should be a rare case. Core of typechecker should deal with
    subtyping and joins instead.) *)

type t =
  | Null  (** The only possible value for that type is null *)
  | Nullable  (** No guarantees on the nullability *)
  | ThirdPartyNonnull
      (** Values coming from third-party methods and fields not explictly annotated neither as
          [@Nullable], nor as [@Nonnull]. We still consider those as non-nullable but with the least
          level of confidence. *)
  | UncheckedNonnull
      (** The type comes from a signature that is annotated (explicitly or implicitly according to
          conventions) as non-nullable. Hovewer, the class is not checked under [@Nullsafe], so its
          actual nullability is not truthworhy, as the class might contain arbitrary number of
          nullability issues *)
  | LocallyTrustedNonnull
      (** The same as [UncheckedNonnull], but the value comes from a class explicitly listed as
          trusted in the class under analysis. It is less truthworthy than [LocallyCheckedNonnull]
          as no actual verification were performed. *)
  | LocallyCheckedNonnull
      (** Non-nullable value that comes from a class checked under [@NullsafeLocal] mode. Local mode
          type-checks files against its dependencies but does not require the dependencies to be
          transitively checked. Therefore this type of non-nullable value is differentiated from
          StrictNonnull. *)
  | ProvisionallyNullable
      (** Only for "annotation graph" mode. Indicates the value coming from a not annotated object
          in the class under analysis. Since the object is not annotated, it should be treated as
          non-null for all cases related to user-facing issue generation. *)
  | StrictNonnull
      (** Non-nullable value with the highest degree of certainty, because it is either:

          - a non-null literal,
          - an expression that semantically cannot be null,
          - comes from code checked under [@NullsafeStrict] mode,
          - comes from a third-party method with an appropriate built-in model, user-defined
            nullability signature, or explicit [@NonNull] annotation.

          The latter two are potential sources of unsoundness issues for nullsafe, but we need to
          strike the balance between the strictness of analysis, convenience, and real-world risk. *)
[@@deriving compare, equal]

type pair = t * t [@@deriving compare, equal]

val top : t
(** The most generic type. *)

val is_subtype : subtype:t -> supertype:t -> bool
(** A is a subtype of B, if all values of A can be represented in B. Subtype relation is reflexive:
    everything is a subtype of itself. *)

val join : t -> t -> t
(** Unique upper bound over two types: the most precise type that is a supertype of both.
    Practically, joins occur e.g. when two branches of execution flow are getting merged. *)

val is_considered_nonnull : nullsafe_mode:NullsafeMode.t -> t -> bool
(** Check whether a given nullability is considered non-nullable within a given [nullsafe_mode]. *)

val is_nonnullish : t -> bool
(** Check whether a given nullability is one of the non-nullable types with no regards to the mode. *)

val pp : F.formatter -> t -> unit
