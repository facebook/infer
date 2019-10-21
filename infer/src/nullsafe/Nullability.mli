(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Nullability is a central concept for Nullsafe type-checker.
    Informally, nullability is a "type" - set of values together with some additional context.
      All nullsafe is interested about if whether a value can be null, and if it can,
      what are potential causes of leaking the null inside it.
    Formally, nullability values form a lattice with partial order and upper bound operations.
    Practically, most of nullsafe core should remain agnostic over exact values of nullability
    (e.g. pattern-mathching over them should be a rare case. Core of typechecker should deal with subtyping and joins instead.)
 *)

type t =
  | Nullable  (** No guarantees on the nullability *)
  | DeclaredNonnull
      (** The type comes from a signature that is annotated (explicitly or implicitly according to conventions)
      as non-nullable. Hovewer, it might still contain null since the truthfullness of the declaration was
      not checked.
   *)
  | Nonnull
      (** We believe that this value can not be null. If it is not the case, this is
          an unsoundness issue for Nullsafe, and we aim to minimize number of such issues
          occuring in real-world programs. *)
[@@deriving compare, equal]

val top : t
(** The most generic type. *)

val is_subtype : subtype:t -> supertype:t -> bool
(** A is a subtype of B, if all values of A can be represented in B.
    Subtype relation is reflexive: everything is a subtype of itself. *)

val is_strict_subtype : subtype:t -> supertype:t -> bool
(** The same as subtype, but non-reflexive version. *)

val join : t -> t -> t
(** Unique upper bound over two types: the most precise type that is a supertype of both.
    Practically, joins occur e.g. when two branches of execution flow are getting merged. *)

val to_string : t -> string
