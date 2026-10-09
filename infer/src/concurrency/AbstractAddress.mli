(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

(** Type meant to represent abstract addresses based on access paths. It currently distinguishes
    between paths

    - rooted at formal parameters (these are identified by the parameter index and the path without
      the root variable, though that variable is kept for pretty printing);
    - rooted at global variables;
    - non access-path expressions representing class objects (java only);
    - rooted at values passed to a callee that the caller cannot name, eg returned by a call
      (opaque).

    Notably, there are no addresses rooted at the address of a local (because proving aliasing
    between those is difficult).

    There are two notions of equality:

    - Equality for comparing two addresses within the same thread/process/trace. Under this,
      identical globals and identical class objects compare equal. Parameter-rooted paths compare
      equal if their parameter indices, types and lists of accesses are equal. Opaque paths compare
      equal if their types and lists of accesses are equal.
    - Equality for comparing two addresses in two distinct threads/traces. Globals and class objects
      are compared in the same way, but parameter-rooted and opaque paths need only have equal
      access lists (ie [x.f.g == y.f.g]). This allows demonically aliasing parameters in *distinct*
      threads. *)

include PrettyPrintable.PrintableOrderedType

val describe : F.formatter -> t -> unit
(** human readable description *)

val equal : t -> t -> bool

val equal_across_threads : Tenv.t -> t -> t -> bool

val normalise_across_threads : Tenv.t -> t -> t
(** a coarse key for the address such that [equal_across_threads tenv t1 t2] implies
    [equal (normalise_across_threads tenv t1) (normalise_across_threads tenv t2)]; the converse does
    not hold, so users of the key must still check [equal_across_threads] *)

val root_class : t -> Typ.name option
(** Class of the root variable of the expression representing the address *)

val get_typ : Tenv.t -> t -> Typ.t option

val make : FormalMap.t -> HilExp.t -> t option
(** convert an expression to a canonical form for an address *)

val is_class_object : t -> bool
(** is the address a Java class object such as in [synchronized(MyClass.class){}] or
    [static synchronized void foo()] *)

(** A substitution from formal position indices to address options. [None] is used to for actuals
    that cannot be resolved to an address (eg addresses of locals or arithmetic expressions). *)
type subst

val pp_subst : F.formatter -> subst -> unit [@@warning "-unused-value-declaration"]

val make_subst : FormalMap.t -> HilExp.t list -> subst
(** [make_subst formals actuals] maps the position of each actual to its address in terms of the
    caller's [formals] *)

val without_opaque : subst -> subst
(** maps the actuals that only have an opaque address to [None] *)

val apply_subst : subst -> t -> t option
