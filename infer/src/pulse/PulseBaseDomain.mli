(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PulseBasicInterface

module Stack : sig
  include
    PrettyPrintable.MonoMap with type key = Var.t and type value = AbstractValue.t * ValueHistory.t

  (* need to shadow the declaration in [MonoMap] even though it is unused since [MapS.compare] has a
     different type *)
  val compare : t -> t -> int [@@warning "-32"]
end

type t = {heap: PulseBaseMemory.t; stack: Stack.t}

val empty : t

include AbstractDomain.NoJoin with type t := t

val reachable_addresses : t -> AbstractValue.Set.t
(** compute the set of abstract addresses that are "used" in the abstract state, i.e. reachable
    from the stack variables *)

type mapping

val empty_mapping : mapping

type isograph_relation =
  | NotIsomorphic  (** no mapping was found that can make LHS the same as the RHS *)
  | IsomorphicUpTo of mapping  (** [mapping(lhs)] is isomorphic to [rhs] *)

val isograph_map : lhs:t -> rhs:t -> mapping -> isograph_relation

val is_isograph : lhs:t -> rhs:t -> mapping -> bool
