(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PulseISLBasicInterface
module F = Format

type t = {heap: PulseISLBaseMemory.t; stack: PulseISLBaseStack.t; attrs: PulseISLBaseAddressAttributes.t}

type cell = PulseISLBaseMemory.Edges.t * Attributes.t

val empty : t

val is_empty : t -> bool

val reachable_addresses : t -> AbstractValue.Set.t
(** compute the set of abstract addresses that are "used" in the abstract state, i.e. reachable from
    the stack variables *)

type mapping

val empty_mapping : mapping

type isograph_relation =
  | NotIsomorphic  (** no mapping was found that can make LHS the same as the RHS *)
  | IsomorphicUpTo of mapping  (** [mapping(lhs)] is isomorphic to [rhs] *)

val isograph_map : lhs:t -> rhs:t -> mapping -> isograph_relation

val is_isograph : lhs:t -> rhs:t -> mapping -> bool

val find_cell_opt : AbstractValue.t -> t -> cell option

val pp : F.formatter -> t -> unit

val reachable_address_vars : t -> AbstractValue.Set.t * Var.t list

val is_reachable : AbstractValue.t  -> Var.Set.t -> t -> bool
  
val is_aliasing_abducible : Var.Set.t -> AbstractValue.t -> t -> bool
