(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PulseBasicInterface
module F = Format

type t = {heap: PulseBaseMemory.t; stack: PulseBaseStack.t; attrs: PulseBaseAddressAttributes.t}
[@@deriving compare, equal, yojson_of]

type cell = PulseBaseMemory.Edges.t * Attributes.t

val empty : t

val reachable_addresses : t -> AbstractValue.Set.t
(** compute the set of abstract addresses that are "used" in the abstract state, i.e. reachable from
    the stack variables *)

val reachable_addresses_from : AbstractValue.t list -> t -> AbstractValue.Set.t
(** compute the set of abstract addresses that are reachable from given abstract addresses *)

type mapping

val empty_mapping : mapping

type isograph_relation =
  | NotIsomorphic  (** no mapping was found that can make LHS the same as the RHS *)
  | IsomorphicUpTo of mapping  (** [mapping(lhs)] is isomorphic to [rhs] *)

val isograph_map : lhs:t -> rhs:t -> mapping -> isograph_relation

val is_isograph : lhs:t -> rhs:t -> mapping -> bool

val find_cell_opt : AbstractValue.t -> t -> cell option

val pp : F.formatter -> t -> unit

val subst_var : AbstractValue.t * AbstractValue.t -> t -> t SatUnsat.t
