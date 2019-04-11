(*
 * Copyright (c) 2019-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module AbstractAddress = PulseDomain.AbstractAddress
module Attributes = PulseDomain.Attributes

(* layer on top of {!PulseDomain} to propagate operations on the current state to the pre-condition
   when necessary

   The abstract type [t] is a pre/post pair in the style of biabduction.
*)

include AbstractDomain.NoJoin

val empty : t

(** stack operations like {!PulseDomain.Stack} but that also take care of propagating facts to the
    precondition *)
module Stack : sig
  val add : Var.t -> PulseDomain.Stack.value -> t -> t

  val remove_vars : Var.t list -> t -> t

  val fold : (Var.t -> PulseDomain.Stack.value -> 'a -> 'a) -> t -> 'a -> 'a

  val find_opt : Var.t -> t -> PulseDomain.Stack.value option

  val materialize : Var.t -> t -> t * PulseDomain.Stack.value
end

(** stack operations like {!PulseDomain.Heap} but that also take care of propagating facts to the
    precondition *)
module Memory : sig
  module Access = PulseDomain.Memory.Access
  module Edges = PulseDomain.Memory.Edges

  val add_attributes : AbstractAddress.t -> Attributes.t -> t -> t

  val add_edge : AbstractAddress.t -> Access.t -> PulseDomain.AddrTracePair.t -> t -> t

  val add_edge_and_back_edge :
    AbstractAddress.t -> Access.t -> PulseDomain.AddrTracePair.t -> t -> t

  val check_valid :
    PulseDiagnostic.actor -> AbstractAddress.t -> t -> (t, PulseInvalidation.t) result

  val find_opt : AbstractAddress.t -> t -> PulseDomain.Memory.cell option

  val set_cell : AbstractAddress.t -> PulseDomain.Memory.cell -> t -> t

  val invalidate : AbstractAddress.t -> PulseInvalidation.t -> t -> t

  val is_std_vector_reserved : AbstractAddress.t -> t -> bool

  val std_vector_reserve : AbstractAddress.t -> t -> t

  val materialize_edge : AbstractAddress.t -> Access.t -> t -> t * PulseDomain.AddrTracePair.t
end

val discard_unreachable : t -> t
(** garbage collect unreachable addresses in the state to make it smaller, just for convenience and
   keep its size down *)
