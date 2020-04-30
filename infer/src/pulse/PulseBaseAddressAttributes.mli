(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format
open PulseBasicInterface

type t

val empty : t

val filter : (AbstractValue.t -> Attributes.t -> bool) -> t -> t

val partition : (AbstractValue.t -> Attributes.t -> bool) -> t -> t * t

val find_opt : AbstractValue.t -> t -> Attributes.t option

val add_one : AbstractValue.t -> Attribute.t -> t -> t

val add : AbstractValue.t -> Attributes.t -> t -> t

val allocate : Procname.t -> AbstractValue.t * ValueHistory.t -> Location.t -> t -> t

val fold : (AbstractValue.t -> Attributes.t -> 'a -> 'a) -> t -> 'a -> 'a

val check_valid : AbstractValue.t -> t -> (unit, Invalidation.t * Trace.t) result

val invalidate : AbstractValue.t * ValueHistory.t -> Invalidation.t -> Location.t -> t -> t

val get_closure_proc_name : AbstractValue.t -> t -> Procname.t option

val get_citv : AbstractValue.t -> t -> CItv.t option

val get_bo_itv : AbstractValue.t -> t -> Itv.ItvPure.t

val get_must_be_valid : AbstractValue.t -> t -> Trace.t option

val std_vector_reserve : AbstractValue.t -> t -> t

val is_std_vector_reserved : AbstractValue.t -> t -> bool

val pp : F.formatter -> t -> unit

val remove_allocation_attr : AbstractValue.t -> t -> t
