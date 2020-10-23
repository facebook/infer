(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format
open PulseBasicInterface

type t [@@deriving yojson_of]

val empty : t

val filter : (AbstractValue.t -> Attributes.t -> bool) -> t -> t

val filter_with_discarded_addrs :
  (AbstractValue.t -> Attributes.t -> bool) -> t -> t * AbstractValue.t list

val find_opt : AbstractValue.t -> t -> Attributes.t option

val add_one : AbstractValue.t -> Attribute.t -> t -> t

val add : AbstractValue.t -> Attributes.t -> t -> t

val allocate : Procname.t -> AbstractValue.t * ValueHistory.t -> Location.t -> t -> t

val add_dynamic_type : Typ.Name.t -> AbstractValue.t -> t -> t

val fold : (AbstractValue.t -> Attributes.t -> 'a -> 'a) -> t -> 'a -> 'a

val check_valid : AbstractValue.t -> t -> (unit, Invalidation.t * Trace.t) result

val invalidate : AbstractValue.t * ValueHistory.t -> Invalidation.t -> Location.t -> t -> t

val get_closure_proc_name : AbstractValue.t -> t -> Procname.t option

val get_must_be_valid : AbstractValue.t -> t -> Trace.t option

val std_vector_reserve : AbstractValue.t -> t -> t

val is_std_vector_reserved : AbstractValue.t -> t -> bool

val mark_as_end_of_collection : AbstractValue.t -> t -> t

val is_end_of_collection : AbstractValue.t -> t -> bool

val pp : F.formatter -> t -> unit

val remove_allocation_attr : AbstractValue.t -> t -> t
