(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format
open PulseBasicInterface

type t [@@deriving compare, equal, yojson_of]

val empty : t

val filter : (AbstractValue.t -> Attributes.t -> bool) -> t -> t

val filter_with_discarded_addrs : (AbstractValue.t -> bool) -> t -> t * AbstractValue.t list

val find_opt : AbstractValue.t -> t -> Attributes.t option

val add_one : AbstractValue.t -> Attribute.t -> t -> t

val add : AbstractValue.t -> Attributes.t -> t -> t

val allocate : Attribute.allocator -> AbstractValue.t -> Location.t -> t -> t

val always_reachable : AbstractValue.t -> t -> t

val java_resource_release : AbstractValue.t -> t -> t

val csharp_resource_release : AbstractValue.t -> t -> t

val fold : (AbstractValue.t -> Attributes.t -> 'a -> 'a) -> t -> 'a -> 'a

val check_valid : AbstractValue.t -> t -> (unit, Invalidation.t * Trace.t) result

val check_initialized : AbstractValue.t -> t -> (unit, unit) result

val invalidate : AbstractValue.t * ValueHistory.t -> Invalidation.t -> Location.t -> t -> t

val get_allocation : AbstractValue.t -> t -> (Attribute.allocator * Trace.t) option

val get_closure_proc_name : AbstractValue.t -> t -> Procname.t option

val get_copied_into : AbstractValue.t -> t -> Attribute.CopiedInto.t option

val get_copied_return :
  AbstractValue.t -> t -> (AbstractValue.t * bool * Attribute.CopyOrigin.t * Location.t) option

val remove_copied_return : AbstractValue.t -> t -> t

val get_source_origin_of_copy : AbstractValue.t -> t -> AbstractValue.t option

val is_copied_from_const_ref : AbstractValue.t -> t -> bool

val get_must_be_valid :
  AbstractValue.t -> t -> (Timestamp.t * Trace.t * Invalidation.must_be_valid_reason option) option

val get_must_not_be_tainted : AbstractValue.t -> t -> Attribute.TaintSinkSet.t

val get_returned_from_unknown : AbstractValue.t -> t -> AbstractValue.t list option

val get_must_be_initialized : AbstractValue.t -> t -> (Timestamp.t * Trace.t) option

val add_dynamic_type : Typ.t -> AbstractValue.t -> t -> t

val add_dynamic_type_source_file : Typ.t -> SourceFile.t -> AbstractValue.t -> t -> t

val get_dynamic_type : t -> AbstractValue.t -> Typ.t option

val get_dynamic_type_source_file : t -> AbstractValue.t -> (Typ.t * SourceFile.t option) option

val add_ref_counted : AbstractValue.t -> t -> t

val is_ref_counted : AbstractValue.t -> t -> bool

val get_written_to : AbstractValue.t -> t -> (Timestamp.t * Trace.t) option

val std_vector_reserve : AbstractValue.t -> t -> t

val is_java_resource_released : AbstractValue.t -> t -> bool

val is_csharp_resource_released : AbstractValue.t -> t -> bool

val is_std_moved : AbstractValue.t -> t -> bool

val is_std_vector_reserved : AbstractValue.t -> t -> bool

val mark_as_end_of_collection : AbstractValue.t -> t -> t

val is_end_of_collection : AbstractValue.t -> t -> bool

val add_unreachable_at : AbstractValue.t -> Location.t -> t -> t

val add_copied_return :
     AbstractValue.t
  -> source:AbstractValue.t
  -> is_const_ref:bool
  -> Attribute.CopyOrigin.t
  -> Location.t
  -> t
  -> t

val get_config_usage : AbstractValue.t -> t -> FbPulseConfigName.t option

val get_const_string : AbstractValue.t -> t -> string option

val pp : F.formatter -> t -> unit

val remove_allocation_attr : AbstractValue.t -> t -> t

val remove_taint_attrs : AbstractValue.t -> t -> t

val remove_must_be_valid_attr : AbstractValue.t -> t -> t

val make_suitable_for_pre_summary : t -> t

val initialize : AbstractValue.t -> t -> t

val canonicalize_post : get_var_repr:(AbstractValue.t -> AbstractValue.t) -> t -> t
(** merge the attributes of all the variables that are equal according to [get_var_repr] and remove
    non-canonical variables in favor of their representative *)

val subst_var : AbstractValue.t * AbstractValue.t -> t -> t
