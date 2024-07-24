(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format
open PulseBasicInterface

module type S = sig
  type t [@@deriving compare, equal, yojson_of]

  type key

  val pp : F.formatter -> t -> unit

  val empty : t

  val filter : (key -> Attributes.t -> bool) -> t -> t

  val filter_with_discarded_addrs : (key -> bool) -> t -> t * AbstractValue.t list

  val find_opt : key -> t -> Attributes.t option

  val add_one : key -> Attribute.t -> t -> t

  val add : key -> Attributes.t -> t -> t

  val allocate : Attribute.allocator -> key -> Location.t -> t -> t

  val always_reachable : key -> t -> t

  val java_resource_release : key -> t -> t

  val hack_async_await : key -> t -> t

  val remove_hack_builder : key -> t -> t

  val set_hack_builder : key -> Attribute.Builder.t -> t -> t

  val get_hack_builder : key -> t -> Attribute.Builder.t option

  val csharp_resource_release : key -> t -> t

  val in_reported_retain_cycle : key -> t -> t

  val fold : (key -> Attributes.t -> 'a -> 'a) -> t -> 'a -> 'a

  val check_valid : key -> t -> (unit, Invalidation.t * Trace.t) result

  val check_initialized : key -> t -> (unit, Attribute.UninitializedTyp.t) result

  val invalidate : key * ValueHistory.t -> Invalidation.t -> Location.t -> t -> t

  val get_allocation : key -> t -> (Attribute.allocator * Trace.t) option

  val get_closure_proc_name : key -> t -> Procname.t option

  val get_copied_into : key -> t -> Attribute.CopiedInto.t option

  val get_copied_return :
    key -> t -> (AbstractValue.t * bool * Attribute.CopyOrigin.t * Location.t) option

  val remove_copied_return : key -> t -> t

  val get_source_origin_of_copy : key -> t -> AbstractValue.t option

  val is_copied_from_const_ref : key -> t -> bool

  val get_must_be_valid :
    key -> t -> (Timestamp.t * Trace.t * Invalidation.must_be_valid_reason option) option

  val get_must_not_be_tainted : key -> t -> Attribute.TaintSink.t Attribute.TaintSinkMap.t

  val get_returned_from_unknown : key -> t -> AbstractValue.t list option

  val get_must_be_initialized : key -> t -> (Timestamp.t * Trace.t) option

  val add_dict_contain_const_keys : key -> t -> t

  val remove_dict_contain_const_keys : key -> t -> t

  val is_dict_contain_const_keys : key -> t -> bool

  val add_dict_read_const_key : Timestamp.t -> Trace.t -> key -> Fieldname.t -> t -> t

  val get_dict_read_const_keys : key -> t -> Attribute.ConstKeys.t option

  val add_static_type : Typ.Name.t -> key -> t -> t

  val get_static_type : t -> key -> Typ.Name.t option

  val get_written_to : key -> t -> (Timestamp.t * Trace.t) option

  val std_vector_reserve : key -> t -> t

  val is_java_resource_released : key -> t -> bool

  val is_csharp_resource_released : key -> t -> bool

  val is_in_reported_retain_cycle : key -> t -> bool

  val is_std_moved : key -> t -> bool

  val is_std_vector_reserved : key -> t -> bool

  val get_last_lookup : key -> t -> AbstractValue.t option

  val mark_as_end_of_collection : key -> t -> t

  val is_end_of_collection : key -> t -> bool

  val add_unreachable_at : key -> Location.t -> t -> t

  val add_copied_return :
    key -> source:key -> is_const_ref:bool -> Attribute.CopyOrigin.t -> Location.t -> t -> t

  val get_config_usage : key -> t -> Attribute.ConfigUsage.t option

  val get_used_as_branch_cond : key -> t -> (Procname.t * Location.t * Trace.t) option

  val remove_allocation_attr : key -> t -> t

  val remove_taint_attrs : key -> t -> t

  val remove_all_must_not_be_tainted : ?kinds:TaintConfig.Kind.Set.t -> t -> t

  val remove_must_be_valid_attr : key -> t -> t

  val initialize : key -> t -> t

  val get_address_of_stack_variable : key -> t -> (Var.t * Location.t * ValueHistory.t) option

  val has_unknown_effect : key -> t -> bool

  val is_hack_sinit_called : key -> t -> bool
end

include S with type key := AbstractValue.t

val make_suitable_for_pre_summary : t -> t

val remove_all_taint_related_attrs : t -> t

val canonicalize_post : get_var_repr:(AbstractValue.t -> AbstractValue.t) -> t -> t
(** merge the attributes of all the variables that are equal according to [get_var_repr] and remove
    non-canonical variables in favor of their representative *)

val subst_var : AbstractValue.t * AbstractValue.t -> t -> t
