(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format
module L = Logging
open PulseBasicInterface

module AttributesNoRank = struct
  include Attributes

  let pp fmt t : unit = Attributes.pp ?print_rank:None fmt t
end

module Graph = PrettyPrintable.MakePPMonoMap (AbstractValue) (AttributesNoRank)

type t = Graph.t

let compare = Graph.compare AttributesNoRank.compare

let equal = Graph.equal AttributesNoRank.equal

let yojson_of_t = [%yojson_of: _]

let add_one addr attribute attrs =
  match Graph.find_opt addr attrs with
  | None ->
      Graph.add addr (Attributes.singleton attribute) attrs
  | Some old_attrs ->
      let new_attrs = Attributes.add old_attrs attribute in
      Graph.add addr new_attrs attrs


let add addr attributes attrs =
  match Graph.find_opt addr attrs with
  | None ->
      Graph.add addr attributes attrs
  | Some old_attrs ->
      let new_attrs = Attributes.union_prefer_left old_attrs attributes in
      Graph.add addr new_attrs attrs


let fold = Graph.fold

let find_opt = Graph.find_opt

let empty = Graph.empty

let filter = Graph.filter

(* for an abstract value v, where f_keep(v) == false, find an abstract value v_keep, where
   f_keep(v_keep) == true and where v_keep has taint propagated from v *)
let mk_transitive_taint_from_subst f_keep memory =
  (* construct map v -> [v1,...,vn], where taint is propagated from v1,...,vn to v *)
  let taints_from_map =
    fold
      (fun addr attrs taint_from_map ->
        match Attributes.get_propagate_taint_from attrs with
        | None ->
            taint_from_map
        | Some (_, taints_in) ->
            List.fold ~init:taint_from_map
              ~f:(fun acc {Attribute.v= from} ->
                match AbstractValue.Map.find_opt addr acc with
                | None ->
                    AbstractValue.Map.add addr [from] acc
                | Some taint_from ->
                    AbstractValue.Map.add addr (from :: taint_from) acc )
              taints_in )
      memory AbstractValue.Map.empty
  in
  let rec find_live_and_subst subst addr =
    match AbstractValue.Map.find_opt addr subst with
    | None -> (
        if f_keep addr then subst
        else
          (* to avoid cycles *)
          let subst = AbstractValue.Map.add addr AbstractValue.Set.empty subst in
          match AbstractValue.Map.find_opt addr taints_from_map with
          | None ->
              subst
          | Some taints_from ->
              let subst =
                List.fold taints_from
                  ~f:(fun subst addr -> find_live_and_subst subst addr)
                  ~init:subst
              in
              let new_taints_from =
                List.fold taints_from ~init:AbstractValue.Set.empty ~f:(fun acc addr ->
                    if f_keep addr then AbstractValue.Set.add addr acc
                    else AbstractValue.Set.union (AbstractValue.Map.find addr subst) acc )
              in
              AbstractValue.Map.add addr new_taints_from subst )
    | Some _ ->
        subst
  in
  AbstractValue.Map.fold
    (fun addr _taint_from subst -> find_live_and_subst subst addr)
    taints_from_map AbstractValue.Map.empty


let filter_with_discarded_addrs f_keep memory =
  let taint_from_subst = mk_transitive_taint_from_subst f_keep memory in
  fold
    (fun addr attrs ((memory, discarded) as acc) ->
      if f_keep addr then
        let attrs' =
          Attributes.fold attrs ~init:attrs ~f:(fun attrs' attr ->
              match Attribute.filter_unreachable taint_from_subst f_keep attr with
              | None ->
                  Attributes.remove attr attrs'
              | Some attr' ->
                  if phys_equal attr attr' then attrs'
                  else
                    let attrs' = Attributes.remove attr attrs' in
                    Attributes.add attrs' attr' )
        in
        if phys_equal attrs attrs' then acc
        else
          ( Graph.update addr
              (fun _ -> if Attributes.is_empty attrs' then None else Some attrs')
              memory
          , (* HACK: don't add to the discarded addresses even if we did discard all the attributes
               of the address; this is ok because the list of discarded addresses is only relevant
               to allocation attributes, which are not affected by this filtering... sorry! *)
            discarded )
      else (Graph.remove addr memory, addr :: discarded) )
    memory (memory, [])


let pp = Graph.pp

let invalidate (address, history) invalidation location memory =
  add_one address (Attribute.Invalid (invalidation, Immediate {location; history})) memory


let always_reachable address memory = add_one address Attribute.AlwaysReachable memory

let allocate allocator address location memory =
  add_one address
    (Attribute.Allocated (allocator, Immediate {location; history= ValueHistory.epoch}))
    memory


let java_resource_release address memory = add_one address Attribute.JavaResourceReleased memory

let hack_async_await address memory = add_one address Attribute.HackAsyncAwaited memory

let csharp_resource_release address memory = add_one address Attribute.CSharpResourceReleased memory

let in_reported_retain_cycle address memory = add_one address Attribute.InReportedRetainCycle memory

let mark_as_end_of_collection address memory = add_one address Attribute.EndOfCollection memory

let check_valid address attrs =
  L.d_printfln "Checking validity of %a" AbstractValue.pp address ;
  match Graph.find_opt address attrs |> Option.bind ~f:Attributes.get_invalid with
  | None ->
      Ok ()
  | Some invalidation ->
      L.d_printfln ~color:Red "INVALID: %a" Invalidation.pp (fst invalidation) ;
      Error invalidation


let check_initialized address attrs =
  L.d_printfln "Checking if %a is initialized" AbstractValue.pp address ;
  match Graph.find_opt address attrs |> Option.bind ~f:Attributes.get_uninitialized with
  | Some typ ->
      L.d_printfln ~color:Red "UNINITIALIZED" ;
      Error typ
  | None ->
      Ok ()


let get_attribute getter address attrs =
  let open Option.Monad_infix in
  Graph.find_opt address attrs >>= getter


let remove_attribute remover address attrs =
  Graph.update address
    (function
      | None ->
          None
      | Some attrs ->
          let attrs = remover attrs in
          if AttributesNoRank.is_empty attrs then None else Some attrs )
    attrs


let remove_allocation_attr = remove_attribute Attributes.remove_allocation

let remove_tainted = remove_attribute Attributes.remove_tainted

let remove_taint_sanitizer = remove_attribute Attributes.remove_taint_sanitized

let remove_propagate_taint_from = remove_attribute Attributes.remove_propagate_taint_from

let remove_all_must_not_be_tainted ?kinds = Graph.map (Attributes.remove_must_not_be_tainted ?kinds)

let remove_taint_attrs address memory =
  remove_tainted address memory |> remove_taint_sanitizer address
  |> remove_propagate_taint_from address


let remove_hack_builder = remove_attribute Attributes.remove_hack_builder

let set_hack_builder address builderstate memory =
  remove_hack_builder address memory |> add_one address (Attribute.HackBuilder builderstate)


let get_hack_builder = get_attribute Attributes.get_hack_builder

let remove_must_be_valid_attr = remove_attribute Attributes.remove_must_be_valid

let map_attributes ~f =
  Graph.filter_map (fun _addr attrs ->
      let new_attrs = f attrs in
      if Attributes.is_empty new_attrs then None else Some new_attrs )


let make_suitable_for_pre_summary = map_attributes ~f:Attributes.make_suitable_for_pre_summary

let make_suitable_for_post_summary = map_attributes ~f:Attributes.make_suitable_for_post_summary

let remove_all_taint_related_attrs = map_attributes ~f:Attributes.remove_all_taint_related

let initialize address memory =
  add_one address Initialized memory |> remove_attribute Attributes.remove_uninitialized address


let get_allocation = get_attribute Attributes.get_allocation

let get_closure_proc_name = get_attribute Attributes.get_closure_proc_name

let get_copied_into = get_attribute Attributes.get_copied_into

let get_copied_return = get_attribute Attributes.get_copied_return

let remove_copied_return = remove_attribute Attributes.remove_copied_return

let get_source_origin_of_copy address attrs =
  get_attribute Attributes.get_source_origin_of_copy address attrs |> Option.map ~f:fst


let is_copied_from_const_ref address attrs =
  get_attribute Attributes.get_source_origin_of_copy address attrs
  |> Option.exists ~f:(fun (_, is_const_ref) -> is_const_ref)


let get_must_be_valid = get_attribute Attributes.get_must_be_valid

let get_must_not_be_tainted address memory =
  match Graph.find_opt address memory with
  | None ->
      Attribute.TaintSinkMap.empty
  | Some attrs ->
      Attributes.get_must_not_be_tainted attrs


let get_must_be_initialized = get_attribute Attributes.get_must_be_initialized

let get_written_to = get_attribute Attributes.get_written_to

let get_returned_from_unknown = get_attribute Attributes.get_returned_from_unknown

let add_dict_contain_const_keys address memory = add_one address DictContainConstKeys memory

let remove_dict_contain_const_keys = remove_attribute Attributes.remove_dict_contain_const_keys

let is_dict_contain_const_keys address attrs =
  Graph.find_opt address attrs |> Option.exists ~f:Attributes.is_dict_contain_const_keys


let add_dict_read_const_key timestamp trace address key attrs =
  add_one address (DictReadConstKeys (Attribute.ConstKeys.singleton key (timestamp, trace))) attrs


let get_dict_read_const_keys = get_attribute Attributes.get_dict_read_const_keys

let add_static_type typ address memory = add_one address (Attribute.StaticType typ) memory

let get_static_type attrs v = get_attribute Attributes.get_static_type v attrs

let std_vector_reserve address memory = add_one address Attribute.StdVectorReserve memory

let add_unreachable_at address location memory = add_one address (UnreachableAt location) memory

let add_copied_return address ~source ~is_const_ref from copied_location memory =
  add_one address (Attribute.CopiedReturn {source; is_const_ref; from; copied_location}) memory


let get_config_usage address attrs = get_attribute Attributes.get_config_usage address attrs

let get_used_as_branch_cond address attrs =
  get_attribute Attributes.get_used_as_branch_cond address attrs


let is_end_of_collection address attrs =
  Graph.find_opt address attrs |> Option.exists ~f:Attributes.is_end_of_collection


let is_java_resource_released adress attrs =
  Graph.find_opt adress attrs |> Option.exists ~f:Attributes.is_java_resource_released


let is_csharp_resource_released adress attrs =
  Graph.find_opt adress attrs |> Option.exists ~f:Attributes.is_csharp_resource_released


let is_in_reported_retain_cycle address attrs =
  Graph.find_opt address attrs |> Option.exists ~f:Attributes.is_in_reported_retain_cycle


let is_std_moved address attrs =
  Graph.find_opt address attrs |> Option.exists ~f:Attributes.is_std_moved


let is_std_vector_reserved address attrs =
  Graph.find_opt address attrs |> Option.exists ~f:Attributes.is_std_vector_reserved


let get_last_lookup address attrs = get_attribute Attributes.get_last_lookup address attrs

let get_address_of_stack_variable address attrs =
  get_attribute Attributes.get_address_of_stack_variable address attrs


let has_unknown_effect address attrs =
  Graph.find_opt address attrs
  |> Option.exists ~f:(fun attribute -> Option.is_some (Attributes.get_unknown_effect attribute))


let is_hack_sinit_called address attrs =
  Graph.find_opt address attrs |> Option.exists ~f:Attributes.is_hack_sinit_called


let merge attrs attrs' =
  (* "merge" attributes if two different values ([addr] and [addr']) are found to be
     equal after attributes of the same kind were recorded for them. This arbitrarily
     keeps one of them, with unclear but likely benign consequences. *)
  Attributes.union_prefer_left attrs' attrs


let canonicalize_post ~get_var_repr attrs_map =
  Graph.fold
    (fun addr attrs g ->
      if Attributes.is_empty attrs then g
      else
        let addr' = get_var_repr addr in
        let attrs' = Graph.find_opt addr' g |> Option.fold ~init:attrs ~f:merge in
        add addr' attrs' g )
    (make_suitable_for_post_summary attrs_map)
    Graph.empty


let subst_var (v, v') attrs_map =
  match Graph.find_opt v attrs_map with
  | None ->
      attrs_map
  | Some attrs ->
      let attrs_to_add =
        match Graph.find_opt v' attrs_map with None -> attrs | Some attrs' -> merge attrs attrs'
      in
      add v' attrs_to_add attrs_map


(* copied verbatim from .mli, has to be kept in sync because ocaml *)
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
