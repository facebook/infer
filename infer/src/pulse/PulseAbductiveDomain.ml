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
module BaseDomain = PulseBaseDomain
module DecompilerExpr = PulseDecompilerExpr
module Decompiler = PulseDecompiler
module PathContext = PulsePathContext
module UninitBlocklist = PulseUninitBlocklist

(** {2 Unsafe part of the module implementation wrt normalization} *)

(** signature common to [PostDomain], representing the post at the current program point, and the
    inverted [PreDomain], representing the inferred pre-condition*)
module type BaseDomainSig = sig
  (* private because the lattice is not the same for preconditions and postconditions so we don't
     want to confuse them *)
  type t = private BaseDomain.t [@@deriving compare, equal, yojson_of]
end

(* defined in two parts to avoid exporting internal APIs to the .mli *)
module type BaseDomainSig_ = sig
  include BaseDomainSig

  val empty : t

  val update :
       ?stack:PulseBaseStack.t
    -> ?heap:PulseBaseMemory.t
    -> ?attrs:PulseBaseAddressAttributes.t
    -> t
    -> t

  val subst_var : for_summary:bool -> AbstractValue.t * AbstractValue.t -> t -> t SatUnsat.t

  val pp : F.formatter -> t -> unit
end

(* just to expose record field names without having to type
   [BaseDomain.heap] *)
type base_domain = BaseDomain.t =
  {heap: PulseBaseMemory.t; stack: PulseBaseStack.t; attrs: PulseBaseAddressAttributes.t}

(** represents the post abstract state at each program point *)
module PostDomain : BaseDomainSig_ = struct
  include BaseDomain

  let update ?stack ?heap ?attrs foot =
    let new_stack, new_heap, new_attrs =
      ( Option.value ~default:foot.stack stack
      , Option.value ~default:foot.heap heap
      , Option.value ~default:foot.attrs attrs )
    in
    if
      phys_equal new_stack foot.stack && phys_equal new_heap foot.heap
      && phys_equal new_attrs foot.attrs
    then foot
    else {stack= new_stack; heap= new_heap; attrs= new_attrs}
end

(* NOTE: [PreDomain] and [Domain] theoretically differ in that [PreDomain] should be the inverted lattice of [Domain], but since we never actually join states or check implication the two collapse into one (though we still want to be careful not to mix them up so give them different types). *)

(** represents the inferred pre-condition at each program point, biabduction style *)
module PreDomain : BaseDomainSig_ = PostDomain

(* see documentation in this file's .mli *)
type t =
  { post: PostDomain.t
  ; pre: PreDomain.t
  ; path_condition: Formula.t
  ; decompiler: (Decompiler.t[@yojson.opaque] [@equal.ignore] [@compare.ignore])
  ; topl: (PulseTopl.state[@yojson.opaque])
  ; need_dynamic_type_specialization: (AbstractValue.Set.t[@yojson.opaque])
  ; transitive_info: (TransitiveInfo.t[@yojson.opaque])
  ; recursive_calls: (PulseMutualRecursion.Set.t[@yojson.opaque])
  ; skipped_calls: SkippedCalls.t }
[@@deriving compare, equal, yojson_of]

let pp_ ~is_summary f
    ({ post
     ; pre
     ; path_condition
     ; decompiler
     ; need_dynamic_type_specialization
     ; transitive_info
     ; topl
     ; recursive_calls
     ; skipped_calls } [@warning "+missing-record-field-pattern"] ) =
  let pp_decompiler f =
    if Config.debug_level_analysis >= 3 then F.fprintf f "decompiler=%a;@;" Decompiler.pp decompiler
  in
  (* print pre then post if it's a summary, other print the post (aka current abstract state) first
     *)
  let pp_pre_post f =
    if is_summary then F.fprintf f "PRE=@[%a@]@;POST=@[%a@]" PreDomain.pp pre PostDomain.pp post
    else F.fprintf f "%a@;PRE=[%a]" PostDomain.pp post PreDomain.pp pre
  in
  F.fprintf f
    "@[<v>%a@;\
     %t@;\
     %tneed_dynamic_type_specialization=%a@;\
     transitive_info=%a@;\
     recursive_calls=%a@;\
     skipped_calls=%a@;\
     Topl=%a@]"
    Formula.pp path_condition pp_pre_post pp_decompiler AbstractValue.Set.pp
    need_dynamic_type_specialization TransitiveInfo.pp transitive_info PulseMutualRecursion.Set.pp
    recursive_calls SkippedCalls.pp skipped_calls PulseTopl.pp_state topl


let pp = pp_ ~is_summary:false

let set_path_condition path_condition astate = {astate with path_condition}

let record_transitive_access location astate =
  let trace = Trace.Immediate {location; history= ValueHistory.epoch} in
  let accesses = Trace.Set.add trace astate.transitive_info.accesses in
  let transitive_info = {astate.transitive_info with accesses} in
  {astate with transitive_info}


let record_call_resolution ~caller callsite_loc call_kind resolution astate =
  let {TransitiveInfo.callees} = astate.transitive_info in
  let callees = TransitiveInfo.Callees.record ~caller callsite_loc call_kind resolution callees in
  let transitive_info = {astate.transitive_info with callees} in
  {astate with transitive_info}


let map_decompiler astate ~f = {astate with decompiler= f astate.decompiler}

let canon astate v = Formula.get_var_repr astate.path_condition v

(** The internals define here are safe (wrt normalization) to use but expose the
    canonical/non-canonical (sub-)types of abstract values inherited from [PulseCanonValue]. We want
    to hide these in the external API of this file and pretend everything is just an
    [AbstractValue.t] for ease of use (and because historically this was written with just
    [AbstractValue.t] so feel free to change at some point) so we confine the trickier exposed bits
    to this [Internal] module. *)
module Internal = struct
  (** Versions of the stack/heap/attributes that are safe wrt value normalization; see the
      documentation for [PulseCanonValue] *)
  module CanonValue = PulseCanonValue.Make (struct
    type astate = t

    let canon = canon
  end)

  module RawStack = PulseBaseStack
  module RawMemory = PulseBaseMemory
  module BaseStack = CanonValue.Stack
  module BaseMemory = CanonValue.Memory
  module BaseAddressAttributes = CanonValue.Attributes

  let downcast (x : CanonValue.t) : AbstractValue.t = (x :> AbstractValue.t) [@@inline always]

  let downcast_fst pair = (pair : CanonValue.t * _ :> AbstractValue.t * _) [@@inline always]

  let downcast_snd_fst pair_pair = (pair_pair : _ * (CanonValue.t * _) :> _ * (AbstractValue.t * _))
  [@@inline always]


  let downcast_access access = (access : BaseMemory.Access.t :> RawMemory.Access.t)
  [@@inline always]


  module SafeBaseMemory = struct
    module Edges = struct
      let fold astate edges ~init ~f =
        BaseMemory.Edges.fold edges ~init ~f:(fun acc access_addr_hist ->
            f acc (CanonValue.canon_snd_fst astate access_addr_hist) )


      let mapi astate edges ~f =
        BaseMemory.Edges.mapi edges ~f:(fun access addr_hist ->
            f access (CanonValue.canon_fst astate addr_hist) )
    end

    let fold_edges astate v heap ~init ~f =
      match BaseMemory.find_opt v heap with
      | None ->
          init
      | Some edges ->
          Edges.fold astate edges ~init ~f
  end

  module SafeStack = struct
    let is_abducible astate var =
      (* HACK: formals are pre-registered in the initial state *)
      BaseStack.mem var (astate.pre :> base_domain).stack || Var.is_global var


    (** [astate] with [astate.post.stack = f astate.post.stack] *)
    let map_post_stack ~f astate =
      let new_post = PostDomain.update astate.post ~stack:(f (astate.post :> base_domain).stack) in
      if phys_equal new_post astate.post then astate else {astate with post= new_post}


    let eval origin var astate =
      let astate, addr_hist =
        match BaseStack.find_opt var (astate.post :> base_domain).stack with
        | Some addr_hist ->
            (astate, addr_hist) |> CanonValue.canon_snd_fst astate
        | None ->
            let addr = CanonValue.mk_fresh () in
            let addr_hist = (addr, origin) in
            let post_stack =
              BaseStack.add var (addr_hist |> downcast_fst) (astate.post :> base_domain).stack
            in
            let post_heap = (astate.post :> base_domain).heap in
            let post_attrs = (astate.post :> base_domain).attrs in
            let pre =
              (* do not overwrite values of variables already in the pre *)
              if
                (not (BaseStack.mem var (astate.pre :> base_domain).stack))
                && is_abducible astate var
              then
                (* HACK: do not record the history of values in the pre as they are unused *)
                let foot_stack =
                  BaseStack.add var
                    (downcast addr, ValueHistory.epoch)
                    (astate.pre :> base_domain).stack
                in
                let foot_heap = BaseMemory.register_address addr (astate.pre :> base_domain).heap in
                PreDomain.update ~stack:foot_stack ~heap:foot_heap astate.pre
              else astate.pre
            in
            let post =
              PostDomain.update astate.post ~stack:post_stack ~heap:post_heap ~attrs:post_attrs
            in
            ({astate with post; pre}, addr_hist)
      in
      let astate =
        map_decompiler astate ~f:(fun decompiler ->
            Decompiler.add_var_source (fst addr_hist |> downcast) var decompiler )
      in
      (astate, addr_hist)


    let add var addr_hist astate =
      map_post_stack astate ~f:(fun stack -> BaseStack.add var addr_hist stack)


    let remove_vars vars astate =
      let vars_to_remove =
        let is_in_pre var astate = BaseStack.mem var (astate.pre :> base_domain).stack in
        List.filter vars ~f:(fun var -> not (is_in_pre var astate))
      in
      map_post_stack astate ~f:(fun stack ->
          BaseStack.filter (fun var _ -> not (List.mem ~equal:Var.equal vars_to_remove var)) stack )


    let fold_keys f astate accum =
      BaseStack.fold (fun v _ acc -> f v acc) (astate.post :> base_domain).stack accum


    let fold pre_or_post f astate accum =
      let stack =
        ( match pre_or_post with
        | `Pre ->
            (astate.pre :> base_domain)
        | `Post ->
            (astate.post :> base_domain) )
          .stack
      in
      BaseStack.fold
        (fun var addr_hist acc -> f var (CanonValue.canon_fst astate addr_hist) acc)
        stack accum


    let find_opt var astate =
      BaseStack.find_opt var (astate.post :> base_domain).stack |> CanonValue.canon_opt_fst astate


    let mem var astate = BaseStack.mem var (astate.post :> base_domain).stack

    let exists f astate =
      BaseStack.exists
        (fun var addr_hist -> f var (CanonValue.canon_fst astate addr_hist))
        (astate.post :> base_domain).stack


    let keys astate = fold_keys List.cons astate []
  end

  module SafeAttributes = struct
    open IResult.Let_syntax

    (** if [address] is in [pre] then add the attribute [attr] *)
    let abduce_one (address : CanonValue.t) attribute astate =
      let is_must_be_initialized_on_written =
        match (attribute : Attribute.t) with
        | MustBeInitialized _ ->
            BaseAddressAttributes.find_opt address (astate.post :> base_domain).attrs
            |> Option.exists ~f:(fun attrs -> Attributes.get_written_to attrs |> Option.is_some)
        | _ ->
            false
      in
      let new_pre =
        if is_must_be_initialized_on_written then (
          L.d_printfln "No abducing %a:%a, the address has been written" CanonValue.pp address
            Attribute.pp attribute ;
          astate.pre )
        else (
          L.d_printfln "Abducing %a:%a" CanonValue.pp address Attribute.pp attribute ;
          if BaseMemory.mem address (astate.pre :> base_domain).heap then
            PreDomain.update astate.pre
              ~attrs:
                (BaseAddressAttributes.add_one address attribute (astate.pre :> base_domain).attrs)
          else astate.pre )
      in
      if phys_equal new_pre astate.pre then astate else {astate with pre= new_pre}


    let abduce_all value attrs astate =
      Attributes.fold attrs ~init:astate ~f:(fun astate attr -> abduce_one value attr astate)


    (** [astate] with [astate.post.attrs = f astate.post.attrs] *)
    let map_post_attrs ~f astate =
      let new_post = PostDomain.update astate.post ~attrs:(f (astate.post :> base_domain).attrs) in
      if phys_equal new_post astate.post then astate else {astate with post= new_post}


    (** [astate] with [astate.pre.attrs = f astate.pre.attrs] *)
    let map_pre_attrs ~f astate =
      let new_pre = PreDomain.update astate.pre ~attrs:(f (astate.pre :> base_domain).attrs) in
      if phys_equal new_pre astate.pre then astate else {astate with pre= new_pre}


    let initialize address astate =
      map_post_attrs astate ~f:(fun attrs -> BaseAddressAttributes.initialize address attrs)


    let find_opt address astate =
      BaseAddressAttributes.find_opt address (astate.post :> base_domain).attrs


    let check_initialized path access_trace addr astate =
      let attrs = (astate.post :> base_domain).attrs in
      let+ () = BaseAddressAttributes.check_initialized addr attrs in
      let is_written_to =
        Option.exists (BaseAddressAttributes.find_opt addr attrs) ~f:(fun attrs ->
            Attribute.Attributes.get_written_to attrs |> Option.is_some )
      in
      if is_written_to then astate
      else abduce_one addr (MustBeInitialized (path.PathContext.timestamp, access_trace)) astate


    let add_taint_sink path (sink : TaintItem.t) trace addr astate =
      let taint_sink =
        Attribute.TaintSink.{time= path.PathContext.timestamp; sink= sink.value_tuple; trace}
      in
      let add_to_map map kind = Attribute.TaintSinkMap.add kind taint_sink map in
      let map = List.fold sink.TaintItem.kinds ~init:Attribute.TaintSinkMap.empty ~f:add_to_map in
      abduce_one addr (MustNotBeTainted map) astate


    let get_taint_sources_and_sanitizers addr astate =
      let attrs = (astate.post :> base_domain).attrs in
      match BaseAddressAttributes.find_opt addr attrs with
      | None ->
          (Attribute.TaintedSet.empty, Attribute.TaintSanitizedSet.empty)
      | Some addr_attrs ->
          ( Attribute.Attributes.get_tainted addr_attrs
          , Attribute.Attributes.get_taint_sanitized addr_attrs )


    let get_propagate_taint_from addr astate =
      let attrs = (astate.post :> base_domain).attrs in
      let open IOption.Let_syntax in
      let* addr_attrs = BaseAddressAttributes.find_opt addr attrs in
      Attribute.Attributes.get_propagate_taint_from addr_attrs


    let add_edge_on_src timestamp src location stack =
      match src with
      | `LocalDecl (pvar, addr_opt) -> (
        match addr_opt with
        | None ->
            let addr = CanonValue.mk_fresh () in
            let history = ValueHistory.singleton (VariableDeclared (pvar, location, timestamp)) in
            (BaseStack.add (Var.of_pvar pvar) (downcast addr, history) stack, addr)
        | Some addr ->
            (stack, addr) )
      | `Malloc addr ->
          (stack, addr)


    let rec set_uninitialized_post tenv timestamp src typ location ?(fields_prefix = RevList.empty)
        (post : PostDomain.t) =
      match typ.Typ.desc with
      | Tint _ | Tfloat _ | Tptr _ ->
          let {stack; attrs} = (post :> base_domain) in
          let stack, addr = add_edge_on_src timestamp src location stack in
          let attrs = BaseAddressAttributes.add_one addr (Uninitialized Value) attrs in
          PostDomain.update ~stack ~attrs post
      | Tstruct typ_name when UninitBlocklist.is_blocklisted_struct typ_name ->
          post
      | Tstruct (CUnion _) | Tstruct (CppClass {is_union= true}) ->
          (* Ignore union fields in the uninitialized checker *)
          post
      | Tstruct _ -> (
        match Typ.name typ |> Option.bind ~f:(Tenv.lookup tenv) with
        | None | Some {fields= [_]} ->
            (* Ignore single field structs: see D26146578 *)
            post
        | Some {fields} ->
            let stack, addr = add_edge_on_src timestamp src location (post :> base_domain).stack in
            let init = PostDomain.update ~stack post in
            List.fold fields ~init
              ~f:(fun (acc : PostDomain.t) {Struct.name= field; typ= field_typ} ->
                if Fieldname.is_internal field || Fieldname.is_capture_field_in_closure field then
                  acc
                else
                  let field_addr = CanonValue.mk_fresh () in
                  let fields = RevList.cons field fields_prefix in
                  let history =
                    ValueHistory.singleton (StructFieldAddressCreated (fields, location, timestamp))
                  in
                  let heap =
                    BaseMemory.add_edge addr (FieldAccess field)
                      (downcast field_addr, history)
                      (acc :> base_domain).heap
                  in
                  PostDomain.update ~heap acc
                  |> set_uninitialized_post tenv timestamp (`Malloc field_addr) field_typ location
                       ~fields_prefix:fields ) )
      | Tarray _ | Tvoid | Tfun | TVar _ ->
          (* We ignore tricky types to mark uninitialized addresses. *)
          post


    let invalidate address invalidation location astate =
      map_post_attrs ~f:(BaseAddressAttributes.invalidate address invalidation location) astate


    let always_reachable address astate =
      map_post_attrs astate ~f:(BaseAddressAttributes.always_reachable address)


    let allocate allocator address location astate =
      map_post_attrs astate ~f:(BaseAddressAttributes.allocate allocator address location)


    let java_resource_release address astate =
      map_post_attrs astate ~f:(BaseAddressAttributes.java_resource_release address)


    let hack_async_await address astate =
      map_post_attrs astate ~f:(BaseAddressAttributes.hack_async_await address)


    let remove_hack_builder address astate =
      map_post_attrs astate ~f:(BaseAddressAttributes.remove_hack_builder address)


    let set_hack_builder address builderstate astate =
      map_post_attrs astate ~f:(BaseAddressAttributes.set_hack_builder address builderstate)


    let get_hack_builder address astate =
      BaseAddressAttributes.get_hack_builder address (astate.post :> base_domain).attrs


    let csharp_resource_release address astate =
      map_post_attrs astate ~f:(BaseAddressAttributes.csharp_resource_release address)


    let in_reported_retain_cycle address astate =
      map_post_attrs astate ~f:(BaseAddressAttributes.in_reported_retain_cycle address)


    let get_static_type addr astate =
      BaseAddressAttributes.get_static_type (astate.post :> base_domain).attrs addr


    let get_allocation addr astate =
      BaseAddressAttributes.get_allocation addr (astate.post :> base_domain).attrs


    let get_copied_into addr astate =
      BaseAddressAttributes.get_copied_into addr (astate.post :> base_domain).attrs


    let get_copied_return addr astate =
      BaseAddressAttributes.get_copied_return addr (astate.post :> base_domain).attrs


    let remove_copied_return addr astate =
      map_post_attrs astate ~f:(BaseAddressAttributes.remove_copied_return addr)


    let get_must_be_valid addr astate =
      BaseAddressAttributes.get_must_be_valid addr (astate.pre :> base_domain).attrs


    let get_source_origin_of_copy addr astate =
      BaseAddressAttributes.get_source_origin_of_copy addr (astate.post :> base_domain).attrs


    let add_dict_contain_const_keys address astate =
      map_post_attrs astate ~f:(BaseAddressAttributes.add_dict_contain_const_keys address)


    let remove_dict_contain_const_keys address astate =
      map_post_attrs astate ~f:(BaseAddressAttributes.remove_dict_contain_const_keys address)


    let is_dict_contain_const_keys addr astate =
      BaseAddressAttributes.is_dict_contain_const_keys addr (astate.post :> base_domain).attrs


    let add_dict_read_const_key timestamp trace address key astate =
      map_pre_attrs astate
        ~f:(BaseAddressAttributes.add_dict_read_const_key timestamp trace address key)


    let add_static_type typ address astate =
      map_post_attrs astate ~f:(BaseAddressAttributes.add_static_type typ address)


    let remove_allocation_attr address astate =
      map_post_attrs astate ~f:(BaseAddressAttributes.remove_allocation_attr address)


    let remove_taint_attrs address astate =
      map_post_attrs astate ~f:(BaseAddressAttributes.remove_taint_attrs address)


    let remove_all_must_not_be_tainted ?kinds astate =
      let astate =
        map_post_attrs astate ~f:(BaseAddressAttributes.remove_all_must_not_be_tainted ?kinds)
      in
      map_pre_attrs astate ~f:(BaseAddressAttributes.remove_all_must_not_be_tainted ?kinds)


    let get_closure_proc_name addr astate =
      BaseAddressAttributes.get_closure_proc_name addr (astate.post :> base_domain).attrs


    let std_vector_reserve addr astate =
      map_post_attrs astate ~f:(BaseAddressAttributes.std_vector_reserve addr)


    let is_java_resource_released addr astate =
      BaseAddressAttributes.is_java_resource_released addr (astate.post :> base_domain).attrs


    let is_in_reported_retain_cycle addr astate =
      BaseAddressAttributes.is_in_reported_retain_cycle addr (astate.post :> base_domain).attrs


    let is_csharp_resource_released addr astate =
      BaseAddressAttributes.is_csharp_resource_released addr (astate.post :> base_domain).attrs


    let is_std_vector_reserved addr astate =
      BaseAddressAttributes.is_std_vector_reserved addr (astate.post :> base_domain).attrs


    let get_last_lookup addr astate =
      BaseAddressAttributes.get_last_lookup addr (astate.post :> base_domain).attrs


    let mark_as_end_of_collection addr astate =
      map_post_attrs astate ~f:(BaseAddressAttributes.mark_as_end_of_collection addr)


    let add_unreachable_at addr location astate =
      map_post_attrs astate ~f:(BaseAddressAttributes.add_unreachable_at addr location)


    let is_end_of_collection addr astate =
      BaseAddressAttributes.is_end_of_collection addr (astate.post :> base_domain).attrs


    let add_copied_return addr ~source ~is_const_ref from location astate =
      map_post_attrs astate
        ~f:(BaseAddressAttributes.add_copied_return addr ~source ~is_const_ref from location)


    let get_config_usage addr astate =
      BaseAddressAttributes.get_config_usage addr (astate.post :> base_domain).attrs


    let get_valid_returned_from_unknown addr astate =
      let open IOption.Let_syntax in
      let+ returned_from =
        BaseAddressAttributes.get_returned_from_unknown addr (astate.post :> base_domain).attrs
      in
      List.filter
        ~f:(fun addr ->
          let addr = CanonValue.canon' astate addr in
          BaseAddressAttributes.check_valid addr (astate.post :> base_domain).attrs |> is_ok )
        returned_from


    let get_written_to addr astate =
      BaseAddressAttributes.get_written_to addr (astate.post :> base_domain).attrs


    let is_copied_from_const_ref addr astate =
      BaseAddressAttributes.is_copied_from_const_ref addr (astate.post :> base_domain).attrs


    let is_std_moved addr astate =
      BaseAddressAttributes.is_std_moved addr (astate.post :> base_domain).attrs


    let get_address_of_stack_variable addr astate =
      BaseAddressAttributes.get_address_of_stack_variable addr (astate.post :> base_domain).attrs


    let add_one value (attribute : Attribute.t) astate =
      let astate = match attribute with WrittenTo _ -> initialize value astate | _ -> astate in
      map_post_attrs astate ~f:(BaseAddressAttributes.add_one value attribute)


    let add_all value attrs astate =
      Attributes.fold attrs ~init:astate ~f:(fun astate attr -> add_one value attr astate)


    let check_valid path ?must_be_valid_reason access_trace addr astate =
      let+ () = BaseAddressAttributes.check_valid addr (astate.post :> base_domain).attrs in
      (* if [address] is in [pre] and it should be valid then that fact goes in the precondition *)
      abduce_one addr
        (MustBeValid (path.PathContext.timestamp, access_trace, must_be_valid_reason))
        astate


    let has_unknown_effect addr astate =
      BaseAddressAttributes.has_unknown_effect addr (astate.post :> base_domain).attrs


    let is_hack_sinit_called addr astate =
      BaseAddressAttributes.is_hack_sinit_called addr (astate.post :> base_domain).attrs
  end

  module SafeMemory = struct
    module Access = BaseMemory.Access

    (** [astate] with [astate.post.heap = f astate.post.heap] *)
    let map_post_heap ~f astate =
      let new_post = PostDomain.update astate.post ~heap:(f (astate.post :> base_domain).heap) in
      if phys_equal new_post astate.post then astate else {astate with post= new_post}


    let add_edge {PathContext.timestamp} (addr, history) access new_addr_hist location astate =
      map_post_heap astate ~f:(BaseMemory.add_edge addr access new_addr_hist)
      |> SafeAttributes.map_post_attrs
           ~f:
             (BaseAddressAttributes.add_one addr
                (WrittenTo (timestamp, Trace.Immediate {location; history})) )


    let find_edge_opt address access astate =
      let get_var_repr v = Formula.get_var_repr astate.path_condition v in
      BaseMemory.find_edge_opt ~get_var_repr address access (astate.post :> base_domain).heap
      |> CanonValue.canon_opt_fst astate


    let eval_edge (addr_src, hist_src) access astate =
      let astate, addr_hist_dst =
        match find_edge_opt addr_src access astate with
        | Some addr_hist_dst ->
            (astate, addr_hist_dst)
        | None ->
            let addr_dst = CanonValue.mk_fresh () in
            let pre_heap, post_hist =
              if BaseMemory.mem addr_src (astate.pre :> base_domain).heap then
                let cell_id = ValueHistory.CellId.next () in
                (* HACK: do not record the history of values in the pre as they are unused, except
                   for their cell id to be able to track where the pre values end up in the post
                   condition. *)
                ( BaseMemory.add_edge addr_src access
                    (downcast addr_dst, ValueHistory.from_cell_id cell_id ValueHistory.epoch)
                    (astate.pre :> base_domain).heap
                  |> BaseMemory.register_address addr_dst
                , ValueHistory.from_cell_id cell_id hist_src )
              else ((astate.pre :> base_domain).heap, hist_src)
            in
            let addr_hist_dst = (addr_dst, post_hist) in
            let post_heap =
              BaseMemory.add_edge addr_src access (downcast_fst addr_hist_dst)
                (astate.post :> base_domain).heap
            in
            let astate =
              (* This is the first time we are accessing addr_dst. Because it is accessed
                 via addr_src, it should be tainted the same as is addr_src. This is to be
                 consistent with how sources work: if addr_dst would have been accessed
                 before tainting addr_scr, then it would have been tainted as well *)
              let taint_sources, taint_sanitizers =
                SafeAttributes.get_taint_sources_and_sanitizers addr_src astate
              in
              let conditionally_add condition attr astate =
                if condition then SafeAttributes.add_one addr_dst attr astate else astate
              in
              let open Attribute in
              conditionally_add
                (not (TaintedSet.is_empty taint_sources))
                (Tainted taint_sources) astate
              |> conditionally_add
                   (not (TaintSanitizedSet.is_empty taint_sanitizers))
                   (TaintSanitized taint_sanitizers)
            in
            ( { astate with
                post= PostDomain.update astate.post ~heap:post_heap
              ; pre= PreDomain.update astate.pre ~heap:pre_heap }
            , addr_hist_dst )
      in
      let astate =
        map_decompiler astate ~f:(fun decompiler ->
            Decompiler.add_access_source
              (fst addr_hist_dst |> downcast)
              (downcast_access access) ~src:(downcast addr_src) decompiler )
      in
      (astate, addr_hist_dst)


    let fold_edges pre_or_post address astate ~init ~f =
      let heap =
        match pre_or_post with
        | `Pre ->
            (astate.pre :> base_domain).heap
        | `Post ->
            (astate.post :> base_domain).heap
      in
      SafeBaseMemory.fold_edges astate address heap ~init ~f


    let iter_edges pre_or_post v astate ~f =
      Container.iter ~fold:(fold_edges pre_or_post v) astate ~f


    let exists_edge pre_or_post v astate ~f =
      Container.exists ~iter:(iter_edges pre_or_post v) astate ~f
  end

  let add_static_type tenv typ_name addr location astate =
    let is_final =
      Tenv.lookup tenv typ_name
      |> Option.value_map ~default:false ~f:(fun {Struct.annots} -> Annot.Item.is_final annots)
    in
    if is_final then
      let phi' =
        PulseFormula.add_dynamic_type_unsafe (downcast addr) (Typ.mk_struct typ_name) location
          astate.path_condition
      in
      set_path_condition phi' astate
    else SafeAttributes.add_static_type typ_name addr astate


  let find_cell_opt addr domain =
    match
      (BaseMemory.find_opt addr domain.heap, BaseAddressAttributes.find_opt addr domain.attrs)
    with
    | None, None ->
        None
    (* the following two cases can happen because of [register_address] or because we don't always
       take care to delete empty edges when removing edges (same for attributes) *)
    | Some edges, None when BaseMemory.Edges.is_empty edges ->
        None
    | None, Some attrs when Attributes.is_empty attrs ->
        None
    | edges_opt, attrs_opt ->
        (* at least one of these is not None and not empty *)
        Some
          ( Option.value edges_opt ~default:BaseMemory.Edges.empty
          , Option.value attrs_opt ~default:Attributes.empty )


  let add_static_types tenv location astate formals_and_captured =
    let record_static_type astate (_var, typ, _, (src_addr, src_addr_hist)) =
      match typ with
      | {Typ.desc= Tptr ({desc= Tstruct typ_name}, _)}
        when Typ.Name.is_objc_class typ_name || Typ.Name.is_hack_class typ_name
             || Typ.Name.is_python_class typ_name ->
          let pre_heap = (astate.pre :> BaseDomain.t).heap in
          let post_heap = (astate.post :> BaseDomain.t).heap in
          let addr = CanonValue.mk_fresh () in
          (* safe because this is for creating the initial state only *)
          let[@alert "-deprecated"] src_addr = CanonValue.unsafe_cast src_addr in
          let pre_heap =
            BaseMemory.add_edge src_addr Dereference (downcast addr, src_addr_hist) pre_heap
            |> BaseMemory.register_address addr
          in
          let post_heap =
            BaseMemory.add_edge src_addr Dereference (downcast addr, src_addr_hist) post_heap
          in
          let astate =
            { astate with
              pre= PreDomain.update ~heap:pre_heap astate.pre
            ; post= PostDomain.update ~heap:post_heap astate.post }
          in
          add_static_type tenv typ_name addr location astate
      | _ ->
          astate
    in
    List.fold formals_and_captured ~init:astate ~f:record_static_type


  let is_local var astate = not (Var.is_return var || SafeStack.is_abducible astate var)

  (** Inside a function, formals can be used as local variables. But when exiting the function, the
      initial values of formals must be restored. This recreates the graph structure from addresses
      of formals in the precondition into the post-condition, up to the first dereference. The
      reason for this complexity is purely to handle formals that are struct values. For example, if
      [pre=&x=vx, vx --.f-> v1 --.g-> v2 --*-> v3] and
      [post = &x=vx, vx --.f-> v1 --.g-> v2 --*-> v4, vx --.h->v5] then we change [post] to
      [vx --.f-> v1 --.g-> v2 --*-> v3] (i.e. [post]=[pre]). *)
  let restore_formals_for_summary astate =
    (* The [visited] accumulator is to avoid cycles *)
    let rec restore_pre_var_value visited ~is_value_visible_outside (addr : CanonValue.t) astate =
      if AbstractValue.Set.mem (downcast addr) visited then astate
      else
        let visited = AbstractValue.Set.add (downcast addr) visited in
        let pre_heap = (astate.pre :> BaseDomain.t).heap in
        let post_heap = (astate.post :> BaseDomain.t).heap in
        match BaseMemory.find_opt addr pre_heap with
        | None ->
            if is_value_visible_outside then astate
            else
              SafeMemory.map_post_heap astate ~f:(fun post_heap -> BaseMemory.remove addr post_heap)
        | Some pre_edges ->
            BaseMemory.Edges.fold pre_edges ~init:astate ~f:(fun astate (access, addr_hist_dest) ->
                let ((addr_dest, _) as addr_hist_dest) =
                  CanonValue.canon_fst astate addr_hist_dest
                in
                match access with
                | Dereference ->
                    (* dereference: we reached the actual value and are done *)
                    if is_value_visible_outside && BaseMemory.has_edge addr access post_heap then
                      (* the changes are visible even outside of the procedure: do not restore the pre value
                         if the post has a value for this access too *)
                      astate
                    else
                      SafeMemory.map_post_heap astate ~f:(fun post_heap ->
                          BaseMemory.add_edge addr access (downcast_fst addr_hist_dest) post_heap )
                | FieldAccess _ | ArrayAccess _ ->
                    (* inlined aggregate value: just an offset, not the actual value yet; recurse *)
                    SafeMemory.map_post_heap astate ~f:(fun post_heap ->
                        BaseMemory.add_edge addr access (downcast_fst addr_hist_dest) post_heap )
                    |> restore_pre_var_value visited ~is_value_visible_outside addr_dest )
    in
    let restore_pre_var x addr_hist astate =
      (* the address of a variable never changes *)
      let ((addr, _) as addr_hist) = CanonValue.canon_fst astate addr_hist in
      let astate = SafeStack.add x (downcast_fst addr_hist) astate in
      restore_pre_var_value AbstractValue.Set.empty
        ~is_value_visible_outside:(Var.is_global x || Var.is_return x)
        addr astate
    in
    let post_stack =
      BaseStack.filter
        (fun var _ -> Var.appears_in_source_code var && not (is_local var astate))
        (astate.post :> BaseDomain.t).stack
    in
    BaseStack.fold restore_pre_var (astate.pre :> BaseDomain.t).stack
      {astate with post= PostDomain.update ~stack:post_stack astate.post}


  let add_out_of_scope_attribute addr pvar location history heap typ =
    BaseAddressAttributes.add_one addr
      (Invalid (GoneOutOfScope (pvar, typ), Immediate {location; history}))
      heap


  (** invalidate local variables going out of scope *)
  let invalidate_locals locals astate : t =
    let attrs : BaseAddressAttributes.t = (astate.post :> BaseDomain.t).attrs in
    let attrs' =
      BaseAddressAttributes.fold
        (fun addr attrs acc ->
          Attributes.get_address_of_stack_variable attrs
          |> Option.value_map ~default:acc ~f:(fun (var, location, history) ->
                 let get_local_typ_opt pvar =
                   List.find_map locals ~f:(fun ProcAttributes.{name; typ; modify_in_block} ->
                       if (not modify_in_block) && Mangled.equal name (Pvar.get_name pvar) then
                         Some typ
                       else None )
                 in
                 match var with
                 | Var.ProgramVar pvar when not (Pvar.is_artificial pvar) ->
                     get_local_typ_opt pvar
                     |> Option.value_map ~default:acc
                          ~f:(add_out_of_scope_attribute addr pvar location history acc)
                 | _ ->
                     acc ) )
        attrs attrs
    in
    if phys_equal attrs attrs' then astate
    else {astate with post= PostDomain.update astate.post ~attrs:attrs'}


  let is_heap_allocated {post; pre} v =
    RawMemory.is_allocated (post :> BaseDomain.t).heap v
    || RawMemory.is_allocated (pre :> BaseDomain.t).heap v


  let is_stack_allocated stack_allocations v =
    AbstractValue.Set.mem v (Lazy.force stack_allocations)


  let subst_var subst astate =
    let open SatUnsat.Import in
    let* post = PostDomain.subst_var ~for_summary:false subst astate.post in
    let+ pre = PreDomain.subst_var ~for_summary:false subst astate.pre in
    if phys_equal astate.post post && phys_equal astate.pre pre then astate
    else {astate with pre; post}


  let get_stack_allocated astate =
    (* OPTIM: the post stack contains at least the pre stack so no need to check both *)
    SafeStack.fold `Post
      (fun var (address, _) allocated ->
        if Var.is_pvar var then AbstractValue.Set.add (downcast address) allocated else allocated )
      astate AbstractValue.Set.empty


  let check_new_eqs (eqs : Formula.new_eq list) =
    let module V = struct
      module K = struct
        type t = AbstractValue.t option [@@deriving compare, equal]

        let pp = Fmt.option ~none:(Fmt.any "0") AbstractValue.pp
      end

      include K
      module Map = PrettyPrintable.MakePPMap (K)
    end in
    (* We build a more uniform representation of [new_eq]; [None] represents 0. *)
    let eqs =
      let f (e : Formula.new_eq) =
        match e with Equal (a, b) -> (Some a, Some b) | EqZero a -> (Some a, None)
      in
      List.map ~f eqs
    in
    let values =
      eqs |> List.concat_map ~f:(fun (a, b) -> [a; b]) |> List.dedup_and_sort ~compare:V.compare
    in
    (* We simulate left-to-right substitution. A pair [(a,b)] in [substs] means that [a] becomes [b]
     * after substitution. *)
    let substs =
      let init = List.map ~f:(fun x -> (x, x)) values in
      let apply_subst substs (a, b) =
        List.map ~f:(function c, d when V.equal d a -> (c, b) | s -> s) substs
      in
      List.fold ~init ~f:apply_subst eqs
    in
    (* And then we treat equalities as equalities, using union-find to find a rep. *)
    let cls =
      List.fold ~init:V.Map.empty ~f:(fun m v -> V.Map.add v (Union_find.create v) m) values
    in
    List.iter ~f:(function a, b -> Union_find.union (V.Map.find a cls) (V.Map.find b cls)) eqs ;
    (* Finally, we check that for each pair of values the two approaches agree. *)
    let pairs = List.cartesian_product values values in
    List.iter pairs ~f:(function a, b ->
        let equal_in_subst =
          let aa = List.Assoc.find_exn ~equal:V.equal substs a in
          let bb = List.Assoc.find_exn ~equal:V.equal substs b in
          V.equal aa bb
        in
        let equal_in_uf =
          let aa = V.Map.find a cls in
          let bb = V.Map.find b cls in
          Union_find.same_class aa bb
        in
        if Bool.(equal_in_subst <> equal_in_uf) then (
          F.fprintf F.str_formatter "@[<v>" ;
          List.iter eqs ~f:(function x, y ->
              F.fprintf F.str_formatter "@[%a -> %a@]@;" V.pp x V.pp y ) ;
          F.fprintf F.str_formatter "@[See values %a %a: equal_in_subst=%b equal_in_uf=%b@]@;@]"
            V.pp a V.pp b equal_in_subst equal_in_uf ;
          L.die InternalError "%s" (F.flush_str_formatter ()) ) )


  let incorporate_new_eqs astate new_eqs =
    let new_eqs = RevList.to_list new_eqs in
    if Config.pulse_sanity_checks then check_new_eqs new_eqs ;
    let stack_allocations = lazy (get_stack_allocated astate) in
    List.fold_until new_eqs ~init:(astate, None)
      ~finish:(fun astate_error -> Sat astate_error)
      ~f:(fun (astate, error) (new_eq : PulseFormula.new_eq) ->
        L.d_printfln "incorporating new eq: %a" PulseFormula.pp_new_eq new_eq ;
        match new_eq with
        | Equal (v1, v2) when AbstractValue.equal v1 v2 ->
            Continue (astate, error)
        | Equal (v1, v2) -> (
          match subst_var (v1, v2) astate with
          | Unsat ->
              Stop Unsat
          | Sat astate' ->
              Continue (astate', error) )
        | EqZero v when is_stack_allocated stack_allocations v ->
            L.d_printfln "CONTRADICTION: %a = 0 but is allocated" AbstractValue.pp v ;
            Stop Unsat
        | EqZero v when is_heap_allocated astate v -> (
            (* We want to detect when we know [v|->-] and [v=0] together. This is a contradiction, but
               can also be the sign that there is a real issue. Consider:

                 foo(p) {
                   if(p) {}
                   *p = 42;
                 }

               This creates two paths:
                 p=0 /\ p|->-
                 p≠0 /\ p|->-

               We could discard the first one straight away because it's equivalent to false, but
               the second one will also get discarded when calling this foo(0) because it will fail
               the p≠0 condition, creating FNs. Thus, we create a latent issue instead to report
               on foo(0) calls.

               An alternative design that would be less hacky would be to detect this situation when
               adding pointers from null values, but we'd need to know what values are null at all
               times. This would require normalizing the arithmetic part at each step, which is too
               expensive. *)
            L.d_printfln ~color:Red "Potential ERROR: %a = 0 but is allocated" AbstractValue.pp v ;
            (* unsafe cast safe because these come from the formula *)
            match
              SafeAttributes.get_must_be_valid
                (CanonValue.unsafe_cast v [@alert "-deprecated"])
                astate
            with
            | None ->
                (* we don't know why [v|->-] is in the state, weird and probably cannot happen; drop
                   the path because we won't be able to give a sensible error *)
                L.d_printfln "Not clear why %a should be allocated in the first place, giving up"
                  AbstractValue.pp v ;
                Stop Unsat
            | Some (_, trace, reason_opt) ->
                Stop (Sat (astate, Some (v, (trace, reason_opt)))) )
        | EqZero _ (* [v] not allocated *) ->
            Continue (astate, error) )


  (** it's a good idea to normalize the path condition before calling this function *)
  let canonicalize location astate =
    let remove_taint_attrs_from_opaque_files attrs location =
      let is_opaque_for_taint (location : Location.t) =
        let file = location.file in
        let path = SourceFile.to_abs_path file in
        List.mem Config.pulse_taint_opaque_files ~equal:String.equal path
      in
      if is_opaque_for_taint location then
        PulseBaseAddressAttributes.remove_all_taint_related_attrs attrs
      else attrs
    in
    let open SatUnsat.Import in
    let get_var_repr v = Formula.get_var_repr astate.path_condition v in
    let canonicalize_pre (pre : PreDomain.t) =
      (* (ab)use canonicalization to filter out empty edges in the heap and detect aliasing
         contradictions *)
      let* stack' = RawStack.canonicalize ~get_var_repr (pre :> BaseDomain.t).stack in
      let+ heap' = RawMemory.canonicalize ~get_var_repr (pre :> BaseDomain.t).heap in
      let attrs' =
        PulseBaseAddressAttributes.make_suitable_for_pre_summary (pre :> BaseDomain.t).attrs
      in
      let attrs' = remove_taint_attrs_from_opaque_files attrs' location in
      PreDomain.update ~stack:stack' ~heap:heap' ~attrs:attrs' pre
    in
    let canonicalize_post (post : PostDomain.t) =
      let* stack' = RawStack.canonicalize ~get_var_repr (post :> BaseDomain.t).stack in
      (* note: this step also de-registers addresses pointing to empty edges *)
      let+ heap' = RawMemory.canonicalize ~get_var_repr (post :> BaseDomain.t).heap in
      let attrs' =
        PulseBaseAddressAttributes.canonicalize_post ~get_var_repr (post :> BaseDomain.t).attrs
      in
      let attrs' = remove_taint_attrs_from_opaque_files attrs' location in
      PostDomain.update ~stack:stack' ~heap:heap' ~attrs:attrs' post
    in
    let* pre = canonicalize_pre astate.pre in
    let+ post = canonicalize_post astate.post in
    {astate with pre; post}


  (** comparison between two elements of the domain to determine the [<=] relation

      Given two states [lhs] and [rhs], try to find a bijection [lhs_to_rhs] (with inverse
      [rhs_to_lhs]) between the addresses of [lhs] and [rhs] such that
      [lhs_to_rhs(reachable(lhs)) = reachable(rhs)] (where addresses are reachable if they are
      reachable from stack variables). *)
  module GraphComparison = struct
    module AddressMap = PrettyPrintable.MakePPMap (CanonValue)

    (** translation between the abstract values on the LHS and the ones on the RHS *)
    type mapping =
      { rhs_to_lhs: CanonValue.t AddressMap.t  (** map from RHS values to LHS *)
      ; lhs_to_rhs: CanonValue.t AddressMap.t  (** inverse map from [rhs_to_lhs] *) }

    let empty_mapping = {rhs_to_lhs= AddressMap.empty; lhs_to_rhs= AddressMap.empty}

    let pp_mapping fmt {rhs_to_lhs; lhs_to_rhs} =
      F.fprintf fmt "@[<v>{ rhs_to_lhs=@[<hv2>%a@];@,lhs_to_rhs=@[<hv2>%a@];@,}@]"
        (AddressMap.pp ~pp_value:CanonValue.pp)
        rhs_to_lhs
        (AddressMap.pp ~pp_value:CanonValue.pp)
        lhs_to_rhs


    (** try to add the fact that [addr_lhs] corresponds to [addr_rhs] to the [mapping] *)
    let record_equal ~addr_lhs ~addr_rhs mapping =
      (* have we seen [addr_lhs] before?.. *)
      match AddressMap.find_opt addr_lhs mapping.lhs_to_rhs with
      | Some addr_rhs' when not (CanonValue.equal addr_rhs addr_rhs') ->
          (* ...yes, but it was bound to another address *)
          L.d_printfln
            "Aliasing in LHS not in RHS: LHS address %a in current already bound to %a, not %a@\n\
             State=%a"
            CanonValue.pp addr_lhs CanonValue.pp addr_rhs' CanonValue.pp addr_rhs pp_mapping mapping ;
          `AliasingLHS
      | Some _addr_rhs (* [_addr_rhs = addr_rhs] *) ->
          `AlreadyVisited
      | None -> (
        (* ...and have we seen [addr_rhs] before?.. *)
        match AddressMap.find_opt addr_rhs mapping.rhs_to_lhs with
        | Some addr_lhs' ->
            (* ...yes, but it was bound to another address: [addr_lhs' != addr_lhs] otherwise we would
               have found [addr_lhs] in the [lhs_to_rhs] map above *)
            L.d_printfln
              "Aliasing in RHS not in LHS: RHS address %a in current already bound to %a, not %a@\n\
               State=%a"
              CanonValue.pp addr_rhs CanonValue.pp addr_lhs' CanonValue.pp addr_lhs pp_mapping
              mapping ;
            `AliasingRHS
        | None ->
            (* [addr_rhs] and [addr_lhs] are both new, record that they correspond to each other *)
            let mapping' =
              { rhs_to_lhs= AddressMap.add addr_rhs addr_lhs mapping.rhs_to_lhs
              ; lhs_to_rhs= AddressMap.add addr_lhs addr_rhs mapping.lhs_to_rhs }
            in
            `NotAlreadyVisited mapping' )


    type isograph_relation =
      | NotIsomorphic  (** no mapping was found that can make LHS the same as the RHS *)
      | IsomorphicUpTo of mapping  (** [mapping(lhs)] is isomorphic to [rhs] *)

    (** can we extend [mapping] so that the subgraph of [lhs] rooted at [addr_lhs] is isomorphic to
        the subgraph of [rhs] rooted at [addr_rhs]? *)
    let rec isograph_map_from_address ~astate_lhs ~lhs ~addr_lhs ~astate_rhs ~rhs ~addr_rhs mapping
        =
      L.d_printfln "%a<->%a@\n" CanonValue.pp addr_lhs CanonValue.pp addr_rhs ;
      match record_equal mapping ~addr_lhs ~addr_rhs with
      | `AlreadyVisited ->
          IsomorphicUpTo mapping
      | `AliasingRHS | `AliasingLHS ->
          NotIsomorphic
      | `NotAlreadyVisited mapping -> (
          let get_non_empty_cell addr astate =
            find_cell_opt addr astate
            |> Option.filter ~f:(fun (edges, attrs) ->
                   not (BaseMemory.Edges.is_empty edges && Attributes.is_empty attrs)
                   (* this can happen because of [register_address] or because we don't care to delete empty
                      edges when removing edges *) )
          in
          let lhs_cell_opt = get_non_empty_cell addr_lhs lhs in
          let rhs_cell_opt = get_non_empty_cell addr_rhs rhs in
          match (lhs_cell_opt, rhs_cell_opt) with
          | None, None ->
              IsomorphicUpTo mapping
          | Some _, None | None, Some _ ->
              NotIsomorphic
          | Some (edges_lhs, attrs_lhs), Some (edges_rhs, attrs_rhs) ->
              (* continue the comparison recursively on all edges and attributes *)
              if Attributes.equal attrs_rhs attrs_lhs then
                let bindings_lhs = RawMemory.Edges.bindings edges_lhs in
                let bindings_rhs = RawMemory.Edges.bindings edges_rhs in
                isograph_map_edges ~astate_lhs ~lhs ~edges_lhs:bindings_lhs ~astate_rhs ~rhs
                  ~edges_rhs:bindings_rhs mapping
              else NotIsomorphic )


    (** check that the isograph relation can be extended for all edges *)
    and isograph_map_edges ~astate_lhs ~lhs ~edges_lhs ~astate_rhs ~rhs ~edges_rhs mapping =
      match (edges_lhs, edges_rhs) with
      | [], [] ->
          IsomorphicUpTo mapping
      | (a_lhs, (addr_lhs, _trace_lhs)) :: edges_lhs, (a_rhs, (addr_rhs, _trace_rhs)) :: edges_rhs
        ->
          let access_lhs = CanonValue.canon_access astate_lhs a_lhs in
          let access_rhs = CanonValue.canon_access astate_rhs a_rhs in
          if SafeMemory.Access.equal access_lhs access_rhs then
            (* check isograph relation from the destination addresses *)
            let addr_lhs = CanonValue.canon' astate_lhs addr_lhs in
            let addr_rhs = CanonValue.canon' astate_rhs addr_rhs in
            match
              isograph_map_from_address ~astate_lhs ~lhs ~addr_lhs ~astate_rhs ~rhs ~addr_rhs
                mapping
            with
            | IsomorphicUpTo mapping ->
                (* ok: continue with the other edges *)
                isograph_map_edges ~astate_lhs ~lhs ~edges_lhs ~astate_rhs ~rhs ~edges_rhs mapping
            | NotIsomorphic ->
                NotIsomorphic
          else NotIsomorphic
      | [], _ :: _ | _ :: _, [] ->
          NotIsomorphic


    (** check that the memory graph induced by the addresses in [lhs] reachable from the variables
        in [stack_lhs] is a isograph of the same graph in [rhs] starting from [stack_rhs], up to
        some [mapping] *)
    let rec isograph_map_from_stack ~astate_lhs ~lhs ~stack_lhs ~astate_rhs ~rhs ~stack_rhs mapping
        =
      match (stack_lhs, stack_rhs) with
      | [], [] ->
          IsomorphicUpTo mapping
      | ( (var_lhs, (addr_lhs, _trace_lhs)) :: stack_lhs
        , (var_rhs, (addr_rhs, _trace_rhs)) :: stack_rhs )
        when Var.equal var_lhs var_rhs -> (
          let addr_lhs = CanonValue.canon' astate_lhs addr_lhs in
          let addr_rhs = CanonValue.canon' astate_rhs addr_rhs in
          match
            isograph_map_from_address ~astate_lhs ~lhs ~addr_lhs ~astate_rhs ~rhs ~addr_rhs mapping
          with
          | IsomorphicUpTo mapping ->
              isograph_map_from_stack ~astate_lhs ~lhs ~stack_lhs ~astate_rhs ~rhs ~stack_rhs
                mapping
          | NotIsomorphic ->
              NotIsomorphic )
      | _ :: _, _ :: _ | [], _ :: _ | _ :: _, [] ->
          NotIsomorphic


    let isograph_map ~astate_lhs ~lhs ~astate_rhs ~rhs mapping =
      let stack_lhs = RawStack.bindings lhs.stack in
      let stack_rhs = RawStack.bindings rhs.stack in
      isograph_map_from_stack ~astate_lhs ~astate_rhs ~lhs ~rhs ~stack_lhs ~stack_rhs mapping


    let is_isograph ~astate_lhs ~lhs ~astate_rhs ~rhs mapping =
      match isograph_map ~astate_lhs ~lhs ~astate_rhs ~rhs mapping with
      | IsomorphicUpTo _ ->
          true
      | NotIsomorphic ->
          false
  end

  module GraphVisit : sig
    val fold :
         var_filter:(Var.t -> bool)
      -> ?edge_filter:(SafeMemory.Access.t -> bool)
      -> init:'accum
      -> f:
           (   Var.t
            -> 'accum
            -> CanonValue.t
            -> RawMemory.Access.t list
            -> ('accum, 'final) Continue_or_stop.t )
      -> ?f_revisit:(Var.t -> 'accum -> CanonValue.t -> RawMemory.Access.t list -> 'accum)
      -> finish:('accum -> 'final)
      -> t
      -> [`Pre | `Post]
      -> CanonValue.Set.t * 'final
    (** Generic graph traversal of the memory starting from each variable in the stack that pass
        [var_filter], in order. Returns the result of folding over every address in the graph and
        the set of addresses that have been visited before [f] returned [Stop] or all reachable
        addresses were seen. [f] is passed each address together with the variable from which the
        address was reached and the access path from that variable to the address. *)

    val fold_from_addresses :
         ?edge_filter:(SafeMemory.Access.t -> bool)
      -> already_visited:CanonValue.Set.t
      -> init:'accum
      -> f:('accum -> CanonValue.t -> RawMemory.Access.t list -> ('accum, 'final) Continue_or_stop.t)
      -> ?f_revisit:('accum -> CanonValue.t -> RawMemory.Access.t list -> 'accum)
      -> finish:('accum -> 'final)
      -> CanonValue.t Seq.t
      -> t
      -> [`Pre | `Post]
      -> CanonValue.Set.t * 'final
    (** Similar to [fold], but start from given addresses, instead of stack variables. Use
        already_visited as initial set of visited values. *)
  end = struct
    open Continue_or_stop

    let visit address visited =
      if CanonValue.Set.mem address visited then `AlreadyVisited
      else
        let visited = CanonValue.Set.add address visited in
        `NotAlreadyVisited visited


    let rec visit_address ~edge_filter address ~f ~f_revisit rev_accesses astate heap
        (visited, accum) =
      match visit address visited with
      | `AlreadyVisited ->
          let accum =
            f_revisit accum address
              (rev_accesses : BaseMemory.Access.t list :> RawMemory.Access.t list)
          in
          Continue (visited, accum)
      | `NotAlreadyVisited visited -> (
        match
          f accum address (rev_accesses : BaseMemory.Access.t list :> RawMemory.Access.t list)
        with
        | Continue accum ->
            let visited_accum = (visited, accum) in
            let finish visited_accum = Continue visited_accum in
            Container.fold_until ~fold:(SafeBaseMemory.fold_edges astate address)
              heap ~init:visited_accum ~finish ~f:(fun visited_accum (access, (address, _trace)) ->
                if edge_filter access then
                  match
                    visit_access ~edge_filter ~f ~f_revisit access astate heap visited_accum
                  with
                  | Stop fin ->
                      Stop (Stop fin)
                  | Continue visited_accum -> (
                    match
                      visit_address ~edge_filter address ~f ~f_revisit (access :: rev_accesses)
                        astate heap visited_accum
                    with
                    | Continue _ as cont ->
                        cont
                    | Stop fin ->
                        Stop (Stop fin) )
                else Continue visited_accum )
        | Stop fin ->
            Stop (visited, fin) )


    and visit_access ~edge_filter ~f ~f_revisit (access : SafeMemory.Access.t) astate heap
        visited_accum =
      match access with
      | ArrayAccess (_, addr) ->
          visit_address ~edge_filter addr ~f ~f_revisit [] astate heap visited_accum
      | FieldAccess _ | Dereference ->
          Continue visited_accum


    let visit_address_from_var ~edge_filter (orig_var, (address, _loc)) ~f ~f_revisit rev_accesses
        astate heap visited_accum =
      visit_address ~edge_filter address ~f:(f orig_var) ~f_revisit:(f_revisit orig_var)
        rev_accesses astate heap visited_accum


    let fold_common x astate heap ~fold ~filter ~visit ~init ~already_visited ~f ~f_revisit ~finish
        =
      let finish (visited, accum) = (visited, finish accum) in
      let init = (already_visited, init) in
      Container.fold_until x ~fold ~init ~finish ~f:(fun visited_accum elem ->
          if filter elem then visit elem ~f ~f_revisit [] astate heap visited_accum
          else Continue visited_accum )


    let pre_or_post_heap astate pre_or_post =
      match pre_or_post with
      | `Pre ->
          (astate.pre :> base_domain).heap
      | `Post ->
          (astate.post :> base_domain).heap


    let fold ~var_filter ?(edge_filter = fun _ -> true) ~init ~f
        ?(f_revisit = fun _ accum _ _ -> accum) ~finish astate pre_or_post =
      fold_common astate astate
        (pre_or_post_heap astate pre_or_post)
        ~fold:(IContainer.fold_of_pervasives_map_fold (SafeStack.fold pre_or_post))
        ~filter:(fun (var, _) -> var_filter var)
        ~visit:(visit_address_from_var ~edge_filter)
        ~already_visited:CanonValue.Set.empty ~init ~f ~f_revisit ~finish


    let fold_from_addresses ?(edge_filter = fun _ -> true) ~already_visited ~init ~f
        ?(f_revisit = fun accum _ _ -> accum) ~finish from astate pre_or_post =
      let seq_fold seq ~init ~f = Seq.fold_left f init seq in
      fold_common from astate
        (pre_or_post_heap astate pre_or_post)
        ~fold:seq_fold
        ~filter:(fun _ -> true)
        ~visit:(visit_address ~edge_filter) ~already_visited ~init ~f ~f_revisit ~finish
  end

  let reachable_addresses ?(var_filter = fun _ -> true) ?edge_filter astate pre_or_post =
    GraphVisit.fold astate pre_or_post ~var_filter ?edge_filter ~init:() ~finish:Fn.id
      ~f:(fun _ () _ _ -> Continue () )
    |> fst


  let reachable_addresses_from ?(already_visited = CanonValue.Set.empty) ?edge_filter addresses
      astate pre_or_post =
    GraphVisit.fold_from_addresses ?edge_filter addresses astate pre_or_post ~init:()
      ~already_visited ~finish:Fn.id ~f:(fun () _ _ -> Continue () )
    |> fst
end

(** {2 Interface for the .mli} *)

open Internal

let get_pre {pre} = (pre :> BaseDomain.t)

let get_post {post} = (post :> BaseDomain.t)

let update_pre_for_kotlin_proc astate (proc_attrs : ProcAttributes.t) formals =
  let proc_name = proc_attrs.proc_name in
  let location = proc_attrs.loc in
  (* Drop this reference to make sure that formals' indices are always zero based
     for consistent error reporting *)
  let formals =
    match formals with
    | _ :: tail ->
        if Procname.is_java_instance_method proc_name then tail else formals
    | [] ->
        []
  in
  (* Kotlin procs are Java procs under the hood *)
  if Procname.is_java proc_name then
    List.foldi formals ~init:astate ~f:(fun index (acc : t) (var, _, annot_opt, (_, history)) ->
        (* We're interested specifically in org.jetbrains.annotations.NotNull since this annotation
           is emitted by kotlinc to denote non-nullable function parameters *)
        let is_not_nullable = Option.exists annot_opt ~f:Annotations.ia_is_jetbrains_notnull in
        if is_not_nullable then
          let acc, (value, history) = SafeStack.eval history var acc in
          let acc, (value, history) = SafeMemory.eval_edge (value, history) Dereference acc in
          let attribute =
            Attribute.MustBeValid
              ( Timestamp.t0
              , Trace.Immediate {location; history}
              , Some (NullArgumentWhereNonNullExpected (CallEvent.Call proc_name, Some index)) )
          in
          let new_pre =
            PreDomain.update acc.pre
              ~attrs:(BaseAddressAttributes.add_one value attribute (acc.pre :> base_domain).attrs)
          in
          if phys_equal new_pre acc.pre then acc else {acc with pre= new_pre}
        else acc )
  else astate


let mk_initial tenv (proc_attrs : ProcAttributes.t) =
  let proc_name = proc_attrs.proc_name in
  (* HACK: save the formals in the stacks of the pre and the post to remember which local variables
     correspond to formals *)
  let captured, formals =
    let init_var formal_or_captured pvar typ annot_opt =
      let event =
        match formal_or_captured with
        | `Formal ->
            ValueHistory.FormalDeclared (pvar, proc_attrs.loc, Timestamp.t0)
        | `Captured mode ->
            ValueHistory.Capture
              {captured_as= pvar; mode; location= proc_attrs.loc; timestamp= Timestamp.t0}
      in
      (Var.of_pvar pvar, typ, annot_opt, (AbstractValue.mk_fresh (), ValueHistory.singleton event))
    in
    let formals =
      List.map proc_attrs.formals ~f:(fun (mangled, typ, anno) ->
          init_var `Formal (Pvar.mk mangled proc_name) typ (Some anno) )
    in
    let captured =
      List.map proc_attrs.captured ~f:(fun {CapturedVar.pvar; typ; capture_mode} ->
          init_var (`Captured capture_mode) pvar typ None )
    in
    (captured, formals)
  in
  let formals_and_captured = captured @ formals in
  let initial_stack =
    List.fold formals_and_captured ~init:(PreDomain.empty :> BaseDomain.t).stack
      ~f:(fun stack (formal, _, _, addr_loc) -> BaseStack.add formal addr_loc stack )
  in
  let initial_heap =
    let register heap (_, _, _, (addr, _)) =
      (* safe because the state is empty, i.e. there are no equalities and each value is its own
         canonical representative *)
      let[@alert "-deprecated"] addr = CanonValue.unsafe_cast addr in
      BaseMemory.register_address addr heap
    in
    List.fold formals_and_captured ~init:(PreDomain.empty :> base_domain).heap ~f:register
  in
  let initial_attrs = BaseDomain.empty.attrs in
  let pre =
    PreDomain.update ~stack:initial_stack ~heap:initial_heap ~attrs:initial_attrs PreDomain.empty
  in
  let initial_heap, initial_attrs =
    ((PreDomain.empty :> base_domain).heap, (PreDomain.empty :> base_domain).attrs)
  in
  let post =
    PostDomain.update ~stack:initial_stack ~heap:initial_heap ~attrs:initial_attrs PostDomain.empty
  in
  let post =
    List.fold proc_attrs.locals ~init:post
      ~f:(fun
          (acc : PostDomain.t) {ProcAttributes.name; typ; modify_in_block; is_constexpr; tmp_id} ->
        if
          modify_in_block || is_constexpr || Option.is_some tmp_id
          || not (Language.curr_language_is Clang)
        then acc
        else
          SafeAttributes.set_uninitialized_post tenv Timestamp.t0
            (`LocalDecl (Pvar.mk name proc_name, None))
            typ proc_attrs.loc acc )
  in
  let astate =
    { pre
    ; post
    ; path_condition= Formula.ttrue
    ; decompiler= Decompiler.empty
    ; need_dynamic_type_specialization= AbstractValue.Set.empty
    ; topl= PulseTopl.start ()
    ; transitive_info= TransitiveInfo.bottom
    ; recursive_calls= PulseMutualRecursion.Set.empty
    ; skipped_calls= SkippedCalls.empty }
  in
  let astate =
    if
      Language.curr_language_is Hack || Language.curr_language_is Python
      || Language.curr_language_is Clang
    then
      (* The Hack and Python and Clang frontends do not propagate types from declarations to usage,
         so we redo part of the work ourself *)
      add_static_types tenv proc_attrs.loc astate formals_and_captured
    else astate
  in
  update_pre_for_kotlin_proc astate proc_attrs formals


let leq ~lhs ~rhs =
  phys_equal lhs rhs
  || SkippedCalls.leq ~lhs:lhs.skipped_calls ~rhs:rhs.skipped_calls
     && Formula.equal lhs.path_condition rhs.path_condition
     &&
     match
       GraphComparison.isograph_map GraphComparison.empty_mapping ~astate_lhs:lhs
         ~lhs:(lhs.pre :> BaseDomain.t)
         ~astate_rhs:rhs
         ~rhs:(rhs.pre :> BaseDomain.t)
     with
     | NotIsomorphic ->
         false
     | IsomorphicUpTo foot_mapping ->
         GraphComparison.is_isograph foot_mapping ~astate_lhs:lhs
           ~lhs:(lhs.post :> BaseDomain.t)
           ~astate_rhs:rhs
           ~rhs:(rhs.post :> BaseDomain.t)


let get_unreachable_attributes astate =
  let post_addresses = reachable_addresses astate `Post in
  ( BaseAddressAttributes.fold
      (fun address _ dead_addresses ->
        if CanonValue.Set.mem address post_addresses then dead_addresses
        else address :: dead_addresses )
      (astate.post :> base_domain).attrs []
    : CanonValue.t list
    :> AbstractValue.t list )


exception NoLeak

let filter_live_addresses ~is_dead_root potential_leak_addrs astate =
  (* stop as soon as we find out that all locations that could potentially cause a leak are still
     live *)
  if CanonValue.Set.is_empty potential_leak_addrs then raise_notrace NoLeak ;
  let potential_leaks = ref potential_leak_addrs in
  let mark_reachable addr =
    potential_leaks := CanonValue.Set.remove addr !potential_leaks ;
    if CanonValue.Set.is_empty !potential_leaks then raise_notrace NoLeak
  in
  let pre = (astate.pre :> BaseDomain.t) in
  (* filter out addresses live in the post *)
  ignore
    (GraphVisit.fold
       ~var_filter:(fun var -> not (is_dead_root var))
       astate `Post ~init:()
       ~f:(fun _ () addr _ ->
         mark_reachable addr ;
         Continue () )
       ~finish:(fun () -> ()) ) ;
  let collect_reachable_from addrs pre_or_post =
    GraphVisit.fold_from_addresses addrs astate pre_or_post ~init:()
      ~already_visited:CanonValue.Set.empty
      ~f:(fun () addr _ ->
        mark_reachable addr ;
        Continue () )
      ~finish:(fun () -> ())
    |> fst
  in
  (* any address reachable in the pre-condition is not dead as callers can still be holding on to
     them; so any address reachable from anything reachable from the precondition is live *)
  let reachable_in_pre =
    (* start from the *values* of variables, not their addresses; addresses of formals are
       meaningless for callers so are not reachable outside the current function *)
    let formal_values =
      BaseStack.to_seq pre.stack
      |> Seq.flat_map (fun (_, (formal_addr, _)) ->
             match BaseMemory.find_opt (CanonValue.canon astate formal_addr) pre.heap with
             | None ->
                 Seq.empty
             | Some edges ->
                 BaseMemory.Edges.to_seq edges
                 |> Seq.map (fun (_access, (value, _)) ->
                        let value = CanonValue.canon astate value in
                        mark_reachable value ;
                        value ) )
    in
    collect_reachable_from formal_values `Pre
  in
  let reachable_from_reachable_in_pre =
    collect_reachable_from (CanonValue.Set.to_seq reachable_in_pre) `Post
  in
  ignore reachable_from_reachable_in_pre ;
  !potential_leaks


let mark_potential_leaks location ~dead_roots astate =
  let is_dead_root var = List.mem ~equal:Var.equal dead_roots var in
  (* only consider locations that could actually cause a leak if unreachable *)
  let allocated_reachable_from_dead_root =
    reachable_addresses ~var_filter:is_dead_root astate `Post
    |> CanonValue.Set.filter (fun addr ->
           SafeAttributes.get_allocation addr astate |> Option.is_some )
  in
  match filter_live_addresses ~is_dead_root allocated_reachable_from_dead_root astate with
  | exception NoLeak ->
      astate
  | potential_leaks ->
      (* delay reporting leak as to avoid false positives we need to massage the state some more;
         TODO: this can make use miss reporting memory leaks if another error is found *)
      CanonValue.Set.fold
        (fun addr astate -> SafeAttributes.add_unreachable_at addr location astate)
        potential_leaks astate


let check_memory_leaks ~live_addresses ~unreachable_addresses astate =
  let reaches_into addr addrs astate =
    CanonValue.Set.mem addr addrs
    || GraphVisit.fold_from_addresses (Seq.return addr) astate `Post ~init:()
         ~already_visited:CanonValue.Set.empty
         ~finish:(fun () -> (* didn't find any reachable address in [addrs] *) false)
         ~f:(fun () addr' rev_accesses ->
           (* We want to know if [addr] is still reachable from the value [addr'] by pointer
                arithmetic, hence detect if [addr'] is an offset away from [addr]. In particular,
                if we go through a [Dereference] then we have lost the connexion to the original
                address value.

              TODO: this should be part of [live_addresses] since all these addresses are actually
                live. *)
           let can_recover_root_from_accesses =
             List.for_all rev_accesses ~f:(fun (access : Access.t) ->
                 match access with FieldAccess _ | ArrayAccess _ -> true | _ -> false )
           in
           if can_recover_root_from_accesses then
             if CanonValue.Set.mem addr' addrs then (
               L.d_printfln ~color:Orange "Live address %a is reachable from %a" CanonValue.pp addr'
                 CanonValue.pp addr ;
               Stop true )
             else Continue ()
           else Stop false )
       |> snd
  in
  let check_memory_leak addr attributes =
    let allocated_not_freed_opt = Attributes.get_allocated_not_freed attributes in
    match allocated_not_freed_opt with
    | None ->
        Ok ()
    | Some (allocator, trace) ->
        (* allocated but not freed => leak *)
        L.d_printfln ~color:Red "LEAK: unreachable address %a was allocated by %a" CanonValue.pp
          addr Attribute.pp_allocator allocator ;
        (* last-chance checks: it could be that the value (or its canonical equal) is reachable
           via pointer arithmetic from live values. This is common in libraries that do their own
           memory management, e.g. they return a field of the malloc()'d pointer, and the latter
           is a fat pointer with, say, a reference count. For example in C:

           {[ struct fat_int { size_t count; int data; };

            int *alloc_int() { fat_int *p = malloc(...); p->count = 1; return &(p->data); } ]}

           We don't have a precise enough memory model to understand everything that goes on here
           but we should at least not report a leak. *)
        if reaches_into addr live_addresses astate then (
          L.d_printfln ~color:Orange
            "False alarm: address is still reachable via other means; forgetting about its \
             allocation site to prevent leak false positives." ;
          Ok () )
        else
          (* if the address became unreachable at a known point use that location *)
          let location = Attributes.get_unreachable_at attributes in
          Error (location, allocator, trace)
  in
  List.fold_result unreachable_addresses ~init:() ~f:(fun () addr ->
      let addr = CanonValue.canon' astate addr in
      match SafeAttributes.find_opt addr astate with
      | Some unreachable_attrs ->
          check_memory_leak addr unreachable_attrs
      | None ->
          Ok () )


let get_all_addrs_marked_as_always_reachable {post} =
  BaseAddressAttributes.fold
    (fun address attr addresses ->
      if Attributes.is_always_reachable attr then Seq.cons address addresses else addresses )
    (post :> BaseDomain.t).attrs Seq.empty


let filter_pre_addr ~f (foot : PreDomain.t) =
  let heap' = BaseMemory.filter (fun address _ -> f address) (foot :> BaseDomain.t).heap in
  let attrs' =
    BaseAddressAttributes.filter (fun address _ -> f address) (foot :> BaseDomain.t).attrs
  in
  PreDomain.update ~heap:heap' ~attrs:attrs' foot


let filter_post_addr_with_discarded_addrs ~heap_only ~f (foot : PostDomain.t) =
  let heap' = BaseMemory.filter (fun address _ -> f address) (foot :> BaseDomain.t).heap in
  let attrs', discarded_addresses =
    if heap_only then ((foot :> BaseDomain.t).attrs, [])
    else BaseAddressAttributes.filter_with_discarded_addrs f (foot :> BaseDomain.t).attrs
  in
  (PostDomain.update ~heap:heap' ~attrs:attrs' foot, discarded_addresses)


let get_reachable astate =
  let pre_addresses = reachable_addresses astate `Pre in
  let post_addresses = reachable_addresses astate `Post in
  let post_addresses =
    (* Also include post addresses reachable from pre addresses *)
    reachable_addresses_from
      (CanonValue.Set.to_seq pre_addresses)
      astate `Post ~already_visited:post_addresses
  in
  let always_reachable_addresses = get_all_addrs_marked_as_always_reachable astate in
  let always_reachable_trans_closure =
    reachable_addresses_from always_reachable_addresses astate `Post ~already_visited:post_addresses
  in
  (pre_addresses, post_addresses, always_reachable_trans_closure)


let topl_view ({pre; post; path_condition} as astate) =
  let pulse_pre = (pre :> BaseDomain.t) in
  let pulse_post = (post :> BaseDomain.t) in
  let get_reachable () =
    let pre_addresses, post_addresses, always_reachable_trans_closure = get_reachable astate in
    let in_path_condition =
      Formula.fold_variables path_condition ~init:AbstractValue.Set.empty ~f:(fun vars v ->
          AbstractValue.Set.add v vars )
    in
    let ( ++ ) = CanonValue.Set.union in
    AbstractValue.Set.union in_path_condition
      (pre_addresses ++ post_addresses ++ always_reachable_trans_closure |> CanonValue.downcast_set)
  in
  {PulseTopl.pulse_pre; pulse_post; path_condition; get_reachable}


let discard_unreachable_ ~for_summary ({pre; post} as astate) =
  let pre_addresses, post_addresses, always_reachable_trans_closure = get_reachable astate in
  let pre_new = filter_pre_addr ~f:(fun address -> CanonValue.Set.mem address pre_addresses) pre in
  let post_new, dead_addresses =
    (* keep attributes of dead addresses unless we are creating a summary *)
    filter_post_addr_with_discarded_addrs ~heap_only:(not for_summary)
      ~f:(fun address ->
        CanonValue.Set.mem address pre_addresses
        || CanonValue.Set.mem address post_addresses
        || CanonValue.Set.mem address always_reachable_trans_closure )
      post
  in
  (* note: we don't call {!Formula.simplify} *)
  let astate =
    if phys_equal pre_new pre && phys_equal post_new post then astate
    else {astate with pre= pre_new; post= post_new}
  in
  (astate, pre_addresses, post_addresses, dead_addresses)


let filter_for_summary proc_name location astate0 =
  let open SatUnsat.Import in
  L.d_printfln "state *before* calling canonicalize:" ;
  L.d_printfln "%a" pp astate0 ;
  L.d_printfln "Canonicalizing..." ;
  let* astate_before_filter = canonicalize location astate0 in
  let pp_state = Pp.html_collapsible_block ~name:"Show/hide canonicalized state" HTML pp in
  L.d_printfln "%a" pp_state astate_before_filter ;
  (* Remove the stack from the post as it's not used: the values of formals are the same as in the
     pre. Moreover, formals can be treated as local variables inside the function's body so we need
     to restore their initial values at the end of the function. Removing them altogether achieves
     this. *)
  let astate = restore_formals_for_summary astate_before_filter in
  (* NOTE: the [topl_view] needs to be on the state *before* discarding unreachable addresses *)
  let astate = {astate with topl= PulseTopl.simplify (topl_view astate) astate.topl} in
  let astate, pre_live_addresses, post_live_addresses, dead_addresses =
    discard_unreachable_ ~for_summary:true astate
  in
  let precondition_vocabulary =
    if PatternMatch.is_entry_point proc_name then
      (* report all latent issues at entry points *)
      AbstractValue.Set.empty
    else CanonValue.downcast_set pre_live_addresses
  in
  let live_addresses =
    CanonValue.Set.union pre_live_addresses post_live_addresses |> CanonValue.downcast_set
  in
  let+ path_condition, live_via_arithmetic, new_eqs =
    Formula.simplify ~precondition_vocabulary ~keep:live_addresses astate.path_condition
  in
  (* [unsafe_cast_set] is safe because a) all the values are actually canon_values in disguise,
     and b) we have canonicalised all the values in the state already so all we have left are
     canonical values *)
  let[@alert "-deprecated"] live_addresses =
    AbstractValue.Set.union live_addresses live_via_arithmetic |> CanonValue.unsafe_cast_set
  in
  ( {astate with path_condition; topl= PulseTopl.filter_for_summary (topl_view astate) astate.topl}
  , live_addresses
  , (* we could filter out the [live_addresses] if needed; right now they might overlap *)
    dead_addresses
  , new_eqs )


let should_havoc_if_unknown () =
  match !Language.curr_language with
  | Java
    (* TODO(T138610370): temporary until we improve the way taint propagates for unknown calls *)
  | Hack
  | Python ->
      `ShouldOnlyHavocResources
  | _ ->
      `ShouldHavoc


let apply_unknown_effect ?(havoc_filter = fun _ _ _ -> true) hist x astate =
  let x = CanonValue.canon' astate x in
  let havoc_accesses hist addr heap =
    match BaseMemory.find_opt addr heap with
    | None ->
        heap
    | Some edges ->
        let edges =
          SafeBaseMemory.Edges.mapi astate edges ~f:(fun access value ->
              if havoc_filter (downcast addr) (downcast_access access) (downcast_fst value) then (
                L.d_printfln_escaped "havoc'ing access %a" BaseMemory.Access.pp access ;
                (AbstractValue.mk_fresh (), hist) )
              else downcast_fst value )
        in
        BaseMemory.add addr edges heap
  in
  let post = (astate.post :> BaseDomain.t) in
  let heap, attrs =
    GraphVisit.fold_from_addresses (Seq.return x) astate `Post ~init:(post.heap, post.attrs)
      ~already_visited:CanonValue.Set.empty
      ~f:(fun (heap, attrs) addr _edges ->
        match should_havoc_if_unknown () with
        | `ShouldHavoc ->
            let attrs =
              BaseAddressAttributes.remove_allocation_attr addr attrs
              |> BaseAddressAttributes.initialize addr
            in
            let heap = havoc_accesses hist addr heap in
            Continue (heap, attrs)
        | `ShouldOnlyHavocResources ->
            let attrs = BaseAddressAttributes.remove_allocation_attr addr attrs in
            Continue (heap, attrs) )
      ~finish:Fn.id
    |> snd
  in
  {astate with post= PostDomain.update ~attrs ~heap astate.post}


let add_need_dynamic_type_specialization receiver_addr astate =
  let need_dynamic_type_specialization =
    AbstractValue.Set.add receiver_addr astate.need_dynamic_type_specialization
  in
  {astate with need_dynamic_type_specialization}


let set_post_edges addr edges astate =
  if RawMemory.Edges.is_empty edges then astate
  else SafeMemory.map_post_heap astate ~f:(BaseMemory.add (CanonValue.canon' astate addr) edges)


let find_post_cell_opt addr astate =
  find_cell_opt (CanonValue.canon' astate addr) (astate.post :> BaseDomain.t)


let set_post_cell {PathContext.timestamp} (addr, history) (edges, attr_set) location astate =
  let astate = set_post_edges addr edges astate in
  let addr = CanonValue.canon' astate addr in
  SafeAttributes.map_post_attrs astate ~f:(fun attrs ->
      BaseAddressAttributes.add_one addr
        (WrittenTo (timestamp, Trace.Immediate {location; history}))
        attrs
      |> BaseAddressAttributes.add addr attr_set )


module Summary = struct
  type summary = t [@@deriving compare, equal, yojson_of]

  type t = summary [@@deriving compare, equal, yojson_of]

  let pp = pp_ ~is_summary:true

  let leq = leq

  let get_pre = get_pre

  let get_post = get_post

  let get_path_condition {path_condition} = path_condition

  let get_topl {topl} = topl

  let get_recursive_calls {recursive_calls} = recursive_calls

  let get_skipped_calls {skipped_calls} = skipped_calls

  let is_heap_allocated = is_heap_allocated

  let get_must_be_valid addr summary =
    SafeAttributes.get_must_be_valid (CanonValue.canon' summary addr) summary


  let remove_all_must_not_be_tainted = SafeAttributes.remove_all_must_not_be_tainted

  let of_post_ proc_name (proc_attrs : ProcAttributes.t) location astate0 =
    let open SatUnsat.Import in
    let astate = astate0 in
    let astate_before_filter = astate in
    (* do not store the decompiler in the summary and make sure we only use the original one by
       marking it invalid *)
    let astate = {astate with decompiler= Decompiler.invalid} in
    let* astate, live_addresses, dead_addresses, new_eqs =
      filter_for_summary proc_name location astate
    in
    let+ astate, error = incorporate_new_eqs astate new_eqs in
    match error with
    | None -> (
      (* NOTE: it's important for correctness that we check leaks last because we are going to carry
         on with the astate after the leak and we don't want to accidentally skip modifications of
         the state because of the error monad *)
      match
        check_memory_leaks ~live_addresses ~unreachable_addresses:dead_addresses
          astate_before_filter
      with
      | Ok () ->
          Ok (invalidate_locals proc_attrs.locals astate)
      | Error (unreachable_location, JavaResource class_name, trace) ->
          Error
            (`JavaResourceLeak
              ( astate
              , astate_before_filter
              , class_name
              , trace
              , Option.value unreachable_location ~default:location ) )
      | Error (unreachable_location, HackAsync, trace) ->
          Error
            (`HackUnawaitedAwaitable
              ( astate
              , astate_before_filter
              , trace
              , Option.value unreachable_location ~default:location ) )
      | Error (unreachable_location, HackBuilderResource builder_type, trace) ->
          Error
            (`HackUnfinishedBuilder
              ( astate
              , astate_before_filter
              , trace
              , Option.value unreachable_location ~default:location
              , builder_type ) )
      | Error (unreachable_location, CSharpResource class_name, trace) ->
          Error
            (`CSharpResourceLeak
              ( astate
              , astate_before_filter
              , class_name
              , trace
              , Option.value unreachable_location ~default:location ) )
      | Error (unreachable_location, allocator, trace) ->
          Error
            (`MemoryLeak
              ( astate
              , astate_before_filter
              , allocator
              , trace
              , Option.value unreachable_location ~default:location ) ) )
    | Some (address, must_be_valid) ->
        Error
          (`PotentialInvalidAccessSummary
            (astate, astate_before_filter, Decompiler.find address astate0.decompiler, must_be_valid)
            )


  let of_post proc_name proc_attrs location astate0 =
    let summary_sat = of_post_ proc_name proc_attrs location astate0 in
    match summary_sat with
    | Sat _ ->
        summary_sat
    | Unsat ->
        Stats.incr_pulse_summaries_contradictions () ;
        Unsat


  let add_need_dynamic_type_specialization = add_need_dynamic_type_specialization

  let get_transitive_info {transitive_info} = transitive_info

  let heap_paths_that_need_dynamic_type_specialization summary =
    let rec mk_heap_path var rev_accesses =
      let open IOption.Let_syntax in
      match (rev_accesses : PulseAccess.t list) with
      | [] ->
          let+ pvar = Var.get_pvar var in
          Specialization.HeapPath.Pvar pvar
      | FieldAccess fieldname :: rev_accesses ->
          let+ heap_path = mk_heap_path var rev_accesses in
          Specialization.HeapPath.FieldAccess (fieldname, heap_path)
      | Dereference :: rev_accesses ->
          let+ heap_path = mk_heap_path var rev_accesses in
          Specialization.HeapPath.Dereference heap_path
      | ArrayAccess _ :: _ ->
          None
    in
    (* safe because this is a summary so these addresses are actually canon values *)
    let[@alert "-deprecated"] addresses =
      CanonValue.unsafe_cast_set summary.need_dynamic_type_specialization
    in
    GraphVisit.fold summary `Pre
      ~var_filter:(fun _ -> true)
      ~init:(Specialization.HeapPath.Map.empty, CanonValue.Set.empty)
      ~finish:(fun heap_paths -> heap_paths)
      ~f:(fun var (heap_paths, already_found) addr rev_accesses ->
        Continue
          ( if CanonValue.Set.mem addr addresses && not (CanonValue.Set.mem addr already_found) then
              mk_heap_path var rev_accesses
              |> Option.value_map ~default:(heap_paths, already_found) ~f:(fun heap_path ->
                     ( Specialization.HeapPath.Map.add heap_path (downcast addr) heap_paths
                     , CanonValue.Set.add addr already_found ) )
            else (heap_paths, already_found) ) )
    |> snd |> fst


  let pre_heap_has_assumptions astate =
    let exception AssumptionDetected in
    (* since we operate on a summary all the values are normalized already so we can use [RawMemory]
       and pretend everything is an [AbstractValue] (instead of [CanonValue]) *)
    let visit_addr seen addr =
      if AbstractValue.is_restricted addr then (
        L.d_printfln_escaped "assumption detected: %a is in the pre heap and is restricted (>= 0)"
          AbstractValue.pp addr ;
        raise_notrace AssumptionDetected )
      else if AbstractValue.Set.mem addr seen then (
        L.d_printfln_escaped
          "assumption detected: %a is reachable in the pre heap from at least two different paths"
          AbstractValue.pp addr ;
        raise_notrace AssumptionDetected )
      else AbstractValue.Set.add addr seen
    in
    let visit_edge seen (_access, (addr, _history)) = visit_addr seen addr in
    let visit_binding _addr edges seen = RawMemory.Edges.fold edges ~init:seen ~f:visit_edge in
    try
      RawMemory.fold visit_binding (astate.pre :> BaseDomain.t).heap AbstractValue.Set.empty
      |> ignore ;
      false
    with AssumptionDetected -> true
end

module Topl = struct
  let small_step tenv loc event astate =
    {astate with topl= PulseTopl.small_step tenv loc (topl_view astate) event astate.topl}


  let large_step ~call_location ~callee_proc_name ~substitution ~callee_summary ~callee_is_manifest
      astate =
    { astate with
      topl=
        PulseTopl.large_step ~call_location ~callee_proc_name ~substitution (topl_view astate)
          ~callee_summary ~callee_is_manifest astate.topl }


  let report_errors proc_desc err_log ~pulse_is_manifest astate =
    PulseTopl.report_errors proc_desc err_log ~pulse_is_manifest astate.topl
end

let add_missed_captures missed_captures ({transitive_info} as astate) =
  if Config.pulse_monitor_transitive_missed_captures || Config.reactive_capture then
    let missed_captures = Typ.Name.Set.union missed_captures transitive_info.missed_captures in
    let transitive_info = {transitive_info with TransitiveInfo.missed_captures} in
    {astate with transitive_info}
  else astate


let add_recursive_call location callee astate =
  let trace = PulseMutualRecursion.mk location callee in
  let recursive_calls = PulseMutualRecursion.Set.add trace astate.recursive_calls in
  ({astate with recursive_calls}, trace)


let add_recursive_calls traces astate =
  {astate with recursive_calls= PulseMutualRecursion.Set.union astate.recursive_calls traces}


let add_skipped_call pname trace astate =
  let new_skipped_calls = SkippedCalls.add pname trace astate.skipped_calls in
  if phys_equal new_skipped_calls astate.skipped_calls then astate
  else {astate with skipped_calls= new_skipped_calls}


let add_skipped_calls new_skipped_calls astate =
  (* favor calls we already knew about somewhat arbitrarily *)
  let skipped_calls =
    SkippedCalls.union
      (fun _ orig_call _new_call -> Some orig_call)
      astate.skipped_calls new_skipped_calls
  in
  if phys_equal skipped_calls astate.skipped_calls then astate else {astate with skipped_calls}


let transfer_transitive_info_to_caller callee_pname call_loc summary caller_astate =
  let caller = caller_astate.transitive_info in
  let summary = summary.transitive_info in
  let transitive_info = TransitiveInfo.apply_summary ~callee_pname ~call_loc ~summary caller in
  {caller_astate with transitive_info}


let is_local = is_local

let is_allocated_this_pointer proc_attrs astate address =
  let open IOption.Let_syntax in
  let address = CanonValue.canon' astate address in
  Option.exists ~f:Fn.id
    (let* this = ProcAttributes.get_this proc_attrs in
     let* this_pointer_address, _ = SafeStack.find_opt (Var.of_pvar this) astate in
     let+ this_pointer, _ = SafeMemory.find_edge_opt this_pointer_address Dereference astate in
     CanonValue.equal this_pointer address )


let incorporate_new_eqs new_eqs astate =
  let open SatUnsat.Import in
  let proc_attrs = Procdesc.get_attributes (PulseContext.proc_desc () |> Option.value_exn) in
  let* astate, potential_invalid_access_opt = incorporate_new_eqs astate new_eqs in
  match potential_invalid_access_opt with
  | None ->
      Sat (Ok astate)
  | Some (address, must_be_valid) ->
      L.d_printfln ~color:Red "potential error if %a is null" AbstractValue.pp address ;
      if is_allocated_this_pointer proc_attrs astate address then Unsat
      else Sat (Error (`PotentialInvalidAccess (astate, address, must_be_valid)))


let incorporate_new_eqs_on_val new_eqs v =
  let rec subst remaining_eqs w =
    match remaining_eqs with
    | PulseFormula.Equal (v1, v2) :: remaining_eqs when AbstractValue.equal v1 w ->
        subst remaining_eqs v2
    | _ :: remaining_eqs ->
        subst remaining_eqs w
    | [] ->
        w
  in
  subst (RevList.to_list new_eqs) v


let downcast_filter f canon_access = f (downcast_access canon_access)

(* HACK: too lazy to canonicalize [edge_filter] as appropriate and don't need it for now so ignore
   it until someone needs it *)
let fold_all ?(var_filter = fun _ -> true) ~init ~finish ~f ?f_revisit astate pre_or_post =
  GraphVisit.fold astate pre_or_post ~var_filter ~init ~finish
    ~f:(fun root_var acc v_canon accesses_from_root ->
      f root_var acc (downcast v_canon) accesses_from_root )
    ?f_revisit:
      (Option.map f_revisit ~f:(fun f_revisit root_var acc v_canon accesses_from_root ->
           f_revisit root_var acc (downcast v_canon) accesses_from_root ) )
  |> snd


let reachable_addresses_from ?edge_filter addresses astate pre_or_post =
  let edge_filter = Option.map edge_filter ~f:downcast_filter in
  reachable_addresses_from ?edge_filter
    (Seq.map (CanonValue.canon' astate) addresses)
    astate pre_or_post
  |> CanonValue.downcast_set


module Stack = struct
  include SafeStack

  let fold ?(pre_or_post = `Post) f astate init =
    SafeStack.fold pre_or_post
      (fun var addr_hist acc -> f var (downcast_fst addr_hist) acc)
      astate init


  let find_opt var astate = SafeStack.find_opt var astate |> Option.map ~f:downcast_fst

  let eval origin var astate = SafeStack.eval origin var astate |> downcast_snd_fst

  let exists f astate =
    SafeStack.exists (fun var addr_hist -> f var (downcast_fst addr_hist)) astate
end

module Memory = struct
  include SafeMemory

  let add_edge path orig_addr_hist access dest_addr_hist location astate =
    SafeMemory.add_edge path
      (CanonValue.canon_fst' astate orig_addr_hist)
      (CanonValue.canon_access astate access)
      dest_addr_hist location astate


  let eval_edge addr_hist access astate =
    SafeMemory.eval_edge
      (CanonValue.canon_fst' astate addr_hist)
      (CanonValue.canon_access astate access)
      astate
    |> downcast_snd_fst


  let fold_edges ?(pre_or_post = `Post) addr astate ~init ~f =
    SafeMemory.fold_edges pre_or_post (CanonValue.canon' astate addr) astate ~init
      ~f:(fun acc access_addr_hist ->
        f acc
          ( access_addr_hist
            : Access.t * (CanonValue.t * ValueHistory.t)
            :> PulseAccess.t * (AbstractValue.t * ValueHistory.t) ) )


  let find_edge_opt address access astate =
    SafeMemory.find_edge_opt
      (CanonValue.canon' astate address)
      (CanonValue.canon_access astate access)
      astate
    |> Option.map ~f:downcast_fst


  let exists_edge ?(pre_or_post = `Post) addr astate ~f =
    SafeMemory.exists_edge pre_or_post (CanonValue.canon' astate addr) astate
      ~f:(fun access_addr_hist ->
        f
          ( access_addr_hist
            : Access.t * (CanonValue.t * ValueHistory.t)
            :> PulseAccess.t * (AbstractValue.t * ValueHistory.t) ) )
end

let add_block_source v block astate =
  map_decompiler astate ~f:(fun decompiler -> Decompiler.add_block_source v block decompiler)


module AddressAttributes = struct
  let abduce_one v attr astate = SafeAttributes.abduce_one (CanonValue.canon' astate v) attr astate

  let abduce_all v attrs astate =
    SafeAttributes.abduce_all (CanonValue.canon' astate v) attrs astate


  let add_one v attr astate = SafeAttributes.add_one (CanonValue.canon' astate v) attr astate

  let add_all v attrs astate = SafeAttributes.add_all (CanonValue.canon' astate v) attrs astate

  let find_opt v astate = SafeAttributes.find_opt (CanonValue.canon' astate v) astate

  let check_valid path ?must_be_valid_reason trace v astate =
    SafeAttributes.check_valid path ?must_be_valid_reason trace (CanonValue.canon' astate v) astate


  let check_initialized path trace v astate =
    SafeAttributes.check_initialized path trace (CanonValue.canon' astate v) astate


  let add_taint_sink path taint trace v astate =
    SafeAttributes.add_taint_sink path taint trace (CanonValue.canon' astate v) astate


  let invalidate addr_hist invalidation location astate =
    SafeAttributes.invalidate (CanonValue.canon_fst' astate addr_hist) invalidation location astate


  let initialize v astate = SafeAttributes.initialize (CanonValue.canon' astate v) astate

  let set_uninitialized tenv {PathContext.timestamp} src typ location astate =
    if Language.curr_language_is Clang then
      let src =
        match src with
        | `LocalDecl (pvar, v_opt) ->
            `LocalDecl (pvar, CanonValue.canon_opt' astate v_opt)
        | `Malloc v ->
            `Malloc (CanonValue.canon' astate v)
      in
      { astate with
        post= SafeAttributes.set_uninitialized_post tenv timestamp src typ location astate.post }
    else astate


  let always_reachable v astate =
    SafeAttributes.always_reachable (CanonValue.canon' astate v) astate


  let allocate allocator v location astate =
    SafeAttributes.allocate allocator (CanonValue.canon' astate v) location astate


  let java_resource_release v astate =
    SafeAttributes.java_resource_release (CanonValue.canon' astate v) astate


  let hack_async_await v astate =
    SafeAttributes.hack_async_await (CanonValue.canon' astate v) astate


  let remove_hack_builder v astate =
    SafeAttributes.remove_hack_builder (CanonValue.canon' astate v) astate


  let set_hack_builder v builderstate astate =
    SafeAttributes.set_hack_builder (CanonValue.canon' astate v) builderstate astate


  let get_hack_builder v astate =
    SafeAttributes.get_hack_builder (CanonValue.canon' astate v) astate


  let is_java_resource_released v astate =
    SafeAttributes.is_java_resource_released (CanonValue.canon' astate v) astate


  let csharp_resource_release v astate =
    SafeAttributes.csharp_resource_release (CanonValue.canon' astate v) astate


  let is_csharp_resource_released v astate =
    SafeAttributes.is_csharp_resource_released (CanonValue.canon' astate v) astate


  let in_reported_retain_cycle v astate =
    SafeAttributes.in_reported_retain_cycle (CanonValue.canon' astate v) astate


  let is_in_reported_retain_cycle v astate =
    SafeAttributes.is_in_reported_retain_cycle (CanonValue.canon' astate v) astate


  let add_dict_contain_const_keys v astate =
    SafeAttributes.add_dict_contain_const_keys (CanonValue.canon' astate v) astate


  let remove_dict_contain_const_keys v astate =
    SafeAttributes.remove_dict_contain_const_keys (CanonValue.canon' astate v) astate


  let is_dict_contain_const_keys v astate =
    SafeAttributes.is_dict_contain_const_keys (CanonValue.canon' astate v) astate


  let add_dict_read_const_key timestamp trace v key astate =
    SafeAttributes.add_dict_read_const_key timestamp trace (CanonValue.canon' astate v) key astate


  let add_static_type tenv typ v location astate =
    add_static_type tenv typ (CanonValue.canon' astate v) location astate


  let get_allocation_attr v astate =
    SafeAttributes.get_allocation (CanonValue.canon' astate v) astate


  let remove_allocation_attr v astate =
    SafeAttributes.remove_allocation_attr (CanonValue.canon' astate v) astate


  let remove_taint_attrs v astate =
    SafeAttributes.remove_taint_attrs (CanonValue.canon' astate v) astate


  let get_closure_proc_name v astate =
    SafeAttributes.get_closure_proc_name (CanonValue.canon' astate v) astate


  let get_static_type v astate = SafeAttributes.get_static_type (CanonValue.canon' astate v) astate

  let get_copied_into v astate = SafeAttributes.get_copied_into (CanonValue.canon' astate v) astate

  let get_copied_return v astate =
    ( SafeAttributes.get_copied_return (CanonValue.canon' astate v) astate
      |> CanonValue.canon_opt_fst4' astate
      : (CanonValue.t * bool * Attribute.CopyOrigin.t * Location.t) option
      :> (AbstractValue.t * bool * Attribute.CopyOrigin.t * Location.t) option )


  let remove_copied_return v astate =
    SafeAttributes.remove_copied_return (CanonValue.canon' astate v) astate


  let get_source_origin_of_copy v astate =
    ( SafeAttributes.get_source_origin_of_copy (CanonValue.canon' astate v) astate
      |> CanonValue.canon_opt' astate
      : CanonValue.t option
      :> AbstractValue.t option )


  let get_taint_sources_and_sanitizers v astate =
    SafeAttributes.get_taint_sources_and_sanitizers (CanonValue.canon' astate v) astate


  let get_propagate_taint_from v astate =
    SafeAttributes.get_propagate_taint_from (CanonValue.canon' astate v) astate


  let is_end_of_collection v astate =
    SafeAttributes.is_end_of_collection (CanonValue.canon' astate v) astate


  let mark_as_end_of_collection v astate =
    SafeAttributes.mark_as_end_of_collection (CanonValue.canon' astate v) astate


  let is_std_vector_reserved v astate =
    SafeAttributes.is_std_vector_reserved (CanonValue.canon' astate v) astate


  let get_last_lookup v astate = SafeAttributes.get_last_lookup (CanonValue.canon' astate v) astate

  let std_vector_reserve v astate =
    SafeAttributes.std_vector_reserve (CanonValue.canon' astate v) astate


  let add_copied_return v ~source ~is_const_ref copy_origin location astate =
    SafeAttributes.add_copied_return (CanonValue.canon' astate v)
      ~source:(CanonValue.canon' astate source) ~is_const_ref copy_origin location astate


  let get_config_usage v astate =
    SafeAttributes.get_config_usage (CanonValue.canon' astate v) astate


  let get_valid_returned_from_unknown v astate =
    SafeAttributes.get_valid_returned_from_unknown (CanonValue.canon' astate v) astate


  let get_written_to v astate = SafeAttributes.get_written_to (CanonValue.canon' astate v) astate

  let is_copied_from_const_ref v astate =
    SafeAttributes.is_copied_from_const_ref (CanonValue.canon' astate v) astate


  let is_std_moved v astate = SafeAttributes.is_std_moved (CanonValue.canon' astate v) astate

  let get_address_of_stack_variable v astate =
    SafeAttributes.get_address_of_stack_variable (CanonValue.canon' astate v) astate


  let has_unknown_effect v astate =
    SafeAttributes.has_unknown_effect (CanonValue.canon' astate v) astate


  let is_hack_sinit_called v astate =
    SafeAttributes.is_hack_sinit_called (CanonValue.canon' astate v) astate
end

module CanonValue = struct
  include CanonValue

  let downcast = downcast
end
