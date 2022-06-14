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
module BaseStack = PulseBaseStack
module BaseMemory = PulseBaseMemory
module BaseAddressAttributes = PulseBaseAddressAttributes
module Decompiler = PulseDecompiler
module PathContext = PulsePathContext
module UninitBlocklist = PulseUninitBlocklist

(** signature common to the "normal" [Domain], representing the post at the current program point,
    and the inverted [PreDomain], representing the inferred pre-condition*)
module type BaseDomainSig = sig
  (* private because the lattice is not the same for preconditions and postconditions so we don't
     want to confuse them *)
  type t = private BaseDomain.t [@@deriving compare, equal, yojson_of]
end

(* defined in two parts to avoid exporting internal APIs to the .mli *)
module type BaseDomainSig_ = sig
  include BaseDomainSig

  val empty : t

  val update : ?stack:BaseStack.t -> ?heap:BaseMemory.t -> ?attrs:BaseAddressAttributes.t -> t -> t

  val filter_addr : f:(AbstractValue.t -> bool) -> t -> t
  (** filter both heap and attrs *)

  val filter_addr_with_discarded_addrs :
    heap_only:bool -> f:(AbstractValue.t -> bool) -> t -> t * AbstractValue.t list
  (** compute new state containing only reachable addresses in its heap and attributes, as well as
      the list of discarded unreachable addresses *)

  val subst_var : for_summary:bool -> AbstractValue.t * AbstractValue.t -> t -> t SatUnsat.t

  val pp : F.formatter -> t -> unit
end

(* just to expose record field names without having to type
   [BaseDomain.heap] *)
type base_domain = BaseDomain.t =
  {heap: BaseMemory.t; stack: BaseStack.t; attrs: BaseAddressAttributes.t}

(** represents the post abstract state at each program point *)
module PostDomain : sig
  include BaseDomainSig_

  val initialize : AbstractValue.t -> t -> t
end = struct
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


  let filter_addr ~f foot =
    let heap' = BaseMemory.filter (fun address _ -> f address) foot.heap in
    let attrs' = BaseAddressAttributes.filter (fun address _ -> f address) foot.attrs in
    update ~heap:heap' ~attrs:attrs' foot


  let filter_addr_with_discarded_addrs ~heap_only ~f foot =
    let heap' = BaseMemory.filter (fun address _ -> f address) foot.heap in
    let attrs', discarded_addresses =
      if heap_only then (foot.attrs, [])
      else BaseAddressAttributes.filter_with_discarded_addrs f foot.attrs
    in
    (update ~heap:heap' ~attrs:attrs' foot, discarded_addresses)


  let initialize address x =
    let attrs = BaseAddressAttributes.initialize address x.attrs in
    update ~attrs x
end

(* NOTE: [PreDomain] and [Domain] theoretically differ in that [PreDomain] should be the inverted lattice of [Domain], but since we never actually join states or check implication the two collapse into one. *)

(** represents the inferred pre-condition at each program point, biabduction style *)
module PreDomain : BaseDomainSig_ = PostDomain

(* see documentation in this file's .mli *)
type t =
  { post: PostDomain.t
  ; pre: PreDomain.t
  ; path_condition: PathCondition.t
  ; decompiler: (Decompiler.t[@yojson.opaque] [@equal.ignore] [@compare.ignore])
  ; topl: (PulseTopl.state[@yojson.opaque])
  ; need_specialization: bool
  ; skipped_calls: SkippedCalls.t }
[@@deriving compare, equal, yojson_of]

let pp f {post; pre; path_condition; decompiler; need_specialization; topl; skipped_calls} =
  let pp_decompiler f =
    if Config.debug_level_analysis >= 3 then F.fprintf f "decompiler=%a;@;" Decompiler.pp decompiler
  in
  F.fprintf f "@[<v>%a@;%a@;PRE=[%a]@;%tneed_specialization=%b@;skipped_calls=%a@;Topl=%a@]"
    PathCondition.pp path_condition PostDomain.pp post PreDomain.pp pre pp_decompiler
    need_specialization SkippedCalls.pp skipped_calls PulseTopl.pp_state topl


let set_path_condition path_condition astate = {astate with path_condition}

let set_need_specialization astate = {astate with need_specialization= true}

let unset_need_specialization astate = {astate with need_specialization= false}

let map_decompiler astate ~f = {astate with decompiler= f astate.decompiler}

let leq ~lhs ~rhs =
  phys_equal lhs rhs
  || SkippedCalls.leq ~lhs:lhs.skipped_calls ~rhs:rhs.skipped_calls
     && PathCondition.equal lhs.path_condition rhs.path_condition
     &&
     match
       BaseDomain.isograph_map BaseDomain.empty_mapping
         ~lhs:(lhs.pre :> BaseDomain.t)
         ~rhs:(rhs.pre :> BaseDomain.t)
     with
     | NotIsomorphic ->
         false
     | IsomorphicUpTo foot_mapping ->
         BaseDomain.is_isograph foot_mapping
           ~lhs:(lhs.post :> BaseDomain.t)
           ~rhs:(rhs.post :> BaseDomain.t)


let initialize address astate = {astate with post= PostDomain.initialize address astate.post}

module Stack = struct
  let is_abducible astate var =
    (* HACK: formals are pre-registered in the initial state *)
    BaseStack.mem var (astate.pre :> base_domain).stack || Var.is_global var


  (** [astate] with [astate.post.stack = f astate.post.stack] *)
  let map_post_stack ~f astate =
    let new_post = PostDomain.update astate.post ~stack:(f (astate.post :> base_domain).stack) in
    if phys_equal new_post astate.post then astate else {astate with post= new_post}


  let eval path location origin var astate =
    let astate, addr_hist =
      match BaseStack.find_opt var (astate.post :> base_domain).stack with
      | Some addr_hist ->
          (astate, addr_hist)
      | None ->
          let addr = AbstractValue.mk_fresh () in
          let addr_hist = (addr, origin) in
          let post_stack = BaseStack.add var addr_hist (astate.post :> base_domain).stack in
          let post_heap =
            if Config.pulse_isl then
              BaseMemory.register_address addr (astate.post :> base_domain).heap
            else (astate.post :> base_domain).heap
          in
          let post_attrs =
            if Config.pulse_isl then
              let access_trace = Trace.Immediate {location; history= ValueHistory.epoch} in
              BaseAddressAttributes.add_one addr
                (MustBeValid (path.PathContext.timestamp, access_trace, None))
                (astate.post :> base_domain).attrs
            else (astate.post :> base_domain).attrs
          in
          let pre =
            (* do not overwrite values of variables already in the pre *)
            if
              (not (BaseStack.mem var (astate.pre :> base_domain).stack)) && is_abducible astate var
            then
              (* HACK: do not record the history of values in the pre as they are unused *)
              let foot_stack =
                BaseStack.add var (addr, ValueHistory.epoch) (astate.pre :> base_domain).stack
              in
              let foot_heap = BaseMemory.register_address addr (astate.pre :> base_domain).heap in
              PreDomain.update ~stack:foot_stack ~heap:foot_heap astate.pre
            else astate.pre
          in
          ( { post= PostDomain.update astate.post ~stack:post_stack ~heap:post_heap ~attrs:post_attrs
            ; pre
            ; path_condition= astate.path_condition
            ; decompiler= astate.decompiler
            ; need_specialization= astate.need_specialization
            ; topl= astate.topl
            ; skipped_calls= astate.skipped_calls }
          , addr_hist )
    in
    let astate =
      map_decompiler astate ~f:(fun decompiler ->
          Decompiler.add_var_source (fst addr_hist) var decompiler )
    in
    (astate, addr_hist)


  let add var addr_loc_opt astate =
    map_post_stack astate ~f:(fun stack -> BaseStack.add var addr_loc_opt stack)


  let remove_vars vars astate =
    let vars_to_remove =
      let is_in_pre var astate = BaseStack.mem var (astate.pre :> base_domain).stack in
      List.filter vars ~f:(fun var -> not (is_in_pre var astate))
    in
    map_post_stack astate ~f:(fun stack ->
        BaseStack.filter (fun var _ -> not (List.mem ~equal:Var.equal vars_to_remove var)) stack )


  let fold f astate accum = BaseStack.fold f (astate.post :> base_domain).stack accum

  let find_opt var astate = BaseStack.find_opt var (astate.post :> base_domain).stack

  let mem var astate = BaseStack.mem var (astate.post :> base_domain).stack

  let exists f astate = BaseStack.exists f (astate.post :> base_domain).stack

  let keys astate =
    BaseStack.fold (fun key _ keys -> key :: keys) (astate.post :> base_domain).stack []
end

module AddressAttributes = struct
  open IResult.Let_syntax

  (** if [address] is in [pre] then add the attribute [attr] *)
  let abduce_attribute address attribute astate =
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
        L.d_printfln "No abducing %a:%a, the address has been written" AbstractValue.pp address
          Attribute.pp attribute ;
        astate.pre )
      else (
        L.d_printfln "Abducing %a:%a" AbstractValue.pp address Attribute.pp attribute ;
        if BaseMemory.mem address (astate.pre :> base_domain).heap then
          PreDomain.update astate.pre
            ~attrs:
              (BaseAddressAttributes.add_one address attribute (astate.pre :> base_domain).attrs)
        else astate.pre )
    in
    if phys_equal new_pre astate.pre then astate else {astate with pre= new_pre}


  let check_valid path ?must_be_valid_reason access_trace addr astate =
    let+ () = BaseAddressAttributes.check_valid addr (astate.post :> base_domain).attrs in
    (* if [address] is in [pre] and it should be valid then that fact goes in the precondition *)
    abduce_attribute addr
      (MustBeValid (path.PathContext.timestamp, access_trace, must_be_valid_reason))
      astate


  let check_initialized path access_trace addr astate =
    let attrs = (astate.post :> base_domain).attrs in
    let+ () = BaseAddressAttributes.check_initialized addr attrs in
    let is_written_to =
      Option.exists (BaseAddressAttributes.find_opt addr attrs) ~f:(fun attrs ->
          Attribute.Attributes.get_written_to attrs |> Option.is_some )
    in
    if is_written_to then astate
    else abduce_attribute addr (MustBeInitialized (path.PathContext.timestamp, access_trace)) astate


  let add_taint_sink path sink trace addr astate =
    abduce_attribute addr (MustNotBeTainted (path.PathContext.timestamp, sink, trace)) astate


  let get_taint_source_and_sanitizer addr astate =
    let attrs = (astate.post :> base_domain).attrs in
    let open IOption.Let_syntax in
    let* addr_attrs = BaseAddressAttributes.find_opt addr attrs in
    let+ source = Attribute.Attributes.get_tainted addr_attrs in
    let sanitizer_opt = Attribute.Attributes.get_taint_sanitized addr_attrs in
    (source, sanitizer_opt)


  let get_propagate_taint_from addr astate =
    let attrs = (astate.post :> base_domain).attrs in
    let open IOption.Let_syntax in
    let* addr_attrs = BaseAddressAttributes.find_opt addr attrs in
    Attribute.Attributes.get_propagate_taint_from addr_attrs


  (** [astate] with [astate.post.attrs = f astate.post.attrs] *)
  let map_post_attrs ~f astate =
    let new_post = PostDomain.update astate.post ~attrs:(f (astate.post :> base_domain).attrs) in
    if phys_equal new_post astate.post then astate else {astate with post= new_post}


  (** [astate] with [astate.pre.attrs = f astate.pre.attrs] *)
  let map_pre_attrs ~f astate =
    let new_pre = PreDomain.update astate.pre ~attrs:(f (astate.pre :> base_domain).attrs) in
    if phys_equal new_pre astate.pre then astate else {astate with pre= new_pre}


  let invalidate address invalidation location astate =
    let astate =
      map_post_attrs ~f:(BaseAddressAttributes.invalidate address invalidation location) astate
    in
    if Config.pulse_isl then
      map_post_attrs ~f:(BaseAddressAttributes.remove_must_be_valid_attr (fst address)) astate
      |> map_post_attrs ~f:(BaseAddressAttributes.remove_isl_abduced_attr (fst address))
      |> map_post_attrs ~f:(BaseAddressAttributes.remove_allocation_attr (fst address))
    else astate


  let always_reachable address astate =
    map_post_attrs astate ~f:(BaseAddressAttributes.always_reachable address)


  let allocate allocator address location astate =
    map_post_attrs astate ~f:(BaseAddressAttributes.allocate allocator address location)


  let java_resource_release address astate =
    map_post_attrs astate ~f:(BaseAddressAttributes.java_resource_release address)


  let get_dynamic_type addr astate =
    BaseAddressAttributes.get_dynamic_type (astate.post :> base_domain).attrs addr


  let get_allocation addr astate =
    BaseAddressAttributes.get_allocation addr (astate.post :> base_domain).attrs


  let get_copied_into addr astate =
    BaseAddressAttributes.get_copied_into addr (astate.post :> base_domain).attrs


  let get_source_origin_of_copy addr astate =
    BaseAddressAttributes.get_source_origin_of_copy addr (astate.post :> base_domain).attrs


  let add_dynamic_type typ address astate =
    map_post_attrs astate ~f:(BaseAddressAttributes.add_dynamic_type typ address)


  let add_ref_counted address astate =
    map_post_attrs astate ~f:(BaseAddressAttributes.add_ref_counted address)


  let is_ref_counted addr astate =
    BaseAddressAttributes.is_ref_counted addr (astate.post :> base_domain).attrs


  let remove_allocation_attr address astate =
    map_post_attrs astate ~f:(BaseAddressAttributes.remove_allocation_attr address)


  let remove_taint_attrs address astate =
    map_post_attrs astate ~f:(BaseAddressAttributes.remove_taint_attrs address)


  let add_one address attributes astate =
    map_post_attrs astate ~f:(BaseAddressAttributes.add_one address attributes)


  let remove_islabduced_attr address astate =
    map_post_attrs astate ~f:(BaseAddressAttributes.remove_isl_abduced_attr address)


  let remove_must_be_valid_attr address astate =
    map_pre_attrs astate ~f:(BaseAddressAttributes.remove_must_be_valid_attr address)


  let get_closure_proc_name addr astate =
    BaseAddressAttributes.get_closure_proc_name addr (astate.post :> base_domain).attrs


  let std_vector_reserve addr astate =
    map_post_attrs astate ~f:(BaseAddressAttributes.std_vector_reserve addr)


  let is_java_resource_released addr astate =
    BaseAddressAttributes.is_java_resource_released addr (astate.post :> base_domain).attrs


  let is_std_vector_reserved addr astate =
    BaseAddressAttributes.is_std_vector_reserved addr (astate.post :> base_domain).attrs


  let mark_as_end_of_collection addr astate =
    map_post_attrs astate ~f:(BaseAddressAttributes.mark_as_end_of_collection addr)


  let add_unreachable_at addr location astate =
    map_post_attrs astate ~f:(BaseAddressAttributes.add_unreachable_at addr location)


  let is_end_of_collection addr astate =
    BaseAddressAttributes.is_end_of_collection addr (astate.post :> base_domain).attrs


  let abduce_and_add value attrs astate =
    Attributes.fold attrs ~init:astate ~f:(fun astate attr ->
        let astate =
          if Attribute.is_suitable_for_pre attr then abduce_attribute value attr astate else astate
        in
        let astate =
          if Attribute.is_suitable_for_post attr then add_one value attr astate else astate
        in
        match attr with Attribute.WrittenTo _ -> initialize value astate | _ -> astate )


  let add_attrs value attrs astate =
    let astate =
      match Attributes.get_invalid attrs with
      | Some _ ->
          remove_must_be_valid_attr value astate
          |> remove_allocation_attr value |> remove_islabduced_attr value
      | None ->
          astate
    in
    Attributes.fold attrs ~init:astate ~f:(fun astate attr ->
        let astate = add_one value attr astate in
        match attr with Attribute.WrittenTo _ -> initialize value astate | _ -> astate )


  let find_opt address astate =
    BaseAddressAttributes.find_opt address (astate.post :> base_domain).attrs


  let check_valid_isl path access_trace addr ?(null_noop = false) astate =
    L.d_printfln "*****check_valid_isl: addr*** %a@\n" AbstractValue.pp addr ;
    match BaseAddressAttributes.get_invalid addr (astate.post :> BaseDomain.t).attrs with
    | None ->
        if
          BaseAddressAttributes.is_must_be_valid_or_allocated_isl addr
            (astate.post :> BaseDomain.t).attrs
        then [Ok astate]
        else
          let null_astates =
            if PathCondition.is_known_not_equal_zero astate.path_condition addr then []
            else
              let null_attr =
                Attribute.Invalid (Invalidation.ConstantDereference IntLit.zero, access_trace)
              in
              let null_astate = add_one addr null_attr astate |> abduce_attribute addr null_attr in
              if null_noop then [Ok null_astate] else [Error (`ISLError null_astate)]
          in
          let not_null_astates =
            if PathCondition.is_known_zero astate.path_condition addr then []
            else
              let valid_astate =
                let abdalloc = Attribute.ISLAbduced access_trace in
                let valid_attr =
                  Attribute.MustBeValid (path.PathContext.timestamp, access_trace, None)
                in
                add_one addr abdalloc astate |> abduce_attribute addr valid_attr
                |> abduce_attribute addr abdalloc
              in
              let invalid_free =
                (*C or Cpp?*)
                let invalid_attr = Attribute.Invalid (CFree, access_trace) in
                abduce_attribute addr invalid_attr astate |> add_one addr invalid_attr
              in
              [Ok valid_astate; Error (`ISLError invalid_free)]
          in
          not_null_astates @ null_astates
    | Some (invalidation, invalidation_trace) ->
        [Error (`InvalidAccess (addr, invalidation, invalidation_trace, astate))]
end

module Memory = struct
  module Access = BaseMemory.Access
  module Edges = BaseMemory.Edges

  (** [astate] with [astate.post.heap = f astate.post.heap] *)
  let map_post_heap ~f astate =
    let new_post = PostDomain.update astate.post ~heap:(f (astate.post :> base_domain).heap) in
    if phys_equal new_post astate.post then astate else {astate with post= new_post}


  let add_edge (addr, history) access new_addr_hist location astate =
    map_post_heap astate ~f:(BaseMemory.add_edge addr access new_addr_hist)
    |> AddressAttributes.map_post_attrs
         ~f:(BaseAddressAttributes.add_one addr (WrittenTo (Trace.Immediate {location; history})))


  let find_edge_opt address access astate =
    BaseMemory.find_edge_opt address access (astate.post :> base_domain).heap


  let eval_edge (addr_src, hist_src) access astate =
    let astate, addr_hist_dst =
      match find_edge_opt addr_src access astate with
      | Some addr_hist_dst ->
          (astate, addr_hist_dst)
      | None ->
          let addr_dst = AbstractValue.mk_fresh () in
          let addr_hist_dst = (addr_dst, hist_src) in
          let post_heap =
            BaseMemory.add_edge addr_src access addr_hist_dst (astate.post :> base_domain).heap
          in
          let foot_heap =
            if BaseMemory.mem addr_src (astate.pre :> base_domain).heap then
              (* HACK: do not record the history of values in the pre as they are unused *)
              BaseMemory.add_edge addr_src access (addr_dst, ValueHistory.epoch)
                (astate.pre :> base_domain).heap
              |> BaseMemory.register_address addr_dst
            else (astate.pre :> base_domain).heap
          in
          ( { post= PostDomain.update astate.post ~heap:post_heap
            ; pre= PreDomain.update astate.pre ~heap:foot_heap
            ; path_condition= astate.path_condition
            ; decompiler= astate.decompiler
            ; need_specialization= astate.need_specialization
            ; topl= astate.topl
            ; skipped_calls= astate.skipped_calls }
          , addr_hist_dst )
    in
    let astate =
      map_decompiler astate ~f:(fun decompiler ->
          Decompiler.add_access_source (fst addr_hist_dst) access ~src:addr_src
            (astate.post :> base_domain).attrs decompiler )
    in
    (astate, addr_hist_dst)


  let find_opt address astate = BaseMemory.find_opt address (astate.post :> base_domain).heap
end

let add_edge_on_src timestamp src location stack =
  match src with
  | `LocalDecl (pvar, addr_opt) -> (
    match addr_opt with
    | None ->
        let addr = AbstractValue.mk_fresh () in
        let history = ValueHistory.singleton (VariableDeclared (pvar, location, timestamp)) in
        (BaseStack.add (Var.of_pvar pvar) (addr, history) stack, addr)
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
      let attrs =
        if Config.pulse_isl then
          BaseAddressAttributes.add_one addr
            (MustBeValid (Timestamp.t0, Immediate {location; history= ValueHistory.epoch}, None))
            attrs
        else attrs
      in
      let attrs = BaseAddressAttributes.add_one addr Uninitialized attrs in
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
        List.fold fields ~init ~f:(fun (acc : PostDomain.t) (field, field_typ, _) ->
            if Fieldname.is_internal field then acc
            else
              let field_addr = AbstractValue.mk_fresh () in
              let fields = RevList.cons field fields_prefix in
              let history =
                ValueHistory.singleton (StructFieldAddressCreated (fields, location, timestamp))
              in
              let heap =
                BaseMemory.add_edge addr (HilExp.Access.FieldAccess field) (field_addr, history)
                  (acc :> base_domain).heap
              in
              PostDomain.update ~heap acc
              |> set_uninitialized_post tenv timestamp (`Malloc field_addr) field_typ location
                   ~fields_prefix:fields ) )
  | Tarray _ | Tvoid | Tfun | TVar _ ->
      (* We ignore tricky types to mark uninitialized addresses. *)
      post


let set_uninitialized tenv {PathContext.timestamp} src typ location x =
  {x with post= set_uninitialized_post tenv timestamp src typ location x.post}


let mk_initial tenv proc_desc =
  (* HACK: save the formals in the stacks of the pre and the post to remember which local variables
     correspond to formals *)
  let proc_name = Procdesc.get_proc_name proc_desc in
  let location = Procdesc.get_loc proc_desc in
  let formals_and_captured =
    let init_var formal_or_captured pvar typ =
      let event =
        match formal_or_captured with
        | `Formal ->
            ValueHistory.FormalDeclared (pvar, location, Timestamp.t0)
        | `Captured mode ->
            ValueHistory.Capture {captured_as= pvar; mode; location; timestamp= Timestamp.t0}
      in
      (Var.of_pvar pvar, typ, (AbstractValue.mk_fresh (), ValueHistory.singleton event))
    in
    let formals =
      Procdesc.get_formals proc_desc
      |> List.map ~f:(fun (mangled, typ, _) -> init_var `Formal (Pvar.mk mangled proc_name) typ)
    in
    let captured =
      Procdesc.get_captured proc_desc
      |> List.map ~f:(fun {CapturedVar.pvar; typ; capture_mode} ->
             init_var (`Captured capture_mode) pvar typ )
    in
    captured @ formals
  in
  let initial_stack =
    List.fold formals_and_captured ~init:(PreDomain.empty :> BaseDomain.t).stack
      ~f:(fun stack (formal, _, addr_loc) -> BaseStack.add formal addr_loc stack)
  in
  let initial_heap =
    let register heap (_, _, (addr, _)) = BaseMemory.register_address addr heap in
    let isl_register_and_add_edge heap ((_, typ, (addr, _)) as arg) =
      let heap = register heap arg in
      match typ.Typ.desc with
      | Typ.Tptr _ ->
          let addr_dst = AbstractValue.mk_fresh () in
          BaseMemory.add_edge addr Dereference (addr_dst, ValueHistory.epoch) heap
          |> BaseMemory.register_address addr_dst
      | _ ->
          heap
    in
    List.fold formals_and_captured ~init:(PreDomain.empty :> base_domain).heap
      ~f:(if Config.pulse_isl then isl_register_and_add_edge else register)
  in
  let initial_attrs =
    if Config.pulse_isl then
      List.fold formals_and_captured ~init:(PreDomain.empty :> base_domain).attrs
        ~f:(fun attrs (_, _, (addr, _)) ->
          BaseAddressAttributes.add_one addr
            (MustBeValid (Timestamp.t0, Immediate {location; history= ValueHistory.epoch}, None))
            attrs )
    else BaseDomain.empty.attrs
  in
  let pre =
    PreDomain.update ~stack:initial_stack ~heap:initial_heap ~attrs:initial_attrs PreDomain.empty
  in
  let initial_heap, initial_attrs =
    if Config.pulse_isl then (initial_heap, initial_attrs)
    else ((PreDomain.empty :> base_domain).heap, (PreDomain.empty :> base_domain).attrs)
  in
  let post =
    PostDomain.update ~stack:initial_stack ~heap:initial_heap ~attrs:initial_attrs PostDomain.empty
  in
  let locals = Procdesc.get_locals proc_desc in
  let post =
    List.fold locals ~init:post
      ~f:(fun (acc : PostDomain.t) {ProcAttributes.name; typ; modify_in_block; is_constexpr} ->
        if modify_in_block || is_constexpr then acc
        else
          set_uninitialized_post tenv Timestamp.t0
            (`LocalDecl (Pvar.mk name proc_name, None))
            typ location acc )
  in
  let astate =
    { pre
    ; post
    ; path_condition= PathCondition.true_
    ; decompiler= Decompiler.empty
    ; need_specialization= false
    ; topl= PulseTopl.start ()
    ; skipped_calls= SkippedCalls.empty }
  in
  let apply_aliases astate =
    (* If a function is alias-specialized, then we want to make sure all the captured
       variables and parameters aliasing each other share the same memory. To do so, we
       simply add a dereference access from each aliasing variables' address to the same
       address in the pre and the post of the initial state.

       e.g. f(x, y) with the aliasing information x = y will have the following pre:
         roots={ &x=v1, &y=v2 };
         mem  ={ v1 -> { * -> v3 }, v2 -> { * -> v3 }, v3 -> { } };
         attrs={ };
    *)
    match Procdesc.get_specialized_with_aliasing_info proc_desc with
    | None ->
        astate
    | Some {aliases} ->
        let pre_heap = (astate.pre :> base_domain).heap in
        let post_heap = (astate.post :> base_domain).heap in
        let pre_heap, post_heap =
          List.fold aliases ~init:(pre_heap, post_heap) ~f:(fun (pre_heap, post_heap) alias ->
              let pre_heap, post_heap, _ =
                List.fold alias ~init:(pre_heap, post_heap, None)
                  ~f:(fun (pre_heap, post_heap, addr) pvar ->
                    let addr =
                      match addr with None -> AbstractValue.mk_fresh () | Some addr -> addr
                    in
                    let src_addr, addr_hist = BaseStack.find (Var.of_pvar pvar) initial_stack in
                    let pre_heap =
                      BaseMemory.add_edge src_addr Dereference (addr, ValueHistory.epoch) pre_heap
                      |> BaseMemory.register_address addr
                    in
                    let post_heap =
                      BaseMemory.add_edge src_addr Dereference (addr, addr_hist) post_heap
                    in
                    (pre_heap, post_heap, Some addr) )
              in
              (pre_heap, post_heap) )
        in
        { astate with
          pre= PreDomain.update ~heap:pre_heap pre
        ; post= PostDomain.update ~heap:post_heap post }
  in
  apply_aliases astate


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


let skipped_calls_match_pattern astate =
  (* For every skipped function, there needs to be at least one regexp given in --pulse_report_ignore_java_methods_patterns
     that matches it *)
  Option.value_map Config.pulse_report_ignore_unknown_java_methods_patterns ~default:true
    ~f:(fun patt ->
      SkippedCalls.for_all
        (fun skipped_proc _ -> Str.string_match patt (Procname.to_string skipped_proc) 0)
        astate.skipped_calls )


let check_memory_leaks ~live_addresses ~unreachable_addresses astate =
  let reaches_into addr addrs astate =
    AbstractValue.Set.mem addr addrs
    || BaseDomain.GraphVisit.fold_from_addresses (Seq.return addr) astate ~init:()
         ~already_visited:AbstractValue.Set.empty
         ~finish:(fun () -> (* didn't find any reachable address in [addrs] *) false)
         ~f:(fun () addr' rev_accesses ->
           (* We want to know if [addr] is still reachable from the value [addr'] by pointer
              arithmetic, hence detect if [addr'] is an offset away from [addr]. In particular, if
              we go through a [Dereference] then we have lost the connexion to the original
              address value.

              TODO: this should be part of [live_addresses] since all these addresses are actually
              live. *)
           let can_recover_root_from_accesses =
             List.for_all rev_accesses ~f:(fun (access : BaseMemory.Access.t) ->
                 match access with FieldAccess _ | ArrayAccess _ -> true | _ -> false )
           in
           if can_recover_root_from_accesses then
             if AbstractValue.Set.mem addr' addrs then (
               L.d_printfln ~color:Orange "Live address %a is reachable from %a" AbstractValue.pp
                 addr' AbstractValue.pp addr ;
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
        L.d_printfln ~color:Red "LEAK: unreachable address %a was allocated by %a" AbstractValue.pp
          addr Attribute.pp_allocator allocator ;
        (* last-chance checks: it could be that the value (or its canonical equal) is reachable via
           pointer arithmetic from live values. This is common in libraries that do their own memory
           management, e.g. they return a field of the malloc()'d pointer, and the latter is a fat
           pointer with, say, a reference count. For example in C:

           {[
            struct fat_int {
              size_t count;
              int data;
            };

            int *alloc_int() {
              fat_int *p = malloc(...);
              p->count = 1;
              return &(p->data);
            }
           ]}

           We don't have a precise enough memory model to understand everything that
           goes on here but we should at least not report a leak. *)
        let addr_canon = PathCondition.get_both_var_repr astate.path_condition addr in
        if
          reaches_into addr live_addresses (astate.post :> BaseDomain.t)
          || reaches_into addr_canon live_addresses (astate.post :> BaseDomain.t)
        then (
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
      match AddressAttributes.find_opt addr astate with
      | Some unreachable_attrs ->
          check_memory_leak addr unreachable_attrs
      | None ->
          Ok () )


(* A retain cycle is a memory path from an address to itself, following only
   strong references. From that definition, detecting them can be made
   trivial:
   Given an address and a list of adresses seen on the path, if the adress is
   part of the path then it is part of a retain cycle. Otherwise add that
   adress to the path and reiterate for each strongly referenced adresses
   there is from that adress. Explore paths starting from all dead addresses
   (if an address is still reachable from outside the function, then the
   cycle could be broken).
   To improve on that simple algorithm, we can keep track of another marker
   to indicate adresses that have already been explored to indicate there
   will not be any retain cycle to be found down that path and skip it.
   This is handled by [check_retain_cycle] which will recursively explore
   paths from a given adress and mark explored adresses in the [checked]
   list. This function is called over all the [dead_addresses].

   When reporting a retain cycle, we want to give the location of its
   creation, therefore we need to remember location of the latest assignement
   in the cycle *)
let check_retain_cycles ~dead_addresses tenv astate =
  let get_assignment_trace addr =
    match AddressAttributes.find_opt addr astate with
    | None ->
        None
    | Some attributes ->
        Attributes.get_written_to attributes
  in
  let compare_locs trace1 trace2 =
    let loc1 = Trace.get_outer_location trace1 in
    let loc2 = Trace.get_outer_location trace2 in
    Location.compare loc2 loc1
  in
  (* remember explored adresses to avoid reexploring path without retain cycles *)
  let checked = ref [] in
  let check_retain_cycle src_addr =
    (* [assignment_traces] tracks the assignments met in the retain cycle
       [seen] tracks addresses met in the current path
       [addr] is the address to explore
    *)
    let rec contains_cycle decompiler assignment_traces seen addr =
      if List.exists ~f:(AbstractValue.equal addr) !checked then Ok ()
      else if List.exists ~f:(AbstractValue.equal addr) seen then
        let assignment_traces = List.sort ~compare:compare_locs assignment_traces in
        match assignment_traces with
        | [] ->
            Ok ()
        | most_recent_trace :: _ ->
            let location = Trace.get_outer_location most_recent_trace in
            let value = Decompiler.find addr astate.decompiler in
            let path = Decompiler.find addr decompiler in
            Error (List.rev assignment_traces, value, path, location)
      else
        let res =
          match BaseMemory.find_opt addr (astate.post :> BaseDomain.t).heap with
          | None ->
              Ok ()
          | Some edges_pre ->
              BaseMemory.Edges.fold ~init:(Ok ()) edges_pre
                ~f:(fun acc (access, (accessed_addr, _)) ->
                  match acc with
                  | Error _ ->
                      acc
                  | Ok () ->
                      if BaseMemory.Access.is_strong_access tenv access then
                        let assignment_traces =
                          match access with
                          | HilExp.Access.FieldAccess _ -> (
                            match get_assignment_trace accessed_addr with
                            | None ->
                                assignment_traces
                            | Some assignment_trace ->
                                assignment_trace :: assignment_traces )
                          | _ ->
                              assignment_traces
                        in
                        let decompiler =
                          Decompiler.add_access_source accessed_addr access ~src:addr
                            (astate.post :> base_domain).attrs decompiler
                        in
                        contains_cycle decompiler assignment_traces (addr :: seen) accessed_addr
                      else Ok () )
        in
        (* all paths down [addr] have been explored *)
        checked := addr :: !checked ;
        res
    in
    contains_cycle astate.decompiler [] [] src_addr
  in
  List.fold_result dead_addresses ~init:() ~f:(fun () addr ->
      match AddressAttributes.find_opt addr astate with
      | None ->
          Ok ()
      | Some attributes ->
          (* retain cycles exist in the context of reference counting *)
          if Attributes.is_ref_counted attributes then check_retain_cycle addr else Ok () )


let get_all_addrs_marked_as_always_reachable {post} =
  BaseAddressAttributes.fold
    (fun address attr addresses ->
      if Attributes.is_always_reachable attr then Seq.cons address addresses else addresses )
    (post :> BaseDomain.t).attrs Seq.empty


let discard_unreachable_ ~for_summary ({pre; post} as astate) =
  let pre_addresses = BaseDomain.reachable_addresses (pre :> BaseDomain.t) in
  let pre_new =
    PreDomain.filter_addr ~f:(fun address -> AbstractValue.Set.mem address pre_addresses) pre
  in
  let post_addresses = BaseDomain.reachable_addresses (post :> BaseDomain.t) in
  let always_reachable_addresses = get_all_addrs_marked_as_always_reachable astate in
  let always_reachable_trans_closure =
    BaseDomain.reachable_addresses_from always_reachable_addresses
      (post :> BaseDomain.t)
      ~already_visited:post_addresses
  in
  let canon_addresses =
    AbstractValue.Set.map (PathCondition.get_both_var_repr astate.path_condition) pre_addresses
    |> AbstractValue.Set.fold
         (fun addr acc ->
           AbstractValue.Set.add (PathCondition.get_both_var_repr astate.path_condition addr) acc )
         post_addresses
  in
  let post_new, dead_addresses =
    (* keep attributes of dead addresses unless we are creating a summary *)
    PostDomain.filter_addr_with_discarded_addrs ~heap_only:(not for_summary)
      ~f:(fun address ->
        AbstractValue.Set.mem address pre_addresses
        || AbstractValue.Set.mem address post_addresses
        || AbstractValue.Set.mem address always_reachable_trans_closure
        ||
        let canon_addr = PathCondition.get_both_var_repr astate.path_condition address in
        AbstractValue.Set.mem canon_addr canon_addresses )
      post
  in
  (* note: we don't call {!PulsePathCondition.simplify} *)
  let astate =
    if phys_equal pre_new pre && phys_equal post_new post then astate
    else {astate with pre= pre_new; post= post_new}
  in
  (astate, pre_addresses, post_addresses, dead_addresses)


let get_unreachable_attributes {post} =
  let post_addresses = BaseDomain.reachable_addresses (post :> BaseDomain.t) in
  BaseAddressAttributes.fold
    (fun address _ dead_addresses ->
      if AbstractValue.Set.mem address post_addresses then dead_addresses
      else address :: dead_addresses )
    (post :> BaseDomain.t).attrs []


let get_reachable {pre; post} =
  let pre_keep = BaseDomain.reachable_addresses (pre :> BaseDomain.t) in
  let post_keep = BaseDomain.reachable_addresses (post :> BaseDomain.t) in
  AbstractValue.Set.union pre_keep post_keep


let apply_unknown_effect ?(havoc_filter = fun _ _ _ -> true) hist x astate =
  let havoc_accesses hist addr heap =
    match BaseMemory.find_opt addr heap with
    | None ->
        heap
    | Some edges ->
        let edges =
          BaseMemory.Edges.mapi edges ~f:(fun access value ->
              if havoc_filter addr access value then (
                L.d_printfln_escaped "havoc'ing access %a" BaseMemory.Access.pp access ;
                (AbstractValue.mk_fresh (), hist) )
              else value )
        in
        BaseMemory.add addr edges heap
  in
  let post = (astate.post :> BaseDomain.t) in
  let heap, attrs =
    BaseDomain.GraphVisit.fold_from_addresses (Seq.return x) post ~init:(post.heap, post.attrs)
      ~already_visited:AbstractValue.Set.empty
      ~f:(fun (heap, attrs) addr _edges ->
        let attrs =
          BaseAddressAttributes.remove_allocation_attr addr attrs
          |> BaseAddressAttributes.initialize addr
        in
        let heap = havoc_accesses hist addr heap in
        Continue (heap, attrs) )
      ~finish:Fn.id
    |> snd
  in
  {astate with post= PostDomain.update ~attrs ~heap astate.post}


let is_local var astate = not (Var.is_return var || Stack.is_abducible astate var)

let set_post_edges addr edges astate =
  if BaseMemory.Edges.is_empty edges then astate
  else Memory.map_post_heap astate ~f:(BaseMemory.add addr edges)


(* {3 Helper functions to traverse the two maps at once } *)

let find_post_cell_opt addr {post} = BaseDomain.find_cell_opt addr (post :> BaseDomain.t)

let set_post_cell (addr, history) (edges, attr_set) location astate =
  set_post_edges addr edges astate
  |> AddressAttributes.map_post_attrs ~f:(fun attrs ->
         BaseAddressAttributes.add_one addr (WrittenTo (Trace.Immediate {location; history})) attrs
         |> BaseAddressAttributes.add addr attr_set )


(** Inside a function, formals can be used as local variables. But when exiting the function, the
    initial values of formals must be restored. This recreates the graph structure from addresses of
    formals in the precondition into the post-condition, up to the first dereference. The reason for
    this complexity is purely to handle formals that are struct values. For example, if
    [pre=&x=vx, vx --.f-> v1 --.g-> v2 --*-> v3] and
    [post = &x=vx, vx --.f-> v1 --.g-> v2 --*-> v4, vx --.h->v5] then we change [post] to
    [vx --.f-> v1 --.g-> v2 --*-> v3] (i.e. [post]=[pre]). *)
let restore_formals_for_summary astate =
  (* The [visited] accumulator is to avoid cycles; they are impossible in theory at the moment but
     this could change. *)
  let rec restore_pre_var_value visited ~is_value_visible_outside addr astate =
    if AbstractValue.Set.mem addr visited then (
      L.internal_error
        "Found a cycle when restoring the values of formals, how did this happen?@\n%a@\n" pp astate ;
      astate )
    else
      let visited = AbstractValue.Set.add addr visited in
      let pre_heap = (astate.pre :> BaseDomain.t).heap in
      let post_heap = (astate.post :> BaseDomain.t).heap in
      match BaseMemory.find_opt addr pre_heap with
      | None ->
          if is_value_visible_outside then astate
          else Memory.map_post_heap astate ~f:(fun post_heap -> BaseMemory.remove addr post_heap)
      | Some pre_edges ->
          BaseMemory.Edges.fold pre_edges ~init:astate
            ~f:(fun astate (access, ((addr_dest, _) as addr_hist_dest)) ->
              match access with
              | Dereference ->
                  (* dereference: we reached the actual value and are done *)
                  if is_value_visible_outside && BaseMemory.has_edge addr access post_heap then
                    (* the changes are visible even outside of the procedure: do not restore the pre value
                       if the post has a value for this access too *)
                    astate
                  else
                    Memory.map_post_heap astate ~f:(fun post_heap ->
                        BaseMemory.add_edge addr access addr_hist_dest post_heap )
              | FieldAccess _ | ArrayAccess _ ->
                  (* inlined aggregate value: just an offset, not the actual value yet; recurse *)
                  Memory.map_post_heap astate ~f:(fun post_heap ->
                      BaseMemory.add_edge addr access addr_hist_dest post_heap )
                  |> restore_pre_var_value visited ~is_value_visible_outside addr_dest
              | TakeAddress ->
                  assert false )
  in
  let restore_pre_var x ((addr, _) as addr_hist) astate =
    (* the address of a variable never changes *)
    let astate = Stack.add x addr_hist astate in
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
let invalidate_locals pdesc astate : t =
  let attrs : BaseAddressAttributes.t = (astate.post :> BaseDomain.t).attrs in
  let attrs' =
    BaseAddressAttributes.fold
      (fun addr attrs acc ->
        Attributes.get_address_of_stack_variable attrs
        |> Option.value_map ~default:acc ~f:(fun (var, location, history) ->
               let get_local_typ_opt pvar =
                 Procdesc.get_locals pdesc
                 |> List.find_map ~f:(fun ProcAttributes.{name; typ; modify_in_block} ->
                        if (not modify_in_block) && Mangled.equal name (Pvar.get_name pvar) then
                          Some typ
                        else None )
               in
               match var with
               | Var.ProgramVar pvar ->
                   get_local_typ_opt pvar
                   |> Option.value_map ~default:acc
                        ~f:(add_out_of_scope_attribute addr pvar location history acc)
               | _ ->
                   acc ) )
      attrs attrs
  in
  if phys_equal attrs attrs' then astate
  else {astate with post= PostDomain.update astate.post ~attrs:attrs'}


let is_isl_without_allocation astate =
  BaseStack.for_all
    (fun _ (addr, _) ->
      match Memory.find_edge_opt addr HilExp.Access.Dereference astate with
      | Some (addr, _) -> (
        match AddressAttributes.find_opt addr astate with
        | None ->
            true
        | Some attrs ->
            not (Option.is_some (Attribute.Attributes.get_allocation attrs)) )
      | None ->
          true )
    (astate.post :> base_domain).stack


let is_pre_without_isl_abduced astate =
  BaseAddressAttributes.for_all
    (fun _ attrs -> match Attributes.get_isl_abduced attrs with None -> true | Some _ -> false)
    (astate.pre :> base_domain).attrs


type summary = t [@@deriving compare, equal, yojson_of]

let summary_with_need_specialization summary = {summary with need_specialization= true}

let is_heap_allocated {post; pre} v =
  BaseMemory.is_allocated (post :> BaseDomain.t).heap v
  || BaseMemory.is_allocated (pre :> BaseDomain.t).heap v


let is_stack_allocated stack_allocations v = AbstractValue.Set.mem v (Lazy.force stack_allocations)

let subst_var_in_post ~for_summary subst astate =
  let open SatUnsat.Import in
  let+ post = PostDomain.subst_var ~for_summary subst astate.post in
  if phys_equal astate.post post then astate else {astate with post}


let get_stack_allocated {post} =
  (* OPTIM: the post stack contains at least the pre stack so no need to check both *)
  BaseStack.fold
    (fun var (address, _) allocated ->
      if Var.is_pvar var then AbstractValue.Set.add address allocated else allocated )
    (post :> BaseDomain.t).stack AbstractValue.Set.empty


let incorporate_new_eqs ~for_summary astate new_eqs =
  let stack_allocations = lazy (get_stack_allocated astate) in
  List.fold_until new_eqs ~init:(astate, None)
    ~finish:(fun astate_error -> Sat astate_error)
    ~f:(fun (astate, error) (new_eq : PulseFormula.new_eq) ->
      match new_eq with
      | Equal (v1, v2) when AbstractValue.equal v1 v2 ->
          Continue (astate, error)
      | Equal (v1, v2) -> (
        match subst_var_in_post ~for_summary (v1, v2) astate with
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
          match BaseAddressAttributes.get_must_be_valid v (astate.pre :> base_domain).attrs with
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
let canonicalize astate =
  let open SatUnsat.Import in
  let get_var_repr v = PathCondition.get_known_var_repr astate.path_condition v in
  let canonicalize_pre (pre : PreDomain.t) =
    (* (ab)use canonicalization to filter out empty edges in the heap and detect aliasing
       contradictions *)
    let* stack' = BaseStack.canonicalize ~get_var_repr:Fn.id (pre :> BaseDomain.t).stack in
    let+ heap' = BaseMemory.canonicalize ~get_var_repr:Fn.id (pre :> BaseDomain.t).heap in
    let attrs' = BaseAddressAttributes.remove_unsuitable_for_summary (pre :> BaseDomain.t).attrs in
    PreDomain.update ~stack:stack' ~heap:heap' ~attrs:attrs' pre
  in
  let canonicalize_post (post : PostDomain.t) =
    let* stack' = BaseStack.canonicalize ~get_var_repr (post :> BaseDomain.t).stack in
    (* note: this step also de-registers addresses pointing to empty edges *)
    let+ heap' = BaseMemory.canonicalize ~get_var_repr (post :> BaseDomain.t).heap in
    let attrs' = BaseAddressAttributes.canonicalize ~get_var_repr (post :> BaseDomain.t).attrs in
    PostDomain.update ~stack:stack' ~heap:heap' ~attrs:attrs' post
  in
  let* pre = canonicalize_pre astate.pre in
  let+ post = canonicalize_post astate.post in
  {astate with pre; post}


let filter_for_summary tenv proc_name astate0 =
  let open SatUnsat.Import in
  L.d_printfln "Canonicalizing...@\n" ;
  let* astate_before_filter = canonicalize astate0 in
  L.d_printfln "Canonicalized state: %a@\n" pp astate_before_filter ;
  (* Remove the stack from the post as it's not used: the values of formals are the same as in the
     pre. Moreover, formals can be treated as local variables inside the function's body so we need
     to restore their initial values at the end of the function. Removing them altogether achieves
     this. *)
  let astate = restore_formals_for_summary astate_before_filter in
  let astate =
    { astate with
      topl=
        PulseTopl.filter_for_summary
          ~get_dynamic_type:
            (BaseAddressAttributes.get_dynamic_type (astate.post :> BaseDomain.t).attrs)
          astate.path_condition astate.topl }
  in
  let astate, pre_live_addresses, post_live_addresses, dead_addresses =
    discard_unreachable_ ~for_summary:true astate
  in
  let can_be_pruned =
    if PatternMatch.is_entry_point proc_name then
      (* report all latent issues at entry points *)
      AbstractValue.Set.empty
    else pre_live_addresses
  in
  let live_addresses = AbstractValue.Set.union pre_live_addresses post_live_addresses in
  let get_dynamic_type =
    BaseAddressAttributes.get_dynamic_type (astate_before_filter.post :> BaseDomain.t).attrs
  in
  let+ path_condition, live_via_arithmetic, new_eqs =
    PathCondition.simplify tenv ~get_dynamic_type ~can_be_pruned ~keep:live_addresses
      astate.path_condition
  in
  let live_addresses = AbstractValue.Set.union live_addresses live_via_arithmetic in
  ( { astate with
      path_condition
    ; topl= PulseTopl.simplify ~keep:live_addresses ~get_dynamic_type ~path_condition astate.topl }
  , live_addresses
  , (* we could filter out the [live_addresses] if needed; right now they might overlap *)
    dead_addresses
  , new_eqs )


let summary_of_post tenv pdesc location astate0 =
  let open SatUnsat.Import in
  (* do not store the decompiler in the summary and make sure we only use the original one by
     marking it invalid *)
  let astate = {astate0 with decompiler= Decompiler.invalid} in
  (* NOTE: we normalize (to strengthen the equality relation used by canonicalization) then
     canonicalize *before* garbage collecting unused addresses in case we detect any last-minute
     contradictions about addresses we are about to garbage collect *)
  let path_condition, is_unsat, new_eqs =
    PathCondition.is_unsat_expensive tenv
      ~get_dynamic_type:(BaseAddressAttributes.get_dynamic_type (astate.post :> BaseDomain.t).attrs)
      astate.path_condition
  in
  let* () = if is_unsat then Unsat else Sat () in
  let astate = {astate with path_condition} in
  let* astate, error = incorporate_new_eqs ~for_summary:true astate new_eqs in
  let astate_before_filter = astate in
  let* astate, live_addresses, dead_addresses, new_eqs =
    filter_for_summary tenv (Procdesc.get_proc_name pdesc) astate
  in
  let+ astate, error =
    match error with
    | None ->
        incorporate_new_eqs ~for_summary:true astate new_eqs
    | Some _ ->
        Sat (astate, error)
  in
  match error with
  | None -> (
    match
      check_retain_cycles ~dead_addresses tenv
        {astate_before_filter with decompiler= astate0.decompiler}
    with
    | Error (assignment_traces, value, path, location) ->
        Error (`RetainCycle (astate, assignment_traces, value, path, location))
    | Ok () -> (
      (* NOTE: it's important for correctness that we check leaks last because we are going to carry
         on with the astate after the leak and we don't want to accidentally skip modifications of
         the state because of the error monad *)
      match
        check_memory_leaks ~live_addresses ~unreachable_addresses:dead_addresses
          astate_before_filter
      with
      | Ok () ->
          Ok (invalidate_locals pdesc astate)
      | Error (unreachable_location, JavaResource class_name, trace) ->
          Error
            (`ResourceLeak
              (astate, class_name, trace, Option.value unreachable_location ~default:location) )
      | Error (unreachable_location, allocator, trace) ->
          Error
            (`MemoryLeak
              (astate, allocator, trace, Option.value unreachable_location ~default:location) ) ) )
  | Some (address, must_be_valid) ->
      Error
        (`PotentialInvalidAccessSummary
          (astate, Decompiler.find address astate0.decompiler, must_be_valid) )


let get_pre {pre} = (pre :> BaseDomain.t)

let get_post {post} = (post :> BaseDomain.t)

(* re-exported for mli *)
let incorporate_new_eqs new_eqs astate =
  if PathCondition.is_unsat_cheap astate.path_condition then Ok astate
  else
    match incorporate_new_eqs ~for_summary:false astate new_eqs with
    | Unsat ->
        Ok {astate with path_condition= PathCondition.false_}
    | Sat (astate, None) ->
        Ok astate
    | Sat (astate, Some (address, must_be_valid)) ->
        L.d_printfln ~color:Red "potential error if %a is null" AbstractValue.pp address ;
        Error (`PotentialInvalidAccess (astate, address, must_be_valid))


let incorporate_new_eqs_on_val new_eqs v =
  List.find_map new_eqs ~f:(function
    | PulseFormula.Equal (v1, v2) when AbstractValue.equal v1 v ->
        Some v2
    | _ ->
        None )
  |> Option.value ~default:v


module Topl = struct
  let small_step loc ~keep event astate =
    { astate with
      topl=
        PulseTopl.small_step loc ~keep
          ~get_dynamic_type:
            (BaseAddressAttributes.get_dynamic_type (astate.post :> BaseDomain.t).attrs)
          ~path_condition:astate.path_condition event astate.topl }


  let large_step ~call_location ~callee_proc_name ~substitution ~keep ~path_condition
      ~callee_prepost astate =
    { astate with
      topl=
        PulseTopl.large_step ~call_location ~callee_proc_name ~substitution ~keep
          ~get_dynamic_type:
            (BaseAddressAttributes.get_dynamic_type (astate.post :> BaseDomain.t).attrs)
          ~path_condition ~callee_prepost astate.topl }


  let get {topl} = topl
end
