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
module UninitBlocklist = PulseUninitBlocklist

(** signature common to the "normal" [Domain], representing the post at the current program point,
    and the inverted [PreDomain], representing the inferred pre-condition*)
module type BaseDomainSig = sig
  (* private because the lattice is not the same for preconditions and postconditions so we don't
     want to confuse them *)
  type t = private BaseDomain.t [@@deriving compare, equal, yojson_of]

  val empty : t

  val update : ?stack:BaseStack.t -> ?heap:BaseMemory.t -> ?attrs:BaseAddressAttributes.t -> t -> t

  val filter_addr : f:(AbstractValue.t -> bool) -> t -> t
  (** filter both heap and attrs *)

  val filter_addr_with_discarded_addrs :
    f:(AbstractValue.t -> bool) -> t -> t * AbstractValue.t list
  (** compute new state containing only reachable addresses in its heap and attributes, as well as
      the list of discarded unreachable addresses *)

  val subst_var : AbstractValue.t * AbstractValue.t -> t -> t SatUnsat.t

  val pp : F.formatter -> t -> unit
end

(* just to expose record field names without having to type
   [BaseDomain.heap] *)
type base_domain = BaseDomain.t =
  {heap: BaseMemory.t; stack: BaseStack.t; attrs: BaseAddressAttributes.t}

(** represents the post abstract state at each program point *)
module PostDomain : sig
  include BaseDomainSig

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


  let filter_addr_with_discarded_addrs ~f foot =
    let heap' = BaseMemory.filter (fun address _ -> f address) foot.heap in
    let attrs', discarded_addresses =
      BaseAddressAttributes.filter_with_discarded_addrs (fun address _ -> f address) foot.attrs
    in
    (update ~heap:heap' ~attrs:attrs' foot, discarded_addresses)


  let initialize address x =
    let attrs = BaseAddressAttributes.initialize address x.attrs in
    update ~attrs x
end

(* NOTE: [PreDomain] and [Domain] theoretically differ in that [PreDomain] should be the inverted lattice of [Domain], but since we never actually join states or check implication the two collapse into one. *)

(** represents the inferred pre-condition at each program point, biabduction style *)
module PreDomain : BaseDomainSig = PostDomain

(* see documentation in this file's .mli *)
type t =
  { post: PostDomain.t
  ; pre: PreDomain.t
  ; path_condition: PathCondition.t
  ; topl: (PulseTopl.state[@yojson.opaque])
  ; skipped_calls: SkippedCalls.t }
[@@deriving compare, equal, yojson_of]

let pp f {post; pre; topl; path_condition; skipped_calls} =
  F.fprintf f "@[<v>%a@;%a@;PRE=[%a]@;skipped_calls=%a@;Topl=%a@]" PathCondition.pp path_condition
    PostDomain.pp post PreDomain.pp pre SkippedCalls.pp skipped_calls PulseTopl.pp_state topl


let set_path_condition path_condition astate = {astate with path_condition}

let leq ~lhs ~rhs =
  phys_equal lhs rhs
  || SkippedCalls.leq ~lhs:lhs.skipped_calls ~rhs:rhs.skipped_calls
     && PathCondition.equal lhs.path_condition rhs.path_condition
     &&
     match
       BaseDomain.isograph_map BaseDomain.empty_mapping
         ~lhs:(rhs.pre :> BaseDomain.t)
         ~rhs:(lhs.pre :> BaseDomain.t)
     with
     | NotIsomorphic ->
         false
     | IsomorphicUpTo foot_mapping ->
         BaseDomain.is_isograph foot_mapping
           ~lhs:(lhs.post :> BaseDomain.t)
           ~rhs:(rhs.post :> BaseDomain.t)


let initialize address astate = {astate with post= PostDomain.initialize address astate.post}

let simplify_instanceof tenv astate =
  let attrs = (astate.post :> BaseDomain.t).attrs in
  let path_condition =
    PathCondition.simplify_instanceof tenv
      ~get_dynamic_type:(BaseAddressAttributes.get_dynamic_type attrs)
      astate.path_condition
  in
  {astate with path_condition}


module Stack = struct
  let is_abducible astate var =
    (* HACK: formals are pre-registered in the initial state *)
    BaseStack.mem var (astate.pre :> base_domain).stack || Var.is_global var


  (** [astate] with [astate.post.stack = f astate.post.stack] *)
  let map_post_stack ~f astate =
    let new_post = PostDomain.update astate.post ~stack:(f (astate.post :> base_domain).stack) in
    if phys_equal new_post astate.post then astate else {astate with post= new_post}


  let eval location origin var astate =
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
            let access_trace = Trace.Immediate {location; history= []} in
            BaseAddressAttributes.add_one addr (MustBeValid access_trace)
              (astate.post :> base_domain).attrs
          else (astate.post :> base_domain).attrs
        in
        let pre =
          (* do not overwrite values of variables already in the pre *)
          if (not (BaseStack.mem var (astate.pre :> base_domain).stack)) && is_abducible astate var
          then
            (* HACK: do not record the history of values in the pre as they are unused *)
            let foot_stack = BaseStack.add var (addr, []) (astate.pre :> base_domain).stack in
            let foot_heap = BaseMemory.register_address addr (astate.pre :> base_domain).heap in
            PreDomain.update ~stack:foot_stack ~heap:foot_heap astate.pre
          else astate.pre
        in
        ( { post= PostDomain.update astate.post ~stack:post_stack ~heap:post_heap ~attrs:post_attrs
          ; pre
          ; topl= astate.topl
          ; skipped_calls= astate.skipped_calls
          ; path_condition= astate.path_condition }
        , addr_hist )


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


  let check_valid access_trace addr astate =
    let+ () = BaseAddressAttributes.check_valid addr (astate.post :> base_domain).attrs in
    (* if [address] is in [pre] and it should be valid then that fact goes in the precondition *)
    abduce_attribute addr (MustBeValid access_trace) astate


  let check_initialized access_trace addr astate =
    let attrs = (astate.post :> base_domain).attrs in
    let+ () = BaseAddressAttributes.check_initialized addr attrs in
    let is_written_to =
      Option.exists (BaseAddressAttributes.find_opt addr attrs) ~f:(fun attrs ->
          Attribute.Attributes.get_written_to attrs |> Option.is_some )
    in
    if is_written_to then astate else abduce_attribute addr (MustBeInitialized access_trace) astate


  (** [astate] with [astate.post.attrs = f astate.post.attrs] *)
  let map_post_attrs ~f astate =
    let new_post = PostDomain.update astate.post ~attrs:(f (astate.post :> base_domain).attrs) in
    if phys_equal new_post astate.post then astate else {astate with post= new_post}


  let invalidate address invalidation location astate =
    let astate =
      map_post_attrs ~f:(BaseAddressAttributes.invalidate address invalidation location) astate
    in
    if Config.pulse_isl then
      map_post_attrs ~f:(BaseAddressAttributes.remove_must_be_valid_attr (fst address)) astate
      |> map_post_attrs ~f:(BaseAddressAttributes.remove_isl_abduced_attr (fst address))
      |> map_post_attrs ~f:(BaseAddressAttributes.remove_allocation_attr (fst address))
    else astate


  let allocate procname address location astate =
    map_post_attrs astate ~f:(BaseAddressAttributes.allocate procname address location)


  let add_dynamic_type typ address astate =
    map_post_attrs astate ~f:(BaseAddressAttributes.add_dynamic_type typ address)


  let remove_allocation_attr address astate =
    map_post_attrs astate ~f:(BaseAddressAttributes.remove_allocation_attr address)


  let add_one address attributes astate =
    map_post_attrs astate ~f:(BaseAddressAttributes.add_one address attributes)


  let remove_islabduced_attr address astate =
    map_post_attrs astate ~f:(BaseAddressAttributes.remove_isl_abduced_attr address)


  let remove_must_be_valid_attr address astate =
    map_post_attrs astate ~f:(BaseAddressAttributes.remove_must_be_valid_attr address)


  let get_closure_proc_name addr astate =
    BaseAddressAttributes.get_closure_proc_name addr (astate.post :> base_domain).attrs


  let std_vector_reserve addr astate =
    map_post_attrs astate ~f:(BaseAddressAttributes.std_vector_reserve addr)


  let is_std_vector_reserved addr astate =
    BaseAddressAttributes.is_std_vector_reserved addr (astate.post :> base_domain).attrs


  let mark_as_end_of_collection addr astate =
    map_post_attrs astate ~f:(BaseAddressAttributes.mark_as_end_of_collection addr)


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


  let check_valid_isl access_trace addr ?(null_noop = false) astate =
    L.d_printfln "*****check_valid_isl: addr*** %a@\n" AbstractValue.pp addr ;
    match BaseAddressAttributes.get_invalid addr (astate.post :> BaseDomain.t).attrs with
    | None -> (
      match
        BaseAddressAttributes.get_must_be_valid_or_allocated_isl addr
          (astate.post :> BaseDomain.t).attrs
      with
      | None ->
          let is_eq_null = PathCondition.is_known_zero astate.path_condition addr in
          let null_astates =
            if PathCondition.is_known_not_equal_zero astate.path_condition addr then []
            else
              let null_attr =
                Attribute.Invalid (Invalidation.ConstantDereference IntLit.zero, access_trace)
              in
              let null_astate = add_one addr null_attr astate in
              let null_astate =
                if is_eq_null then null_astate else abduce_attribute addr null_attr null_astate
              in
              if null_noop then [Ok null_astate] else [Error (`ISLError null_astate)]
          in
          if is_eq_null then null_astates
          else
            let valid_astate =
              let abdalloc = Attribute.ISLAbduced access_trace in
              let valid_attr = Attribute.MustBeValid access_trace in
              add_one addr abdalloc astate |> abduce_attribute addr valid_attr
              |> abduce_attribute addr abdalloc
            in
            let invalid_free =
              (*C or Cpp?*)
              let invalid_attr = Attribute.Invalid (CFree, access_trace) in
              abduce_attribute addr invalid_attr astate |> add_one addr invalid_attr
            in
            Ok valid_astate :: Error (`ISLError invalid_free) :: null_astates
      | Some _ ->
          [Ok astate] )
    | Some (invalidation, invalidation_trace) ->
        [Error (`InvalidAccess (invalidation, invalidation_trace, astate))]
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
            BaseMemory.add_edge addr_src access (addr_dst, []) (astate.pre :> base_domain).heap
            |> BaseMemory.register_address addr_dst
          else (astate.pre :> base_domain).heap
        in
        ( { post= PostDomain.update astate.post ~heap:post_heap
          ; pre= PreDomain.update astate.pre ~heap:foot_heap
          ; topl= astate.topl
          ; skipped_calls= astate.skipped_calls
          ; path_condition= astate.path_condition }
        , addr_hist_dst )


  let find_opt address astate = BaseMemory.find_opt address (astate.post :> base_domain).heap
end

let add_edge_on_src src location stack =
  match src with
  | `LocalDecl (pvar, addr_opt) -> (
    match addr_opt with
    | None ->
        let addr = AbstractValue.mk_fresh () in
        let history = [ValueHistory.VariableDeclared (pvar, location)] in
        (BaseStack.add (Var.of_pvar pvar) (addr, history) stack, addr)
    | Some addr ->
        (stack, addr) )
  | `Malloc addr ->
      (stack, addr)


let rec set_uninitialized_post tenv src typ location ?(fields_prefix = RevList.empty)
    (post : PostDomain.t) =
  match typ.Typ.desc with
  | Tint _ | Tfloat _ | Tptr _ ->
      let {stack; attrs} = (post :> base_domain) in
      let stack, addr = add_edge_on_src src location stack in
      let attrs =
        if Config.pulse_isl then
          BaseAddressAttributes.add_one addr (MustBeValid (Immediate {location; history= []})) attrs
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
        let stack, addr = add_edge_on_src src location (post :> base_domain).stack in
        let init = PostDomain.update ~stack post in
        List.fold fields ~init ~f:(fun (acc : PostDomain.t) (field, field_typ, _) ->
            if Fieldname.is_internal field then acc
            else
              let field_addr = AbstractValue.mk_fresh () in
              let fields = RevList.cons field fields_prefix in
              let history = [ValueHistory.StructFieldAddressCreated (fields, location)] in
              let heap =
                BaseMemory.add_edge addr (HilExp.Access.FieldAccess field) (field_addr, history)
                  (acc :> base_domain).heap
              in
              PostDomain.update ~heap acc
              |> set_uninitialized_post tenv (`Malloc field_addr) field_typ location
                   ~fields_prefix:fields ) )
  | Tarray _ | Tvoid | Tfun | TVar _ ->
      (* We ignore tricky types to mark uninitialized addresses. *)
      post


let set_uninitialized tenv src typ location x =
  {x with post= set_uninitialized_post tenv src typ location x.post}


let mk_initial tenv proc_desc =
  (* HACK: save the formals in the stacks of the pre and the post to remember which local variables
     correspond to formals *)
  let proc_name = Procdesc.get_proc_name proc_desc in
  let location = Procdesc.get_loc proc_desc in
  let formals_and_captured =
    let init_var mangled typ =
      let pvar = Pvar.mk mangled proc_name in
      ( Var.of_pvar pvar
      , typ
      , (AbstractValue.mk_fresh (), [ValueHistory.FormalDeclared (pvar, location)]) )
    in
    let formals =
      Procdesc.get_formals proc_desc |> List.map ~f:(fun (mangled, typ) -> init_var mangled typ)
    in
    let captured =
      Procdesc.get_captured proc_desc
      |> List.map ~f:(fun {CapturedVar.name; CapturedVar.typ} -> init_var name typ)
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
          BaseMemory.add_edge addr Dereference (addr_dst, []) heap
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
          BaseAddressAttributes.add_one addr (MustBeValid (Immediate {location; history= []})) attrs )
    else (PreDomain.empty :> base_domain).attrs
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
    List.fold locals ~init:post ~f:(fun (acc : PostDomain.t) {ProcAttributes.name; typ} ->
        set_uninitialized_post tenv (`LocalDecl (Pvar.mk name proc_name, None)) typ location acc )
  in
  { pre
  ; post
  ; topl= PulseTopl.start ()
  ; skipped_calls= SkippedCalls.empty
  ; path_condition= PathCondition.true_ }


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


let discard_unreachable ({pre; post} as astate) =
  let pre_addresses = BaseDomain.reachable_addresses (pre :> BaseDomain.t) in
  let pre_new =
    PreDomain.filter_addr ~f:(fun address -> AbstractValue.Set.mem address pre_addresses) pre
  in
  let post_addresses = BaseDomain.reachable_addresses (post :> BaseDomain.t) in
  let live_addresses = AbstractValue.Set.union pre_addresses post_addresses in
  let post_new, discard_addresses =
    PostDomain.filter_addr_with_discarded_addrs
      ~f:(fun address -> AbstractValue.Set.mem address live_addresses)
      post
  in
  (* note: we don't call {!PulsePathCondition.simplify} *)
  let astate =
    if phys_equal pre_new pre && phys_equal post_new post then astate
    else {astate with pre= pre_new; post= post_new}
  in
  (astate, live_addresses, discard_addresses)


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


let filter_stack_for_summary astate =
  let post_stack =
    BaseStack.filter
      (fun var _ -> Var.appears_in_source_code var && not (is_local var astate))
      (astate.post :> BaseDomain.t).stack
  in
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
                 |> List.find_map ~f:(fun ProcAttributes.{name; typ} ->
                        if Mangled.equal name (Pvar.get_name pvar) then Some typ else None )
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


type summary = t [@@deriving compare, equal, yojson_of]

let is_heap_allocated {post; pre} v =
  BaseMemory.is_allocated (post :> BaseDomain.t).heap v
  || BaseMemory.is_allocated (pre :> BaseDomain.t).heap v


let is_stack_allocated stack_allocations v = AbstractValue.Set.mem v (Lazy.force stack_allocations)

let subst_var_in_post subst astate =
  let open SatUnsat.Import in
  let+ post = PostDomain.subst_var subst astate.post in
  if phys_equal astate.post post then astate else {astate with post}


let get_stack_allocated {post} =
  (* OPTIM: the post stack contains at least the pre stack so no need to check both *)
  BaseStack.fold
    (fun var (address, _) allocated ->
      if Var.is_pvar var then AbstractValue.Set.add address allocated else allocated )
    (post :> BaseDomain.t).stack AbstractValue.Set.empty


let incorporate_new_eqs astate new_eqs =
  let stack_allocations = lazy (get_stack_allocated astate) in
  List.fold_until new_eqs ~init:(astate, None)
    ~finish:(fun astate_error -> Sat astate_error)
    ~f:(fun (astate, error) (new_eq : PulseFormula.new_eq) ->
      match new_eq with
      | Equal (v1, v2) when AbstractValue.equal v1 v2 ->
          Continue (astate, error)
      | Equal (v1, v2) -> (
        match subst_var_in_post (v1, v2) astate with
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
          L.d_printfln "Potential ERROR: %a = 0 but is allocated" AbstractValue.pp v ;
          match BaseAddressAttributes.get_must_be_valid v (astate.pre :> base_domain).attrs with
          | None ->
              (* we don't know why [v|->-] is in the state, weird and probably cannot happen; drop
                 the path because we won't be able to give a sensible error *)
              L.d_printfln "Not clear why %a should be allocated in the first place, giving up"
                AbstractValue.pp v ;
              Stop Unsat
          | Some must_be_valid ->
              Stop (Sat (astate, Some (v, must_be_valid))) )
      | EqZero _ (* [v] not allocated *) ->
          Continue (astate, error) )


(** it's a good idea to normalize the path condition before calling this function *)
let canonicalize astate =
  let open SatUnsat.Import in
  let get_var_repr v = PathCondition.get_var_repr astate.path_condition v in
  let canonicalize_pre (pre : PreDomain.t) =
    (* (ab)use canonicalization to filter out empty edges in the heap and detect aliasing
       contradictions *)
    let* stack' = BaseStack.canonicalize ~get_var_repr:Fn.id (pre :> BaseDomain.t).stack in
    let+ heap' = BaseMemory.canonicalize ~get_var_repr:Fn.id (pre :> BaseDomain.t).heap in
    PreDomain.update ~stack:stack' ~heap:heap' pre
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


let filter_for_summary tenv astate0 =
  let open SatUnsat.Import in
  L.d_printfln "Canonicalizing...@\n" ;
  let* astate_before_filter = canonicalize astate0 in
  L.d_printfln "Canonicalized state: %a@\n" pp astate_before_filter ;
  let astate = filter_stack_for_summary astate_before_filter in
  let astate = {astate with topl= PulseTopl.filter_for_summary astate.path_condition astate.topl} in
  let astate, live_addresses, _ = discard_unreachable astate in
  let+ path_condition, new_eqs =
    PathCondition.simplify tenv
      ~get_dynamic_type:
        (BaseAddressAttributes.get_dynamic_type (astate_before_filter.post :> BaseDomain.t).attrs)
      ~keep:live_addresses astate.path_condition
  in
  ({astate with path_condition; topl= PulseTopl.simplify ~keep:live_addresses astate.topl}, new_eqs)


let summary_of_post tenv pdesc astate =
  let open SatUnsat.Import in
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
  let* astate, error = incorporate_new_eqs astate new_eqs in
  let* astate, new_eqs = filter_for_summary tenv astate in
  let+ astate, error =
    match error with None -> incorporate_new_eqs astate new_eqs | Some _ -> Sat (astate, error)
  in
  match error with
  | None ->
      Ok (invalidate_locals pdesc astate)
  | Some (address, must_be_valid) ->
      Error (`PotentialInvalidAccessSummary (astate, address, must_be_valid))


let get_pre {pre} = (pre :> BaseDomain.t)

let get_post {post} = (post :> BaseDomain.t)

(* re-exported for mli *)
let incorporate_new_eqs new_eqs astate =
  if PathCondition.is_unsat_cheap astate.path_condition then Ok astate
  else
    match incorporate_new_eqs astate new_eqs with
    | Unsat ->
        Ok {astate with path_condition= PathCondition.false_}
    | Sat (astate, None) ->
        Ok astate
    | Sat (astate, Some (address, must_be_valid)) ->
        Error (`PotentialInvalidAccess (astate, address, must_be_valid))


module Topl = struct
  let small_step loc event astate =
    {astate with topl= PulseTopl.small_step loc astate.path_condition event astate.topl}


  let large_step ~call_location ~callee_proc_name ~substitution ?(condition = PathCondition.true_)
      ~callee_prepost astate =
    { astate with
      topl=
        PulseTopl.large_step ~call_location ~callee_proc_name ~substitution ~condition
          ~callee_prepost astate.topl }


  let get {topl} = topl
end
