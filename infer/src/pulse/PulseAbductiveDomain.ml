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
module VarSet = Var.Set

(** signature common to the "normal" [Domain], representing the post at the current program point,
    and the inverted [PreDomain], representing the inferred pre-condition*)
module type BaseDomainSig = sig
  (* private because the lattice is not the same for preconditions and postconditions so we don't
     want to confuse them *)
  type t = private BaseDomain.t

  val empty : t

  val update : ?stack:BaseStack.t -> ?heap:BaseMemory.t -> ?attrs:BaseAddressAttributes.t -> t -> t

  val filter_addr : f:(AbstractValue.t -> bool) -> t -> t
  (** filter both heap and attrs *)

  val filter_addr_with_discarded_addrs :
    f:(AbstractValue.t -> bool) -> t -> t * AbstractValue.t list
  (** compute new state containing only reachable addresses in its heap and attributes, as well as
      the list of discarded unreachable addresses *)

  val pp : F.formatter -> t -> unit
end

(* just to expose record field names without having to type
   [BaseDomain.heap] *)
type base_domain = BaseDomain.t =
  {heap: BaseMemory.t; stack: BaseStack.t; attrs: BaseAddressAttributes.t}

(** operations common to [Domain] and [PreDomain], see also the [BaseDomain] signature *)
module BaseDomainCommon = struct
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
end

(** represents the post abstract state at each program point *)
module PostDomain : BaseDomainSig = struct
  include BaseDomainCommon
  include BaseDomain
end

(* NOTE: [PreDomain] and [Domain] theoretically differ in that [PreDomain] should be the inverted lattice of [Domain], but since we never actually join states or check implication the two collapse into one. *)

(** represents the inferred pre-condition at each program point, biabduction style *)
module PreDomain : BaseDomainSig = PostDomain

module PostStatus = struct
type t = Ok | Er of Diagnostic.t option

let pp f s=
  match s with
  | Ok ->
     F.pp_print_string f "ok:"
  | Er may_er ->
     F.pp_print_string f ("er:" ^
     (match may_er with | None -> "" | Some err -> " " ^ (Diagnostic.get_message err)))
                                                                                          
let eq s1 s2=
  match s1, s2 with
    | Ok, Ok
      | Er _, Er _ -> true
    | _ -> false
end

(** biabduction-style pre/post state + skipped calls *)
type t =
  { post: PostDomain.t  (** state at the current program point*)
  ; pre: PreDomain.t  (** inferred pre at the current program point *)
  ; skipped_calls: SkippedCalls.t  (** set of skipped calls *)
  ; path_condition: PathCondition.t
  ; status: PostStatus.t
  ; local_ptrvars : VarSet.t
  ; nonref_formals : VarSet.t
  ; imm_params : VarSet.t
  ; mod_addrs : AbstractValue.Set.t }

let pp f {post; pre; path_condition; skipped_calls; local_ptrvars; nonref_formals; imm_params;mod_addrs; status} =
  F.fprintf f "@[<v>%a\n%a@;%a@;PRE=[%a]@;skipped_calls=%a@;local_ptrvars=%a@; nonref_formals=%a@;imm_params=%a@;mod_addrs=%a@]" PathCondition.pp path_condition
      PostStatus.pp status PostDomain.pp post PreDomain.pp pre SkippedCalls.pp skipped_calls
      VarSet.pp local_ptrvars VarSet.pp nonref_formals
      VarSet.pp imm_params AbstractValue.Set.pp mod_addrs

let set_status status astate = {astate with status}

let set_er_status astate = {astate with status = PostStatus.Er None}

let remove_imm_param var astate =
  {astate with imm_params = VarSet.remove var astate.imm_params}

let add_mod_addr addr astate =
  {astate with mod_addrs = AbstractValue.Set.add addr astate.mod_addrs}

let remove_local_var var astate =
  {astate with local_ptrvars = VarSet.remove var astate.local_ptrvars}


let set_path_condition path_condition astate = {astate with path_condition}

let leq ~lhs ~rhs =
  SkippedCalls.leq ~lhs:lhs.skipped_calls ~rhs:rhs.skipped_calls
  &&
      PostStatus.eq lhs.status rhs.status
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
        let post_heap = if not Config.pulse_isl then (astate.post :> base_domain).heap else BaseMemory.register_address addr (astate.post :> base_domain).heap in
        let post_attrs = if not Config.pulse_isl then (astate.post :> base_domain).attrs else let access_trace = Trace.Immediate {location; history=[]} in
              BaseAddressAttributes.add_one addr (MustBeValid access_trace)  (astate.post :> base_domain).attrs in
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
          ; skipped_calls= astate.skipped_calls
          ; path_condition= astate.path_condition
          ; status = astate.status
          ; local_ptrvars = astate.local_ptrvars
          ; imm_params = astate.imm_params
          ; mod_addrs = astate.mod_addrs
          ; nonref_formals = astate.nonref_formals }
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

  (** [astate] with [astate.post.attrs = f astate.post.attrs] *)
  let map_post_attrs ~f astate =
    let new_post = PostDomain.update astate.post ~attrs:(f (astate.post :> base_domain).attrs) in
    if phys_equal new_post astate.post then astate else {astate with post= new_post}

  let map_pre_attrs ~f astate =
    let new_pre = PreDomain.update astate.pre ~attrs:(f (astate.pre :> base_domain).attrs) in
    if phys_equal new_pre astate.pre then astate else {astate with pre= new_pre}
    
  let add_one address attribute astate =
    map_post_attrs astate ~f:(BaseAddressAttributes.add_one address attribute)

  (** if [address] is in [pre] then add the attribute [attr] *)
  let abduce_attribute address attribute astate =
    L.d_printfln "Abducing %a:%a" AbstractValue.pp address Attribute.pp attribute ;
    let new_pre =
      if BaseMemory.mem address (astate.pre :> base_domain).heap then
        PreDomain.update astate.pre
          ~attrs:(BaseAddressAttributes.add_one address attribute (astate.pre :> base_domain).attrs)
      else astate.pre
    in
    if phys_equal new_pre astate.pre then astate else {astate with pre= new_pre}


  let check_valid access_trace addr astate =
    let+ () = BaseAddressAttributes.check_valid addr (astate.post :> base_domain).attrs in
    (* if [address] is in [pre] and it should be valid then that fact goes in the precondition *)
    if not Config.pulse_isl then
        abduce_attribute addr (MustBeValid access_trace) astate
    else
        abduce_attribute addr (MustBeValid access_trace) astate
        |> add_one addr (MustBeValid access_trace)



  let invalidate address invalidation location astate =
    if not Config.pulse_isl then
        map_post_attrs astate ~f:(BaseAddressAttributes.invalidate address invalidation location)
    else
        map_post_attrs ~f:(BaseAddressAttributes.invalidate address invalidation location) astate
        |> map_post_attrs ~f:(BaseAddressAttributes.remove_must_be_valid_attr (fst address))
        |> map_post_attrs ~f:(BaseAddressAttributes.remove_abdallocation_attr (fst address))
        |> map_post_attrs ~f:(BaseAddressAttributes.remove_allocation_attr (fst address))



  let allocate procname address location astate =
    map_post_attrs astate ~f:(BaseAddressAttributes.allocate procname address location)


  let add_dynamic_type typ address astate =
    map_post_attrs astate ~f:(BaseAddressAttributes.add_dynamic_type typ address)


  let remove_allocation_attr address astate =
    map_post_attrs astate ~f:(BaseAddressAttributes.remove_allocation_attr address)

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


  let is_pre_invalid_const addr astate =
    BaseAddressAttributes.is_invalid_const addr (astate.pre :> base_domain).attrs

  let abduce_and_add value attrs astate =
    Attributes.fold attrs ~init:astate ~f:(fun astate attr ->
        let astate =
          if Attribute.is_suitable_for_pre attr then abduce_attribute value attr astate else astate
        in
        add_one value attr astate )


   let abduce_attrs value attrs astate =
    Attributes.fold attrs ~init:astate ~f:(fun astate attr ->
        let astate =
          if Attribute.is_suitable_for_pre attr then abduce_attribute value attr astate else astate
        in
        astate )

   let add_attrs value attrs astate =
     let astate = match Attributes.get_invalid attrs with | Some _ -> remove_must_be_valid_attr value astate |> remove_allocation_attr value | _ -> astate in
     Attributes.fold attrs ~init:astate ~f:(fun astate attr ->
             add_one value attr
                 astate)


  let find_opt address astate =
    BaseAddressAttributes.find_opt address (astate.post :> base_domain).attrs

  (** case split on [address]:
 - dont need abduce: MustBeValid/Allocation/AbdAllocation. do add_edge
 - abduce failed: lhs_addr = null or Invalid or Uninit
 - abduce succ:
   + MustBeValid: new heap or alias
   + Invalid Free
   + Invalid null
   + Invalid Uninit
*)
  let check_valid_and_abduce procname access_trace addr ?null_noop:(null_noop=false) is_neq_null is_pc_eq_null astate =
    L.d_printfln "*****check_valid_and_abduce: addr*** %a@\n" AbstractValue.pp addr;
    match BaseAddressAttributes.get_invalid addr
                  (astate.post :> BaseDomain.t).attrs with
      | None -> begin
          match BaseAddressAttributes.get_must_be_valid_or_allocated addr
                 (astate.post :> BaseDomain.t).attrs with
          | None -> (
              (* do not do abduction for local variables *)
              (* if (is_local_ptr addr astate) then
               *     Error (Invalidation.UninitializedDereference, access_trace, astate)
               * else *)
              (* abduce *)
              (* invalid null *)
              let null_astates =
                if not is_neq_null then
                    let null_attr = Attribute.Invalid ((Invalidation.ConstantDereference IntLit.zero), access_trace) in
                    let null_astate = { astate with status = if null_noop then astate.status else PostStatus.Er None;}  (* path_condition = PathCondition.and_eq_int addr IntLit.zero astate.path_condition*)
                                    |> add_one addr null_attr
                    in
                    let null_astate =
                      if is_pc_eq_null then null_astate
                      else (* abduce null into both pre *)
                          abduce_attribute addr null_attr null_astate
                    in
                    [null_astate]
                else []
              in
              if is_pc_eq_null then Ok null_astates else
                  let valid_astate =
                    let abdalloc = Attribute.AbdAllocated (procname, access_trace) in
                    let valid_attr = (Attribute.MustBeValid access_trace) in
                    add_one addr abdalloc astate (* {astate with path_condition = PathCondition.and_positive addr astate.path_condition} *)
                    (* |> add_one addr valid_attr *) (* must be valid attribute just for pre *)
                    |> abduce_attribute addr valid_attr
                    |> abduce_attribute addr abdalloc in
              (* invalid free *)
              let invalid_free =
                (*C or Cpp?*)
                let invalid_attr = Attribute.Invalid (CFree, access_trace) in
                { astate with status = PostStatus.Er None;(* path_condition = PathCondition.and_positive addr astate.path_condition *)}
                (* abduce invalid heap into both pre and post *)
                |> abduce_attribute addr invalid_attr
                |> add_one addr invalid_attr
              in
              (* invalid uninit *)
              let invalid_uninit =
                let invalid_attr = Attribute.Invalid (UninitializedDereference, access_trace) in
                { astate with status = PostStatus.Er None;}
                (* abduce invalid heap into both pre and post *)
                |> abduce_attribute addr invalid_attr
                |> add_one addr invalid_attr
              in
              Ok ([valid_astate;invalid_free]@null_astates@[invalid_uninit]))
          | Some _ -> (
              (* presume MustBeValid and Invalid were not added together *)
              (* dont need abduce *)
              Ok [astate])
          end
      | Some (invalidation, invalidation_trace) -> (
          (* abduce failed *)
          Error (invalidation, invalidation_trace, {astate with status = PostStatus.Er (Some (Diagnostic.AccessToInvalidAddress {invalidation; invalidation_trace; access_trace}))})
      )
   
    
end

module Memory = struct
  module Access = BaseMemory.Access
  module Edges = BaseMemory.Edges

  (** [astate] with [astate.post.heap = f astate.post.heap] *)
  let map_post_heap ~f astate =
    let new_post = PostDomain.update astate.post ~heap:(f (astate.post :> base_domain).heap) in
    if phys_equal new_post astate.post then astate else {astate with post= new_post}

   let map_pre_heap ~f astate =
    let new_pre = PreDomain.update astate.pre ~heap:(f (astate.pre :> base_domain).heap) in
    if phys_equal new_pre astate.pre then astate else {astate with pre= new_pre}


  let add_edge (addr, history) access new_addr_hist location astate =
    map_post_heap astate ~f:(BaseMemory.add_edge addr access new_addr_hist)
    |> AddressAttributes.map_post_attrs
         ~f:(BaseAddressAttributes.add_one addr (WrittenTo (Trace.Immediate {location; history})))

  let register_address addr astate=
    map_post_heap astate ~f:(BaseMemory.register_address addr)

  let remove_address addr astate=
    map_post_heap astate ~f:(BaseMemory.remove_address addr)
    
  let find_edge_opt address access astate =
    BaseMemory.find_edge_opt address access (astate.post :> base_domain).heap

  let find_edges_opt address astate =
    BaseMemory.find_edges_opt address (astate.post :> base_domain).heap

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
          ; skipped_calls= astate.skipped_calls
          ; path_condition= astate.path_condition
          ; status=astate.status
          ; local_ptrvars = astate.local_ptrvars
          ; imm_params = astate.imm_params
          ; mod_addrs = astate.mod_addrs
          ; nonref_formals = astate.nonref_formals }
        , addr_hist_dst )


  let find_opt address astate = BaseMemory.find_opt address (astate.post :> base_domain).heap
end

let mk_initial proc_desc =
  (* HACK: save the formals in the stacks of the pre and the post to remember which local variables
     correspond to formals *)
  let location = Procdesc.get_loc proc_desc in
  let proc_name = Procdesc.get_proc_name proc_desc in
  let formals=
    ((Procdesc.get_formals proc_desc)@(List.map  (Procdesc.get_captured proc_desc) ~f:(fun (a,b,_) -> (a, b))))
    |> List.map ~f:(fun (mangled, typ) ->
               let pvar = Pvar.mk mangled proc_name in
               ( Var.of_pvar pvar, typ
                 ,  (AbstractValue.mk_fresh (), [ValueHistory.FormalDeclared (pvar, typ, location)]) ) )
  in
  let formal_vars, initial_stack =
    List.fold formals ~init:([], (PreDomain.empty :> BaseDomain.t).stack)
      ~f:(fun (vars, stack) (formal, typ, addr_loc) -> vars@[(formal,typ)],BaseStack.add formal addr_loc stack)
  in
  let access_trace = Trace.Immediate {location; history=[]} in
  let initial_heap =
    List.fold formals ~init:((PreDomain.empty :> base_domain).heap) ~f:(fun heap (_, typ, (addr, _)) ->
            let heap = BaseMemory.register_address addr heap in
            if not Config.pulse_isl then
              heap
            else
            match typ.Typ.desc with
            | Typ.Tptr _ ->
               (let addr_dst = AbstractValue.mk_fresh () in
                BaseMemory.add_edge addr Dereference (addr_dst, []) heap
                |> BaseMemory.register_address addr_dst)
            | _ -> heap)
  in
  let initial_attrs =
    if not Config.pulse_isl then
      (PreDomain.empty :> base_domain).attrs
    else
    List.fold formals ~init:(PreDomain.empty :> base_domain).attrs ~f:(fun attrs (_, _, (addr, _)) ->
            BaseAddressAttributes.add_one addr (MustBeValid access_trace) attrs)
  in
  let pre =
    PreDomain.update ~stack:initial_stack ~heap:initial_heap ~attrs:initial_attrs PreDomain.empty
  in
  let local_ptrvars,initial_stack, initial_heap, initial_attrs =
    if not Config.pulse_isl then
       (Var.Set.empty, initial_stack, (PreDomain.empty :> base_domain).heap, (PreDomain.empty :> base_domain).attrs)
    else
    (Procdesc.get_locals proc_desc)
      |> List.fold ~init:(Var.Set.empty, initial_stack, initial_heap, initial_attrs) ~f:(fun (vars, stack, heap, attrs) {ProcAttributes.name;ProcAttributes.typ} ->
                 let pvar = Pvar.mk name proc_name in
                 match typ.Typ.desc with
                 | Typ.Tptr _ when not (Pvar.is_frontend_tmp pvar) ->
                    let var = Var.of_pvar pvar in
                    let n_addr = AbstractValue.mk_fresh () in
                    let stack = BaseStack.add var (n_addr, [ValueHistory.VariableDeclared (pvar, location)]) stack in
                    let addr_dst = AbstractValue.mk_fresh () in
                    let heap =
                      BaseMemory.register_address n_addr heap
                      |> BaseMemory.add_edge n_addr Dereference (addr_dst, [])
                    in
                    let attrs =
                      BaseAddressAttributes.add_one n_addr (MustBeValid access_trace) attrs
                      |> BaseAddressAttributes.add_one addr_dst (Invalid (UninitializedDereference, access_trace))
                    in
                    (Var.Set.add var vars, stack, heap, attrs)
                 | _ ->
                    vars, stack, heap, attrs)
  in
  let post =  PostDomain.update ~stack:initial_stack ~heap:initial_heap ~attrs:initial_attrs PostDomain.empty in
  let nonref_formals, imm_params =
    List.fold formal_vars ~init:(VarSet.empty, VarSet.empty) ~f:(fun (acc_nonref, acc_imm) (v,t) ->
            if (Var.is_this v) then
                (acc_nonref, acc_imm)
            else
                (match t.Typ.desc with
                 | Typ.Tptr (_, ptr_kind) ->
                     (match ptr_kind with
                      | Pk_reference -> acc_nonref
                      | _ -> Var.Set.add v acc_nonref
                     )
                 | _ -> Var.Set.add v acc_nonref), Var.Set.add v acc_imm)
  in
  {pre; post; skipped_calls= SkippedCalls.empty; path_condition= PathCondition.true_;
     status = Ok
   ; local_ptrvars = local_ptrvars
   ; nonref_formals = nonref_formals
   ; imm_params = imm_params
   ; mod_addrs = AbstractValue.Set.empty}


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

let set_post_edges addr edges astate = if BaseMemory.Edges.is_empty edges then astate else Memory.map_post_heap astate ~f:(BaseMemory.add addr edges)

(* {3 Helper functions to traverse the two maps at once } *)

let find_post_cell_opt addr {post} = BaseDomain.find_cell_opt addr (post :> BaseDomain.t)

let set_post_cell (addr, history) (edges, attr_set) location astate =
  set_post_edges addr edges astate
  |> AddressAttributes.map_post_attrs ~f:(fun attrs ->
         BaseAddressAttributes.add_one addr (WrittenTo (Trace.Immediate {location; history})) attrs
         |> BaseAddressAttributes.add addr attr_set )


let filter_for_summary astate =
  let post_stack =
    BaseStack.filter
      (fun var _ -> Var.appears_in_source_code var && not (is_local var astate))
      (astate.post :> BaseDomain.t).stack
  in
  (* deregister empty edges *)
  let deregister_empty heap =
    BaseMemory.filter (fun _addr edges -> not (BaseMemory.Edges.is_empty edges)) heap
  in
  let pre_heap = deregister_empty (astate.pre :> base_domain).heap in
  let post_heap = deregister_empty (astate.post :> base_domain).heap in
  { astate with
    pre= PreDomain.update astate.pre ~heap:pre_heap
  ; post= PostDomain.update ~stack:post_stack ~heap:post_heap astate.post }


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


let of_post pdesc astate =
  let astate = filter_for_summary astate in
  let astate, live_addresses, _ = discard_unreachable astate in
  let astate =
    {astate with path_condition= PathCondition.simplify ~keep:live_addresses astate.path_condition}
  in
  invalidate_locals pdesc astate


let get_pre {pre} = (pre :> BaseDomain.t)

let get_post {post} = (post :> BaseDomain.t)
