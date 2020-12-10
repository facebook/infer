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

(** signature common to the "normal" [Domain], representing the post at the current program point,
    and the inverted [PreDomain], representing the inferred pre-condition*)
module type BaseDomainSig = sig
  (* private because the lattice is not the same for preconditions and postconditions so we don't
     want to confuse them *)
  type t = private BaseDomain.t [@@deriving yojson_of]

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

(** biabduction-style pre/post state + skipped calls *)
type t =
  { post: PostDomain.t  (** state at the current program point*)
  ; pre: PreDomain.t  (** inferred pre at the current program point *)
  ; topl: (PulseTopl.state[@yojson.opaque])
  ; skipped_calls: SkippedCalls.t  (** set of skipped calls *)
  ; path_condition: PathCondition.t }
[@@deriving yojson_of]

let pp f {post; pre; topl; path_condition; skipped_calls} =
  F.fprintf f "@[<v>%a@;%a@;PRE=[%a]@;skipped_calls=%a@;TOPL=%a@]" PathCondition.pp path_condition
    PostDomain.pp post PreDomain.pp pre SkippedCalls.pp skipped_calls PulseTopl.pp_state topl


let set_path_condition path_condition astate = {astate with path_condition}

let leq ~lhs ~rhs =
  SkippedCalls.leq ~lhs:lhs.skipped_calls ~rhs:rhs.skipped_calls
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

module Stack = struct
  let is_abducible astate var =
    (* HACK: formals are pre-registered in the initial state *)
    BaseStack.mem var (astate.pre :> base_domain).stack || Var.is_global var


  (** [astate] with [astate.post.stack = f astate.post.stack] *)
  let map_post_stack ~f astate =
    let new_post = PostDomain.update astate.post ~stack:(f (astate.post :> base_domain).stack) in
    if phys_equal new_post astate.post then astate else {astate with post= new_post}


  let eval origin var astate =
    match BaseStack.find_opt var (astate.post :> base_domain).stack with
    | Some addr_hist ->
        (astate, addr_hist)
    | None ->
        let addr = AbstractValue.mk_fresh () in
        let addr_hist = (addr, origin) in
        let post_stack = BaseStack.add var addr_hist (astate.post :> base_domain).stack in
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
        ( { post= PostDomain.update astate.post ~stack:post_stack
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
    map_post_attrs astate ~f:(BaseAddressAttributes.invalidate address invalidation location)


  let allocate procname address location astate =
    map_post_attrs astate ~f:(BaseAddressAttributes.allocate procname address location)


  let add_dynamic_type typ address astate =
    map_post_attrs astate ~f:(BaseAddressAttributes.add_dynamic_type typ address)


  let remove_allocation_attr address astate =
    map_post_attrs astate ~f:(BaseAddressAttributes.remove_allocation_attr address)


  let add_one address attributes astate =
    map_post_attrs astate ~f:(BaseAddressAttributes.add_one address attributes)


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
        let astate = add_one value attr astate in
        match attr with Attribute.WrittenTo _ -> initialize value astate | _ -> astate )


  let find_opt address astate =
    BaseAddressAttributes.find_opt address (astate.post :> base_domain).attrs
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

let set_uninitialized_post src typ location (post : PostDomain.t) =
  match typ.Typ.desc with
  | Tint _ | Tfloat _ | Tptr _ ->
      let {stack; attrs} = (post :> base_domain) in
      let stack, addr, history =
        match src with
        | `LocalDecl (pvar, addr_opt) ->
            let history = [ValueHistory.VariableDeclared (pvar, location)] in
            let stack, addr =
              match addr_opt with
              | None ->
                  let addr = AbstractValue.mk_fresh () in
                  (BaseStack.add (Var.of_pvar pvar) (addr, history) stack, addr)
              | Some addr ->
                  (stack, addr)
            in
            (stack, addr, history)
        | `Malloc (addr, history) ->
            (stack, addr, history)
      in
      let attrs =
        BaseAddressAttributes.add_one addr (Uninitialized (Immediate {location; history})) attrs
      in
      PostDomain.update ~stack ~attrs post
  | Tstruct _ ->
      (* TODO: set uninitialized attributes for fields *)
      post
  | Tarray _ ->
      (* TODO: set uninitialized attributes for elements *)
      post
  | Tvoid | Tfun | TVar _ ->
      (* We ignore tricky types to mark uninitialized addresses. *)
      post


let set_uninitialized src typ location x =
  {x with post= set_uninitialized_post src typ location x.post}


let mk_initial proc_desc =
  (* HACK: save the formals in the stacks of the pre and the post to remember which local variables
     correspond to formals *)
  let proc_name = Procdesc.get_proc_name proc_desc in
  let location = Procdesc.get_loc proc_desc in
  let formals_and_captured =
    let init_var mangled =
      let pvar = Pvar.mk mangled proc_name in
      (Var.of_pvar pvar, (AbstractValue.mk_fresh (), [ValueHistory.FormalDeclared (pvar, location)]))
    in
    let formals =
      Procdesc.get_formals proc_desc |> List.map ~f:(fun (mangled, _) -> init_var mangled)
    in
    let captured =
      Procdesc.get_captured proc_desc |> List.map ~f:(fun {CapturedVar.name} -> init_var name)
    in
    captured @ formals
  in
  let initial_stack =
    List.fold formals_and_captured ~init:(PreDomain.empty :> BaseDomain.t).stack
      ~f:(fun stack (formal, addr_loc) -> BaseStack.add formal addr_loc stack)
  in
  let pre =
    let initial_heap =
      List.fold formals_and_captured ~init:(PreDomain.empty :> base_domain).heap
        ~f:(fun heap (_, (addr, _)) -> BaseMemory.register_address addr heap)
    in
    PreDomain.update ~stack:initial_stack ~heap:initial_heap PreDomain.empty
  in
  let locals = Procdesc.get_locals proc_desc in
  let post = PostDomain.update ~stack:initial_stack PostDomain.empty in
  let post =
    List.fold locals ~init:post ~f:(fun (acc : PostDomain.t) {ProcAttributes.name; typ} ->
        set_uninitialized_post (`LocalDecl (Pvar.mk name proc_name, None)) typ location acc )
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

let set_post_edges addr edges astate = Memory.map_post_heap astate ~f:(BaseMemory.add addr edges)

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


type summary = t [@@deriving yojson_of]

let is_allocated {post; pre} v =
  let is_pvar = function Var.ProgramVar _ -> true | Var.LogicalVar _ -> false in
  let is_stack_allocated base_mem v =
    BaseStack.exists
      (fun var (address, _) -> is_pvar var && AbstractValue.equal address v)
      base_mem.stack
  in
  (* OPTIM: the post stack contains at least the pre stack so no need to check both *)
  BaseMemory.is_allocated (post :> BaseDomain.t).heap v
  || BaseMemory.is_allocated (pre :> BaseDomain.t).heap v
  || is_stack_allocated (post :> BaseDomain.t) v


let incorporate_new_eqs astate new_eqs =
  List.fold_until new_eqs ~init:()
    ~finish:(fun () -> Sat ())
    ~f:(fun () (new_eq : PulseFormula.new_eq) ->
      match new_eq with
      | EqZero v when is_allocated astate v ->
          L.d_printfln "CONTRADICTION: %a = 0 but is allocated" AbstractValue.pp v ;
          Stop Unsat
      | Equal (v1, v2)
        when (not (AbstractValue.equal v1 v2)) && is_allocated astate v1 && is_allocated astate v2
        ->
          L.d_printfln "CONTRADICTION: %a = %a but both are separately allocated" AbstractValue.pp
            v1 AbstractValue.pp v2 ;
          Stop Unsat
      | _ ->
          Continue () )


(** it's a good idea to normalize the path condition before calling this function *)
let canonicalize astate =
  let open SatUnsat.Import in
  let get_var_repr v = PathCondition.get_var_repr astate.path_condition v in
  let canonicalize_base stack heap attrs =
    let stack' = BaseStack.canonicalize ~get_var_repr stack in
    (* note: this step also de-registers addresses pointing to empty edges *)
    let+ heap' = BaseMemory.canonicalize ~get_var_repr heap in
    let attrs' = BaseAddressAttributes.canonicalize ~get_var_repr attrs in
    (stack', heap', attrs')
  in
  (* need different functions for pre and post to appease the type system *)
  let canonicalize_pre (pre : PreDomain.t) =
    let+ stack', heap', attrs' =
      canonicalize_base (pre :> BaseDomain.t).stack (pre :> BaseDomain.t).heap
        (pre :> BaseDomain.t).attrs
    in
    PreDomain.update ~stack:stack' ~heap:heap' ~attrs:attrs' pre
  in
  let canonicalize_post (post : PostDomain.t) =
    let+ stack', heap', attrs' =
      canonicalize_base (post :> BaseDomain.t).stack (post :> BaseDomain.t).heap
        (post :> BaseDomain.t).attrs
    in
    PostDomain.update ~stack:stack' ~heap:heap' ~attrs:attrs' post
  in
  let* pre = canonicalize_pre astate.pre in
  let+ post = canonicalize_post astate.post in
  {astate with pre; post}


let summary_of_post pdesc astate =
  let open SatUnsat.Import in
  (* NOTE: we normalize (to strengthen the equality relation used by canonicalization) then
     canonicalize *before* garbage collecting unused addresses in case we detect any last-minute
     contradictions about addresses we are about to garbage collect *)
  let path_condition, is_unsat, new_eqs = PathCondition.is_unsat_expensive astate.path_condition in
  if is_unsat then Unsat
  else
    let astate = {astate with path_condition} in
    let* () = incorporate_new_eqs astate new_eqs in
    L.d_printfln "Canonicalizing...@\n" ;
    let* astate = canonicalize astate in
    L.d_printfln "Canonicalized state: %a@\n" pp astate ;
    let astate = filter_stack_for_summary astate in
    let astate =
      {astate with topl= PulseTopl.filter_for_summary astate.path_condition astate.topl}
    in
    let astate, live_addresses, _ = discard_unreachable astate in
    let* path_condition, new_eqs =
      PathCondition.simplify ~keep:live_addresses astate.path_condition
    in
    let+ () = incorporate_new_eqs astate new_eqs in
    let astate =
      {astate with path_condition; topl= PulseTopl.simplify ~keep:live_addresses astate.topl}
    in
    invalidate_locals pdesc astate


let get_pre {pre} = (pre :> BaseDomain.t)

let get_post {post} = (post :> BaseDomain.t)

(* re-exported for mli *)
let incorporate_new_eqs astate (phi, new_eqs) =
  if PathCondition.is_unsat_cheap phi then phi
  else match incorporate_new_eqs astate new_eqs with Unsat -> PathCondition.false_ | Sat () -> phi


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
