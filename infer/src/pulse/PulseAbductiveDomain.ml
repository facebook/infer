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
  type t = private BaseDomain.t

  val empty : t

  val update : ?stack:BaseStack.t -> ?heap:BaseMemory.t -> ?attrs:BaseAddressAttributes.t -> t -> t

  val filter_addr : f:(AbstractValue.t -> bool) -> t -> t
  (**filter both heap and attrs *)

  val partition_addr :
       f:(AbstractValue.t -> bool)
    -> t
    -> (BaseMemory.t * BaseAddressAttributes.t) * (BaseMemory.t * BaseAddressAttributes.t)
  (**partition both heap and attrs *)

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


  let partition_addr ~f foot =
    let heap_yes, heap_no = BaseMemory.partition (fun address _ -> f address) foot.heap in
    let attrs_yes, attrs_no =
      BaseAddressAttributes.partition (fun address _ -> f address) foot.attrs
    in
    ((heap_yes, attrs_yes), (heap_no, attrs_no))
end

(** represents the post abstract state at each program point *)
module Domain : BaseDomainSig = struct
  include BaseDomainCommon
  include BaseDomain
end

(* NOTE: [PreDomain] and [Domain] theoretically differ in that [PreDomain] should be the inverted lattice of [Domain], but since we never actually join states or check implication the two collapse into one. *)
module PreDomain : BaseDomainSig = Domain
(** represents the inferred pre-condition at each program point, biabduction style *)

module SkippedTrace = struct
  type t = PulseTrace.t [@@deriving compare]

  let pp fmt =
    PulseTrace.pp fmt ~pp_immediate:(fun fmt ->
        F.pp_print_string fmt "call to skipped function occurs here" )


  let leq ~lhs ~rhs = phys_equal lhs rhs

  let join s1 _ = s1

  let widen ~prev ~next ~num_iters:_ = join prev next
end

module SkippedCalls = AbstractDomain.Map (Procname) (SkippedTrace)

(** biabduction-style pre/post state + skipped calls *)
type t =
  { post: Domain.t  (** state at the current program point*)
  ; pre: PreDomain.t  (** inferred pre at the current program point *)
  ; skipped_calls: SkippedCalls.t  (** set of skipped calls *) }

let pp f {post; pre; skipped_calls} =
  F.fprintf f "@[<v>%a@;PRE=[%a]@;skipped_calls=%a@]" Domain.pp post PreDomain.pp pre
    SkippedCalls.pp skipped_calls


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


module Stack = struct
  let is_abducible astate var =
    (* HACK: formals are pre-registered in the initial state *)
    BaseStack.mem var (astate.pre :> base_domain).stack || Var.is_global var


  (** [astate] with [astate.post.stack = f astate.post.stack] *)
  let map_post_stack ~f astate =
    let new_post = Domain.update astate.post ~stack:(f (astate.post :> base_domain).stack) in
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
        ( { post= Domain.update astate.post ~stack:post_stack
          ; pre
          ; skipped_calls= astate.skipped_calls }
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


  (** [astate] with [astate.post.attrs = f astate.post.attrs] *)
  let map_post_attrs ~f astate =
    let new_post = Domain.update astate.post ~attrs:(f (astate.post :> base_domain).attrs) in
    if phys_equal new_post astate.post then astate else {astate with post= new_post}


  let invalidate address invalidation location astate =
    map_post_attrs astate ~f:(BaseAddressAttributes.invalidate address invalidation location)


  let allocate procname address location astate =
    map_post_attrs astate ~f:(BaseAddressAttributes.allocate procname address location)


  let add_one address attributes astate =
    map_post_attrs astate ~f:(BaseAddressAttributes.add_one address attributes)


  let get_closure_proc_name addr astate =
    BaseAddressAttributes.get_closure_proc_name addr (astate.post :> base_domain).attrs


  let get_citv addr astate = BaseAddressAttributes.get_citv addr (astate.post :> base_domain).attrs

  let get_bo_itv addr astate =
    BaseAddressAttributes.get_bo_itv addr (astate.post :> base_domain).attrs


  let std_vector_reserve addr astate =
    map_post_attrs astate ~f:(BaseAddressAttributes.std_vector_reserve addr)


  let is_std_vector_reserved addr astate =
    BaseAddressAttributes.is_std_vector_reserved addr (astate.post :> base_domain).attrs


  let abduce_and_add value attrs astate =
    Attributes.fold attrs ~init:astate ~f:(fun astate attr ->
        let astate =
          if Attribute.is_suitable_for_pre attr then abduce_attribute value attr astate else astate
        in
        add_one value attr astate )


  let find_opt address astate =
    BaseAddressAttributes.find_opt address (astate.post :> base_domain).attrs
end

module Memory = struct
  module Access = BaseMemory.Access
  module Edges = BaseMemory.Edges

  (** [astate] with [astate.post.heap = f astate.post.heap] *)
  let map_post_heap ~f astate =
    let new_post = Domain.update astate.post ~heap:(f (astate.post :> base_domain).heap) in
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
        ( { post= Domain.update astate.post ~heap:post_heap
          ; pre= PreDomain.update astate.pre ~heap:foot_heap
          ; skipped_calls= astate.skipped_calls }
        , addr_hist_dst )


  let find_opt address astate = BaseMemory.find_opt address (astate.post :> base_domain).heap
end

let mk_initial proc_desc =
  (* HACK: save the formals in the stacks of the pre and the post to remember which local variables
     correspond to formals *)
  let formals =
    let proc_name = Procdesc.get_proc_name proc_desc in
    let location = Procdesc.get_loc proc_desc in
    Procdesc.get_formals proc_desc
    |> List.map ~f:(fun (mangled, _) ->
           let pvar = Pvar.mk mangled proc_name in
           ( Var.of_pvar pvar
           , (AbstractValue.mk_fresh (), [ValueHistory.FormalDeclared (pvar, location)]) ) )
  in
  let initial_stack =
    List.fold formals ~init:(PreDomain.empty :> BaseDomain.t).stack
      ~f:(fun stack (formal, addr_loc) -> BaseStack.add formal addr_loc stack)
  in
  let pre =
    let initial_heap =
      List.fold formals ~init:(PreDomain.empty :> base_domain).heap ~f:(fun heap (_, (addr, _)) ->
          BaseMemory.register_address addr heap )
    in
    PreDomain.update ~stack:initial_stack ~heap:initial_heap PreDomain.empty
  in
  let post = Domain.update ~stack:initial_stack Domain.empty in
  let skipped_calls = SkippedCalls.empty in
  {pre; post; skipped_calls}


let add_skipped_calls pname trace astate =
  let new_skipped_calls = SkippedCalls.add pname trace astate.skipped_calls in
  if phys_equal new_skipped_calls astate.skipped_calls then astate
  else {astate with skipped_calls= new_skipped_calls}


let discard_unreachable ({pre; post} as astate) =
  let pre_addresses = BaseDomain.reachable_addresses (pre :> BaseDomain.t) in
  let pre_new =
    PreDomain.filter_addr ~f:(fun address -> AbstractValue.Set.mem address pre_addresses) pre
  in
  let post_addresses = BaseDomain.reachable_addresses (post :> BaseDomain.t) in
  let all_addresses = AbstractValue.Set.union pre_addresses post_addresses in
  let (heap_new, attrs_new), (_, attrs_unreachable) =
    Domain.partition_addr ~f:(fun address -> AbstractValue.Set.mem address all_addresses) post
  in
  let post_new = Domain.update ~heap:heap_new ~attrs:attrs_new post in
  let astate =
    if phys_equal pre_new pre && phys_equal post_new post then astate
    else {astate with pre= pre_new; post= post_new}
  in
  (astate, attrs_unreachable)


let is_local var astate = not (Var.is_return var || Stack.is_abducible astate var)

(* {3 Helper functions to traverse the two maps at once } *)

let find_post_cell_opt addr {post} = BaseDomain.find_cell_opt addr (post :> BaseDomain.t)

let set_post_cell (addr, history) (edges_map, attr_set) location astate =
  Memory.map_post_heap astate ~f:(BaseMemory.add addr edges_map)
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
  ; post= Domain.update ~stack:post_stack ~heap:post_heap astate.post }


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
  else {astate with pre= astate.pre; post= Domain.update astate.post ~attrs:attrs'}


let of_post pdesc astate =
  let domain = filter_for_summary astate in
  let domain, _ = discard_unreachable domain in
  invalidate_locals pdesc domain


(* {2 machinery to apply a pre/post pair corresponding to a function's summary in a function call
   to the current state} *)

module AddressSet = AbstractValue.Set
module AddressMap = AbstractValue.Map

(** stuff we carry around when computing the result of applying one pre/post pair *)
type call_state =
  { astate: t  (** caller's abstract state computed so far *)
  ; subst: (AbstractValue.t * ValueHistory.t) AddressMap.t
        (** translation from callee addresses to caller addresses and their caller histories *)
  ; rev_subst: AbstractValue.t AddressMap.t
        (** the inverse translation from [subst] from caller addresses to callee addresses *)
  ; visited: AddressSet.t
        (** set of callee addresses that have been visited already

            NOTE: this is not always equal to the domain of [rev_subst]: when applying the post we
            visit each subgraph from each formal independently so we reset [visited] between the
            visit of each formal *) }

let pp_call_state fmt {astate; subst; rev_subst; visited} =
  F.fprintf fmt
    "@[<v>{ astate=@[<hv2>%a@];@,\
    \ subst=@[<hv2>%a@];@,\
    \ rev_subst=@[<hv2>%a@];@,\
    \ visited=@[<hv2>%a@]@,\
    \ }@]" pp astate
    (AddressMap.pp ~pp_value:(fun fmt (addr, _) -> AbstractValue.pp fmt addr))
    subst
    (AddressMap.pp ~pp_value:AbstractValue.pp)
    rev_subst AddressSet.pp visited


type contradiction =
  | Aliasing of
      { addr_caller: AbstractValue.t
      ; addr_callee: AbstractValue.t
      ; addr_callee': AbstractValue.t
      ; call_state: call_state }
      (** raised when the precondition and the current state disagree on the aliasing, i.e. some
          addresses [callee_addr] and [callee_addr'] that are distinct in the pre are aliased to a
          single address [caller_addr] in the caller's current state. Typically raised when calling
          [foo(z,z)] where the spec for [foo(x,y)] says that [x] and [y] are disjoint. *)
  | CItv of
      { addr_caller: AbstractValue.t
      ; addr_callee: AbstractValue.t
      ; arith_caller: CItv.t option
      ; arith_callee: CItv.t option
      ; call_state: call_state }
      (** raised when the pre asserts arithmetic facts that are demonstrably false in the caller
          state *)
  | ArithmeticBo of
      { addr_caller: AbstractValue.t
      ; addr_callee: AbstractValue.t
      ; arith_callee: Itv.ItvPure.t
      ; call_state: call_state }
      (** raised when the pre asserts arithmetic facts that are demonstrably false in the caller
          state *)
  | FormalActualLength of
      {formals: Var.t list; actuals: ((AbstractValue.t * ValueHistory.t) * Typ.t) list}

let pp_contradiction fmt = function
  | Aliasing {addr_caller; addr_callee; addr_callee'; call_state} ->
      F.fprintf fmt
        "address %a in caller already bound to %a, not %a@\nnote: current call state was %a"
        AbstractValue.pp addr_caller AbstractValue.pp addr_callee' AbstractValue.pp addr_callee
        pp_call_state call_state
  | CItv {addr_caller; addr_callee; arith_caller; arith_callee; call_state} ->
      F.fprintf fmt
        "caller addr %a%a but callee addr %a%a; %a=%a is unsatisfiable@\n\
         note: current call state was %a" AbstractValue.pp addr_caller (Pp.option CItv.pp)
        arith_caller AbstractValue.pp addr_callee (Pp.option CItv.pp) arith_callee AbstractValue.pp
        addr_caller AbstractValue.pp addr_callee pp_call_state call_state
  | ArithmeticBo {addr_caller; addr_callee; arith_callee; call_state} ->
      F.fprintf fmt
        "callee addr %a%a is incompatible with caller addr %a's arithmetic constraints@\n\
         note: current call state was %a" AbstractValue.pp addr_callee Itv.ItvPure.pp arith_callee
        AbstractValue.pp addr_caller pp_call_state call_state
  | FormalActualLength {formals; actuals} ->
      F.fprintf fmt "formals have length %d but actuals have length %d" (List.length formals)
        (List.length actuals)


exception Contradiction of contradiction

let fold_globals_of_stack call_loc stack call_state ~f =
  Container.fold_result ~fold:(IContainer.fold_of_pervasives_map_fold ~fold:BaseStack.fold)
    stack ~init:call_state ~f:(fun call_state (var, stack_value) ->
      match var with
      | Var.ProgramVar pvar when Pvar.is_global pvar ->
          let call_state, addr_hist_caller =
            let astate, var_value =
              Stack.eval [ValueHistory.VariableAccessed (pvar, call_loc)] var call_state.astate
            in
            if phys_equal astate call_state.astate then (call_state, var_value)
            else ({call_state with astate}, var_value)
          in
          f pvar ~stack_value ~addr_hist_caller call_state
      | _ ->
          Ok call_state )


let visit call_state ~addr_callee ~addr_hist_caller =
  let addr_caller = fst addr_hist_caller in
  ( match AddressMap.find_opt addr_caller call_state.rev_subst with
  | Some addr_callee' when not (AbstractValue.equal addr_callee addr_callee') ->
      raise (Contradiction (Aliasing {addr_caller; addr_callee; addr_callee'; call_state}))
  | _ ->
      () ) ;
  if AddressSet.mem addr_callee call_state.visited then (`AlreadyVisited, call_state)
  else
    ( `NotAlreadyVisited
    , { call_state with
        visited= AddressSet.add addr_callee call_state.visited
      ; subst= AddressMap.add addr_callee addr_hist_caller call_state.subst
      ; rev_subst= AddressMap.add addr_caller addr_callee call_state.rev_subst } )


let pp f {pre; post; skipped_calls} =
  F.fprintf f "PRE:@\n  @[%a@]@\n" BaseDomain.pp (pre :> BaseDomain.t) ;
  F.fprintf f "POST:@\n  @[%a@]@\n" BaseDomain.pp (post :> BaseDomain.t) ;
  F.fprintf f "SKIPPED_CALLS:@ @[%a@]@\n" SkippedCalls.pp skipped_calls


(* {3 reading the pre from the current state} *)

let add_call_to_trace proc_name call_location caller_history in_call =
  Trace.ViaCall {f= Call proc_name; location= call_location; history= caller_history; in_call}


(** Materialize the (abstract memory) subgraph of [pre] reachable from [addr_pre] in
    [call_state.astate] starting from address [addr_caller]. Report an error if some invalid
    addresses are traversed in the process. *)
let rec materialize_pre_from_address callee_proc_name call_location ~pre ~addr_pre ~addr_hist_caller
    call_state =
  match visit call_state ~addr_callee:addr_pre ~addr_hist_caller with
  | `AlreadyVisited, call_state ->
      Ok call_state
  | `NotAlreadyVisited, call_state -> (
    match BaseMemory.find_opt addr_pre pre.BaseDomain.heap with
    | None ->
        Ok call_state
    | Some edges_pre ->
        Container.fold_result ~fold:(IContainer.fold_of_pervasives_map_fold ~fold:Memory.Edges.fold)
          ~init:call_state edges_pre ~f:(fun call_state (access, (addr_pre_dest, _)) ->
            let astate, addr_hist_dest_caller =
              Memory.eval_edge addr_hist_caller access call_state.astate
            in
            let call_state = {call_state with astate} in
            materialize_pre_from_address callee_proc_name call_location ~pre ~addr_pre:addr_pre_dest
              ~addr_hist_caller:addr_hist_dest_caller call_state ) )


(** materialize subgraph of [pre] rooted at the address represented by a [formal] parameter that has
    been instantiated with the corresponding [actual] into the current state [call_state.astate] *)
let materialize_pre_from_actual callee_proc_name call_location ~pre ~formal ~actual call_state =
  L.d_printfln "Materializing PRE from [%a <- %a]" Var.pp formal AbstractValue.pp (fst actual) ;
  (let open IOption.Let_syntax in
  let* addr_formal_pre, _ = BaseStack.find_opt formal pre.BaseDomain.stack in
  let+ formal_pre, _ = BaseMemory.find_edge_opt addr_formal_pre Dereference pre.BaseDomain.heap in
  materialize_pre_from_address callee_proc_name call_location ~pre ~addr_pre:formal_pre
    ~addr_hist_caller:actual call_state)
  |> function Some result -> result | None -> Ok call_state


let is_cell_read_only ~edges_pre_opt ~cell_post:(edges_post, attrs_post) =
  match edges_pre_opt with
  | None ->
      false
  | Some edges_pre when not (Attributes.is_modified attrs_post) ->
      let are_edges_equal =
        BaseMemory.Edges.equal
          (fun (addr_dest_pre, _) (addr_dest_post, _) ->
            (* NOTE: ignores traces

               TODO: can the traces be leveraged here? maybe easy to detect writes by looking at
               the post trace *)
            AbstractValue.equal addr_dest_pre addr_dest_post )
          edges_pre edges_post
      in
      if CommandLineOption.strict_mode then assert are_edges_equal ;
      are_edges_equal
  | _ ->
      false


let materialize_pre_for_parameters callee_proc_name call_location pre_post ~formals ~actuals
    call_state =
  (* For each [(formal, actual)] pair, resolve them to addresses in their respective states then
     call [materialize_pre_from] on them.  Give up if calling the function introduces aliasing.
  *)
  match
    IList.fold2_result formals actuals ~init:call_state ~f:(fun call_state formal (actual, _) ->
        materialize_pre_from_actual callee_proc_name call_location
          ~pre:(pre_post.pre :> BaseDomain.t)
          ~formal ~actual call_state )
  with
  | Unequal_lengths ->
      raise (Contradiction (FormalActualLength {formals; actuals}))
  | Ok result ->
      result


let materialize_pre_for_globals callee_proc_name call_location pre_post call_state =
  fold_globals_of_stack call_location (pre_post.pre :> BaseDomain.t).stack call_state
    ~f:(fun _var ~stack_value:(addr_pre, _) ~addr_hist_caller call_state ->
      materialize_pre_from_address callee_proc_name call_location
        ~pre:(pre_post.pre :> BaseDomain.t)
        ~addr_pre ~addr_hist_caller call_state )


let eval_sym_of_subst astate subst s bound_end =
  let v = Symb.Symbol.get_pulse_value_exn s in
  match PulseAbstractValue.Map.find_opt v !subst with
  | Some (v', _) ->
      Itv.ItvPure.get_bound (AddressAttributes.get_bo_itv v' astate) bound_end
  | None ->
      let v' = PulseAbstractValue.mk_fresh () in
      subst := PulseAbstractValue.Map.add v (v', []) !subst ;
      Bounds.Bound.of_pulse_value v'


let subst_attribute call_state subst_ref astate ~addr_caller attr ~addr_callee =
  match (attr : Attribute.t) with
  | CItv (arith_callee, hist) -> (
      let arith_caller_opt = AddressAttributes.get_citv addr_caller astate |> Option.map ~f:fst in
      match CItv.abduce_binop_is_true ~negated:false Eq arith_caller_opt (Some arith_callee) with
      | Unsatisfiable ->
          raise
            (Contradiction
               (CItv
                  { addr_caller
                  ; addr_callee
                  ; arith_caller= arith_caller_opt
                  ; arith_callee= Some arith_callee
                  ; call_state }))
      | Satisfiable (Some abduce_caller, _abduce_callee) ->
          Attribute.CItv (abduce_caller, hist)
      | Satisfiable (None, _) ->
          attr )
  | BoItv itv -> (
    match
      Itv.ItvPure.subst itv (fun symb bound ->
          AbstractDomain.Types.NonBottom (eval_sym_of_subst astate subst_ref symb bound) )
    with
    | NonBottom itv' ->
        Attribute.BoItv itv'
    | Bottom ->
        raise
          (Contradiction (ArithmeticBo {addr_callee; addr_caller; arith_callee= itv; call_state})) )
  | AddressOfCppTemporary _
  | AddressOfStackVariable _
  | Allocated _
  | Closure _
  | Invalid _
  | MustBeValid _
  | StdVectorReserve
  | WrittenTo _ ->
      (* non-relational attributes *)
      attr


let add_call_to_attributes proc_name call_location ~addr_callee ~addr_caller caller_history attrs
    call_state =
  let add_call_to_attribute attr =
    match (attr : Attribute.t) with
    | Invalid (invalidation, trace) ->
        Attribute.Invalid
          (invalidation, add_call_to_trace proc_name call_location caller_history trace)
    | CItv (arith, trace) ->
        Attribute.CItv (arith, add_call_to_trace proc_name call_location caller_history trace)
    | Allocated (procname, trace) ->
        Attribute.Allocated
          (procname, add_call_to_trace proc_name call_location caller_history trace)
    | AddressOfCppTemporary _
    | AddressOfStackVariable _
    | BoItv _
    | Closure _
    | MustBeValid _
    | StdVectorReserve
    | WrittenTo _ ->
        attr
  in
  let subst_ref = ref call_state.subst in
  let attrs =
    Attributes.map attrs ~f:(fun attr ->
        let attr =
          subst_attribute call_state subst_ref call_state.astate ~addr_callee ~addr_caller attr
        in
        add_call_to_attribute attr )
  in
  (!subst_ref, attrs)


let apply_arithmetic_constraints callee_proc_name call_location pre_post call_state =
  let one_address_sat addr_callee callee_attrs (addr_caller, caller_history) call_state =
    let subst, attrs_caller =
      add_call_to_attributes callee_proc_name call_location ~addr_callee ~addr_caller caller_history
        callee_attrs call_state
    in
    let astate = AddressAttributes.abduce_and_add addr_caller attrs_caller call_state.astate in
    if phys_equal subst call_state.subst && phys_equal astate call_state.astate then call_state
    else {call_state with subst; astate}
  in
  (* check all callee addresses that make sense for the caller, i.e. the domain of [call_state.subst] *)
  AddressMap.fold
    (fun addr_callee addr_hist_caller call_state ->
      match BaseAddressAttributes.find_opt addr_callee (pre_post.pre :> BaseDomain.t).attrs with
      | None ->
          call_state
      | Some callee_attrs ->
          one_address_sat addr_callee callee_attrs addr_hist_caller call_state )
    call_state.subst call_state


let materialize_pre callee_proc_name call_location pre_post ~formals ~actuals call_state =
  PerfEvent.(log (fun logger -> log_begin_event logger ~name:"pulse call pre" ())) ;
  let r =
    let open IResult.Let_syntax in
    (* first make as large a mapping as we can between callee values and caller values... *)
    materialize_pre_for_parameters callee_proc_name call_location pre_post ~formals ~actuals
      call_state
    >>= materialize_pre_for_globals callee_proc_name call_location pre_post
    >>| (* ...then relational arithmetic constraints in the callee's attributes will make sense in
           terms of the caller's values *)
    apply_arithmetic_constraints callee_proc_name call_location pre_post
  in
  PerfEvent.(log (fun logger -> log_end_event logger ())) ;
  r


(* {3 applying the post to the current state} *)

let subst_find_or_new subst addr_callee ~default_hist_caller =
  match AddressMap.find_opt addr_callee subst with
  | None ->
      let addr_hist_fresh = (AbstractValue.mk_fresh (), default_hist_caller) in
      (AddressMap.add addr_callee addr_hist_fresh subst, addr_hist_fresh)
  | Some addr_hist_caller ->
      (subst, addr_hist_caller)


let call_state_subst_find_or_new call_state addr_callee ~default_hist_caller =
  let new_subst, addr_hist_caller =
    subst_find_or_new call_state.subst addr_callee ~default_hist_caller
  in
  if phys_equal new_subst call_state.subst then (call_state, addr_hist_caller)
  else ({call_state with subst= new_subst}, addr_hist_caller)


let delete_edges_in_callee_pre_from_caller ~addr_callee:_ ~edges_pre_opt ~addr_caller call_state =
  match BaseMemory.find_opt addr_caller (call_state.astate.post :> base_domain).heap with
  | None ->
      BaseMemory.Edges.empty
  | Some old_post_edges -> (
    match edges_pre_opt with
    | None ->
        old_post_edges
    | Some edges_pre ->
        BaseMemory.Edges.merge
          (fun _access old_opt pre_opt ->
            (* TODO: should apply [call_state.subst] to [_access]! Actually, should rewrite the
               whole [cell_pre] beforehand so that [Edges.merge] makes sense. *)
            if Option.is_some pre_opt then
              (* delete edge if some edge for the same access exists in the pre *)
              None
            else (* keep old edge if it exists *) old_opt )
          old_post_edges edges_pre )


let record_post_cell callee_proc_name call_loc ~addr_callee ~edges_pre_opt
    ~cell_post:(edges_post, attrs_post) ~addr_hist_caller:(addr_caller, hist_caller) call_state =
  let post_edges_minus_pre =
    delete_edges_in_callee_pre_from_caller ~addr_callee ~edges_pre_opt ~addr_caller call_state
  in
  let call_state =
    let subst, attrs_post_caller =
      add_call_to_attributes ~addr_callee ~addr_caller callee_proc_name call_loc hist_caller
        attrs_post call_state
    in
    let astate = AddressAttributes.abduce_and_add addr_caller attrs_post_caller call_state.astate in
    {call_state with subst; astate}
  in
  let heap = (call_state.astate.post :> base_domain).heap in
  let attrs = (call_state.astate.post :> base_domain).attrs in
  let subst, translated_post_edges =
    BaseMemory.Edges.fold_map ~init:call_state.subst edges_post
      ~f:(fun subst (addr_callee, trace_post) ->
        let subst, (addr_curr, hist_curr) =
          subst_find_or_new subst addr_callee ~default_hist_caller:hist_caller
        in
        ( subst
        , ( addr_curr
          , ValueHistory.Call {f= Call callee_proc_name; location= call_loc; in_call= trace_post}
            :: hist_curr ) ) )
  in
  let heap =
    let edges_post_caller =
      BaseMemory.Edges.union
        (fun _ _ post_cell -> Some post_cell)
        post_edges_minus_pre translated_post_edges
    in
    BaseMemory.add addr_caller edges_post_caller heap
  in
  let attrs =
    let written_to_callee_opt =
      let open IOption.Let_syntax in
      let* attrs = BaseAddressAttributes.find_opt addr_caller attrs in
      Attributes.get_written_to attrs
    in
    match written_to_callee_opt with
    | None ->
        attrs
    | Some callee_trace ->
        let written_to =
          Attribute.WrittenTo
            (ViaCall
               { in_call= callee_trace
               ; f= Call callee_proc_name
               ; location= call_loc
               ; history= hist_caller })
        in
        BaseAddressAttributes.add_one addr_caller written_to attrs
  in
  let caller_post = Domain.update ~heap ~attrs call_state.astate.post in
  {call_state with subst; astate= {call_state.astate with post= caller_post}}


let rec record_post_for_address callee_proc_name call_loc ({pre} as pre_post) ~addr_callee
    ~addr_hist_caller call_state =
  L.d_printfln "%a<->%a" AbstractValue.pp addr_callee AbstractValue.pp (fst addr_hist_caller) ;
  match visit call_state ~addr_callee ~addr_hist_caller with
  | `AlreadyVisited, call_state ->
      call_state
  | `NotAlreadyVisited, call_state -> (
    match find_post_cell_opt addr_callee pre_post with
    | None ->
        call_state
    | Some ((edges_post, _attrs_post) as cell_post) ->
        let edges_pre_opt = BaseMemory.find_opt addr_callee (pre :> BaseDomain.t).BaseDomain.heap in
        let call_state_after_post =
          if is_cell_read_only ~edges_pre_opt ~cell_post then call_state
          else
            record_post_cell callee_proc_name call_loc ~addr_callee ~edges_pre_opt ~addr_hist_caller
              ~cell_post call_state
        in
        IContainer.fold_of_pervasives_map_fold ~fold:Memory.Edges.fold ~init:call_state_after_post
          edges_post ~f:(fun call_state (_access, (addr_callee_dest, _)) ->
            let call_state, addr_hist_curr_dest =
              call_state_subst_find_or_new call_state addr_callee_dest
                ~default_hist_caller:(snd addr_hist_caller)
            in
            record_post_for_address callee_proc_name call_loc pre_post ~addr_callee:addr_callee_dest
              ~addr_hist_caller:addr_hist_curr_dest call_state ) )


let record_post_for_actual callee_proc_name call_loc pre_post ~formal ~actual call_state =
  L.d_printfln_escaped "Recording POST from [%a] <-> %a" Var.pp formal AbstractValue.pp (fst actual) ;
  match
    let open IOption.Let_syntax in
    let* addr_formal_pre, _ =
      BaseStack.find_opt formal (pre_post.pre :> BaseDomain.t).BaseDomain.stack
    in
    let+ formal_pre, _ =
      BaseMemory.find_edge_opt addr_formal_pre Dereference
        (pre_post.pre :> BaseDomain.t).BaseDomain.heap
    in
    record_post_for_address callee_proc_name call_loc pre_post ~addr_callee:formal_pre
      ~addr_hist_caller:actual call_state
  with
  | Some call_state ->
      call_state
  | None ->
      call_state


let record_post_for_return callee_proc_name call_loc pre_post call_state =
  let return_var = Var.of_pvar (Pvar.get_ret_pvar callee_proc_name) in
  match BaseStack.find_opt return_var (pre_post.post :> BaseDomain.t).stack with
  | None ->
      (call_state, None)
  | Some (addr_return, _) -> (
    match
      BaseMemory.find_edge_opt addr_return Dereference
        (pre_post.post :> BaseDomain.t).BaseDomain.heap
    with
    | None ->
        (call_state, None)
    | Some (return_callee, _) ->
        let return_caller_addr_hist =
          match AddressMap.find_opt return_callee call_state.subst with
          | Some return_caller_hist ->
              return_caller_hist
          | None ->
              ( AbstractValue.mk_fresh ()
              , [ (* this could maybe include an event like "returned here" *) ] )
        in
        let call_state =
          record_post_for_address callee_proc_name call_loc pre_post ~addr_callee:return_callee
            ~addr_hist_caller:return_caller_addr_hist call_state
        in
        (call_state, Some return_caller_addr_hist) )


let apply_post_for_parameters callee_proc_name call_location pre_post ~formals ~actuals call_state =
  (* for each [(formal_i, actual_i)] pair, do [post_i = post union subst(graph reachable from
     formal_i in post)], deleting previous info when comparing pre and post shows a difference
     (TODO: record in the pre when a location is written to instead of just comparing values
     between pre and post since it's unreliable, eg replace value read in pre with same value in
     post but nuke other fields in the meantime? is that possible?). *)
  match
    List.fold2 formals actuals ~init:call_state ~f:(fun call_state formal (actual, _) ->
        record_post_for_actual callee_proc_name call_location pre_post ~formal ~actual call_state )
  with
  | Unequal_lengths ->
      (* should have been checked before by [materialize_pre] *)
      assert false
  | Ok call_state ->
      call_state


let apply_post_for_globals callee_proc_name call_location pre_post call_state =
  match
    fold_globals_of_stack call_location (pre_post.pre :> BaseDomain.t).stack call_state
      ~f:(fun _var ~stack_value:(addr_callee, _) ~addr_hist_caller call_state ->
        Ok
          (record_post_for_address callee_proc_name call_location pre_post ~addr_callee
             ~addr_hist_caller call_state) )
  with
  | Error _ ->
      (* always return [Ok _] above *) assert false
  | Ok call_state ->
      call_state


let record_post_remaining_attributes callee_proc_name call_loc pre_post call_state =
  BaseAddressAttributes.fold
    (fun addr_callee attrs call_state ->
      if AddressSet.mem addr_callee call_state.visited then
        (* already recorded the attributes when we were walking the edges map *)
        call_state
      else
        match AddressMap.find_opt addr_callee call_state.subst with
        | None ->
            (* callee address has no meaning for the caller *) call_state
        | Some (addr_caller, history) ->
            let subst, attrs' =
              add_call_to_attributes callee_proc_name call_loc ~addr_callee ~addr_caller history
                attrs call_state
            in
            let astate = AddressAttributes.abduce_and_add addr_caller attrs' call_state.astate in
            {call_state with subst; astate} )
    (pre_post.post :> BaseDomain.t).attrs call_state


let record_skipped_calls callee_proc_name call_loc pre_post call_state =
  let callee_skipped_map = pre_post.skipped_calls in
  let caller_skipped_map =
    SkippedCalls.map
      (fun trace -> add_call_to_trace callee_proc_name call_loc [] trace)
      callee_skipped_map
    |> (* favor calls we already knew about somewhat arbitrarily *)
    SkippedCalls.union
      (fun _ orig_call _callee_call -> Some orig_call)
      call_state.astate.skipped_calls
  in
  {call_state with astate= {call_state.astate with skipped_calls= caller_skipped_map}}


let apply_post callee_proc_name call_location pre_post ~formals ~actuals call_state =
  PerfEvent.(log (fun logger -> log_begin_event logger ~name:"pulse call post" ())) ;
  let r =
    apply_post_for_parameters callee_proc_name call_location pre_post ~formals ~actuals call_state
    |> apply_post_for_globals callee_proc_name call_location pre_post
    |> record_post_for_return callee_proc_name call_location pre_post
    |> fun (call_state, return_caller) ->
    ( record_post_remaining_attributes callee_proc_name call_location pre_post call_state
      |> record_skipped_calls callee_proc_name call_location pre_post
    , return_caller )
    |> fun ({astate}, return_caller) -> (astate, return_caller)
  in
  PerfEvent.(log (fun logger -> log_end_event logger ())) ;
  r


let check_all_valid callee_proc_name call_location {pre; post= _} call_state =
  AddressMap.fold
    (fun addr_pre (addr_caller, hist_caller) astate_result ->
      match astate_result with
      | Error _ ->
          astate_result
      | Ok astate -> (
        match BaseAddressAttributes.get_must_be_valid addr_pre (pre :> BaseDomain.t).attrs with
        | None ->
            astate_result
        | Some callee_access_trace ->
            let access_trace =
              Trace.ViaCall
                { in_call= callee_access_trace
                ; f= Call callee_proc_name
                ; location= call_location
                ; history= hist_caller }
            in
            AddressAttributes.check_valid access_trace addr_caller astate
            |> Result.map_error ~f:(fun (invalidation, invalidation_trace) ->
                   L.d_printfln "ERROR: caller's %a invalid!" AbstractValue.pp addr_caller ;
                   Diagnostic.AccessToInvalidAddress {invalidation; invalidation_trace; access_trace}
               ) ) )
    call_state.subst (Ok call_state.astate)


(* - read all the pre, assert validity of addresses and materializes *everything* (to throw stuff
   in the *current* pre as appropriate so that callers of the current procedure will also know
   about the deeper reads)

   - for each actual, write the post for that actual

   - if aliasing is introduced at any time then give up

   questions:

   - what if some preconditions raise lifetime issues but others don't? Have to be careful with
   the noise that this will introduce since we don't care about values. For instance, if the pre
   is for a path where [formal != 0] and we pass [0] then it will be an FP. Maybe the solution is
   to bake in some value analysis. *)
let apply callee_proc_name call_location pre_post ~formals ~actuals astate =
  L.d_printfln "Applying pre/post for %a(%a):@\n%a" Procname.pp callee_proc_name
    (Pp.seq ~sep:"," Var.pp) formals pp pre_post ;
  let empty_call_state =
    {astate; subst= AddressMap.empty; rev_subst= AddressMap.empty; visited= AddressSet.empty}
  in
  (* read the precondition *)
  match
    materialize_pre callee_proc_name call_location pre_post ~formals ~actuals empty_call_state
  with
  | exception Contradiction reason ->
      (* can't make sense of the pre-condition in the current context: give up on that particular
         pre/post pair *)
      L.d_printfln "Cannot apply precondition: %a" pp_contradiction reason ;
      Ok None
  | Error _ as error ->
      (* error: the function call requires to read some state known to be invalid *)
      error
  | Ok call_state -> (
      L.d_printfln "Pre applied successfully. call_state=%a" pp_call_state call_state ;
      match
        let open IResult.Let_syntax in
        let+ astate = check_all_valid callee_proc_name call_location pre_post call_state in
        (* reset [visited] *)
        let call_state = {call_state with astate; visited= AddressSet.empty} in
        (* apply the postcondition *)
        apply_post callee_proc_name call_location pre_post ~formals ~actuals call_state
      with
      | Ok post ->
          Ok (Some post)
      | exception Contradiction reason ->
          L.d_printfln "Cannot apply post-condition: %a" pp_contradiction reason ;
          Ok None
      | Error _ as error ->
          error )


let get_pre {pre} = (pre :> BaseDomain.t)

let get_post {post} = (post :> BaseDomain.t)

let get_skipped_calls {skipped_calls} = skipped_calls
