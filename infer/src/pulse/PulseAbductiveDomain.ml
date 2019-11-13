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

(** signature common to the "normal" [Domain], representing the post at the current program point,
    and the inverted [InvertedDomain], representing the inferred pre-condition*)
module type BaseDomain = sig
  (* private because the lattice is not the same for preconditions and postconditions so we don't
     want to confuse them *)
  type t = private BaseDomain.t

  val empty : t

  val make : BaseStack.t -> BaseMemory.t -> t

  val update : ?stack:BaseStack.t -> ?heap:BaseMemory.t -> t -> t

  include AbstractDomain.NoJoin with type t := t
end

(* just to expose the [heap] and [stack] record field names without having to type
   [BaseDomain.heap] *)
type base_domain = BaseDomain.t = {heap: BaseMemory.t; stack: BaseStack.t}

(** operations common to [Domain] and [InvertedDomain], see also the [BaseDomain] signature *)
module BaseDomainCommon = struct
  let make stack heap = {stack; heap}

  let update ?stack ?heap foot =
    let new_stack, new_heap =
      (Option.value ~default:foot.stack stack, Option.value ~default:foot.heap heap)
    in
    if phys_equal new_stack foot.stack && phys_equal new_heap foot.heap then foot
    else {stack= new_stack; heap= new_heap}
end

(** represents the post abstract state at each program point *)
module Domain : BaseDomain = struct
  include BaseDomainCommon
  include BaseDomain
end

(** represents the inferred pre-condition at each program point, biabduction style *)
module InvertedDomain : BaseDomain = struct
  include BaseDomainCommon

  type t = BaseDomain.t

  let empty = BaseDomain.empty

  let pp = BaseDomain.pp

  (** inverted lattice *)
  let leq ~lhs ~rhs = BaseDomain.leq ~rhs:lhs ~lhs:rhs
end

(** biabduction-style pre/post state *)
type t =
  { post: Domain.t  (** state at the current program point*)
  ; pre: InvertedDomain.t  (** inferred pre at the current program point *) }

let pp f {post; pre} = F.fprintf f "@[<v>%a@;PRE=[%a]@]" Domain.pp post InvertedDomain.pp pre

let leq ~lhs ~rhs =
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
            InvertedDomain.make foot_stack foot_heap
          else astate.pre
        in
        ({post= Domain.update astate.post ~stack:post_stack; pre}, addr_hist)


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

module Memory = struct
  open Result.Monad_infix
  module Access = BaseMemory.Access

  (** [astate] with [astate.post.heap = f astate.post.heap] *)
  let map_post_heap ~f astate =
    let new_post = Domain.update astate.post ~heap:(f (astate.post :> base_domain).heap) in
    if phys_equal new_post astate.post then astate else {astate with post= new_post}


  (** if [address] is in [pre] then add the attribute [attr] *)
  let abduce_attribute address attribute astate =
    L.d_printfln "Abducing %a:%a" AbstractValue.pp address Attribute.pp attribute ;
    let new_pre =
      if BaseMemory.mem_edges address (astate.pre :> base_domain).heap then
        InvertedDomain.update astate.pre
          ~heap:(BaseMemory.add_attribute address attribute (astate.pre :> base_domain).heap)
      else astate.pre
    in
    if phys_equal new_pre astate.pre then astate else {astate with pre= new_pre}


  let check_valid access_trace addr astate =
    BaseMemory.check_valid addr (astate.post :> base_domain).heap
    >>| fun () ->
    (* if [address] is in [pre] and it should be valid then that fact goes in the precondition *)
    abduce_attribute addr (MustBeValid access_trace) astate


  let add_edge (addr, history) access new_addr_hist location astate =
    map_post_heap astate ~f:(fun heap ->
        BaseMemory.add_edge addr access new_addr_hist heap
        |> BaseMemory.add_attribute addr (WrittenTo (Trace.Immediate {location; history})) )


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
          if BaseMemory.mem_edges addr_src (astate.pre :> base_domain).heap then
            (* HACK: do not record the history of values in the pre as they are unused *)
            BaseMemory.add_edge addr_src access (addr_dst, []) (astate.pre :> base_domain).heap
            |> BaseMemory.register_address addr_dst
          else (astate.pre :> base_domain).heap
        in
        ( { post= Domain.update astate.post ~heap:post_heap
          ; pre= InvertedDomain.update astate.pre ~heap:foot_heap }
        , addr_hist_dst )


  let invalidate address invalidation location astate =
    map_post_heap astate ~f:(fun heap -> BaseMemory.invalidate address invalidation location heap)


  let add_attribute address attributes astate =
    map_post_heap astate ~f:(fun heap -> BaseMemory.add_attribute address attributes heap)


  let get_closure_proc_name addr astate =
    BaseMemory.get_closure_proc_name addr (astate.post :> base_domain).heap


  let get_arithmetic addr astate = BaseMemory.get_arithmetic addr (astate.post :> base_domain).heap

  let std_vector_reserve addr astate =
    map_post_heap astate ~f:(fun heap -> BaseMemory.std_vector_reserve addr heap)


  let is_std_vector_reserved addr astate =
    BaseMemory.is_std_vector_reserved addr (astate.post :> base_domain).heap


  let find_opt address astate = BaseMemory.find_opt address (astate.post :> base_domain).heap

  let set_cell (addr, history) cell location astate =
    map_post_heap astate ~f:(fun heap ->
        BaseMemory.set_cell addr cell heap
        |> BaseMemory.add_attribute addr (WrittenTo (Trace.Immediate {location; history})) )


  module Edges = BaseMemory.Edges
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
    List.fold formals ~init:(InvertedDomain.empty :> BaseDomain.t).stack
      ~f:(fun stack (formal, addr_loc) -> BaseStack.add formal addr_loc stack)
  in
  let pre =
    let initial_heap =
      List.fold formals ~init:(InvertedDomain.empty :> base_domain).heap
        ~f:(fun heap (_, (addr, _)) -> BaseMemory.register_address addr heap)
    in
    InvertedDomain.make initial_stack initial_heap
  in
  let post = Domain.update ~stack:initial_stack Domain.empty in
  {pre; post}


let discard_unreachable ({pre; post} as astate) =
  let pre_addresses = BaseDomain.reachable_addresses (pre :> BaseDomain.t) in
  let pre_old_heap = (pre :> BaseDomain.t).heap in
  let pre_new_heap =
    BaseMemory.filter (fun address -> AbstractValue.Set.mem address pre_addresses) pre_old_heap
  in
  let post_addresses = BaseDomain.reachable_addresses (post :> BaseDomain.t) in
  let all_addresses = AbstractValue.Set.union pre_addresses post_addresses in
  let post_old_heap = (post :> BaseDomain.t).heap in
  let post_new_heap =
    BaseMemory.filter (fun address -> AbstractValue.Set.mem address all_addresses) post_old_heap
  in
  if phys_equal pre_new_heap pre_old_heap && phys_equal post_new_heap post_old_heap then astate
  else
    { pre= InvertedDomain.make (pre :> BaseDomain.t).stack pre_new_heap
    ; post= Domain.make (post :> BaseDomain.t).stack post_new_heap }


let is_local var astate = not (Var.is_return var || Stack.is_abducible astate var)

module PrePost = struct
  type domain_t = t

  type t = domain_t

  let filter_for_summary astate =
    let post_stack =
      BaseStack.filter
        (fun var _ -> Var.appears_in_source_code var && not (is_local var astate))
        (astate.post :> BaseDomain.t).stack
    in
    (* deregister empty edges *)
    let deregister_empty heap =
      BaseMemory.filter_heap (fun _addr edges -> not (BaseMemory.Edges.is_empty edges)) heap
    in
    let pre_heap = deregister_empty (astate.pre :> base_domain).heap in
    let post_heap = deregister_empty (astate.post :> base_domain).heap in
    { pre= InvertedDomain.update astate.pre ~heap:pre_heap
    ; post= Domain.update ~stack:post_stack ~heap:post_heap astate.post }


  let add_out_of_scope_attribute addr pvar location history heap typ =
    BaseMemory.add_attribute addr
      (Invalid (GoneOutOfScope (pvar, typ), Immediate {location; history}))
      heap


  (** invalidate local variables going out of scope *)
  let invalidate_locals pdesc astate : t =
    let heap : BaseMemory.t = (astate.post :> BaseDomain.t).heap in
    let heap' =
      BaseMemory.fold_attrs
        (fun addr attrs heap ->
          Attributes.get_address_of_stack_variable attrs
          |> Option.value_map ~default:heap ~f:(fun (var, location, history) ->
                 let get_local_typ_opt pvar =
                   Procdesc.get_locals pdesc
                   |> List.find_map ~f:(fun ProcAttributes.{name; typ} ->
                          if Mangled.equal name (Pvar.get_name pvar) then Some typ else None )
                 in
                 match var with
                 | Var.ProgramVar pvar ->
                     get_local_typ_opt pvar
                     |> Option.value_map ~default:heap
                          ~f:(add_out_of_scope_attribute addr pvar location history heap)
                 | _ ->
                     heap ) )
        heap heap
    in
    if phys_equal heap heap' then astate
    else {pre= astate.pre; post= Domain.update astate.post ~heap:heap'}


  let of_post pdesc astate =
    filter_for_summary astate |> discard_unreachable |> invalidate_locals pdesc


  (* {2 machinery to apply a pre/post pair corresponding to a function's summary in a function call
     to the current state} *)

  module AddressSet = AbstractValue.Set
  module AddressMap = AbstractValue.Map

  type cannot_apply_pre =
    | Aliasing of
        {addr_caller: AbstractValue.t; addr_callee: AbstractValue.t; addr_callee': AbstractValue.t}
        (** raised when the precondition and the current state disagree on the aliasing, i.e. some
            addresses [callee_addr] and [callee_addr'] that are distinct in the pre are aliased to
            a single address [caller_addr] in the caller's current state. Typically raised when
            calling [foo(z,z)] where the spec for [foo(x,y)] says that [x] and [y] are disjoint. *)
    | Arithmetic of
        { addr_caller: AbstractValue.t
        ; addr_callee: AbstractValue.t
        ; arith_caller: Arithmetic.t option
        ; arith_callee: Arithmetic.t option }
        (** raised when the pre asserts arithmetic facts that are demonstrably false in the caller
            state *)

  let pp_cannot_apply_pre fmt = function
    | Aliasing {addr_caller; addr_callee; addr_callee'} ->
        F.fprintf fmt "address %a in caller already bound to %a, not %a" AbstractValue.pp
          addr_caller AbstractValue.pp addr_callee' AbstractValue.pp addr_callee
    | Arithmetic {addr_caller; addr_callee; arith_caller; arith_callee} ->
        F.fprintf fmt "caller addr %a%a but callee addr %a%a; %a=%a is unsatisfiable"
          AbstractValue.pp addr_caller (Pp.option Arithmetic.pp) arith_caller AbstractValue.pp
          addr_callee (Pp.option Arithmetic.pp) arith_callee AbstractValue.pp addr_caller
          AbstractValue.pp addr_callee


  exception CannotApplyPre of cannot_apply_pre

  (** stuff we carry around when computing the result of applying one pre/post pair *)
  type call_state =
    { astate: t  (** caller's abstract state computed so far *)
    ; subst: (AbstractValue.t * ValueHistory.t) AddressMap.t
          (** translation from callee addresses to caller addresses and their caller histories *)
    ; rev_subst: AbstractValue.t AddressMap.t
          (** the inverse translation from [subst] from caller addresses to callee addresses *)
    ; visited: AddressSet.t
          (** set of callee addresses that have been visited already

               NOTE: this is not always equal to the domain of [rev_subst]: when applying the post
               we visit each subgraph from each formal independently so we reset [visited] between
              the visit of each formal *)
    }

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
        raise (CannotApplyPre (Aliasing {addr_caller; addr_callee; addr_callee'}))
    | _ ->
        () ) ;
    if AddressSet.mem addr_callee call_state.visited then (`AlreadyVisited, call_state)
    else
      ( `NotAlreadyVisited
      , { call_state with
          visited= AddressSet.add addr_callee call_state.visited
        ; subst= AddressMap.add addr_callee addr_hist_caller call_state.subst
        ; rev_subst= AddressMap.add addr_caller addr_callee call_state.rev_subst } )


  let pp f {pre; post} =
    F.fprintf f "PRE:@\n  @[%a@]@\n" BaseDomain.pp (pre :> BaseDomain.t) ;
    F.fprintf f "POST:@\n  @[%a@]@\n" BaseDomain.pp (post :> BaseDomain.t)


  (* {3 reading the pre from the current state} *)

  let add_call_to_trace proc_name call_location caller_history in_call =
    Trace.ViaCall {f= Call proc_name; location= call_location; history= caller_history; in_call}


  let solve_arithmetic_constraints callee_proc_name call_location ~addr_pre ~attrs_pre
      ~addr_hist_caller call_state =
    match Attributes.get_arithmetic attrs_pre with
    | None ->
        call_state
    | Some (arith_callee, arith_callee_hist) -> (
        let addr_caller, hist_caller = addr_hist_caller in
        let astate = call_state.astate in
        let arith_caller_opt = Memory.get_arithmetic addr_caller astate |> Option.map ~f:fst in
        (* TODO: we don't use [abduced_callee] but we could probably use it to refine the attributes
           for that address in the post since abstract values are immutable *)
        match
          Arithmetic.abduce_binop_is_true ~negated:false Eq arith_caller_opt (Some arith_callee)
        with
        | Unsatisfiable ->
            raise
              (CannotApplyPre
                 (Arithmetic
                    { addr_caller
                    ; addr_callee= addr_pre
                    ; arith_caller= arith_caller_opt
                    ; arith_callee= Some arith_callee }))
        | Satisfiable (abduce_caller, _abduce_callee) ->
            let new_astate =
              Option.fold abduce_caller ~init:astate ~f:(fun astate abduce_caller ->
                  let attribute =
                    Attribute.Arithmetic
                      ( abduce_caller
                      , add_call_to_trace callee_proc_name call_location hist_caller
                          arith_callee_hist )
                  in
                  Memory.abduce_attribute addr_caller attribute astate
                  |> Memory.add_attribute addr_caller attribute )
            in
            {call_state with astate= new_astate} )


  (** Materialize the (abstract memory) subgraph of [pre] reachable from [addr_pre] in
     [call_state.astate] starting from address [addr_caller]. Report an error if some invalid
     addresses are traversed in the process. *)
  let rec materialize_pre_from_address callee_proc_name call_location ~pre ~addr_pre
      ~addr_hist_caller call_state =
    match visit call_state ~addr_callee:addr_pre ~addr_hist_caller with
    | `AlreadyVisited, call_state ->
        Ok call_state
    | `NotAlreadyVisited, call_state -> (
      match BaseMemory.find_opt addr_pre pre.BaseDomain.heap with
      | None ->
          Ok call_state
      | Some (edges_pre, attrs_pre) ->
          let call_state =
            solve_arithmetic_constraints callee_proc_name call_location ~addr_pre ~attrs_pre
              ~addr_hist_caller call_state
          in
          Container.fold_result
            ~fold:(IContainer.fold_of_pervasives_map_fold ~fold:Memory.Edges.fold)
            ~init:call_state edges_pre ~f:(fun call_state (access, (addr_pre_dest, _)) ->
              let astate, addr_hist_dest_caller =
                Memory.eval_edge addr_hist_caller access call_state.astate
              in
              let call_state = {call_state with astate} in
              materialize_pre_from_address callee_proc_name call_location ~pre
                ~addr_pre:addr_pre_dest ~addr_hist_caller:addr_hist_dest_caller call_state ) )


  (** materialize subgraph of [pre] rooted at the address represented by a [formal] parameter that
      has been instantiated with the corresponding [actual] into the current state
      [call_state.astate] *)
  let materialize_pre_from_actual callee_proc_name call_location ~pre ~formal ~actual call_state =
    L.d_printfln "Materializing PRE from [%a <- %a]" Var.pp formal AbstractValue.pp (fst actual) ;
    (let open Option.Monad_infix in
    BaseStack.find_opt formal pre.BaseDomain.stack
    >>= fun (addr_formal_pre, _) ->
    BaseMemory.find_edge_opt addr_formal_pre Dereference pre.BaseDomain.heap
    >>| fun (formal_pre, _) ->
    materialize_pre_from_address callee_proc_name call_location ~pre ~addr_pre:formal_pre
      ~addr_hist_caller:actual call_state)
    |> function Some result -> result | None -> Ok call_state


  let is_cell_read_only ~cell_pre_opt ~cell_post:(edges_post, attrs_post) =
    match cell_pre_opt with
    | None ->
        false
    | Some (edges_pre, _) when not (Attributes.is_modified attrs_post) ->
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
        L.d_printfln "ERROR: formals have length %d but actuals have length %d"
          (List.length formals) (List.length actuals) ;
        None
    | Ok result ->
        Some result


  let materialize_pre_for_globals callee_proc_name call_location pre_post call_state =
    fold_globals_of_stack call_location (pre_post.pre :> BaseDomain.t).stack call_state
      ~f:(fun _var ~stack_value:(addr_pre, _) ~addr_hist_caller call_state ->
        materialize_pre_from_address callee_proc_name call_location
          ~pre:(pre_post.pre :> BaseDomain.t)
          ~addr_pre ~addr_hist_caller call_state )


  let materialize_pre callee_proc_name call_location pre_post ~formals ~actuals call_state =
    PerfEvent.(log (fun logger -> log_begin_event logger ~name:"pulse call pre" ())) ;
    let r =
      materialize_pre_for_parameters callee_proc_name call_location pre_post ~formals ~actuals
        call_state
      |> Option.map
           ~f:
             (Result.bind ~f:(fun call_state ->
                  materialize_pre_for_globals callee_proc_name call_location pre_post call_state ))
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


  let delete_edges_in_callee_pre_from_caller ~addr_callee:_ ~cell_pre_opt ~addr_caller call_state =
    match BaseMemory.find_edges_opt addr_caller (call_state.astate.post :> base_domain).heap with
    | None ->
        BaseMemory.Edges.empty
    | Some old_post_edges -> (
      match cell_pre_opt with
      | None ->
          old_post_edges
      | Some (edges_pre, _) ->
          BaseMemory.Edges.merge
            (fun _access old_opt pre_opt ->
              (* TODO: should apply [call_state.subst] to [_access]! Actually, should rewrite the
                 whole [cell_pre] beforehand so that [Edges.merge] makes sense. *)
              if Option.is_some pre_opt then
                (* delete edge if some edge for the same access exists in the pre *)
                None
              else (* keep old edge if it exists *) old_opt )
            old_post_edges edges_pre )


  let add_call_to_attr proc_name call_location caller_history attr =
    match (attr : Attribute.t) with
    | Invalid (invalidation, trace) ->
        Attribute.Invalid
          (invalidation, add_call_to_trace proc_name call_location caller_history trace)
    | Arithmetic (arith, trace) ->
        Attribute.Arithmetic (arith, add_call_to_trace proc_name call_location caller_history trace)
    | AddressOfCppTemporary (_, _)
    | AddressOfStackVariable (_, _, _)
    | Closure _
    | MustBeValid _
    | StdVectorReserve
    | WrittenTo _ ->
        attr


  let record_post_cell callee_proc_name call_loc ~addr_callee ~cell_pre_opt
      ~cell_post:(edges_post, attrs_post) ~addr_hist_caller:(addr_caller, hist_caller) call_state =
    let post_edges_minus_pre =
      delete_edges_in_callee_pre_from_caller ~addr_callee ~cell_pre_opt ~addr_caller call_state
    in
    let heap = (call_state.astate.post :> base_domain).heap in
    let heap =
      let attrs_post_caller =
        Attributes.map attrs_post ~f:(fun attr ->
            add_call_to_attr callee_proc_name call_loc hist_caller attr )
      in
      BaseMemory.set_attrs addr_caller attrs_post_caller heap
    in
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
      let written_to =
        let open Option.Monad_infix in
        BaseMemory.find_opt addr_caller heap
        >>= (fun (_edges, attrs) -> Attributes.get_written_to attrs)
        |> fun written_to_callee_opt ->
        let callee_trace =
          match written_to_callee_opt with
          | None ->
              Trace.Immediate {location= call_loc; history= []}
          | Some access_trace ->
              access_trace
        in
        Attribute.WrittenTo
          (ViaCall
             { in_call= callee_trace
             ; f= Call callee_proc_name
             ; location= call_loc
             ; history= hist_caller })
      in
      BaseMemory.set_edges addr_caller edges_post_caller heap
      |> BaseMemory.add_attribute addr_caller written_to
    in
    let caller_post = Domain.make (call_state.astate.post :> base_domain).stack heap in
    {call_state with subst; astate= {call_state.astate with post= caller_post}}


  let rec record_post_for_address callee_proc_name call_loc ({pre; post} as pre_post) ~addr_callee
      ~addr_hist_caller call_state =
    L.d_printfln "%a<->%a" AbstractValue.pp addr_callee AbstractValue.pp (fst addr_hist_caller) ;
    match visit call_state ~addr_callee ~addr_hist_caller with
    | `AlreadyVisited, call_state ->
        call_state
    | `NotAlreadyVisited, call_state -> (
      match BaseMemory.find_opt addr_callee (post :> BaseDomain.t).BaseDomain.heap with
      | None ->
          call_state
      | Some ((edges_post, _attrs_post) as cell_post) ->
          let cell_pre_opt =
            BaseMemory.find_opt addr_callee (pre :> BaseDomain.t).BaseDomain.heap
          in
          let call_state_after_post =
            if is_cell_read_only ~cell_pre_opt ~cell_post then call_state
            else
              record_post_cell callee_proc_name call_loc ~addr_callee ~cell_pre_opt
                ~addr_hist_caller ~cell_post call_state
          in
          IContainer.fold_of_pervasives_map_fold ~fold:Memory.Edges.fold ~init:call_state_after_post
            edges_post ~f:(fun call_state (_access, (addr_callee_dest, _)) ->
              let call_state, addr_hist_curr_dest =
                call_state_subst_find_or_new call_state addr_callee_dest
                  ~default_hist_caller:(snd addr_hist_caller)
              in
              record_post_for_address callee_proc_name call_loc pre_post
                ~addr_callee:addr_callee_dest ~addr_hist_caller:addr_hist_curr_dest call_state ) )


  let record_post_for_actual callee_proc_name call_loc pre_post ~formal ~actual call_state =
    L.d_printfln_escaped "Recording POST from [%a] <-> %a" Var.pp formal AbstractValue.pp
      (fst actual) ;
    match
      let open Option.Monad_infix in
      BaseStack.find_opt formal (pre_post.pre :> BaseDomain.t).BaseDomain.stack
      >>= fun (addr_formal_pre, _) ->
      BaseMemory.find_edge_opt addr_formal_pre Dereference
        (pre_post.pre :> BaseDomain.t).BaseDomain.heap
      >>| fun (formal_pre, _) ->
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


  let apply_post_for_parameters callee_proc_name call_location pre_post ~formals ~actuals call_state
      =
    (* for each [(formal_i, actual_i)] pair, do [post_i = post union subst(graph reachable from
       formal_i in post)], deleting previous info when comparing pre and post shows a difference
       (TODO: record in the pre when a location is written to instead of just comparing values
       between pre and post since it's unreliable, eg replace value read in pre with same value in
       post but nuke other fields in the meantime? is that possible?). *)
    match
      List.fold2 formals actuals ~init:call_state ~f:(fun call_state formal (actual, _) ->
          record_post_for_actual callee_proc_name call_location pre_post ~formal ~actual call_state
      )
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
    let heap0 = (call_state.astate.post :> base_domain).heap in
    let heap =
      BaseMemory.fold_attrs
        (fun addr_callee attrs heap ->
          if AddressSet.mem addr_callee call_state.visited then
            (* already recorded the attributes when we were walking the edges map *) heap
          else
            match AddressMap.find_opt addr_callee call_state.subst with
            | None ->
                (* callee address has no meaning for the caller *) heap
            | Some (addr_caller, history) ->
                let attrs' =
                  Attributes.map
                    ~f:(fun attr -> add_call_to_attr callee_proc_name call_loc history attr)
                    attrs
                in
                BaseMemory.set_attrs addr_caller attrs' heap )
        (pre_post.post :> BaseDomain.t).heap heap0
    in
    if phys_equal heap heap0 then call_state
    else
      let post = Domain.make (call_state.astate.post :> BaseDomain.t).stack heap in
      {call_state with astate= {call_state.astate with post}}


  let apply_post callee_proc_name call_location pre_post ~formals ~actuals call_state =
    PerfEvent.(log (fun logger -> log_begin_event logger ~name:"pulse call post" ())) ;
    let r =
      apply_post_for_parameters callee_proc_name call_location pre_post ~formals ~actuals call_state
      |> apply_post_for_globals callee_proc_name call_location pre_post
      |> record_post_for_return callee_proc_name call_location pre_post
      |> fun (call_state, return_caller) ->
      ( record_post_remaining_attributes callee_proc_name call_location pre_post call_state
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
          match BaseMemory.get_must_be_valid addr_pre (pre :> BaseDomain.t).heap with
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
              Memory.check_valid access_trace addr_caller astate
              |> Result.map_error ~f:(fun (invalidation, invalidation_trace) ->
                     L.d_printfln "ERROR: caller's %a invalid!" AbstractValue.pp addr_caller ;
                     Diagnostic.AccessToInvalidAddress
                       {invalidation; invalidation_trace; access_trace} ) ) )
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
    L.d_printfln "Applying pre/post for %a(%a):@\n%a" Typ.Procname.pp callee_proc_name
      (Pp.seq ~sep:"," Var.pp) formals pp pre_post ;
    let empty_call_state =
      {astate; subst= AddressMap.empty; rev_subst= AddressMap.empty; visited= AddressSet.empty}
    in
    (* read the precondition *)
    match
      materialize_pre callee_proc_name call_location pre_post ~formals ~actuals empty_call_state
    with
    | exception CannotApplyPre reason ->
        (* can't make sense of the pre-condition in the current context: give up on that particular
           pre/post pair *)
        L.d_printfln "Cannot apply precondition: %a" pp_cannot_apply_pre reason ;
        Ok None
    | None ->
        (* couldn't apply the pre for some technical reason (as in: not by the fault of the
           programmer as far as we know) *)
        Ok (Some (astate, None))
    | Some (Error _ as error) ->
        (* error: the function call requires to read some state known to be invalid *)
        error
    | Some (Ok call_state) ->
        L.d_printfln "Pre applied successfully. call_state=%a" pp_call_state call_state ;
        let open Result.Monad_infix in
        check_all_valid callee_proc_name call_location pre_post call_state
        >>= fun astate ->
        (* reset [visited] *)
        let call_state = {call_state with astate; visited= AddressSet.empty} in
        (* apply the postcondition *)
        Ok (Some (apply_post callee_proc_name call_location pre_post ~formals ~actuals call_state))
end

let extract_pre {pre} = (pre :> BaseDomain.t)

let extract_post {post} = (post :> BaseDomain.t)
