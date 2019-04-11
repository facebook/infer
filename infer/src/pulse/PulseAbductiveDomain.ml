(*
 * Copyright (c) 2019-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format
module L = Logging
module AbstractAddress = PulseDomain.AbstractAddress
module Attributes = PulseDomain.Attributes
module BaseStack = PulseDomain.Stack
module BaseMemory = PulseDomain.Memory

(** signature common to the "normal" [Domain], representing the post at the current program point,
    and the inverted [InvertedDomain], representing the inferred pre-condition*)
module type BaseDomain = sig
  (* private because the lattice is not the same for preconditions and postconditions so we don't
     want to confuse them *)
  type t = private PulseDomain.t

  val empty : t

  val make : BaseStack.t -> BaseMemory.t -> t

  val update : ?stack:BaseStack.t -> ?heap:BaseMemory.t -> t -> t

  include AbstractDomain.NoJoin with type t := t
end

(* just to expose the [heap] and [stack] record field names without having to type
   [PulseDomain.heap] *)
type base_domain = PulseDomain.t = {heap: BaseMemory.t; stack: BaseStack.t}

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
  include PulseDomain
end

(** represents the inferred pre-condition at each program point, biabduction style *)
module InvertedDomain : BaseDomain = struct
  include BaseDomainCommon

  type t = PulseDomain.t

  let empty = PulseDomain.empty

  let pp = PulseDomain.pp

  (** inverted lattice *)
  let ( <= ) ~lhs ~rhs = PulseDomain.( <= ) ~rhs:lhs ~lhs:rhs
end

(** biabduction-style pre/post state *)
type t =
  { post: Domain.t  (** state at the current program point*)
  ; pre: InvertedDomain.t  (** inferred pre at the current program point *) }

let pp f {post; pre} = F.fprintf f "@[<v>%a@;[%a]@]" Domain.pp post InvertedDomain.pp pre

let ( <= ) ~lhs ~rhs =
  match
    PulseDomain.isograph_map PulseDomain.empty_mapping
      ~lhs:(rhs.pre :> PulseDomain.t)
      ~rhs:(lhs.pre :> PulseDomain.t)
  with
  | NotIsomorphic ->
      false
  | IsomorphicUpTo foot_mapping ->
      PulseDomain.is_isograph foot_mapping
        ~lhs:(lhs.post :> PulseDomain.t)
        ~rhs:(rhs.post :> PulseDomain.t)


module Stack = struct
  let is_abducible _var =
    (* TODO: need to keep only formals + return variable + globals in the pre *) true


  (** [astate] with [astate.post.stack = f astate.post.stack] *)
  let map_post_stack ~f astate =
    let new_post = Domain.update astate.post ~stack:(f (astate.post :> base_domain).stack) in
    if phys_equal new_post astate.post then astate else {astate with post= new_post}


  let materialize var astate =
    match BaseStack.find_opt var (astate.post :> base_domain).stack with
    | Some addr_loc_opt ->
        (astate, addr_loc_opt)
    | None ->
        let addr_loc_opt' = (AbstractAddress.mk_fresh (), None) in
        let post_stack = BaseStack.add var addr_loc_opt' (astate.post :> base_domain).stack in
        let pre =
          if is_abducible var then
            let foot_stack = BaseStack.add var addr_loc_opt' (astate.pre :> base_domain).stack in
            let foot_heap =
              BaseMemory.register_address (fst addr_loc_opt') (astate.pre :> base_domain).heap
            in
            InvertedDomain.make foot_stack foot_heap
          else astate.pre
        in
        ({post= Domain.update astate.post ~stack:post_stack; pre}, addr_loc_opt')


  let add var addr_loc_opt astate =
    map_post_stack astate ~f:(fun stack -> BaseStack.add var addr_loc_opt stack)


  let remove_vars vars astate =
    map_post_stack astate ~f:(fun stack ->
        BaseStack.filter (fun var _ -> not (List.mem ~equal:Var.equal vars var)) stack )


  let fold f astate accum = BaseStack.fold f (astate.post :> base_domain).stack accum

  let find_opt var astate = BaseStack.find_opt var (astate.post :> base_domain).stack
end

module Memory = struct
  open Result.Monad_infix
  module Access = BaseMemory.Access

  (** [astate] with [astate.post.heap = f astate.post.heap] *)
  let map_post_heap ~f astate =
    let new_post = Domain.update astate.post ~heap:(f (astate.post :> base_domain).heap) in
    if phys_equal new_post astate.post then astate else {astate with post= new_post}


  (** if [address] is in [pre] and it should be valid then that fact goes in the precondition *)
  let record_must_be_valid action address (pre : InvertedDomain.t) =
    if BaseMemory.mem_edges address (pre :> base_domain).heap then
      InvertedDomain.update pre
        ~heap:
          (BaseMemory.add_attributes address
             (Attributes.singleton (MustBeValid action))
             (pre :> base_domain).heap)
    else pre


  let check_valid action addr ({post; pre} as astate) =
    BaseMemory.check_valid addr (post :> base_domain).heap
    >>| fun () ->
    let new_pre = record_must_be_valid action addr pre in
    if phys_equal new_pre pre then astate else {astate with pre= new_pre}


  let add_edge addr access new_addr_trace astate =
    map_post_heap astate ~f:(fun heap -> BaseMemory.add_edge addr access new_addr_trace heap)


  let materialize_edge addr access astate =
    match BaseMemory.find_edge_opt addr access (astate.post :> base_domain).heap with
    | Some addr_trace' ->
        (astate, addr_trace')
    | None ->
        let addr_trace' = (AbstractAddress.mk_fresh (), []) in
        let post_heap =
          BaseMemory.add_edge addr access addr_trace' (astate.post :> base_domain).heap
        in
        let foot_heap =
          if BaseMemory.mem_edges addr (astate.pre :> base_domain).heap then
            BaseMemory.add_edge addr access addr_trace' (astate.pre :> base_domain).heap
            |> BaseMemory.register_address (fst addr_trace')
          else (astate.pre :> base_domain).heap
        in
        ( { post= Domain.update astate.post ~heap:post_heap
          ; pre= InvertedDomain.update astate.pre ~heap:foot_heap }
        , addr_trace' )


  let invalidate address action astate =
    map_post_heap astate ~f:(fun heap -> BaseMemory.invalidate address action heap)


  let add_attributes address attributes astate =
    map_post_heap astate ~f:(fun heap -> BaseMemory.add_attributes address attributes heap)


  let std_vector_reserve addr astate =
    map_post_heap astate ~f:(fun heap -> BaseMemory.std_vector_reserve addr heap)


  let is_std_vector_reserved addr astate =
    BaseMemory.is_std_vector_reserved addr (astate.post :> base_domain).heap


  let find_opt address astate = BaseMemory.find_opt address (astate.post :> base_domain).heap

  let set_cell addr cell astate =
    map_post_heap astate ~f:(fun heap -> BaseMemory.set_cell addr cell heap)


  module Edges = BaseMemory.Edges
end

let empty = {post= Domain.empty; pre= InvertedDomain.empty}

let discard_unreachable ({pre; post} as astate) =
  let pre_addresses = PulseDomain.visit (pre :> PulseDomain.t) in
  let pre_old_heap = (pre :> PulseDomain.t).heap in
  let pre_new_heap =
    PulseDomain.Memory.filter
      (fun address -> PulseDomain.AbstractAddressSet.mem address pre_addresses)
      pre_old_heap
  in
  let post_addresses = PulseDomain.visit (post :> PulseDomain.t) in
  let all_addresses = PulseDomain.AbstractAddressSet.union pre_addresses post_addresses in
  let post_old_heap = (post :> PulseDomain.t).heap in
  let post_new_heap =
    PulseDomain.Memory.filter
      (fun address -> PulseDomain.AbstractAddressSet.mem address all_addresses)
      post_old_heap
  in
  if phys_equal pre_new_heap pre_old_heap && phys_equal post_new_heap post_old_heap then astate
  else
    { pre= InvertedDomain.make (pre :> PulseDomain.t).stack pre_new_heap
    ; post= Domain.make (post :> PulseDomain.t).stack post_new_heap }


module PrePost = struct
  type domain_t = t

  type t = domain_t

  let of_post astate = discard_unreachable astate

  (* machinery to apply a pre/post pair corresponding to a function's summary in a function call to
     the current state *)

  module AddressSet = PulseDomain.AbstractAddressSet
  module AddressMap = PulseDomain.AbstractAddressMap

  (** raised when the pre/post pair and the current state disagree on the aliasing, i.e. some
     addresses that are distinct in the pre/post are aliased in the current state. Typically raised
     when calling [foo(z,z)] where the spec for [foo(x,y)] says that [x] and [y] are disjoint. *)
  exception Aliasing

  (** stuff we carry around when computing the result of applying one pre/post pair *)
  type call_state =
    { astate: t  (** caller's abstract state computed so far *)
    ; subst: AbstractAddress.t AddressMap.t
          (** translation from callee addresses to caller addresses *)
    ; rev_subst: AbstractAddress.t AddressMap.t
          (** the inverse translation from [subst] from caller addresses to callee addresses *)
    ; visited: AddressSet.t
          (** set of callee addresses that have been visited already

               NOTE: this is not always equal to the domain of [rev_subst]: when applying the post
               we visit each subgraph from each formal independently so we reset [visited] between
              the visit of each formal *)
    }

  let pp_call_state fmt {astate; subst; rev_subst; visited} =
    F.fprintf fmt
      "@[<v>{ astate=@[<hv2>%a@];@, subst=@[<hv2>%a@];@, rev_subst=@[<hv2>%a@];@, \
       visited=@[<hv2>%a@]@, }@]"
      pp astate
      (AddressMap.pp ~pp_value:AbstractAddress.pp)
      subst
      (AddressMap.pp ~pp_value:AbstractAddress.pp)
      rev_subst AddressSet.pp visited


  let visit call_state ~addr_callee ~addr_caller =
    ( match AddressMap.find_opt addr_caller call_state.rev_subst with
    | Some addr_callee' when not (AbstractAddress.equal addr_callee addr_callee') ->
        L.d_printfln "Huho, address %a in post already bound to %a, not %a@\nState=%a"
          AbstractAddress.pp addr_caller AbstractAddress.pp addr_callee' AbstractAddress.pp
          addr_callee pp_call_state call_state ;
        raise Aliasing
    | _ ->
        () ) ;
    if AddressSet.mem addr_callee call_state.visited then (`AlreadyVisited, call_state)
    else
      ( `NotAlreadyVisited
      , { call_state with
          visited= AddressSet.add addr_callee call_state.visited
        ; subst= AddressMap.add addr_callee addr_caller call_state.subst
        ; rev_subst= AddressMap.add addr_caller addr_callee call_state.rev_subst } )


  let pp f {pre; post} =
    F.fprintf f "PRE:@\n  @[%a@]@\n" PulseDomain.pp (pre :> PulseDomain.t) ;
    F.fprintf f "POST:@\n  @[%a@]@\n" PulseDomain.pp (post :> PulseDomain.t)


  (* reading the pre from the current state *)

  (** Materialize the (abstract memory) subgraph of [pre] reachable from [addr_pre] in
     [call_state.astate] starting from address [addr_caller]. Report an error if some invalid
     addresses are traversed in the process. *)
  let rec materialize_pre_from_address callee_proc_name call_location ~pre ~addr_pre ~addr_caller
      trace call_state =
    let mk_action action =
      PulseTrace.ViaCall {action; proc_name= callee_proc_name; location= call_location}
    in
    match visit call_state ~addr_callee:addr_pre ~addr_caller with
    | `AlreadyVisited, call_state ->
        Ok call_state
    | `NotAlreadyVisited, call_state -> (
        (let open Option.Monad_infix in
        PulseDomain.Memory.find_opt addr_pre pre.PulseDomain.heap
        >>= fun (edges_pre, attrs_pre) ->
        PulseDomain.Attributes.get_must_be_valid attrs_pre
        >>| fun callee_action ->
        let action = mk_action callee_action in
        match Memory.check_valid action addr_caller call_state.astate with
        | Error invalidated_by ->
            Error
              (PulseDiagnostic.AccessToInvalidAddress {invalidated_by; accessed_by= action; trace})
        | Ok astate ->
            let call_state = {call_state with astate} in
            Container.fold_result
              ~fold:(IContainer.fold_of_pervasives_map_fold ~fold:Memory.Edges.fold)
              ~init:call_state edges_pre ~f:(fun call_state (access, (addr_pre_dest, _)) ->
                let astate, (addr_caller_dest, _) =
                  Memory.materialize_edge addr_caller access call_state.astate
                in
                let call_state = {call_state with astate} in
                materialize_pre_from_address callee_proc_name call_location ~pre
                  ~addr_pre:addr_pre_dest ~addr_caller:addr_caller_dest trace call_state ))
        |> function Some result -> result | None -> Ok call_state )


  (** materialize subgraph of [pre] rooted at the address represented by a [formal] parameter that
      has been instantiated with the corresponding [actual] into the current state
      [call_state.astate] *)
  let materialize_pre_from_actual callee_proc_name call_location ~pre ~formal ~actual call_state =
    L.d_printfln "Materializing PRE from [%a <- %a]" Var.pp formal (Pp.option AbstractAddress.pp)
      (Option.map ~f:fst actual) ;
    match actual with
    | None ->
        (* the expression representing the actual couldn't be evaluated down to an abstract address
           *)
        Ok call_state
    | Some (addr_caller, trace) -> (
        (let open Option.Monad_infix in
        PulseDomain.Stack.find_opt formal pre.PulseDomain.stack
        >>= fun (addr_formal_pre, _) ->
        PulseDomain.Memory.find_edge_opt addr_formal_pre Dereference pre.PulseDomain.heap
        >>| fun (formal_pre, _) ->
        materialize_pre_from_address callee_proc_name call_location ~pre ~addr_pre:formal_pre
          ~addr_caller trace call_state)
        |> function Some result -> result | None -> Ok call_state )


  let is_cell_read_only ~cell_pre_opt ~cell_post:(edges_post, attrs_post) =
    match cell_pre_opt with
    | None ->
        false
    | Some (edges_pre, _) ->
        Attributes.is_empty attrs_post
        && PulseDomain.Memory.Edges.equal
             (fun (addr_dest_pre, _) (addr_dest_post, _) ->
               (* NOTE: ignores traces

                  TODO: can the traces be leveraged here? maybe easy to detect writes by looking at
                  the post trace *)
               AbstractAddress.equal addr_dest_pre addr_dest_post )
             edges_pre edges_post


  let materialize_pre callee_proc_name call_location pre_post ~formals ~actuals call_state =
    (* For each [(formal, actual)] pair, resolve them to addresses in their respective states then
       call [materialize_pre_from] on them.  Give up if calling the function introduces aliasing.
       *)
    match
      PerfEvent.(log (fun logger -> log_begin_event logger ~name:"pulse call pre" ())) ;
      let r =
        IList.fold2_result formals actuals ~init:call_state ~f:(fun call_state formal actual ->
            materialize_pre_from_actual callee_proc_name call_location
              ~pre:(pre_post.pre :> PulseDomain.t)
              ~formal ~actual call_state )
      in
      PerfEvent.(log (fun logger -> log_end_event logger ())) ;
      r
    with
    | Ok result ->
        Some result
    | Unequal_lengths ->
        L.d_printfln "ERROR: formals have length %d but actuals have length %d"
          (List.length formals) (List.length actuals) ;
        None
    | exception Aliasing ->
        (* can't make sense of the pre-condition in the current context: give up on that particular
           pre/post pair *)
        None


  (* applying the post to the current state *)

  let subst_find_or_new subst addr_callee =
    match AddressMap.find_opt addr_callee subst with
    | None ->
        let addr_fresh = AbstractAddress.mk_fresh () in
        (addr_fresh, AddressMap.add addr_callee addr_fresh subst)
    | Some addr_caller ->
        (addr_caller, subst)


  let call_state_subst_find_or_new call_state addr_callee =
    let addr_caller, new_subst = subst_find_or_new call_state.subst addr_callee in
    if phys_equal new_subst call_state.subst then (call_state, addr_caller)
    else ({call_state with subst= new_subst}, addr_caller)


  let delete_edges_in_callee_pre_from_caller ~addr_callee:_ ~cell_pre_opt ~addr_caller call_state =
    match
      PulseDomain.Memory.find_edges_opt addr_caller (call_state.astate.post :> base_domain).heap
    with
    | None ->
        PulseDomain.Memory.Edges.empty
    | Some old_post_edges -> (
      match cell_pre_opt with
      | None ->
          old_post_edges
      | Some (edges_pre, _) ->
          PulseDomain.Memory.Edges.merge
            (fun _access old_opt pre_opt ->
              (* TODO: should apply [call_state.subst] to [_access]! Actually, should rewrite the
                whole [cell_pre] beforehand so that [Edges.merge] makes sense. *)
              if Option.is_some pre_opt then
                (* delete edge if some edge for the same access exists in the pre *)
                None
              else (* keep old edge if it exists *) old_opt )
            old_post_edges edges_pre )


  let add_call_to_attr proc_name location attr =
    match (attr : PulseDomain.Attribute.t) with
    | Invalid action ->
        PulseDomain.Attribute.Invalid (PulseTrace.ViaCall {action; proc_name; location})
    | MustBeValid _ | AddressOfCppTemporary (_, _) | Closure _ | StdVectorReserve ->
        attr


  let rec record_post_for_address callee_proc_name call_loc ({pre; post} as pre_post) ~addr_callee
      ~addr_caller call_state =
    L.d_printfln "%a<->%a" AbstractAddress.pp addr_callee AbstractAddress.pp addr_caller ;
    match visit call_state ~addr_callee ~addr_caller with
    | `AlreadyVisited, call_state ->
        call_state
    | `NotAlreadyVisited, call_state -> (
      match PulseDomain.Memory.find_opt addr_callee (post :> PulseDomain.t).PulseDomain.heap with
      | None ->
          call_state
      | Some ((edges_post, _attrs_post) as cell_post) ->
          let cell_pre_opt =
            PulseDomain.Memory.find_opt addr_callee (pre :> PulseDomain.t).PulseDomain.heap
          in
          let call_state_after_post =
            if is_cell_read_only ~cell_pre_opt ~cell_post then call_state
            else
              record_post_cell callee_proc_name call_loc ~addr_callee ~cell_pre_opt ~addr_caller
                ~cell_post call_state
          in
          IContainer.fold_of_pervasives_map_fold ~fold:Memory.Edges.fold
            ~init:call_state_after_post edges_post
            ~f:(fun call_state (_access, (addr_callee_dest, _)) ->
              let call_state, addr_curr_dest =
                call_state_subst_find_or_new call_state addr_callee_dest
              in
              record_post_for_address callee_proc_name call_loc pre_post
                ~addr_callee:addr_callee_dest ~addr_caller:addr_curr_dest call_state ) )


  and record_post_cell callee_proc_name call_loc ~addr_callee ~cell_pre_opt
      ~cell_post:(edges_post, attrs_post) ~addr_caller call_state =
    let post_edges_minus_pre =
      delete_edges_in_callee_pre_from_caller ~addr_callee ~cell_pre_opt ~addr_caller call_state
    in
    let new_attrs =
      let attrs_post =
        Attributes.map ~f:(fun attr -> add_call_to_attr callee_proc_name call_loc attr) attrs_post
      in
      match
        PulseDomain.Memory.find_attrs_opt addr_caller (call_state.astate.post :> base_domain).heap
      with
      | None ->
          attrs_post
      | Some old_attrs_post ->
          Attributes.union_prefer_left old_attrs_post attrs_post
    in
    let subst, translated_post_edges =
      PulseDomain.Memory.Edges.fold_map ~init:call_state.subst edges_post
        ~f:(fun subst (addr_callee, trace_post) ->
          let addr_curr, subst = subst_find_or_new subst addr_callee in
          ( subst
          , ( addr_curr
            , PulseTrace.Call
                {f= `HilCall (Direct callee_proc_name); actuals= [ (* TODO *) ]; location= call_loc}
              :: trace_post ) ) )
    in
    let caller_post =
      let new_edges =
        PulseDomain.Memory.Edges.union
          (fun _ _ post_cell -> Some post_cell)
          post_edges_minus_pre translated_post_edges
      in
      Domain.make (call_state.astate.post :> base_domain).stack
        (PulseDomain.Memory.set_cell addr_caller (new_edges, new_attrs)
           (call_state.astate.post :> base_domain).heap)
    in
    {call_state with subst; astate= {call_state.astate with post= caller_post}}


  let record_post_for_actual callee_proc_name call_loc pre_post ~formal ~actual call_state =
    L.d_printfln_escaped "Recording POST from [%a] <-> %a" Var.pp formal
      (Pp.option AbstractAddress.pp) (Option.map ~f:fst actual) ;
    match actual with
    | None ->
        call_state
    | Some (addr_caller, _trace) -> (
        let open Option.Monad_infix in
        match
          PulseDomain.Stack.find_opt formal (pre_post.pre :> PulseDomain.t).PulseDomain.stack
          >>= fun (addr_formal_pre, _) ->
          PulseDomain.Memory.find_edge_opt addr_formal_pre Dereference
            (pre_post.pre :> PulseDomain.t).PulseDomain.heap
          >>| fun (formal_pre, _) ->
          record_post_for_address callee_proc_name call_loc pre_post ~addr_callee:formal_pre
            ~addr_caller call_state
        with
        | Some call_state ->
            call_state
        | None ->
            call_state )


  let record_post_for_return callee_proc_name call_loc pre_post ~ret call_state =
    let return_var = Var.of_pvar (Pvar.get_ret_pvar callee_proc_name) in
    match
      PulseDomain.Stack.find_opt return_var (pre_post.pre :> PulseDomain.t).PulseDomain.stack
    with
    | Some (addr_return, _) ->
        record_post_for_address callee_proc_name call_loc pre_post ~addr_callee:addr_return
          ~addr_caller:(fst ret) call_state
    | None ->
        call_state


  let apply_post callee_proc_name call_location pre_post ~formals ~ret ~actuals call_state =
    (* reset [visited] *)
    let call_state_pre = {call_state with visited= AddressSet.empty} in
    (* for each [(formal_i, actual_i)] pair, do [post_i = post union subst(graph reachable from
       formal_i in post)], deleting previous info when comparing pre and post shows a difference
       (TODO: record in the pre when a location is written to instead of just comparing values
       between pre and post since it's unreliable, eg replace value read in pre with same value in
       post but nuke other fields in the meantime? is that possible?).  *)
    match
      PerfEvent.(log (fun logger -> log_begin_event logger ~name:"pulse call post" ())) ;
      let r =
        List.fold2 formals actuals ~init:call_state_pre ~f:(fun call_state formal actual ->
            record_post_for_actual callee_proc_name call_location pre_post ~formal ~actual
              call_state )
      in
      PerfEvent.(log (fun logger -> log_end_event logger ())) ;
      r
    with
    | Ok call_state ->
        record_post_for_return callee_proc_name call_location pre_post ~ret call_state
        |> fun {astate} -> Some astate
    | exception Aliasing ->
        None
    | Unequal_lengths ->
        (* should have been checked before by [materialize_pre] *)
        assert false


  (* - read all the pre, assert validity of addresses and materializes *everything* (to throw stuff
       in the *current* pre as appropriate so that callers of the current procedure will also know
       about the deeper reads)

     - for each actual, write the post for that actual

     - if aliasing is introduced at any time then give up

     questions:

     - what if some preconditions raise lifetime issues but others don't? Have to be careful with
     the noise that this will introduce since we don't care about values. For instance, if the pre
     is for a path where [formal != 0] and we pass [0] then it will be an FP. Maybe the solution is
     to bake in some value analysis.  *)
  let apply callee_proc_name call_location pre_post ~formals ~ret ~actuals astate =
    L.d_printfln "Applying pre/post for %a(%a):@\n%a" Typ.Procname.pp callee_proc_name
      (Pp.seq ~sep:"," Var.pp) formals pp pre_post ;
    let empty_call_state =
      {astate; subst= AddressMap.empty; rev_subst= AddressMap.empty; visited= AddressSet.empty}
    in
    (* read the precondition *)
    match
      materialize_pre callee_proc_name call_location pre_post ~formals ~actuals empty_call_state
    with
    | None ->
        (* couldn't apply the pre for some technical reason (as in: not by the fault of the
           programmer as far as we know) *)
        Ok astate
    | Some (Error _ as error) ->
        (* error: the function call requires to read some state known to be invalid *)
        error
    | Some (Ok call_state) -> (
      (* apply the postcondition *)
      match
        apply_post callee_proc_name call_location pre_post ~formals ~ret ~actuals call_state
      with
      | None ->
          (* same as when trying to apply the pre: give up for that pre/post pair and return the
             original state *)
          Ok astate
      | Some astate_post ->
          Ok astate_post )
end
