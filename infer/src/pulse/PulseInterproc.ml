(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

[@@@warning "+9"]

open! IStd
module F = Format
module L = Logging
open PulseBasicInterface
open PulseDomainInterface

(* {2 machinery to apply a pre/post pair corresponding to a function's summary in a function call
   to the current state} *)

module AddressSet = AbstractValue.Set
module AddressMap = AbstractValue.Map

(** stuff we carry around when computing the result of applying one pre/post pair *)
type call_state =
  { astate: AbductiveDomain.t  (** caller's abstract state computed so far *)
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
    \ }@]" AbductiveDomain.pp astate
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
  | FormalActualLength of
      {formals: Var.t list; actuals: ((AbstractValue.t * ValueHistory.t) * Typ.t) list}
  | PathCondition

let pp_contradiction fmt = function
  | Aliasing {addr_caller; addr_callee; addr_callee'; call_state} ->
      F.fprintf fmt
        "address %a in caller already bound to %a, not %a@\nnote: current call state was %a"
        AbstractValue.pp addr_caller AbstractValue.pp addr_callee' AbstractValue.pp addr_callee
        pp_call_state call_state
  | FormalActualLength {formals; actuals} ->
      F.fprintf fmt "formals have length %d but actuals have length %d" (List.length formals)
        (List.length actuals)
  | PathCondition ->
      F.pp_print_string fmt "path condition evaluates to false"


exception Contradiction of contradiction

let fold_globals_of_stack call_loc stack call_state ~f =
  Container.fold_result ~fold:(IContainer.fold_of_pervasives_map_fold BaseStack.fold)
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


let visit call_state ~pre ~addr_callee ~addr_hist_caller =
  let addr_caller = fst addr_hist_caller in
  ( match AddressMap.find_opt addr_caller call_state.rev_subst with
  | Some addr_callee' when not (AbstractValue.equal addr_callee addr_callee') ->
      (* [addr_caller] corresponds to several values in the callee, see if that's a problem for
         applying the pre-condition, i.e. if both values are addresses in the callee's heap, which
         means they must be disjoint. If so, raise a contradiction, but if not then continue as it
         just means that the callee doesn't care about the value of these variables. *)
      if
        BaseMemory.mem addr_callee pre.BaseDomain.heap
        && BaseMemory.mem addr_callee' pre.BaseDomain.heap
      then raise (Contradiction (Aliasing {addr_caller; addr_callee; addr_callee'; call_state}))
  | _ ->
      () ) ;
  if AddressSet.mem addr_callee call_state.visited then (`AlreadyVisited, call_state)
  else
    ( `NotAlreadyVisited
    , { call_state with
        visited= AddressSet.add addr_callee call_state.visited
      ; subst= AddressMap.add addr_callee addr_hist_caller call_state.subst
      ; rev_subst= AddressMap.add addr_caller addr_callee call_state.rev_subst } )


let pp f {AbductiveDomain.pre; post; path_condition; skipped_calls} =
  F.fprintf f "COND:@\n  @[%a@]@\n" PathCondition.pp path_condition ;
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
  match visit call_state ~pre ~addr_callee:addr_pre ~addr_hist_caller with
  | `AlreadyVisited, call_state ->
      Ok call_state
  | `NotAlreadyVisited, call_state -> (
    match BaseMemory.find_opt addr_pre pre.BaseDomain.heap with
    | None ->
        Ok call_state
    | Some edges_pre ->
        Container.fold_result ~fold:Memory.Edges.fold ~init:call_state edges_pre
          ~f:(fun call_state (access, (addr_pre_dest, _)) ->
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


let is_cell_read_only ~edges_pre_opt ~cell_post:(_, attrs_post) =
  match edges_pre_opt with None -> false | Some _ -> not (Attributes.is_modified attrs_post)


let materialize_pre_for_captured_vars callee_proc_name call_location pre_post
    ~captured_vars_with_actuals call_state =
  List.fold_result captured_vars_with_actuals ~init:call_state
    ~f:(fun call_state (formal, actual) ->
      materialize_pre_from_actual callee_proc_name call_location
        ~pre:(pre_post.AbductiveDomain.pre :> BaseDomain.t)
        ~formal ~actual call_state )


let materialize_pre_for_parameters callee_proc_name call_location pre_post ~formals ~actuals
    call_state =
  (* For each [(formal, actual)] pair, resolve them to addresses in their respective states then
     call [materialize_pre_from] on them.  Give up if calling the function introduces aliasing.
  *)
  match
    IList.fold2_result formals actuals ~init:call_state ~f:(fun call_state formal (actual, _) ->
        materialize_pre_from_actual callee_proc_name call_location
          ~pre:(pre_post.AbductiveDomain.pre :> BaseDomain.t)
          ~formal ~actual call_state )
  with
  | Unequal_lengths ->
      raise (Contradiction (FormalActualLength {formals; actuals}))
  | Ok result ->
      result


let materialize_pre_for_globals callee_proc_name call_location pre_post call_state =
  fold_globals_of_stack call_location (pre_post.AbductiveDomain.pre :> BaseDomain.t).stack
    call_state ~f:(fun _var ~stack_value:(addr_pre, _) ~addr_hist_caller call_state ->
      materialize_pre_from_address callee_proc_name call_location
        ~pre:(pre_post.AbductiveDomain.pre :> BaseDomain.t)
        ~addr_pre ~addr_hist_caller call_state )


let add_call_to_attributes proc_name call_location caller_history attrs =
  Attributes.map attrs ~f:(fun attr ->
      Attribute.map_trace attr ~f:(fun trace ->
          add_call_to_trace proc_name call_location caller_history trace ) )


let subst_find_or_new subst addr_callee ~default_hist_caller =
  match AddressMap.find_opt addr_callee subst with
  | None ->
      let addr_hist_fresh = (AbstractValue.mk_fresh (), default_hist_caller) in
      (AddressMap.add addr_callee addr_hist_fresh subst, addr_hist_fresh)
  | Some addr_hist_caller ->
      (subst, addr_hist_caller)


let conjoin_callee_arith pre_post call_state =
  L.d_printfln "applying callee path condition: (%a)[%a]" PathCondition.pp
    pre_post.AbductiveDomain.path_condition
    (AddressMap.pp ~pp_value:(fun fmt (addr, _) -> AbstractValue.pp fmt addr))
    call_state.subst ;
  let subst, path_condition =
    PathCondition.and_callee call_state.subst call_state.astate.path_condition
      ~callee:pre_post.AbductiveDomain.path_condition
  in
  if PathCondition.is_unsat_cheap path_condition then raise (Contradiction PathCondition)
  else
    let astate = AbductiveDomain.set_path_condition path_condition call_state.astate in
    {call_state with astate; subst}


let apply_arithmetic_constraints callee_proc_name call_location pre_post call_state =
  let one_address_sat callee_attrs (addr_caller, caller_history) call_state =
    let attrs_caller =
      add_call_to_attributes callee_proc_name call_location caller_history callee_attrs
    in
    let astate = AddressAttributes.abduce_and_add addr_caller attrs_caller call_state.astate in
    if phys_equal astate call_state.astate then call_state else {call_state with astate}
  in
  let call_state = conjoin_callee_arith pre_post call_state in
  (* check all callee addresses that make sense for the caller, i.e. the domain of [call_state.subst] *)
  AddressMap.fold
    (fun addr_callee addr_hist_caller call_state ->
      match
        BaseAddressAttributes.find_opt addr_callee
          (pre_post.AbductiveDomain.pre :> BaseDomain.t).attrs
      with
      | None ->
          call_state
      | Some callee_attrs ->
          one_address_sat callee_attrs addr_hist_caller call_state )
    call_state.subst call_state


let materialize_pre callee_proc_name call_location pre_post ~captured_vars_with_actuals ~formals
    ~actuals call_state =
  PerfEvent.(log (fun logger -> log_begin_event logger ~name:"pulse call pre" ())) ;
  let r =
    let open IResult.Let_syntax in
    (* first make as large a mapping as we can between callee values and caller values... *)
    materialize_pre_for_parameters callee_proc_name call_location pre_post ~formals ~actuals
      call_state
    >>= materialize_pre_for_captured_vars callee_proc_name call_location pre_post
          ~captured_vars_with_actuals
    >>= materialize_pre_for_globals callee_proc_name call_location pre_post
    >>| (* ...then relational arithmetic constraints in the callee's attributes will make sense in
           terms of the caller's values *)
    apply_arithmetic_constraints callee_proc_name call_location pre_post
  in
  PerfEvent.(log (fun logger -> log_end_event logger ())) ;
  r


(* {3 applying the post to the current state} *)

let call_state_subst_find_or_new call_state addr_callee ~default_hist_caller =
  let new_subst, addr_hist_caller =
    subst_find_or_new call_state.subst addr_callee ~default_hist_caller
  in
  if phys_equal new_subst call_state.subst then (call_state, addr_hist_caller)
  else ({call_state with subst= new_subst}, addr_hist_caller)


let delete_edges_in_callee_pre_from_caller ~addr_callee:_ ~edges_pre_opt ~addr_caller call_state =
  match
    BaseMemory.find_opt addr_caller (call_state.astate.AbductiveDomain.post :> BaseDomain.t).heap
  with
  | None ->
      BaseMemory.Edges.empty
  | Some old_post_edges -> (
    match edges_pre_opt with
    | None ->
        old_post_edges
    | Some edges_pre ->
        (* TODO: should apply [call_state.subst] to [_access]! Actually, should rewrite the
           whole [cell_pre] beforehand so that [Edges.merge] makes sense. *)
        BaseMemory.Edges.filter old_post_edges ~f:(fun (access, _) ->
            (* delete edge if some edge for the same access exists in the pre *)
            not (BaseMemory.Edges.mem edges_pre access) ) )


let record_post_cell callee_proc_name call_loc ~addr_callee ~edges_pre_opt
    ~cell_callee_post:(edges_callee_post, attrs_callee_post)
    ~addr_hist_caller:(addr_caller, hist_caller) call_state =
  let call_state =
    let attrs_post_caller =
      add_call_to_attributes callee_proc_name call_loc hist_caller attrs_callee_post
    in
    let astate = AddressAttributes.abduce_and_add addr_caller attrs_post_caller call_state.astate in
    {call_state with astate}
  in
  let subst, translated_post_edges =
    BaseMemory.Edges.fold_map ~init:call_state.subst edges_callee_post
      ~f:(fun subst (addr_callee, trace_post) ->
        let subst, (addr_curr, hist_curr) =
          subst_find_or_new subst addr_callee ~default_hist_caller:hist_caller
        in
        ( subst
        , ( addr_curr
          , ValueHistory.Call {f= Call callee_proc_name; location= call_loc; in_call= trace_post}
            :: hist_curr ) ) )
  in
  let astate =
    let post_edges_minus_pre =
      delete_edges_in_callee_pre_from_caller ~addr_callee ~edges_pre_opt ~addr_caller call_state
    in
    let edges_post_caller =
      BaseMemory.Edges.union_left_biased translated_post_edges post_edges_minus_pre
    in
    AbductiveDomain.set_post_edges addr_caller edges_post_caller call_state.astate
  in
  {call_state with subst; astate}


let rec record_post_for_address callee_proc_name call_loc ({AbductiveDomain.pre; _} as pre_post)
    ~addr_callee ~addr_hist_caller call_state =
  L.d_printfln "%a<->%a" AbstractValue.pp addr_callee AbstractValue.pp (fst addr_hist_caller) ;
  match visit call_state ~pre:(pre :> BaseDomain.t) ~addr_callee ~addr_hist_caller with
  | `AlreadyVisited, call_state ->
      call_state
  | `NotAlreadyVisited, call_state -> (
    match AbductiveDomain.find_post_cell_opt addr_callee pre_post with
    | None ->
        call_state
    | Some ((edges_post, _attrs_post) as cell_callee_post) ->
        let edges_pre_opt = BaseMemory.find_opt addr_callee (pre :> BaseDomain.t).BaseDomain.heap in
        let call_state_after_post =
          if is_cell_read_only ~edges_pre_opt ~cell_post:cell_callee_post then call_state
          else
            record_post_cell callee_proc_name call_loc ~addr_callee ~edges_pre_opt ~addr_hist_caller
              ~cell_callee_post call_state
        in
        Memory.Edges.fold ~init:call_state_after_post edges_post
          ~f:(fun call_state (_access, (addr_callee_dest, _)) ->
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
      BaseStack.find_opt formal (pre_post.AbductiveDomain.pre :> BaseDomain.t).BaseDomain.stack
    in
    let+ formal_pre, _ =
      BaseMemory.find_edge_opt addr_formal_pre Dereference
        (pre_post.AbductiveDomain.pre :> BaseDomain.t).BaseDomain.heap
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
  match BaseStack.find_opt return_var (pre_post.AbductiveDomain.post :> BaseDomain.t).stack with
  | None ->
      (call_state, None)
  | Some (addr_return, _) -> (
    match
      BaseMemory.find_edge_opt addr_return Dereference
        (pre_post.AbductiveDomain.post :> BaseDomain.t).BaseDomain.heap
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
        L.d_printfln_escaped "Recording POST from [return] <-> %a" AbstractValue.pp
          (fst return_caller_addr_hist) ;
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


let apply_post_for_captured_vars callee_proc_name call_location pre_post ~captured_vars_with_actuals
    call_state =
  List.fold captured_vars_with_actuals ~init:call_state ~f:(fun call_state (formal, actual) ->
      record_post_for_actual callee_proc_name call_location pre_post ~formal ~actual call_state )


let apply_post_for_globals callee_proc_name call_location pre_post call_state =
  match
    fold_globals_of_stack call_location (pre_post.AbductiveDomain.pre :> BaseDomain.t).stack
      call_state ~f:(fun _var ~stack_value:(addr_callee, _) ~addr_hist_caller call_state ->
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
            let attrs' = add_call_to_attributes callee_proc_name call_loc history attrs in
            let astate = AddressAttributes.abduce_and_add addr_caller attrs' call_state.astate in
            {call_state with astate} )
    (pre_post.AbductiveDomain.post :> BaseDomain.t).attrs call_state


let record_skipped_calls callee_proc_name call_loc pre_post call_state =
  let callee_skipped_map =
    pre_post.AbductiveDomain.skipped_calls
    |> SkippedCalls.map (fun trace -> add_call_to_trace callee_proc_name call_loc [] trace)
  in
  let astate = AbductiveDomain.add_skipped_calls callee_skipped_map call_state.astate in
  {call_state with astate}


let apply_post callee_proc_name call_location pre_post ~captured_vars_with_actuals ~formals ~actuals
    call_state =
  PerfEvent.(log (fun logger -> log_begin_event logger ~name:"pulse call post" ())) ;
  let r =
    apply_post_for_parameters callee_proc_name call_location pre_post ~formals ~actuals call_state
    |> apply_post_for_captured_vars callee_proc_name call_location pre_post
         ~captured_vars_with_actuals
    |> apply_post_for_globals callee_proc_name call_location pre_post
    |> record_post_for_return callee_proc_name call_location pre_post
    |> fun (call_state, return_caller) ->
    record_post_remaining_attributes callee_proc_name call_location pre_post call_state
    |> record_skipped_calls callee_proc_name call_location pre_post
    |> conjoin_callee_arith pre_post
    |> fun {astate; _} -> (astate, return_caller)
  in
  PerfEvent.(log (fun logger -> log_end_event logger ())) ;
  r


let check_all_valid callee_proc_name call_location {AbductiveDomain.pre; _} call_state =
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
                   ( Diagnostic.AccessToInvalidAddress
                       {calling_context= []; invalidation; invalidation_trace; access_trace}
                   , astate ) ) ) )
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
let apply_prepost callee_proc_name call_location ~callee_prepost:pre_post
    ~captured_vars_with_actuals ~formals ~actuals astate =
  L.d_printfln "Applying pre/post for %a(%a):@\n%a" Procname.pp callee_proc_name
    (Pp.seq ~sep:"," Var.pp) formals pp pre_post ;
  let empty_call_state =
    {astate; subst= AddressMap.empty; rev_subst= AddressMap.empty; visited= AddressSet.empty}
  in
  (* read the precondition *)
  match
    materialize_pre callee_proc_name call_location pre_post ~captured_vars_with_actuals ~formals
      ~actuals empty_call_state
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
        apply_post callee_proc_name call_location pre_post ~captured_vars_with_actuals ~formals
          ~actuals call_state
      with
      | Ok post ->
          Ok (Some post)
      | exception Contradiction reason ->
          L.d_printfln "Cannot apply post-condition: %a" pp_contradiction reason ;
          Ok None
      | Error _ as error ->
          error )
