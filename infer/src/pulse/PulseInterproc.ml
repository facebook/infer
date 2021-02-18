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
  ; invalid_subst: (AbstractValue.t * ValueHistory.t) AddressMap.t
        (** this is a subset of subst that contains all invalid matches between callee-pre and
            caller *)
  ; rev_subst: AbstractValue.t AddressMap.t
        (** the inverse translation from [subst] from caller addresses to callee addresses *)
  ; visited: AddressSet.t
        (** set of callee addresses that have been visited already

            NOTE: this is not always equal to the domain of [rev_subst]: when applying the post we
            visit each subgraph from each formal independently so we reset [visited] between the
            visit of each formal *) }

let pp_call_state fmt {astate; subst; invalid_subst; rev_subst; visited} =
  F.fprintf fmt
    "@[<v>{ astate=@[<hv2>%a@];@,\
    \ subst=@[<hv2>%a@];@,\
    \ invalid_subst=@[<hv2>%a@];@,\
    \ rev_subst=@[<hv2>%a@];@,\
    \ visited=@[<hv2>%a@]@,\
    \ }@]" AbductiveDomain.pp astate
    (AddressMap.pp ~pp_value:(fun fmt (addr, _) -> AbstractValue.pp fmt addr))
    subst
    (AddressMap.pp ~pp_value:(fun fmt (addr, _) -> AbstractValue.pp fmt addr))
    invalid_subst
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
  | ISLPreconditionMismatch
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
  | ISLPreconditionMismatch ->
      F.pp_print_string fmt "pre does not imply call state"


exception Contradiction of contradiction

let fold_globals_of_stack call_loc stack call_state ~f =
  Container.fold_result ~fold:(IContainer.fold_of_pervasives_map_fold BaseStack.fold)
    stack ~init:call_state ~f:(fun call_state (var, stack_value) ->
      match var with
      | Var.ProgramVar pvar when Pvar.is_global pvar ->
          let call_state, addr_hist_caller =
            let astate, var_value =
              Stack.eval call_loc
                [ValueHistory.VariableAccessed (pvar, call_loc)]
                var call_state.astate
            in
            if phys_equal astate call_state.astate then (call_state, var_value)
            else ({call_state with astate}, var_value)
          in
          f pvar ~stack_value ~addr_hist_caller call_state
      | _ ->
          Ok call_state )


let and_aliasing_arith ~addr_callee ~addr_caller0 call_state =
  match AddressMap.find_opt addr_callee call_state.subst with
  | Some (addr_caller', _) when not (AbstractValue.equal addr_caller' addr_caller0) ->
      (* NOTE: discarding new equalities here instead of passing them to
         {!AbductiveDomain.incorporate_new_eqs} because it would be too slow to do each time we
         visit a new address. We should re-normalize at the end of the call instead (TODO). *)
      let path_condition, _new_eqs =
        PathCondition.and_eq_vars addr_caller0 addr_caller'
          call_state.astate.AbductiveDomain.path_condition
      in
      if PathCondition.is_unsat_cheap path_condition then raise (Contradiction PathCondition)
      else
        {call_state with astate= AbductiveDomain.set_path_condition path_condition call_state.astate}
  | _ ->
      call_state


let visit call_state ~pre ~addr_callee ~addr_hist_caller =
  let addr_caller = fst addr_hist_caller in
  let call_state =
    match AddressMap.find_opt addr_caller call_state.rev_subst with
    | Some addr_callee' when not (AbstractValue.equal addr_callee addr_callee') ->
        if
          (* [addr_caller] corresponds to several values in the callee, see if that's a problem for
             applying the pre-condition, i.e. if both values are addresses in the callee's heap,
             which means they must be disjoint. If so, raise a contradiction, but if not then
             continue as it just means that the callee doesn't care about the value of these
             variables, but record that they are equal. *)
          BaseMemory.mem addr_callee pre.BaseDomain.heap
          && BaseMemory.mem addr_callee' pre.BaseDomain.heap
        then raise (Contradiction (Aliasing {addr_caller; addr_callee; addr_callee'; call_state}))
        else and_aliasing_arith ~addr_callee:addr_callee' ~addr_caller0:addr_caller call_state
    | _ ->
        call_state
  in
  let call_state = and_aliasing_arith ~addr_callee ~addr_caller0:addr_caller call_state in
  if AddressSet.mem addr_callee call_state.visited then (`AlreadyVisited, call_state)
  else
    ( `NotAlreadyVisited
    , { call_state with
        visited= AddressSet.add addr_callee call_state.visited
      ; subst= AddressMap.add addr_callee addr_hist_caller call_state.subst
      ; rev_subst= AddressMap.add addr_caller addr_callee call_state.rev_subst } )


(** HACK: we don't need to update the [rev_subst] of a call state when generating a fresh value for
    the caller because there's no chance that value appears anywhere else in the caller state, hence
    we cannot possibly get clashes about that caller value, which is the only thing [rev_subst] is
    used for. This is why this function is allowed to take only [subst] as argument and not a full
    call state. *)
let subst_find_or_new subst addr_callee ~default_hist_caller =
  match AddressMap.find_opt addr_callee subst with
  | None ->
      let addr_hist_fresh = (AbstractValue.mk_fresh (), default_hist_caller) in
      (AddressMap.add addr_callee addr_hist_fresh subst, addr_hist_fresh)
  | Some addr_hist_caller ->
      (subst, addr_hist_caller)


let translate_access_to_caller subst (access_callee : BaseMemory.Access.t) : _ * BaseMemory.Access.t
    =
  match access_callee with
  | ArrayAccess (typ, val_callee) ->
      let subst, (val_caller, _) = subst_find_or_new subst val_callee ~default_hist_caller:[] in
      (subst, ArrayAccess (typ, val_caller))
  | FieldAccess _ | TakeAddress | Dereference ->
      (subst, access_callee)


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
          ~f:(fun call_state (access_callee, (addr_pre_dest, _)) ->
            (* HACK: we should probably visit the value in the (array) access too, but since it's
               a value normally it shouldn't appear in the heap anyway so there should be nothing
               to visit. *)
            let subst, access_caller = translate_access_to_caller call_state.subst access_callee in
            let astate, addr_hist_dest_caller =
              Memory.eval_edge addr_hist_caller access_caller call_state.astate
            in
            let call_state = {call_state with astate; subst} in
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


let conjoin_callee_arith pre_post call_state =
  L.d_printfln "applying callee path condition: (%a)[%a]" PathCondition.pp
    pre_post.AbductiveDomain.path_condition
    (AddressMap.pp ~pp_value:(fun fmt (addr, _) -> AbstractValue.pp fmt addr))
    call_state.subst ;
  let subst, path_condition, new_eqs =
    PathCondition.and_callee call_state.subst call_state.astate.path_condition
      ~callee:pre_post.AbductiveDomain.path_condition
  in
  let astate = AbductiveDomain.set_path_condition path_condition call_state.astate in
  let astate = AbductiveDomain.incorporate_new_eqs new_eqs astate in
  if PathCondition.is_unsat_cheap astate.path_condition then raise (Contradiction PathCondition)
  else {call_state with astate; subst}


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
  if Config.pulse_isl then
    AddressMap.fold
      (fun addr_callee addr_hist_caller call_state ->
        let callee_attr =
          BaseAddressAttributes.find_opt addr_callee
            (pre_post.AbductiveDomain.pre :> BaseDomain.t).attrs
        in
        let caller_attr =
          BaseAddressAttributes.find_opt (fst addr_hist_caller)
            (call_state.astate.AbductiveDomain.post :> BaseDomain.t).attrs
        in
        match (callee_attr, caller_attr) with
        | Some callee_attrs, None ->
            one_address_sat callee_attrs addr_hist_caller call_state
        | Some callee_attrs, Some caller_attrs ->
            if (* check implication *)
               Attributes.isl_subset callee_attrs caller_attrs then
              { call_state with
                invalid_subst= AddressMap.add addr_callee addr_hist_caller call_state.invalid_subst
              }
            else if Attributes.is_uninitialized caller_attrs then
              one_address_sat callee_attrs addr_hist_caller call_state
            else raise (Contradiction ISLPreconditionMismatch)
        | _ ->
            call_state )
      call_state.subst call_state
  else
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


let delete_edges_in_callee_pre_from_caller ~edges_pre_opt addr_caller call_state =
  match
    BaseMemory.find_opt addr_caller (call_state.astate.AbductiveDomain.post :> BaseDomain.t).heap
  with
  | None ->
      (call_state.subst, BaseMemory.Edges.empty)
  | Some old_post_edges -> (
    match edges_pre_opt with
    | None ->
        (call_state.subst, old_post_edges)
    | Some edges_pre ->
        let subst, translated_accesses_pre =
          BaseMemory.Edges.fold ~init:(call_state.subst, BaseMemory.AccessSet.empty) edges_pre
            ~f:(fun (subst, accesses) (access_callee, _) ->
              let subst, access = translate_access_to_caller subst access_callee in
              (subst, BaseMemory.AccessSet.add access accesses) )
        in
        let post_edges =
          BaseMemory.Edges.filter old_post_edges ~f:(fun (access_caller, _) ->
              (* delete edge if some edge for the same access exists in the pre *)
              not (BaseMemory.AccessSet.mem access_caller translated_accesses_pre) )
        in
        (subst, post_edges) )


let record_post_cell callee_proc_name call_loc ~edges_pre_opt
    ~cell_callee_post:(edges_callee_post, attrs_callee_post) (addr_caller, hist_caller) call_state =
  let call_state =
    let attrs_post_caller =
      add_call_to_attributes callee_proc_name call_loc hist_caller attrs_callee_post
    in
    let astate =
      if Config.pulse_isl then
        AddressAttributes.add_attrs addr_caller attrs_post_caller call_state.astate
      else AddressAttributes.abduce_and_add addr_caller attrs_post_caller call_state.astate
    in
    {call_state with astate}
  in
  let subst, translated_post_edges =
    BaseMemory.Edges.fold ~init:(call_state.subst, BaseMemory.Edges.empty) edges_callee_post
      ~f:(fun (subst, translated_edges) (access_callee, (addr_callee, trace_post)) ->
        let subst, (addr_curr, hist_curr) =
          subst_find_or_new subst addr_callee ~default_hist_caller:hist_caller
        in
        let subst, access = translate_access_to_caller subst access_callee in
        let translated_edges =
          BaseMemory.Edges.add access
            ( addr_curr
            , ValueHistory.Call {f= Call callee_proc_name; location= call_loc; in_call= trace_post}
              :: hist_curr )
            translated_edges
        in
        (subst, translated_edges) )
  in
  let call_state = {call_state with subst} in
  let subst, post_edges_minus_pre =
    delete_edges_in_callee_pre_from_caller ~edges_pre_opt addr_caller call_state
  in
  let edges_post_caller =
    BaseMemory.Edges.union_left_biased translated_post_edges post_edges_minus_pre
  in
  { call_state with
    subst
  ; astate= AbductiveDomain.set_post_edges addr_caller edges_post_caller call_state.astate }


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
    | Some ((edges_post, attrs_post) as cell_callee_post) ->
        let edges_pre_opt = BaseMemory.find_opt addr_callee (pre :> BaseDomain.t).BaseDomain.heap in
        let call_state_after_post =
          if is_cell_read_only ~edges_pre_opt ~cell_post:cell_callee_post then call_state
          else
            let attrs_post =
              if Config.pulse_isl then
                match
                  AbductiveDomain.find_post_cell_opt (fst addr_hist_caller) call_state.astate
                with
                | None ->
                    attrs_post
                | Some (_, attrs_caller) ->
                    (* if post attr is abduced, use the one in caller which is up-to-date *)
                    Attributes.replace_isl_abduced attrs_post attrs_caller
              else attrs_post
            in
            record_post_cell callee_proc_name call_loc ~edges_pre_opt addr_hist_caller
              ~cell_callee_post:(edges_post, attrs_post) call_state
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
    |> fun call_state -> (call_state, return_caller)
  in
  PerfEvent.(log (fun logger -> log_end_event logger ())) ;
  r


let check_all_valid callee_proc_name call_location {AbductiveDomain.pre; _} call_state =
  AddressMap.fold
    (fun addr_pre (addr_caller, hist_caller) astate_result ->
      let mk_access_trace callee_access_trace =
        Trace.ViaCall
          { in_call= callee_access_trace
          ; f= Call callee_proc_name
          ; location= call_location
          ; history= hist_caller }
      in
      let open IResult.Let_syntax in
      let* astate = astate_result in
      let* astate =
        match BaseAddressAttributes.get_must_be_valid addr_pre (pre :> BaseDomain.t).attrs with
        | None ->
            astate_result
        | Some callee_access_trace ->
            let access_trace = mk_access_trace callee_access_trace in
            AddressAttributes.check_valid access_trace addr_caller astate
            |> Result.map_error ~f:(fun (invalidation, invalidation_trace) ->
                   L.d_printfln "ERROR: caller's %a invalid!" AbstractValue.pp addr_caller ;
                   ( Diagnostic.AccessToInvalidAddress
                       {calling_context= []; invalidation; invalidation_trace; access_trace}
                   , astate ) )
      in
      match BaseAddressAttributes.get_must_be_initialized addr_pre (pre :> BaseDomain.t).attrs with
      | None ->
          astate_result
      | Some callee_access_trace ->
          let access_trace = mk_access_trace callee_access_trace in
          AddressAttributes.check_initialized access_trace addr_caller astate
          |> Result.map_error ~f:(fun () ->
                 L.d_printfln "ERROR: caller's %a is uninitialized!" AbstractValue.pp addr_caller ;
                 ( Diagnostic.ReadUninitializedValue {calling_context= []; trace= access_trace}
                 , astate ) ) )
    call_state.subst (Ok call_state.astate)


let isl_check_all_invalid invalid_addr_callers callee_proc_name call_location
    {AbductiveDomain.pre; _} pre_astate astate =
  match astate.AbductiveDomain.isl_status with
  | ISLOk ->
      Ok astate
  | ISLError ->
      AbstractValue.Map.fold
        (fun addr_pre (addr_caller, hist_caller) astate_result ->
          let mk_access_trace callee_access_trace =
            Trace.ViaCall
              { in_call= callee_access_trace
              ; f= Call callee_proc_name
              ; location= call_location
              ; history= hist_caller }
          in
          match astate_result with
          | Error _ ->
              astate_result
          | Ok astate -> (
            match
              BaseAddressAttributes.get_invalid addr_caller
                (pre_astate.AbductiveDomain.post :> BaseDomain.t).attrs
            with
            | None ->
                astate_result
            | Some (invalidation, invalidation_trace) -> (
              match BaseAddressAttributes.get_invalid addr_pre (pre :> BaseDomain.t).attrs with
              | None ->
                  astate_result
              | Some (_, callee_access_trace) ->
                  let access_trace = mk_access_trace callee_access_trace in
                  L.d_printfln "ERROR: caller's %a invalid!" AbstractValue.pp addr_caller ;
                  Error
                    ( Diagnostic.AccessToInvalidAddress
                        {calling_context= []; invalidation; invalidation_trace; access_trace}
                    , AbductiveDomain.set_isl_status ISLError astate ) ) ) )
        invalid_addr_callers (Ok astate)


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
    (Pp.seq ~sep:"," Var.pp) formals AbductiveDomain.pp pre_post ;
  let empty_call_state =
    { astate
    ; subst= AddressMap.empty
    ; invalid_subst= AddressMap.empty
    ; rev_subst= AddressMap.empty
    ; visited= AddressSet.empty }
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
      Unsat
  | Error _ as error ->
      (* error: the function call requires to read some state known to be invalid *)
      Sat error
  | Ok call_state -> (
      L.d_printfln "Pre applied successfully. call_state=%a" pp_call_state call_state ;
      match
        let open IResult.Let_syntax in
        let* astate =
          if Config.pulse_isl then
            Ok
              (AbductiveDomain.set_isl_status pre_post.AbductiveDomain.isl_status call_state.astate)
          else check_all_valid callee_proc_name call_location pre_post call_state
        in
        (* reset [visited] *)
        let invalid_subst = call_state.invalid_subst in
        let pre_astate = astate in
        let call_state = {call_state with astate; visited= AddressSet.empty} in
        (* apply the postcondition *)
        let call_state, return_caller =
          apply_post callee_proc_name call_location pre_post ~captured_vars_with_actuals ~formals
            ~actuals call_state
        in
        let astate =
          if Topl.is_deep_active () then
            AbductiveDomain.Topl.large_step ~call_location ~callee_proc_name
              ~substitution:call_state.subst ~condition:call_state.astate.path_condition
              ~callee_prepost:pre_post.AbductiveDomain.topl call_state.astate
          else call_state.astate
        in
        let+ astate =
          if Config.pulse_isl then
            isl_check_all_invalid invalid_subst callee_proc_name call_location pre_post pre_astate
              astate
          else Ok astate
        in
        (astate, return_caller)
      with
      | post ->
          Sat post
      | exception Contradiction reason ->
          L.d_printfln "Cannot apply post-condition: %a" pp_contradiction reason ;
          Unsat )
