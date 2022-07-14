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
  | CapturedFormalActualLength of
      { captured_formals: (Var.t * Typ.t) list
      ; captured_actuals: ((AbstractValue.t * ValueHistory.t) * Typ.t) list }
  | FormalActualLength of
      {formals: (Var.t * Typ.t) list; actuals: ((AbstractValue.t * ValueHistory.t) * Typ.t) list}
  | ISLPreconditionMismatch
  | PathCondition

let pp_contradiction fmt = function
  | Aliasing {addr_caller; addr_callee; addr_callee'; call_state} ->
      F.fprintf fmt
        "address %a in caller already bound to %a, not %a@\nnote: current call state was %a"
        AbstractValue.pp addr_caller AbstractValue.pp addr_callee' AbstractValue.pp addr_callee
        pp_call_state call_state
  | CapturedFormalActualLength {captured_formals; captured_actuals} ->
      F.fprintf fmt "captured formals have length %d but captured actuals have length %d"
        (List.length captured_formals) (List.length captured_actuals)
  | FormalActualLength {formals; actuals} ->
      F.fprintf fmt "formals have length %d but actuals have length %d" (List.length formals)
        (List.length actuals)
  | PathCondition ->
      F.pp_print_string fmt "path condition evaluates to false"
  | ISLPreconditionMismatch ->
      F.pp_print_string fmt "pre does not imply call state"


let log_contradiction = function
  | Aliasing _ ->
      Stats.incr_pulse_aliasing_contradictions ()
  | FormalActualLength _ ->
      Stats.incr_pulse_args_length_contradictions ()
  | CapturedFormalActualLength _ ->
      Stats.incr_pulse_captured_vars_length_contradictions ()
  | ISLPreconditionMismatch | PathCondition ->
      ()


exception Contradiction of contradiction

let fold_globals_of_stack ({PathContext.timestamp} as path) call_loc stack call_state ~f =
  PulseResult.container_fold ~fold:(IContainer.fold_of_pervasives_map_fold BaseStack.fold)
    stack ~init:call_state ~f:(fun call_state (var, stack_value) ->
      match var with
      | Var.ProgramVar pvar when Pvar.is_global pvar ->
          let call_state, addr_hist_caller =
            let astate, var_value =
              Stack.eval path call_loc
                (ValueHistory.singleton (VariableAccessed (pvar, call_loc, timestamp)))
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
      (* map restricted (≥0) values to restricted values to preserve their semantics *)
      let addr_caller = AbstractValue.mk_fresh_same_kind addr_callee in
      L.d_printfln "new subst %a <-> %a (fresh)" AbstractValue.pp addr_callee AbstractValue.pp
        addr_caller ;
      let addr_hist_fresh = (addr_caller, default_hist_caller) in
      (AddressMap.add addr_callee addr_hist_fresh subst, addr_hist_fresh)
  | Some addr_hist_caller ->
      (subst, addr_hist_caller)


let translate_access_to_caller subst (access_callee : BaseMemory.Access.t) : _ * BaseMemory.Access.t
    =
  match access_callee with
  | ArrayAccess (typ, val_callee) ->
      let subst, (val_caller, _) =
        subst_find_or_new subst val_callee ~default_hist_caller:ValueHistory.epoch
      in
      (subst, ArrayAccess (typ, val_caller))
  | FieldAccess _ | TakeAddress | Dereference ->
      (subst, access_callee)


(* {3 reading the pre from the current state} *)

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
        PulseResult.container_fold ~fold:Memory.Edges.fold ~init:call_state edges_pre
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


let deref_non_c_struct addr typ astate =
  match typ.Typ.desc with
  | Tstruct _ ->
      Some addr
  | _ ->
      BaseMemory.find_edge_opt addr Dereference astate |> Option.map ~f:fst


(** materialize subgraph of [pre] rooted at the address represented by a [formal] parameter that has
    been instantiated with the corresponding [actual] into the current state [call_state.astate] *)
let materialize_pre_from_actual callee_proc_name call_location ~pre ~formal:(formal, typ)
    ~actual:(actual, _) call_state =
  L.d_printfln "Materializing PRE from [%a <- %a]" Var.pp formal AbstractValue.pp (fst actual) ;
  (let open IOption.Let_syntax in
  let* addr_formal_pre, _ = BaseStack.find_opt formal pre.BaseDomain.stack in
  let+ formal_pre = deref_non_c_struct addr_formal_pre typ pre.BaseDomain.heap in
  materialize_pre_from_address callee_proc_name call_location ~pre ~addr_pre:formal_pre
    ~addr_hist_caller:actual call_state)
  |> function Some result -> result | None -> Ok call_state


let is_cell_read_only ~edges_pre_opt ~cell_post:(_, attrs_post) =
  match edges_pre_opt with None -> false | Some _ -> not (Attributes.is_modified attrs_post)


let materialize_pre_for_captured_vars callee_proc_name call_location pre_post ~captured_formals
    ~captured_actuals call_state =
  match
    PulseResult.list_fold2 captured_formals captured_actuals ~init:call_state
      ~f:(fun call_state formal actual ->
        materialize_pre_from_actual callee_proc_name call_location
          ~pre:(pre_post.AbductiveDomain.pre :> BaseDomain.t)
          ~formal ~actual call_state )
  with
  | Unequal_lengths ->
      raise (Contradiction (CapturedFormalActualLength {captured_formals; captured_actuals}))
  | Ok result ->
      result


let materialize_pre_for_parameters callee_proc_name call_location pre_post ~formals ~actuals
    call_state =
  (* For each [(formal, actual)] pair, resolve them to addresses in their respective states then
     call [materialize_pre_from] on them.  Give up if calling the function introduces aliasing.
  *)
  match
    PulseResult.list_fold2 formals actuals ~init:call_state ~f:(fun call_state formal actual ->
        materialize_pre_from_actual callee_proc_name call_location
          ~pre:(pre_post.AbductiveDomain.pre :> BaseDomain.t)
          ~formal ~actual call_state )
  with
  | Unequal_lengths ->
      raise (Contradiction (FormalActualLength {formals; actuals}))
  | Ok result ->
      result


let materialize_pre_for_globals path callee_proc_name call_location pre_post call_state =
  fold_globals_of_stack path call_location (pre_post.AbductiveDomain.pre :> BaseDomain.t).stack
    call_state ~f:(fun _var ~stack_value:(addr_pre, _) ~addr_hist_caller call_state ->
      materialize_pre_from_address callee_proc_name call_location
        ~pre:(pre_post.AbductiveDomain.pre :> BaseDomain.t)
        ~addr_pre ~addr_hist_caller call_state )


let conjoin_callee_arith pre_post call_state =
  let open PulseResult.Let_syntax in
  L.d_printfln "applying callee path condition: (%a)[%a]" PathCondition.pp
    pre_post.AbductiveDomain.path_condition
    (AddressMap.pp ~pp_value:(fun fmt (addr, _) -> AbstractValue.pp fmt addr))
    call_state.subst ;
  let subst, path_condition, new_eqs =
    PathCondition.and_callee call_state.subst call_state.astate.path_condition
      ~callee:pre_post.AbductiveDomain.path_condition
  in
  if PathCondition.is_unsat_cheap path_condition then raise (Contradiction PathCondition)
  else
    let astate = AbductiveDomain.set_path_condition path_condition call_state.astate in
    let+ astate =
      AbductiveDomain.incorporate_new_eqs new_eqs astate |> AccessResult.of_abductive_result
    in
    if PathCondition.is_unsat_cheap astate.AbductiveDomain.path_condition then
      raise (Contradiction PathCondition)
    else {call_state with astate; subst}


let caller_attrs_of_callee_attrs timestamp callee_proc_name call_location caller_history call_state
    callee_attrs =
  let subst_ref = ref call_state.subst in
  let f_subst v =
    let subst, (v', _hist) =
      subst_find_or_new !subst_ref v ~default_hist_caller:ValueHistory.epoch
    in
    subst_ref := subst ;
    v'
  in
  let attrs =
    Attributes.add_call_and_subst f_subst timestamp callee_proc_name call_location caller_history
      callee_attrs
  in
  ({call_state with subst= !subst_ref}, attrs)


let apply_arithmetic_constraints {PathContext.timestamp} callee_proc_name call_location pre_post
    call_state =
  let open PulseResult.Let_syntax in
  let one_address_sat callee_attrs (addr_caller, caller_history) call_state =
    let call_state, attrs_caller =
      caller_attrs_of_callee_attrs timestamp callee_proc_name call_location caller_history
        call_state callee_attrs
    in
    let astate = AddressAttributes.abduce_and_add addr_caller attrs_caller call_state.astate in
    if phys_equal astate call_state.astate then call_state else {call_state with astate}
  in
  let+ call_state = conjoin_callee_arith pre_post call_state in
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


let materialize_pre path callee_proc_name call_location pre_post ~captured_formals ~captured_actuals
    ~formals ~actuals call_state =
  PerfEvent.(log (fun logger -> log_begin_event logger ~name:"pulse call pre" ())) ;
  let r =
    let open PulseResult.Let_syntax in
    (* first make as large a mapping as we can between callee values and caller values... *)
    materialize_pre_for_parameters callee_proc_name call_location pre_post ~formals ~actuals
      call_state
    >>= materialize_pre_for_captured_vars callee_proc_name call_location pre_post ~captured_formals
          ~captured_actuals
    >>= materialize_pre_for_globals path callee_proc_name call_location pre_post
    >>= (* ...then relational arithmetic constraints in the callee's attributes will make sense in
           terms of the caller's values *)
    apply_arithmetic_constraints path callee_proc_name call_location pre_post
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


let record_post_cell ({PathContext.timestamp} as path) callee_proc_name call_loc ~edges_pre_opt
    ~cell_callee_post:(edges_callee_post, attrs_callee_post) (addr_caller, hist_caller) call_state =
  let call_state =
    let call_state, attrs_post_caller =
      caller_attrs_of_callee_attrs timestamp callee_proc_name call_loc hist_caller call_state
        attrs_callee_post
    in
    let astate =
      if Attributes.is_java_resource_released attrs_post_caller then
        PulseOperations.java_resource_release ~recursive:true addr_caller call_state.astate
      else call_state.astate
    in
    let astate =
      if Config.pulse_isl then AddressAttributes.add_attrs addr_caller attrs_post_caller astate
      else AddressAttributes.abduce_and_add addr_caller attrs_post_caller astate
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
            , ValueHistory.sequence ~context:path.conditions
                (Call {f= Call callee_proc_name; location= call_loc; in_call= trace_post; timestamp})
                hist_curr )
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


let rec record_post_for_address path callee_proc_name call_loc
    ({AbductiveDomain.pre; _} as pre_post) ~addr_callee ~addr_hist_caller call_state =
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
          if is_cell_read_only ~edges_pre_opt ~cell_post:cell_callee_post then (
            L.d_printfln "cell at %a is read-only, not modifying@\n" AbstractValue.pp addr_callee ;
            call_state )
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
            record_post_cell path callee_proc_name call_loc ~edges_pre_opt addr_hist_caller
              ~cell_callee_post:(edges_post, attrs_post) call_state
        in
        Memory.Edges.fold ~init:call_state_after_post edges_post
          ~f:(fun call_state (_access, (addr_callee_dest, _)) ->
            let call_state, addr_hist_curr_dest =
              call_state_subst_find_or_new call_state addr_callee_dest
                ~default_hist_caller:(snd addr_hist_caller)
            in
            record_post_for_address path callee_proc_name call_loc pre_post
              ~addr_callee:addr_callee_dest ~addr_hist_caller:addr_hist_curr_dest call_state ) )


let record_post_for_actual path callee_proc_name call_loc pre_post ~formal:(formal, typ)
    ~actual:(actual, _) call_state =
  L.d_printfln_escaped "Recording POST from [%a] <-> %a" Var.pp formal AbstractValue.pp (fst actual) ;
  match
    let open IOption.Let_syntax in
    let* addr_formal_pre, _ =
      BaseStack.find_opt formal (pre_post.AbductiveDomain.pre :> BaseDomain.t).BaseDomain.stack
    in
    let+ formal_pre =
      deref_non_c_struct addr_formal_pre typ
        (pre_post.AbductiveDomain.pre :> BaseDomain.t).BaseDomain.heap
    in
    record_post_for_address path callee_proc_name call_loc pre_post ~addr_callee:formal_pre
      ~addr_hist_caller:actual call_state
  with
  | Some call_state ->
      call_state
  | None ->
      call_state


let record_post_for_return ({PathContext.timestamp} as path) callee_proc_name call_loc pre_post
    call_state =
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
    | Some (return_callee, return_callee_hist) ->
        let return_caller, return_caller_hist =
          match AddressMap.find_opt return_callee call_state.subst with
          | Some return_caller_hist ->
              return_caller_hist
          | None ->
              (AbstractValue.mk_fresh_same_kind return_callee, ValueHistory.epoch)
        in
        L.d_printfln_escaped "Recording POST from [return] <-> %a" AbstractValue.pp return_caller ;
        let call_state =
          record_post_for_address path callee_proc_name call_loc pre_post ~addr_callee:return_callee
            ~addr_hist_caller:(return_caller, return_caller_hist)
            call_state
        in
        (* need to add the call to the returned history too *)
        let return_caller_hist =
          ValueHistory.sequence ~context:path.conditions
            (Call
               {f= Call callee_proc_name; location= call_loc; in_call= return_callee_hist; timestamp}
            )
            return_caller_hist
        in
        (call_state, Some (return_caller, return_caller_hist)) )


let apply_post_for_parameters path callee_proc_name call_location pre_post ~formals ~actuals
    call_state =
  (* for each [(formal_i, actual_i)] pair, do [post_i = post union subst(graph reachable from
     formal_i in post)], deleting previous info when comparing pre and post shows a difference
     (TODO: record in the pre when a location is written to instead of just comparing values
     between pre and post since it's unreliable, eg replace value read in pre with same value in
     post but nuke other fields in the meantime? is that possible?). *)
  match
    List.fold2 formals actuals ~init:call_state ~f:(fun call_state formal actual ->
        record_post_for_actual path callee_proc_name call_location pre_post ~formal ~actual
          call_state )
  with
  | Unequal_lengths ->
      (* should have been checked before by [materialize_pre] *)
      L.(die InternalError) "formals and actuals have different lenghts"
  | Ok call_state ->
      call_state


let apply_post_for_captured_vars path callee_proc_name call_location pre_post ~captured_formals
    ~captured_actuals call_state =
  match
    List.fold2 captured_formals captured_actuals ~init:call_state
      ~f:(fun call_state formal actual ->
        record_post_for_actual path callee_proc_name call_location pre_post ~formal ~actual
          call_state )
  with
  | Unequal_lengths ->
      (* should have been checked before by [materialize_pre] *)
      L.(die InternalError) "captured formals and captured actuals have different lenghts"
  | Ok result ->
      result


let apply_post_for_globals path callee_proc_name call_location pre_post call_state =
  fold_globals_of_stack path call_location (pre_post.AbductiveDomain.pre :> BaseDomain.t).stack
    call_state ~f:(fun _var ~stack_value:(addr_callee, _) ~addr_hist_caller call_state ->
      Ok
        (record_post_for_address path callee_proc_name call_location pre_post ~addr_callee
           ~addr_hist_caller call_state ) )
  |> (* always return [Ok _] above *) PulseResult.ok_exn


let record_post_remaining_attributes {PathContext.timestamp} callee_proc_name call_loc pre_post
    call_state =
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
            let call_state, attrs' =
              caller_attrs_of_callee_attrs timestamp callee_proc_name call_loc history call_state
                attrs
            in
            let astate = AddressAttributes.abduce_and_add addr_caller attrs' call_state.astate in
            {call_state with astate} )
    (pre_post.AbductiveDomain.post :> BaseDomain.t).attrs call_state


let record_skipped_calls callee_proc_name call_loc pre_post call_state =
  let callee_skipped_map =
    pre_post.AbductiveDomain.skipped_calls
    |> SkippedCalls.map (fun trace ->
           Trace.ViaCall
             { f= Call callee_proc_name
             ; location= call_loc
             ; history= ValueHistory.epoch
             ; in_call= trace } )
  in
  let astate = AbductiveDomain.add_skipped_calls callee_skipped_map call_state.astate in
  {call_state with astate}


let record_need_specialization pre_post call_state =
  if pre_post.AbductiveDomain.need_specialization then
    {call_state with astate= AbductiveDomain.set_need_specialization call_state.astate}
  else call_state


let apply_unknown_effects pre_post call_state =
  let open IOption.Let_syntax in
  L.d_printfln "call_state = {%a}@\n" pp_call_state call_state ;
  let is_modified_by_call addr_caller access =
    match AddressMap.find_opt addr_caller call_state.rev_subst with
    | None ->
        false
    | Some addr_callee ->
        let edges_callee_pre =
          BaseMemory.find_opt addr_callee (pre_post.AbductiveDomain.pre :> BaseDomain.t).heap
          |> Option.value ~default:BaseMemory.Edges.empty
        in
        let edges_callee_post =
          BaseMemory.find_opt addr_callee (pre_post.AbductiveDomain.post :> BaseDomain.t).heap
          |> Option.value ~default:BaseMemory.Edges.empty
        in
        let pre_value = BaseMemory.Edges.find_opt access edges_callee_pre >>| fst in
        let post_value = BaseMemory.Edges.find_opt access edges_callee_post >>| fst in
        (* havoc only fields that haven't been havoc'd already during the call *)
        not (Option.equal AbstractValue.equal post_value pre_value)
  in
  let astate =
    BaseAddressAttributes.fold
      (fun addr_callee attrs astate ->
        (let* _, havoc_hist = Attributes.get_unknown_effect attrs in
         let+ addr_caller, _ = AddressMap.find_opt addr_callee call_state.subst in
         (* NOTE: could be optimized to remember the addresses already visited in case many
            addresses have the [UnknownEffect] attribute and share important parts of the memory
            graph (unlikely) *)
         L.d_printfln "applying unknown effects on %a@\n  @[<2>" AbstractValue.pp addr_caller ;
         let astate =
           AbductiveDomain.apply_unknown_effect havoc_hist addr_caller astate
             ~havoc_filter:(fun addr_caller access _ ->
               (* havoc only fields that haven't been havoc'd already during the call *)
               not (is_modified_by_call addr_caller access) )
         in
         L.d_printfln "@]" ;
         astate )
        |> Option.value ~default:astate )
      (pre_post.AbductiveDomain.post :> BaseDomain.t).attrs call_state.astate
  in
  {call_state with astate}


let apply_post path callee_proc_name call_location pre_post ~captured_formals ~captured_actuals
    ~formals ~actuals call_state =
  let open PulseResult.Let_syntax in
  PerfEvent.(log (fun logger -> log_begin_event logger ~name:"pulse call post" ())) ;
  let r =
    let call_state, return_caller =
      apply_unknown_effects pre_post call_state
      |> apply_post_for_parameters path callee_proc_name call_location pre_post ~formals ~actuals
      |> apply_post_for_captured_vars path callee_proc_name call_location pre_post ~captured_formals
           ~captured_actuals
      |> apply_post_for_globals path callee_proc_name call_location pre_post
      |> record_post_for_return path callee_proc_name call_location pre_post
    in
    let+ call_state =
      record_post_remaining_attributes path callee_proc_name call_location pre_post call_state
      |> record_skipped_calls callee_proc_name call_location pre_post
      |> record_need_specialization pre_post
      |> conjoin_callee_arith pre_post
    in
    (call_state, return_caller)
  in
  PerfEvent.(log (fun logger -> log_end_event logger ())) ;
  r


let check_all_valid path callee_proc_name call_location {AbductiveDomain.pre; _} call_state =
  (* collect all the checks to perform then do each check in timestamp order to make sure we report
     the first issue if any *)
  let addresses_to_check =
    AddressMap.fold
      (fun addr_pre addr_hist_caller to_check ->
        let to_check =
          match BaseAddressAttributes.get_must_be_valid addr_pre (pre :> BaseDomain.t).attrs with
          | None ->
              to_check
          | Some must_be_valid_data ->
              (addr_hist_caller, `MustBeValid must_be_valid_data) :: to_check
        in
        match
          BaseAddressAttributes.get_must_be_initialized addr_pre (pre :> BaseDomain.t).attrs
        with
        | None ->
            to_check
        | Some must_be_init_data ->
            (addr_hist_caller, `MustBeInitialized must_be_init_data) :: to_check )
      call_state.subst []
  in
  let timestamp_of_check = function
    | `MustBeValid (timestamp, _, _) | `MustBeInitialized (timestamp, _) ->
        timestamp
  in
  List.sort addresses_to_check ~compare:(fun (_, check1) (_, check2) ->
      (* smaller timestamp first *)
      Timestamp.compare (timestamp_of_check check1) (timestamp_of_check check2) )
  |> List.fold_result ~init:call_state.astate ~f:(fun astate ((addr_caller, hist_caller), check) ->
         let mk_access_trace callee_access_trace =
           Trace.ViaCall
             { in_call= callee_access_trace
             ; f= Call callee_proc_name
             ; location= call_location
             ; history= hist_caller }
         in
         match check with
         | `MustBeValid (_timestamp, callee_access_trace, must_be_valid_reason) ->
             let access_trace = mk_access_trace callee_access_trace in
             AddressAttributes.check_valid path access_trace addr_caller astate
             |> Result.map_error ~f:(fun (invalidation, invalidation_trace) ->
                    L.d_printfln ~color:Red "ERROR: caller's %a invalid!" AbstractValue.pp
                      addr_caller ;
                    AccessResult.ReportableError
                      { diagnostic=
                          AccessToInvalidAddress
                            { calling_context= []
                            ; invalid_address= Decompiler.find addr_caller astate
                            ; invalidation
                            ; invalidation_trace
                            ; access_trace
                            ; must_be_valid_reason }
                      ; astate } )
         | `MustBeInitialized (_timestamp, callee_access_trace) ->
             let access_trace = mk_access_trace callee_access_trace in
             AddressAttributes.check_initialized path access_trace addr_caller astate
             |> Result.map_error ~f:(fun () ->
                    L.d_printfln ~color:Red "ERROR: caller's %a is uninitialized!" AbstractValue.pp
                      addr_caller ;
                    AccessResult.ReportableError
                      { diagnostic= ReadUninitializedValue {calling_context= []; trace= access_trace}
                      ; astate } ) )


let check_all_taint_valid path callee_proc_name call_location actuals pre_post astate call_state =
  let open PulseResult.Let_syntax in
  let mk_flow_from_taint_source ~source ~destination v astate result =
    let* result in
    Recoverable
      ( result
      , [ PulseOperations.ReportableError
            { astate
            ; diagnostic=
                FlowFromTaintSource
                  {tainted= Decompiler.find v astate; source; destination; location= call_location}
            } ] )
  in
  let* astate =
    AddressMap.fold
      (fun addr_pre (addr_caller, hist_caller) astate_result ->
        let sinks, procedures =
          BaseAddressAttributes.get_must_not_be_tainted addr_pre
            (pre_post.AbductiveDomain.pre :> BaseDomain.t).attrs
        in
        let trace_via_call trace =
          Trace.ViaCall
            {in_call= trace; f= Call callee_proc_name; location= call_location; history= hist_caller}
        in
        (* Check taint flows to known/specified sinks *)
        let* astate =
          Attribute.TaintSinkSet.fold
            (fun Attribute.TaintSink.{sink; trace} astate_result ->
              let* astate = astate_result in
              let sink_and_trace = (sink, trace_via_call trace) in
              PulseTaintOperations.check_flows_wrt_sink path call_location sink_and_trace
                addr_caller astate )
            sinks astate_result
        in
        (* Report taint flows to procedures used within the callee, i.e. flows *via* the callee *)
        let taint_dependencies =
          PulseTaintOperations.gather_taint_dependencies addr_caller astate
        in
        PulseResult.list_fold taint_dependencies ~init:astate ~f:(fun astate v ->
            let sources, _ = AddressAttributes.get_taint_sources_and_sanitizers v astate in
            Attribute.TaintedSet.fold
              (fun {source; hist} result ->
                (* Do not report from data_flow_only sources - these are for reporting flows to sinks *)
                if source.data_flow_only then result
                else
                  Attribute.TaintProcedureSet.fold
                    (fun {origin; proc_name; trace} ->
                      mk_flow_from_taint_source ~source:(source, hist)
                        ~destination:(origin, proc_name, trace_via_call trace)
                        v astate )
                    procedures result )
              sources (Ok astate) ) )
      call_state.subst (Ok astate)
  in
  PulseTaintOperations.report_flows_to_callee path call_location callee_proc_name actuals
    call_state.astate astate


let isl_check_all_invalid invalid_addr_callers callee_proc_name call_location
    {AbductiveDomain.pre; _} pre_astate astate =
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
      | Recoverable _ | FatalError _ ->
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
              L.d_printfln ~color:Red "ERROR: caller's %a invalid!" AbstractValue.pp addr_caller ;
              FatalError
                ( AccessResult.ReportableError
                    { diagnostic=
                        Diagnostic.AccessToInvalidAddress
                          { calling_context= []
                          ; invalid_address= Decompiler.find addr_caller astate
                          ; invalidation
                          ; invalidation_trace
                          ; access_trace
                          ; must_be_valid_reason= None }
                    ; astate }
                , [] ) ) ) )
    invalid_addr_callers (Ok astate)


(* - read all the pre, assert validity of addresses and materializes *everything* (to throw stuff
   in the *current* pre as appropriate so that callers of the current procedure will also know
   about the deeper reads)

   - for each actual, write the post for that actual

   - if aliasing is introduced at any time then give up *)
let apply_prepost path ~is_isl_error_prepost callee_proc_name call_location ~callee_prepost:pre_post
    ~captured_formals ~captured_actuals ~formals ~actuals astate =
  L.d_printfln "Applying pre/post for %a(%a):@\n%a" Procname.pp callee_proc_name
    (Pp.seq ~sep:"," (fun f (var, _) -> Var.pp f var))
    formals AbductiveDomain.pp pre_post ;
  let empty_call_state =
    { astate
    ; subst= AddressMap.empty
    ; invalid_subst= AddressMap.empty
    ; rev_subst= AddressMap.empty
    ; visited= AddressSet.empty }
  in
  (* read the precondition *)
  match
    materialize_pre path callee_proc_name call_location pre_post ~captured_formals ~captured_actuals
      ~formals ~actuals empty_call_state
  with
  | exception Contradiction reason ->
      (* can't make sense of the pre-condition in the current context: give up on that particular
         pre/post pair *)
      L.d_printfln "Cannot apply precondition: %a" pp_contradiction reason ;
      log_contradiction reason ;
      (Unsat, Some reason)
  | result -> (
    try
      let res =
        let open PulseResult.Let_syntax in
        let* call_state = result in
        L.d_printfln "Pre applied successfully. call_state=%a" pp_call_state call_state ;
        (* only call [check_all_valid] when ISL is not active: the ISL mode generates explicit error
           specs (which we recognize here using [is_isl_error_prepost]) instead of relying on
           [check_all_valid], whereas the "normal" mode encodes some error specs implicitly in the
           ContinueProgram ok specs *)
        let* astate =
          if Config.pulse_isl then Ok call_state.astate
          else
            check_all_valid path callee_proc_name call_location pre_post call_state
            |> AccessResult.of_result
        in
        (* reset [visited] *)
        let invalid_subst = call_state.invalid_subst in
        let pre_astate = astate in
        let call_state = {call_state with astate; visited= AddressSet.empty} in
        (* apply the postcondition *)
        let* call_state, return_caller =
          apply_post path callee_proc_name call_location pre_post ~captured_formals
            ~captured_actuals ~formals ~actuals call_state
        in
        let astate =
          if Topl.is_active () then
            let keep = AbductiveDomain.get_reachable call_state.astate in
            AbductiveDomain.Topl.large_step ~call_location ~callee_proc_name
              ~substitution:call_state.subst ~keep ~path_condition:call_state.astate.path_condition
              ~callee_prepost:pre_post.AbductiveDomain.topl call_state.astate
          else call_state.astate
        in
        let astate =
          Option.fold ~init:astate return_caller ~f:(fun astate ret_v ->
              Decompiler.add_call_source (fst ret_v) (Call callee_proc_name) actuals astate )
        in
        let+ astate =
          if is_isl_error_prepost then
            isl_check_all_invalid invalid_subst callee_proc_name call_location pre_post pre_astate
              astate
          else
            (* This has to happen after the post has been applied so that we are aware of any
               sanitizers applied to tainted values too, otherwise we'll report false positives if
               the callee both taints and sanitizes a value *)
            check_all_taint_valid path callee_proc_name call_location actuals pre_post astate
              call_state
        in
        (astate, return_caller, call_state.subst)
      in
      (Sat res, None)
    with Contradiction reason ->
      L.d_printfln "Cannot apply post-condition: %a" pp_contradiction reason ;
      log_contradiction reason ;
      (Unsat, Some reason) )
