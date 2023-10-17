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
open PulseResult.Let_syntax

(** {2 machinery to apply a pre/post pair corresponding to a function's summary in a function call
    to the current state}

    *Unsafe* Stack, etc. modules used here are safe when used on the callee's summary (because
    values in summaries are all canonical) *)

module AddressSet = AbstractValue.Set
module AddressMap = AbstractValue.Map

type callee_index_to_visit =
  { addr_pre_dest: AbstractValue.t
  ; pre_hist: ValueHistory.t
  ; access_callee: Access.t
  ; addr_hist_caller: AbstractValue.t * ValueHistory.t }

(** stuff we carry around when computing the result of applying one pre/post pair *)
type call_state =
  { astate: AbductiveDomain.t  (** caller's abstract state computed so far *)
  ; subst: (AbstractValue.t * ValueHistory.t) AddressMap.t
        (** translation from callee addresses to caller addresses and their caller histories *)
  ; rev_subst: AbstractValue.t AddressMap.t
        (** the inverse translation from [subst] from caller addresses to callee addresses *)
  ; hist_map: ValueHistory.t CellId.Map.t
  ; visited: AddressSet.t
        (** set of callee addresses that have been visited already

            NOTE: this is not always equal to the domain of [rev_subst]: when applying the post we
            visit each subgraph from each formal independently so we reset [visited] between the
            visit of each formal *)
  ; array_indices_to_visit: callee_index_to_visit list  (** delayed visit for array indices *) }

let pp_hist_map fmt hist_map =
  CellId.Map.iteri hist_map ~f:(fun ~key ~data ->
      F.fprintf fmt "%a: %a," CellId.pp key ValueHistory.pp data )


let pp_call_state fmt
    ({astate; subst; rev_subst; hist_map; visited; array_indices_to_visit}
      [@warning "+missing-record-field-pattern"] ) =
  F.fprintf fmt
    "@[<v>{ astate=@[<hv2>%a@];@,\
    \ subst=@[<hv2>%a@];@,\
    \ rev_subst=@[<hv2>%a@];@,\
    \ hist_map=@[<hv2>%a@];@,\
    \ visited=@[<hv2>%a@]@,\
    \ array_indices_to_visit=@[<hv2>%a@]@,\
    \ }@]" AbductiveDomain.pp astate
    (AddressMap.pp ~pp_value:(fun fmt (addr, _) -> AbstractValue.pp fmt addr))
    subst
    (AddressMap.pp ~pp_value:AbstractValue.pp)
    rev_subst pp_hist_map hist_map AddressSet.pp visited
    (Pp.seq (fun _ _ -> ()))
    array_indices_to_visit


let pp_call_state = Pp.html_collapsible_block ~name:"Show/hide the call state" pp_call_state

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
  | DynamicTypeNeeded of AbstractValue.t Specialization.HeapPath.Map.t
  | CapturedFormalActualLength of
      { captured_formals: (Var.t * Typ.t) list
      ; captured_actuals: ((AbstractValue.t * ValueHistory.t) * Typ.t) list }
  | FormalActualLength of
      {formals: (Var.t * Typ.t) list; actuals: ((AbstractValue.t * ValueHistory.t) * Typ.t) list}
  | PathCondition

let pp_contradiction fmt = function
  | Aliasing {addr_caller; addr_callee; addr_callee'; call_state} ->
      F.fprintf fmt
        "address %a in caller already bound to %a, not %a@\nnote: current call state was %a"
        AbstractValue.pp addr_caller AbstractValue.pp addr_callee' AbstractValue.pp addr_callee
        pp_call_state call_state
  | DynamicTypeNeeded heap_paths ->
      F.fprintf fmt "Heap paths %a need to give their dynamic types"
        (Specialization.HeapPath.Map.pp ~pp_value:AbstractValue.pp)
        heap_paths
  | CapturedFormalActualLength {captured_formals; captured_actuals} ->
      F.fprintf fmt "captured formals have length %d but captured actuals have length %d"
        (List.length captured_formals) (List.length captured_actuals)
  | FormalActualLength {formals; actuals} ->
      F.fprintf fmt "formals have length %d but actuals have length %d" (List.length formals)
        (List.length actuals)
  | PathCondition ->
      F.pp_print_string fmt "path condition evaluates to false"


let log_contradiction = function
  | Aliasing _ ->
      Stats.incr_pulse_aliasing_contradictions ()
  | DynamicTypeNeeded _ ->
      ()
  | FormalActualLength _ ->
      Stats.incr_pulse_args_length_contradictions ()
  | CapturedFormalActualLength _ ->
      Stats.incr_pulse_captured_vars_length_contradictions ()
  | PathCondition ->
      ()


let is_aliasing_contradiction = function
  | Aliasing _ ->
      true
  | DynamicTypeNeeded _ | CapturedFormalActualLength _ | FormalActualLength _ | PathCondition ->
      false


let is_dynamic_type_needed_contradiction = function
  | DynamicTypeNeeded heap_paths ->
      Some heap_paths
  | Aliasing _ | CapturedFormalActualLength _ | FormalActualLength _ | PathCondition ->
      None


exception Contradiction of contradiction

let raise_if_unsat contradiction = function
  | Sat x ->
      x
  | Unsat ->
      raise_notrace (Contradiction contradiction)


let fold_globals_of_callee_stack {PathContext.timestamp} call_loc stack call_state ~f =
  (* safe to use [UnsafeStack] because these stacks come from a summary *)
  PulseResult.container_fold ~fold:(IContainer.fold_of_pervasives_map_fold UnsafeStack.fold)
    stack ~init:call_state ~f:(fun call_state (var, stack_value) ->
      match var with
      | Var.ProgramVar pvar when Pvar.is_global pvar ->
          let call_state, addr_hist_caller =
            let astate, var_value =
              Stack.eval
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
      let path_condition, new_eqs =
        Formula.and_equal_vars addr_caller0 addr_caller'
          call_state.astate.AbductiveDomain.path_condition
        |> raise_if_unsat PathCondition
      in
      let+ astate =
        AbductiveDomain.incorporate_new_eqs new_eqs call_state.astate
        |> raise_if_unsat PathCondition |> AccessResult.of_abductive_result
      in
      {call_state with astate= AbductiveDomain.set_path_condition path_condition astate}
  | _ ->
      Ok call_state


let visit call_state ~pre ~addr_callee cell_id ~addr_hist_caller =
  let addr_caller = fst addr_hist_caller in
  let* call_state =
    match AddressMap.find_opt addr_caller call_state.rev_subst with
    | Some addr_callee' when not (AbstractValue.equal addr_callee addr_callee') ->
        if
          (* [addr_caller] corresponds to several values in the callee, see if that's a problem for
             applying the pre-condition, i.e. if both values are addresses in the callee's heap,
             which means they must be disjoint. If so, raise a contradiction, but if not then
             continue as it just means that the callee doesn't care about the value of these
             variables, but record that they are equal. *)
          UnsafeMemory.mem addr_callee pre.BaseDomain.heap
          && UnsafeMemory.mem addr_callee' pre.BaseDomain.heap
        then
          raise_notrace
            (Contradiction (Aliasing {addr_caller; addr_callee; addr_callee'; call_state}))
        else and_aliasing_arith ~addr_callee:addr_callee' ~addr_caller0:addr_caller call_state
    | _ ->
        Ok call_state
  in
  let+ call_state = and_aliasing_arith ~addr_callee ~addr_caller0:addr_caller call_state in
  if AddressSet.mem addr_callee call_state.visited then (`AlreadyVisited, call_state)
  else
    ( `NotAlreadyVisited
    , { call_state with
        visited= AddressSet.add addr_callee call_state.visited
      ; subst= AddressMap.add addr_callee addr_hist_caller call_state.subst
      ; rev_subst= AddressMap.add addr_caller addr_callee call_state.rev_subst
      ; hist_map=
          Option.fold cell_id ~init:call_state.hist_map ~f:(fun hist_map cell_id ->
              CellId.Map.add_exn hist_map ~key:cell_id ~data:(snd addr_hist_caller) ) } )


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


let call_state_subst_find_or_new call_state addr_callee ~default_hist_caller =
  let new_subst, addr_hist_caller =
    subst_find_or_new call_state.subst addr_callee ~default_hist_caller
  in
  if phys_equal new_subst call_state.subst then (call_state, addr_hist_caller)
  else ({call_state with subst= new_subst}, addr_hist_caller)


let translate_access_to_caller subst (access_callee : Access.t) : _ * Access.t =
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
let rec materialize_pre_from_address ~pre ~addr_pre cell_id ~addr_hist_caller call_state =
  let* visited_status, call_state =
    visit call_state ~pre ~addr_callee:addr_pre cell_id ~addr_hist_caller
  in
  match visited_status with
  | `AlreadyVisited ->
      Ok call_state
  | `NotAlreadyVisited -> (
      L.d_printfln "visiting from address %a <-> %a" AbstractValue.pp addr_pre AbstractValue.pp
        (fst addr_hist_caller) ;
      match UnsafeMemory.find_opt addr_pre pre.BaseDomain.heap with
      | None ->
          Ok call_state
      | Some edges_pre ->
          PulseResult.container_fold ~fold:UnsafeMemory.Edges.fold ~init:call_state edges_pre
            ~f:(fun call_state (access_callee, (addr_pre_dest, pre_hist)) ->
              match (access_callee : Access.t) with
              | ArrayAccess _ ->
                  Ok
                    { call_state with
                      array_indices_to_visit=
                        {addr_pre_dest; pre_hist; access_callee; addr_hist_caller}
                        :: call_state.array_indices_to_visit }
              | FieldAccess _ | TakeAddress | Dereference ->
                  (* only array accessess depend on abstract values and need translation *)
                  let access_caller = access_callee in
                  let astate, addr_hist_dest_caller =
                    Memory.eval_edge addr_hist_caller access_caller call_state.astate
                  in
                  let call_state = {call_state with astate} in
                  materialize_pre_from_address ~pre ~addr_pre:addr_pre_dest
                    (ValueHistory.get_cell_id pre_hist)
                    ~addr_hist_caller:addr_hist_dest_caller call_state ) )


let materialize_pre_from_array_index ~pre {addr_pre_dest; pre_hist; access_callee; addr_hist_caller}
    call_state =
  let subst, access_caller = translate_access_to_caller call_state.subst access_callee in
  let astate, addr_hist_dest_caller =
    Memory.eval_edge addr_hist_caller access_caller call_state.astate
  in
  let call_state = {call_state with astate; subst} in
  (* HACK: we should probably visit the value in the (array) access too, but since it's a value
     normally it shouldn't appear in the heap anyway so there should be nothing to visit. *)
  materialize_pre_from_address ~pre ~addr_pre:addr_pre_dest
    (ValueHistory.get_cell_id pre_hist)
    ~addr_hist_caller:addr_hist_dest_caller call_state


let materialize_pre_from_array_indices ~pre call_state =
  let+ call_state =
    PulseResult.list_fold call_state.array_indices_to_visit ~init:call_state
      ~f:(fun call_state array_index_to_translate ->
        materialize_pre_from_array_index ~pre array_index_to_translate call_state )
  in
  {call_state with array_indices_to_visit= []}


let callee_deref_non_c_struct addr typ astate =
  match typ.Typ.desc with
  | Tstruct _ ->
      Some (addr, None)
  | _ ->
      UnsafeMemory.find_edge_opt addr Dereference astate
      |> Option.map ~f:(fun (value, hist) -> (value, ValueHistory.get_cell_id hist))


(** materialize subgraph of [pre] rooted at the address represented by a [formal] parameter that has
    been instantiated with the corresponding [actual] into the current state [call_state.astate] *)
let materialize_pre_from_actual ~pre ~formal:(formal, typ) ~actual:(actual, _) call_state =
  L.d_printfln "Materializing PRE from [%a <- %a]" Var.pp formal AbstractValue.pp (fst actual) ;
  (let open IOption.Let_syntax in
   let* addr_formal_pre, _ = UnsafeStack.find_opt formal pre.BaseDomain.stack in
   let+ formal_pre, cell_id = callee_deref_non_c_struct addr_formal_pre typ pre.BaseDomain.heap in
   materialize_pre_from_address ~pre ~addr_pre:formal_pre cell_id ~addr_hist_caller:actual
     call_state )
  |> function Some result -> result | None -> Ok call_state


let materialize_pre_for_captured_vars ~pre ~captured_formals ~captured_actuals call_state =
  match
    PulseResult.list_fold2 captured_formals captured_actuals ~init:call_state
      ~f:(fun call_state formal actual ->
        materialize_pre_from_actual ~pre ~formal ~actual call_state )
  with
  | Unequal_lengths ->
      raise_notrace (Contradiction (CapturedFormalActualLength {captured_formals; captured_actuals}))
  | Ok result ->
      result


let materialize_pre_for_parameters ~pre ~formals ~actuals call_state =
  (* For each [(formal, actual)] pair, resolve them to addresses in their respective states then
     call [materialize_pre_from] on them.  Give up if calling the function introduces aliasing.
  *)
  match
    PulseResult.list_fold2 formals actuals ~init:call_state ~f:(fun call_state formal actual ->
        materialize_pre_from_actual ~pre ~formal ~actual call_state )
  with
  | Unequal_lengths ->
      raise_notrace (Contradiction (FormalActualLength {formals; actuals}))
  | Ok result ->
      result


let materialize_pre_for_globals path call_location ~pre call_state =
  fold_globals_of_callee_stack path call_location pre.BaseDomain.stack call_state
    ~f:(fun _var ~stack_value:(addr_pre, pre_hist) ~addr_hist_caller call_state ->
      materialize_pre_from_address ~pre ~addr_pre
        (ValueHistory.get_cell_id pre_hist)
        ~addr_hist_caller call_state )


let conjoin_callee_arith pre_or_post callee_path_condition call_state =
  L.d_printfln "applying callee path condition: (%a)[%a]" Formula.pp callee_path_condition
    (AddressMap.pp ~pp_value:(fun fmt (addr, _) -> AbstractValue.pp fmt addr))
    call_state.subst ;
  let subst, path_condition, new_eqs =
    match pre_or_post with
    | `Pre ->
        Formula.and_callee_pre call_state.subst call_state.astate.path_condition
          ~callee:callee_path_condition
        |> raise_if_unsat PathCondition
    | `Post ->
        Formula.and_callee_post call_state.subst call_state.astate.path_condition
          ~callee:callee_path_condition
        |> raise_if_unsat PathCondition
  in
  let astate = AbductiveDomain.set_path_condition path_condition call_state.astate in
  let+ astate =
    AbductiveDomain.incorporate_new_eqs new_eqs astate
    |> raise_if_unsat PathCondition |> AccessResult.of_abductive_result
  in
  {call_state with astate; subst}


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


let add_attributes pre_or_post {PathContext.timestamp} callee_proc_name call_location
    callee_attributes call_state =
  let add_for_address callee_attrs (addr_caller, caller_history) call_state =
    let call_state, attrs_caller =
      caller_attrs_of_callee_attrs timestamp callee_proc_name call_location caller_history
        call_state callee_attrs
    in
    let astate = call_state.astate in
    let astate =
      match pre_or_post with
      | `Post when Attributes.is_java_resource_released attrs_caller ->
          PulseOperations.java_resource_release ~recursive:true addr_caller astate
      | `Post when Attributes.is_csharp_resource_released attrs_caller ->
          PulseOperations.csharp_resource_release ~recursive:true addr_caller astate
      | _ ->
          astate
    in
    let abduce_or_add =
      match pre_or_post with
      | `Pre ->
          AddressAttributes.abduce_all
      | `Post ->
          AddressAttributes.add_all
    in
    let astate = abduce_or_add addr_caller attrs_caller astate in
    if phys_equal astate call_state.astate then call_state else {call_state with astate}
  in
  UnsafeAttributes.fold
    (fun addr_callee callee_attrs call_state ->
      let call_state, addr_hist_caller =
        call_state_subst_find_or_new call_state addr_callee ~default_hist_caller:ValueHistory.epoch
      in
      add_for_address callee_attrs addr_hist_caller call_state )
    callee_attributes call_state


let materialize_pre path callee_proc_name call_location callee_summary ~captured_formals
    ~captured_actuals ~formals ~actuals call_state =
  PerfEvent.(log (fun logger -> log_begin_event logger ~name:"pulse call pre" ())) ;
  let r =
    let callee_precondition = AbductiveDomain.Summary.get_pre callee_summary in
    (* first make as large a mapping as we can between callee values and caller values... *)
    materialize_pre_for_parameters ~pre:callee_precondition ~formals ~actuals call_state
    >>= materialize_pre_for_captured_vars ~pre:callee_precondition ~captured_formals
          ~captured_actuals
    >>= materialize_pre_for_globals path call_location ~pre:callee_precondition
    >>= (* ...then relational arithmetic constraints in the callee's attributes will make sense in
           terms of the caller's values *)
    conjoin_callee_arith `Pre (AbductiveDomain.Summary.get_path_condition callee_summary)
    >>= materialize_pre_from_array_indices ~pre:callee_precondition
    >>| add_attributes `Pre path callee_proc_name call_location callee_precondition.attrs
  in
  PerfEvent.(log (fun logger -> log_end_event logger ())) ;
  r


(* {3 applying the post to the current state} *)

(* Don't use visit when applying the post, it's too heavy due to the careful handling of values it
   does when exploring the pre. Instead use [subst_find_or_new] or
   [call_state_subst_find_or_new]. *)
let visit () = `UseSubstFindOrNewInsteadForThePost [@@warning "-unused-value-declaration"]

let is_cell_read_only ~edges_pre_opt ~cell_post:(_, attrs_post) =
  match edges_pre_opt with None -> false | Some _ -> not (Attributes.is_modified attrs_post)


let delete_edges_in_callee_pre_from_caller ~edges_pre_opt addr_caller call_state =
  match
    UnsafeMemory.find_opt addr_caller (call_state.astate.AbductiveDomain.post :> BaseDomain.t).heap
  with
  | None ->
      (call_state.subst, BaseMemory.Edges.empty)
  | Some old_post_edges -> (
    match edges_pre_opt with
    | None ->
        (call_state.subst, old_post_edges)
    | Some edges_pre ->
        let subst, translated_accesses_pre =
          UnsafeMemory.Edges.fold ~init:(call_state.subst, AccessSet.empty) edges_pre
            ~f:(fun (subst, accesses) (access_callee, _) ->
              let subst, access = translate_access_to_caller subst access_callee in
              (subst, AccessSet.add access accesses) )
        in
        let post_edges =
          (* abuse of [UnsafeMemory.Edges]; it's fine because [post_edges] is used to *write* to
             the current state. Edges are allowed to contain non-normalized values (though it
             shouldn't even be the case here!) since we'll normalize them on the fly on read. *)
          UnsafeMemory.Edges.filter old_post_edges ~f:(fun (access_caller, _) ->
              (* delete edge if some edge for the same access exists in the pre *)
              not (AccessSet.mem access_caller translated_accesses_pre) )
        in
        (subst, post_edges) )


let record_post_cell ({PathContext.timestamp} as path) callee_proc_name call_loc ~edges_pre_opt
    ~edges_callee_post (addr_caller, hist_caller) call_state =
  let subst, translated_post_edges =
    UnsafeMemory.Edges.fold ~init:(call_state.subst, BaseMemory.Edges.empty) edges_callee_post
      ~f:(fun (subst, translated_edges) (access_callee, (addr_callee, trace_post)) ->
        let subst, (addr_curr, hist_curr) =
          subst_find_or_new subst addr_callee ~default_hist_caller:hist_caller
        in
        let hist_caller =
          let open Option.Monad_infix in
          ValueHistory.get_cell_id trace_post
          >>= CellId.Map.find call_state.hist_map
          |> Option.value ~default:hist_curr
        in
        let subst, access = translate_access_to_caller subst access_callee in
        let translated_edges =
          UnsafeMemory.Edges.add access
            ( addr_curr
            , ValueHistory.sequence ~context:path.conditions
                (Call {f= Call callee_proc_name; location= call_loc; in_call= trace_post; timestamp})
                hist_caller )
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


let rec record_post_for_address path callee_proc_name call_loc callee_summary ~addr_callee
    ~addr_hist_caller call_state =
  L.d_printf "visiting %a<->%a.. " AbstractValue.pp addr_callee AbstractValue.pp
    (fst addr_hist_caller) ;
  if AddressSet.mem addr_callee call_state.visited then Ok call_state
  else
    let call_state = {call_state with visited= AddressSet.add addr_callee call_state.visited} in
    match
      AbductiveDomain.find_post_cell_opt addr_callee
        (callee_summary : AbductiveDomain.Summary.t :> AbductiveDomain.t)
    with
    | None ->
        Ok call_state
    | Some ((edges_callee_post, _) as cell_callee_post) ->
        let edges_pre_opt =
          UnsafeMemory.find_opt addr_callee
            (AbductiveDomain.Summary.get_pre callee_summary).BaseDomain.heap
        in
        let call_state_after_post =
          if is_cell_read_only ~edges_pre_opt ~cell_post:cell_callee_post then (
            L.d_printfln "cell at %a is read-only, not modifying@\n" AbstractValue.pp addr_callee ;
            call_state )
          else
            record_post_cell path callee_proc_name call_loc ~edges_pre_opt addr_hist_caller
              ~edges_callee_post call_state
        in
        UnsafeMemory.Edges.fold ~init:(Ok call_state_after_post) edges_callee_post
          ~f:(fun call_state (_access, (addr_callee_dest, _)) ->
            let* call_state in
            let call_state, addr_hist_curr_dest =
              call_state_subst_find_or_new call_state addr_callee_dest
                ~default_hist_caller:(snd addr_hist_caller)
            in
            record_post_for_address path callee_proc_name call_loc callee_summary
              ~addr_callee:addr_callee_dest ~addr_hist_caller:addr_hist_curr_dest call_state )


let apply_post_from_callee_pre path callee_proc_name call_location callee_summary call_state =
  UnsafeMemory.fold
    (fun addr_callee _edges_callee_pre call_state ->
      let* call_state in
      match AddressMap.find_opt addr_callee call_state.subst with
      | Some addr_hist_caller ->
          record_post_for_address path callee_proc_name call_location callee_summary ~addr_callee
            ~addr_hist_caller call_state
      | None ->
          Ok call_state )
    (AbductiveDomain.Summary.get_pre callee_summary).heap (Ok call_state)


let apply_post_from_callee_post path callee_proc_name call_location callee_summary call_state =
  UnsafeMemory.fold
    (fun addr_callee _edges_callee call_state_result ->
      if AddressSet.mem addr_callee call_state.visited then call_state_result
      else
        let* call_state = call_state_result in
        let call_state, addr_hist_caller =
          call_state_subst_find_or_new call_state addr_callee
            ~default_hist_caller:ValueHistory.epoch
        in
        record_post_for_address path callee_proc_name call_location callee_summary ~addr_callee
          ~addr_hist_caller call_state )
    (AbductiveDomain.Summary.get_post callee_summary).heap (Ok call_state)


let record_skipped_calls callee_proc_name call_loc callee_summary call_state =
  let callee_skipped_calls =
    SkippedCalls.map
      (fun trace ->
        Trace.ViaCall
          {f= Call callee_proc_name; location= call_loc; history= ValueHistory.epoch; in_call= trace}
        )
      (AbductiveDomain.Summary.get_skipped_calls callee_summary)
  in
  let astate = AbductiveDomain.add_skipped_calls callee_skipped_calls call_state.astate in
  {call_state with astate}


let record_need_closure_specialization callee_summary call_state =
  if AbductiveDomain.Summary.need_closure_specialization callee_summary then
    {call_state with astate= AbductiveDomain.set_need_closure_specialization call_state.astate}
  else call_state


let apply_unknown_effects callee_summary call_state =
  let open IOption.Let_syntax in
  L.d_printfln "Applying unknown effects, call_state before = %a" pp_call_state call_state ;
  let is_modified_by_call addr_caller access =
    match AddressMap.find_opt addr_caller call_state.rev_subst with
    | None ->
        false
    | Some addr_callee ->
        let edges_callee_pre =
          UnsafeMemory.find_opt addr_callee (AbductiveDomain.Summary.get_pre callee_summary).heap
          |> Option.value ~default:BaseMemory.Edges.empty
        in
        let edges_callee_post =
          UnsafeMemory.find_opt addr_callee (AbductiveDomain.Summary.get_post callee_summary).heap
          |> Option.value ~default:BaseMemory.Edges.empty
        in
        let pre_value = UnsafeMemory.Edges.find_opt access edges_callee_pre >>| fst in
        let post_value = UnsafeMemory.Edges.find_opt access edges_callee_post >>| fst in
        (* havoc only fields that haven't been havoc'd already during the call *)
        not (Option.equal AbstractValue.equal post_value pre_value)
  in
  let astate =
    BaseAddressAttributes.fold
      (fun addr_callee attrs astate ->
        (let* _, havoc_hist = Attributes.get_unknown_effect attrs in
         let+ addr_caller, _ =
           AddressMap.find_opt (CanonValue.downcast addr_callee) call_state.subst
         in
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
      (AbductiveDomain.Summary.get_post callee_summary).attrs call_state.astate
  in
  {call_state with astate}


let read_return_value {PathContext.conditions; timestamp} callee_proc_name call_loc
    (callee_summary : AbductiveDomain.Summary.t) call_state =
  let return_var = Var.of_pvar (Pvar.get_ret_pvar callee_proc_name) in
  match Stack.find_opt return_var (callee_summary :> AbductiveDomain.t) with
  | None ->
      (call_state, None)
  | Some (addr_return, _) -> (
    match
      UnsafeMemory.find_edge_opt addr_return Dereference
        (AbductiveDomain.Summary.get_post callee_summary).BaseDomain.heap
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
        L.d_printfln_escaped "Found [return] <-> %a" AbstractValue.pp return_caller ;
        (* need to add the call to the returned history too *)
        let return_caller_hist =
          ValueHistory.sequence ~context:conditions
            (Call
               {f= Call callee_proc_name; location= call_loc; in_call= return_callee_hist; timestamp}
            )
            return_caller_hist
        in
        ( call_state
        , Some
            (Formula.get_var_repr call_state.astate.path_condition return_caller, return_caller_hist)
        ) )


let apply_post path callee_proc_name call_location callee_summary call_state =
  PerfEvent.(log (fun logger -> log_begin_event logger ~name:"pulse call post" ())) ;
  let normalize_subst_for_post call_state =
    (* Now that all equalities are deduced and set, we can exploit them to use the canonical
       representation of values in the subst and rev_subst maps *)
    let get_var_repr v = Formula.get_var_repr call_state.astate.AbductiveDomain.path_condition v in
    let subst =
      AddressMap.map
        (fun (v_caller, hist) ->
          let v_caller_canon = get_var_repr v_caller in
          (v_caller_canon, hist) )
        call_state.subst
    in
    let rev_subst =
      AddressMap.fold
        (fun v_caller v_callee rev_subst ->
          let v_caller_canon = get_var_repr v_caller in
          AddressMap.add v_caller_canon v_callee rev_subst )
        call_state.rev_subst AddressMap.empty
    in
    {call_state with subst; rev_subst}
  in
  let r =
    (* subst was suitable for pre but post may know more equalities, take them into account now *)
    normalize_subst_for_post call_state
    |> apply_unknown_effects callee_summary
    |> apply_post_from_callee_pre path callee_proc_name call_location callee_summary
    >>= apply_post_from_callee_post path callee_proc_name call_location callee_summary
    >>| add_attributes `Post path callee_proc_name call_location
          (AbductiveDomain.Summary.get_post callee_summary).attrs
    >>| record_skipped_calls callee_proc_name call_location callee_summary
    >>| record_need_closure_specialization callee_summary
    >>= conjoin_callee_arith `Post (AbductiveDomain.Summary.get_path_condition callee_summary)
    (* normalize subst again now that we know more arithmetic facts *)
    >>| normalize_subst_for_post
    >>| read_return_value path callee_proc_name call_location callee_summary
  in
  PerfEvent.(log (fun logger -> log_end_event logger ())) ;
  r


let check_all_valid path callee_proc_name call_location ~pre call_state =
  (* collect all the checks to perform then do each check in timestamp order to make sure we report
     the first issue if any *)
  let addresses_to_check =
    AddressMap.fold
      (fun addr_pre addr_hist_caller to_check ->
        let to_check =
          match UnsafeAttributes.get_must_be_valid addr_pre pre.BaseDomain.attrs with
          | None ->
              to_check
          | Some must_be_valid_data ->
              (addr_hist_caller, `MustBeValid must_be_valid_data) :: to_check
        in
        match UnsafeAttributes.get_must_be_initialized addr_pre pre.BaseDomain.attrs with
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
             |> Result.map_error ~f:(fun typ ->
                    L.d_printfln ~color:Red "ERROR: caller's %a is uninitialized!" AbstractValue.pp
                      addr_caller ;
                    AccessResult.ReportableError
                      { diagnostic= ReadUninitialized {typ; calling_context= []; trace= access_trace}
                      ; astate } ) )


let check_config_usage_at_call location ~pre:{BaseDomain.attrs= pre_attrs} subst astate =
  AddressMap.fold
    (fun addr_pre addr_hist acc ->
      Option.value_map (UnsafeAttributes.get_used_as_branch_cond addr_pre pre_attrs) ~default:acc
        ~f:(fun (pname_using_config, branch_location, trace) ->
          let* acc in
          PulseOperations.check_used_as_branch_cond addr_hist ~pname_using_config ~branch_location
            ~location trace acc ) )
    subst (Ok astate)


let check_all_taint_valid path callee_proc_name call_location callee_summary astate call_state =
  AddressMap.fold
    (fun addr_pre ((_, hist_caller) as addr_hist_caller) astate_result ->
      let sinks =
        UnsafeAttributes.get_must_not_be_tainted addr_pre
          (AbductiveDomain.Summary.get_pre callee_summary).attrs
      in
      let trace_via_call trace =
        Trace.ViaCall
          {in_call= trace; f= Call callee_proc_name; location= call_location; history= hist_caller}
      in
      Attribute.TaintSinkSet.fold
        (fun Attribute.TaintSink.{sink; trace} astate_result ->
          let* astate = astate_result in
          let sink_and_trace = (sink, trace_via_call trace) in
          let+ _, astate =
            PulseTaintOperations.check_flows_wrt_sink path call_location ~sink:sink_and_trace
              ~source:addr_hist_caller astate
          in
          astate )
        sinks astate_result )
    call_state.subst (Ok astate)


(* - read all the pre, assert validity of addresses and materializes *everything* (to throw stuff
   in the *current* pre as appropriate so that callers of the current procedure will also know
   about the deeper reads)

   - for each actual, write the post for that actual

   - if aliasing is introduced at any time then give up *)
let apply_summary path callee_proc_name call_location ~callee_summary ~captured_formals
    ~captured_actuals ~formals ~actuals astate =
  let aux () =
    let empty_call_state =
      { astate
      ; subst= AddressMap.empty
      ; rev_subst= AddressMap.empty
      ; hist_map= CellId.Map.empty
      ; visited= AddressSet.empty
      ; array_indices_to_visit= [] }
    in
    (* read the precondition *)
    match
      materialize_pre path callee_proc_name call_location callee_summary ~captured_formals
        ~captured_actuals ~formals ~actuals empty_call_state
    with
    | exception Contradiction reason ->
        (* can't make sense of the pre-condition in the current context: give up on that particular
           pre/post pair *)
        L.d_printfln ~color:Orange "Cannot apply precondition: %a@\n" pp_contradiction reason ;
        log_contradiction reason ;
        (Unsat, Some reason)
    | result -> (
      try
        let subst =
          (let open IOption.Let_syntax in
           let+ call_state = PulseResult.ok result in
           call_state.subst )
          |> Option.value ~default:AbstractValue.Map.empty
        in
        let res =
          let* call_state = result in
          L.d_printfln "Pre applied successfully, call_state after = %a" pp_call_state call_state ;
          let pre = AbductiveDomain.Summary.get_pre callee_summary in
          let* astate =
            check_all_valid path callee_proc_name call_location ~pre call_state
            |> AccessResult.of_result
          in
          let* astate = check_config_usage_at_call call_location ~pre call_state.subst astate in
          (* reset [visited] *)
          let call_state = {call_state with astate; visited= AddressSet.empty} in
          (* apply the postcondition *)
          let* call_state, return_caller =
            apply_post path callee_proc_name call_location callee_summary call_state
          in
          let astate =
            if Topl.is_active () then
              let callee_is_manifest = PulseArithmetic.is_manifest callee_summary in
              AbductiveDomain.Topl.large_step ~call_location ~callee_proc_name
                ~substitution:call_state.subst
                ~callee_summary:(AbductiveDomain.Summary.get_topl callee_summary)
                ~callee_is_manifest call_state.astate
            else call_state.astate
          in
          let astate =
            Option.fold ~init:astate return_caller ~f:(fun astate ret_v ->
                Decompiler.add_call_source (fst ret_v) (Call callee_proc_name) actuals astate )
          in
          let+ astate =
            (* This has to happen after the post has been applied so that we are aware of any
               sanitizers applied to tainted values too, otherwise we'll report false positives if
               the callee both taints and sanitizes a value *)
            check_all_taint_valid path callee_proc_name call_location callee_summary astate
              call_state
          in
          (astate, return_caller, call_state.subst, call_state.hist_map)
        in
        let contradiciton =
          let callee_heap_paths =
            AbductiveDomain.Summary.heap_paths_that_need_dynamic_type_specialization callee_summary
          in
          if Specialization.HeapPath.Map.is_empty callee_heap_paths then None
          else
            let caller_heap_paths =
              Specialization.HeapPath.Map.fold
                (fun heap_path addr map ->
                  match AbstractValue.Map.find_opt addr subst with
                  | Some (addr_in_caller, _) ->
                      L.d_printfln
                        "dynamic type is required for address %a reachable from heap path %a in \
                         callee (%a in caller)"
                        AbstractValue.pp addr Specialization.HeapPath.pp heap_path AbstractValue.pp
                        addr_in_caller ;
                      Specialization.HeapPath.Map.add heap_path addr_in_caller map
                  | None ->
                      L.d_printfln
                        "dynamic type is required for address %a reachable from heap path %a in \
                         callee (not found in caller)"
                        AbstractValue.pp addr Specialization.HeapPath.pp heap_path ;
                      map )
                callee_heap_paths Specialization.HeapPath.Map.empty
            in
            Some (DynamicTypeNeeded caller_heap_paths)
        in
        (Sat res, contradiciton)
      with Contradiction reason ->
        L.d_printfln "Cannot apply post-condition: %a" pp_contradiction reason ;
        log_contradiction reason ;
        (Unsat, Some reason) )
  in
  let pp_formals = Pp.seq ~sep:"," (fun f (var, _) -> Var.pp f var) in
  let pp_summary =
    Pp.html_collapsible_block ~name:"Show/hide the summary" AbductiveDomain.Summary.pp
  in
  L.d_with_indent ~collapsible:true "Applying pre/post for %a(%a):" Procname.pp callee_proc_name
    pp_formals formals ~f:(fun () ->
      L.d_printfln "%a" pp_summary callee_summary ;
      aux () )


let merge_contradictions contradiction1 contradiction2 =
  match (contradiction1, contradiction2) with
  | None, contradiction
  | contradiction, None
  | (Some (Aliasing _) as contradiction), _
  | _, (Some (Aliasing _) as contradiction) ->
      contradiction
  | Some (DynamicTypeNeeded heapmap1), Some (DynamicTypeNeeded heapmap2) ->
      let heapmap =
        Specialization.HeapPath.Map.fold
          (fun path addr map ->
            (* we may end up in a strange situation where [path] is bound in both
               [heapmap1] and [heapmap2], but since [path] is a heap path in the
               precondition space, we do not expect the binding to matter *)
            Specialization.HeapPath.Map.add path addr map )
          heapmap2 heapmap1
      in
      Some (DynamicTypeNeeded heapmap)
  | Some (DynamicTypeNeeded heapmap), _ | _, Some (DynamicTypeNeeded heapmap) ->
      Some (DynamicTypeNeeded heapmap)
  | contradiction, _ ->
      contradiction
