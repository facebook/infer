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
    values in summaries are all canonical) but not for their corresponding caller values because
    equalities can be added as we explore the summary. *)

module AddressSet = AbstractValue.Set
module AddressMap = AbstractValue.Map
module HeapPath = Specialization.HeapPath

module LazyHeapPath : sig
  type t

  val pp : F.formatter -> t -> unit

  val from_pvar : Pvar.t -> t

  val unsupported : t

  val push : Access.t -> t -> t

  val force : t -> HeapPath.t option
  (** returns None if the stored path contains unsupported memory accesses *)
end = struct
  type t = Supported of {stack: Access.t list; pvar: Pvar.t} | Unsupported

  let pp fmt = function
    | Unsupported ->
        F.pp_print_string fmt "unsupported"
    | Supported {stack; pvar} ->
        Pvar.pp Pp.text fmt pvar ;
        List.iter stack ~f:(fun access -> F.fprintf fmt " -> %a" Access.pp access)


  let from_pvar pvar = Supported {stack= []; pvar}

  let unsupported = Unsupported

  let push access = function
    | Unsupported ->
        Unsupported
    | Supported {stack; pvar} ->
        Supported {stack= access :: stack; pvar}


  let force = function
    | Unsupported ->
        None
    | Supported {stack; pvar} ->
        (* Note: Specialization.HeapPath.t are reversed *)
        List.fold_left stack ~init:(Some (HeapPath.Pvar pvar)) ~f:(fun opt_path access ->
            Option.bind opt_path ~f:(fun path ->
                match access with
                | Access.FieldAccess fieldname ->
                    Some (HeapPath.FieldAccess (fieldname, path))
                | Access.Dereference ->
                    Some (HeapPath.Dereference path)
                | _ ->
                    None ) )
end

type callee_index_to_visit =
  { addr_pre_dest: AbstractValue.t
  ; pre_hist: ValueHistory.t
  ; access_callee: Access.t
  ; addr_hist_caller: AbstractValue.t * ValueHistory.t }

module Unsafe : sig
  (** Opaque because we need to deal with on-the-fly normalization of caller values; secretely a
      [(AbstractValue.t * ValueHistory.t) AddressMap.t]: a map from callee addresses to caller
      addresses and a candidate history (see [hist_map] below: this history may not be accurate in
      general). The caller values stored in the map may need additional normalization as we explore
      more of the summary hence why the type is kept abstract. *)
  type to_caller_subst

  val pp_to_caller_subst : F.formatter -> to_caller_subst -> unit

  val empty_to_caller_subst : to_caller_subst

  val add_to_caller_subst :
    AbstractValue.t -> AbstractValue.t * ValueHistory.t -> to_caller_subst -> to_caller_subst

  val to_caller_subst_fold_constant_astate :
       AbductiveDomain.t
    -> (AbstractValue.t -> AbstractValue.t * ValueHistory.t -> 'a -> 'a)
    -> to_caller_subst
    -> 'a
    -> 'a
  (** [AddressMap.fold] when the abstract state remains constant over the fold; the abstract state
      is used to normalize caller values *)

  val to_caller_subst_fold_astate_result :
       (   AbstractValue.t
        -> AbstractValue.t * ValueHistory.t
        -> AbductiveDomain.t
        -> AbductiveDomain.t AccessResult.t )
    -> to_caller_subst
    -> AbductiveDomain.t
    -> AbductiveDomain.t AccessResult.t
  (** specific but common fold operation where the accumulator is a
      [AbductiveDomain.t AccessResult.t] that we can use for normalizing caller values *)

  val normalize_to_caller_subst :
    AbductiveDomain.t -> to_caller_subst -> (AbstractValue.t * ValueHistory.t) AddressMap.t
  (** eagerly normalize the substitution; returns a normalized map *)

  val raw_map_of_to_caller_subst :
    to_caller_subst -> (AbstractValue.t * ValueHistory.t) AddressMap.t
  (** escape hatch for APIs that want a map but don't need normalization *)

  val to_caller_subst_of_raw_map :
    (AbstractValue.t * ValueHistory.t) AddressMap.t -> to_caller_subst

  (** stuff we carry around when computing the result of applying one pre/post pair *)
  type call_state =
    { astate: AbductiveDomain.t  (** caller's abstract state computed so far *)
    ; subst: to_caller_subst
          (** translation from callee addresses to caller addresses and their caller histories *)
    ; rev_subst: (AbstractValue.t * LazyHeapPath.t) AddressMap.t
          (** the inverse translation from [subst] from caller addresses to callee addresses. We
              also store the path that leads to the caller adresses in the caller heap *)
    ; hist_map: ValueHistory.t CellId.Map.t
          (** The caller history that corresponds to a specific memory cell in the callee. The
              histories in [subst] are unreliable because the same value can appear several times in
              the pre of the summary and correspond to different histories in the caller. However,
              [subst] only remembers one caller history per callee value. The correct thing to do
              instead is this mapping. *)
    ; visited: AddressSet.t
          (** set of callee addresses that have been visited already

              NOTE: this is not always equal to the domain of [rev_subst]: when applying the post we
              visit each subgraph from each formal independently so we reset [visited] between the
              visit of each formal *)
    ; array_indices_to_visit: callee_index_to_visit list  (** delayed visit for array indices *)
    ; first_error: AbstractValue.t option
          (** during summary application, more precisely when we "materialize" the precondition of
              the summary, we may notice that we cannot follow some memory edges from an address
              because it is invalid in the current state. Then we know the summary isn't going to
              make sense for the caller, or lead to an error. In that case we stop exploring the
              precondition from that particular address. If all goes well we should pick up an error
              (or a contradiction) at the end of applying the precondition. We set this field to
              make sure we actually do, so in case something goes wrong we don't just continue with
              a broken (partial) summary application. *)
    ; aliases: HeapPath.Set.t HeapPath.Map.t
          (** if an alias was detected between a [addr_callee] and [addr_callee'] with heap path
              accesses [path] nad [path'], then [aliases[addr_callee]] contains [addr_callee'],
              where [rev_subst[addr_caller] = (addr_callee, path)]. *) }

  val to_caller_value_ :
       AbductiveDomain.t
    -> to_caller_subst
    -> AbstractValue.t
    -> (AbstractValue.t * ValueHistory.t) option
  (** query the [to_caller_subst] map and normalizes the caller value *)

  val to_caller_value : call_state -> AbstractValue.t -> (AbstractValue.t * ValueHistory.t) option
  (** apply [to_caller_value_] with the abstract state and substitution found inside the
      [call_state] *)

  val incorporate_new_eqs : Formula.new_eqs -> call_state -> call_state AccessResult.t SatUnsat.t
  (** update the internal abstract state and the maps with new equality information *)
end = struct
  let canon_fst astate pair =
    ( AbductiveDomain.CanonValue.canon_fst' astate pair
      : AbductiveDomain.CanonValue.t * _
      :> AbstractValue.t * _ )


  type to_caller_subst = (AbstractValue.t * ValueHistory.t) AddressMap.t

  let pp_to_caller_subst fmt subst =
    AddressMap.pp ~pp_value:(fun fmt (addr, _) -> AbstractValue.pp fmt addr) fmt subst


  let empty_to_caller_subst = AddressMap.empty

  let add_to_caller_subst = AddressMap.add

  let to_caller_subst_fold_astate_result f subst astate =
    AddressMap.fold
      (fun addr_callee addr_hist_caller astate_result ->
        let* astate = astate_result in
        f addr_callee (canon_fst astate addr_hist_caller) astate )
      subst (Ok astate)


  let to_caller_subst_fold_constant_astate astate f subst init =
    AddressMap.fold
      (fun addr_callee addr_hist_caller acc -> f addr_callee (canon_fst astate addr_hist_caller) acc)
      subst init


  let to_caller_value_ astate subst x =
    AddressMap.find_opt x subst |> Option.map ~f:(fun y -> canon_fst astate y)


  let normalize_to_caller_subst astate subst =
    AddressMap.map (fun addr_hist_caller -> canon_fst astate addr_hist_caller) subst


  let raw_map_of_to_caller_subst = Fn.id

  let to_caller_subst_of_raw_map = Fn.id

  type call_state =
    { astate: AbductiveDomain.t
    ; subst: to_caller_subst
    ; rev_subst: (AbstractValue.t * LazyHeapPath.t) AddressMap.t
    ; hist_map: ValueHistory.t CellId.Map.t
    ; visited: AddressSet.t
    ; array_indices_to_visit: callee_index_to_visit list
    ; first_error: AbstractValue.t option
    ; aliases: HeapPath.Set.t HeapPath.Map.t }

  let to_caller_value call_state x = to_caller_value_ call_state.astate call_state.subst x

  let incorporate_new_eqs new_eqs call_state =
    let open PulseOperationResult.Import in
    let++ astate =
      let open SatUnsat.Import in
      AbductiveDomain.incorporate_new_eqs new_eqs call_state.astate
      >>| AccessResult.of_abductive_result
    in
    (* we need to update [call_state.rev_subst] so it always has canonical values in its domain

       no need to update the *range* of [call_state.susbt] (which are also caller values) similarly
       because values get normalized on the fly when we read from the map, eg in [to_caller_value]

       no need to update the range of [call_state.rev_subst] or the domain of [call_state.subst]
       because we never learn new equalities about callee variables (which have been normalized
       during summary creation) *)
    RevList.to_list new_eqs
    |> List.fold ~init:{call_state with astate} ~f:(fun call_state new_eq ->
           match (new_eq : Formula.new_eq) with
           | EqZero _ ->
               call_state
           | Equal (v_old, v_new) -> (
             match AddressMap.find_opt v_old call_state.rev_subst with
             | None ->
                 call_state
             | Some v_callee_old ->
                 (* TODO: there could already be a binding for [v_new], we should do something
                    similar to [visit] if so *)
                 let rev_subst =
                   AddressMap.remove v_old call_state.rev_subst |> AddressMap.add v_new v_callee_old
                 in
                 {call_state with rev_subst} ) )
end

include Unsafe

let pp_hist_map fmt hist_map =
  let is_first = ref false in
  CellId.Map.iter
    (fun id hist ->
      if not !is_first then F.pp_print_cut fmt () ;
      is_first := false ;
      F.fprintf fmt "%a: @[%a@]" CellId.pp id ValueHistory.pp hist )
    hist_map


let pp_call_state fmt
    ({astate; subst; rev_subst; hist_map; visited; array_indices_to_visit; first_error; aliases}
      [@warning "+missing-record-field-pattern"] ) =
  let pp_value_and_path fmt (value, path) =
    F.fprintf fmt "[value %a from %a]" AbstractValue.pp value LazyHeapPath.pp path
  in
  let pp_aliases fmt aliases = HeapPath.Map.pp ~pp_value:HeapPath.Set.pp fmt aliases in
  F.fprintf fmt
    "@[<v>{ astate=@[%a@];@,\
    \ subst=@[%a@];@,\
    \ rev_subst=@[%a@];@,\
    \ hist_map=@[%a@];@,\
    \ visited=@[%a@]@,\
    \ array_indices_to_visit=@[%a@]@,\
    \ %t@,\
    \ %a@}@]" AbductiveDomain.pp astate pp_to_caller_subst subst
    (AddressMap.pp ~pp_value:pp_value_and_path)
    rev_subst pp_hist_map hist_map AddressSet.pp visited
    (Pp.seq (fun _ _ -> ()))
    array_indices_to_visit
    (* only print [first_error] if there is an error *)
      (fun fmt ->
      match first_error with
      | None ->
          ()
      | Some v ->
          F.fprintf fmt " first_error= ERROR ON %a@," AbstractValue.pp v )
    pp_aliases aliases


let pp_call_state = Pp.html_collapsible_block ~name:"Show/hide the call state" HTML pp_call_state

let to_callee_addr call_state x = AddressMap.find_opt x call_state.rev_subst

type contradiction =
  | Aliasing of
      { addr_caller: AbstractValue.t
      ; addr_callee: AbstractValue.t
      ; addr_callee': AbstractValue.t
      ; call_state: call_state }
  | AliasingWithAllAliases of HeapPath.t list list
  | DynamicTypeNeeded of AbstractValue.t HeapPath.Map.t
  | CapturedFormalActualLength of
      { captured_formals: (Pvar.t * Typ.t) list
      ; captured_actuals: ((AbstractValue.t * ValueHistory.t) * Typ.t) list }
  | FormalActualLength of
      {formals: (Pvar.t * Typ.t) list; actuals: ((AbstractValue.t * ValueHistory.t) * Typ.t) list}
  | PathCondition

let pp_aliases fmt aliases =
  let pp fmt group = F.fprintf fmt "[%a]" (Pp.seq ~sep:";" HeapPath.pp) group in
  F.fprintf fmt "[%a]" (Pp.seq ~sep:"; " pp) aliases


let pp_contradiction fmt = function
  | Aliasing {addr_caller; addr_callee; addr_callee'; call_state} ->
      F.fprintf fmt
        "address %a in caller already bound to %a, not %a@\nnote: current call state was %a"
        AbstractValue.pp addr_caller AbstractValue.pp addr_callee' AbstractValue.pp addr_callee
        pp_call_state call_state
  | AliasingWithAllAliases aliases ->
      F.fprintf fmt "aliases: %a" pp_aliases aliases
  | DynamicTypeNeeded heap_paths ->
      F.fprintf fmt "Heap paths %a need to give their dynamic types"
        (HeapPath.Map.pp ~pp_value:AbstractValue.pp)
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
  | Aliasing _ | AliasingWithAllAliases _ ->
      Stats.incr_pulse_aliasing_contradictions ()
  | DynamicTypeNeeded _ ->
      ()
  | FormalActualLength _ ->
      Stats.incr_pulse_args_length_contradictions ()
  | CapturedFormalActualLength _ ->
      Stats.incr_pulse_captured_vars_length_contradictions ()
  | PathCondition ->
      ()


let is_dynamic_type_needed_contradiction = function
  | DynamicTypeNeeded heap_paths ->
      Some heap_paths
  | Aliasing _
  | AliasingWithAllAliases _
  | CapturedFormalActualLength _
  | FormalActualLength _
  | PathCondition ->
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
  match to_caller_value call_state addr_callee with
  | Some (addr_caller', _) when not (AbstractValue.equal addr_caller' addr_caller0) ->
      let path_condition, new_eqs =
        Formula.and_equal_vars addr_caller0 addr_caller'
          call_state.astate.AbductiveDomain.path_condition
        |> raise_if_unsat PathCondition
      in
      let+ call_state = incorporate_new_eqs new_eqs call_state |> raise_if_unsat PathCondition in
      {call_state with astate= AbductiveDomain.set_path_condition path_condition call_state.astate}
  | _ ->
      Ok call_state


let and_restricted_arith ~addr_callee ~addr_caller call_state =
  if AbstractValue.is_restricted addr_callee && AbstractValue.is_unrestricted addr_caller then
    (* [addr_callee] is implicitly [≥0] but [addr_caller] isn't, we need to propagate that fact to
       the caller address (no need to do anything in all other cases) *)
    let+ astate =
      PulseArithmetic.prune_nonnegative addr_caller call_state.astate
      |> raise_if_unsat PathCondition
    in
    {call_state with astate}
  else Ok call_state


let add_alias call_state ~current_head ~new_elem =
  let aliases = call_state.aliases in
  let elems =
    HeapPath.Map.find_opt current_head aliases
    |> Option.value ~default:HeapPath.Set.empty
    |> HeapPath.Set.add new_elem
  in
  let aliases = HeapPath.Map.add current_head elems aliases in
  {call_state with aliases}


let visit call_state ~pre ~addr_callee cell_id ~addr_hist_caller path =
  let addr_caller = fst addr_hist_caller in
  let check_if_alias =
    match to_callee_addr call_state addr_caller with
    | Some (addr_callee', path') when not (AbstractValue.equal addr_callee addr_callee') ->
        if
          (* [addr_caller] corresponds to several values in the callee, see if that's a problem for
             applying the pre-condition, i.e. if both values are addresses in the callee's heap,
             which means they must be disjoint. If so, raise a contradiction, but if not then
             continue as it just means that the callee doesn't care about the value of these
             variables, but record that they are equal. *)
          UnsafeMemory.mem addr_callee pre.BaseDomain.heap
          && UnsafeMemory.mem addr_callee' pre.BaseDomain.heap
        then
          match (LazyHeapPath.force path, LazyHeapPath.force path') with
          | Some path, Some path' ->
              `AliasFound (add_alias ~current_head:path' ~new_elem:path call_state)
          | _ ->
              (* some path elements are not supported yet, then we gave up with alias specialization *)
              raise_notrace
                (Contradiction (Aliasing {addr_caller; addr_callee; addr_callee'; call_state}))
        else
          `NoAliasFound
            (and_aliasing_arith ~addr_callee:addr_callee' ~addr_caller0:addr_caller call_state)
    | _ ->
        `NoAliasFound (Ok call_state)
  in
  match check_if_alias with
  | `NoAliasFound call_state ->
      let* call_state in
      let* call_state = and_aliasing_arith ~addr_callee ~addr_caller0:addr_caller call_state in
      let+ call_state = and_restricted_arith ~addr_callee ~addr_caller call_state in
      let hist_map =
        Option.fold cell_id ~init:call_state.hist_map ~f:(fun hist_map cell_id ->
            CellId.Map.add cell_id (snd addr_hist_caller) hist_map )
      in
      if AddressSet.mem addr_callee call_state.visited then
        (`AlreadyVisited, {call_state with hist_map})
      else
        ( `NotAlreadyVisited
        , { call_state with
            visited= AddressSet.add addr_callee call_state.visited
          ; subst= add_to_caller_subst addr_callee addr_hist_caller call_state.subst
          ; rev_subst= AddressMap.add addr_caller (addr_callee, path) call_state.rev_subst
          ; hist_map } )
  | `AliasFound call_state ->
      Ok (`AlreadyVisited, call_state)


(** HACK: we don't need to update the [rev_subst] of a call state when generating a fresh value for
    the caller because there's no chance that value appears anywhere else in the caller state, hence
    we cannot possibly get clashes about that caller value, which is the only thing [rev_subst] is
    used for. This is why this function is allowed to take only [subst] as argument and not a full
    call state. *)
let subst_find_or_new astate subst addr_callee ~default_hist_caller =
  match to_caller_value_ astate subst addr_callee with
  | None ->
      (* map restricted (≥0) values to restricted values to preserve their semantics *)
      let addr_caller = AbstractValue.mk_fresh_same_kind addr_callee in
      L.d_printfln "new subst %a <-> %a (fresh)" AbstractValue.pp addr_callee AbstractValue.pp
        addr_caller ;
      let addr_hist_fresh = (addr_caller, default_hist_caller) in
      (add_to_caller_subst addr_callee addr_hist_fresh subst, addr_hist_fresh)
  | Some addr_hist_caller ->
      (subst, addr_hist_caller)


let call_state_subst_find_or_new call_state addr_callee ~default_hist_caller =
  let new_subst, addr_hist_caller =
    subst_find_or_new call_state.astate call_state.subst addr_callee ~default_hist_caller
  in
  if phys_equal new_subst call_state.subst then (call_state, addr_hist_caller)
  else ({call_state with subst= new_subst}, addr_hist_caller)


let translate_access_to_caller astate subst (access_callee : Access.t) : _ * Access.t =
  match access_callee with
  | ArrayAccess (typ, val_callee) ->
      let subst, (val_caller, _) =
        subst_find_or_new astate subst val_callee ~default_hist_caller:ValueHistory.epoch
      in
      (subst, ArrayAccess (typ, val_caller))
  | FieldAccess _ | Dereference ->
      (subst, access_callee)


let check_dict_keys callee call_location ~pre call_state =
  let keys_to_check =
    to_caller_subst_fold_constant_astate call_state.astate
      (fun addr_pre addr_hist_caller keys_to_check ->
        match UnsafeAttributes.get_dict_read_const_keys addr_pre pre.BaseDomain.attrs with
        | None ->
            keys_to_check
        | Some keys ->
            (addr_hist_caller, keys) :: keys_to_check )
      call_state.subst []
  in
  PulseResult.list_fold keys_to_check ~init:call_state.astate
    ~f:(fun astate ((addr_caller, hist_caller), keys) ->
      PulseResult.container_fold
        ~fold:(IContainer.fold_of_pervasives_map_fold Attribute.ConstKeys.fold) keys ~init:astate
        ~f:(fun astate (key, (timestamp, trace)) ->
          let trace =
            Trace.ViaCall
              {f= Call callee; location= call_location; history= hist_caller; in_call= trace}
          in
          PulseOperations.add_dict_read_const_key timestamp trace addr_caller key astate ) )


(* {3 reading the pre from the current state} *)

(** Materialize the (abstract memory) subgraph of [pre] reachable from [addr_pre] in
    [call_state.astate] starting from address [addr_caller]. Report an error if some invalid
    addresses are traversed in the process. *)
let rec materialize_pre_from_address callee call_location ~pre ~addr_pre cell_id ~addr_hist_caller
    path call_state =
  let* visited_status, call_state =
    visit call_state ~pre ~addr_callee:addr_pre cell_id ~addr_hist_caller path
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
      | Some edges_pre -> (
        match
          BaseAddressAttributes.check_valid
            (AbductiveDomain.CanonValue.canon' call_state.astate (fst addr_hist_caller))
            (call_state.astate.post :> BaseDomain.t).attrs
        with
        | Error _ ->
            (* The address is invalid in the current state but the summary has edges starting from
               that address in memory. Don't "dereference" invalid addresses by accessing their
               edges. Instead, stop the exploration of the precondition from that address and carry
               on with the rest of the precondition. We need to explore as much of the precondition
               as we can to discover potential contradictions from the calle's path condition
               (mapped to the caller, and exploring more of the state will make the mapping more
               complete). But, we don't want to explore from the current address because that will
               materialize edges from that address that a) don't make sense because the address is
               invalid, and b) can confuse the solver into thinking the path is infeasible, for
               example if the address is null in the caller (if it is null *and* has edges coming
               out of it then it is a contradiction). We'll report the error later in
               [check_all_valid] after we have made sure that the path is satisfiable. Also there
               could be several addresses in that situation and [check_all_valid] will take care of
               reporting on the first one accessed by the callee (another reason why we don't want
               to report the error straight away). *)
            Ok
              { call_state with
                first_error= Option.first_some call_state.first_error (Some (fst addr_hist_caller))
              }
        | Ok () ->
            let* astate = check_dict_keys callee call_location ~pre call_state in
            PulseResult.container_fold ~fold:UnsafeMemory.Edges.fold ~init:{call_state with astate}
              edges_pre ~f:(fun call_state (access_callee, (addr_pre_dest, pre_hist)) ->
                match (access_callee : Access.t) with
                | ArrayAccess _ ->
                    Ok
                      { call_state with
                        array_indices_to_visit=
                          {addr_pre_dest; pre_hist; access_callee; addr_hist_caller}
                          :: call_state.array_indices_to_visit }
                | FieldAccess _ | Dereference ->
                    (* only array accessess depend on abstract values and need translation *)
                    let access_caller = access_callee in
                    let astate, addr_hist_dest_caller =
                      Memory.eval_edge addr_hist_caller access_caller call_state.astate
                    in
                    let call_state = {call_state with astate} in
                    let path = LazyHeapPath.push access_callee path in
                    materialize_pre_from_address callee call_location ~pre ~addr_pre:addr_pre_dest
                      (ValueHistory.get_cell_id_exn pre_hist)
                      ~addr_hist_caller:addr_hist_dest_caller path call_state ) ) )


let materialize_pre_from_array_index callee call_location ~pre
    {addr_pre_dest; pre_hist; access_callee; addr_hist_caller} path call_state =
  let subst, access_caller =
    translate_access_to_caller call_state.astate call_state.subst access_callee
  in
  let astate, addr_hist_dest_caller =
    Memory.eval_edge addr_hist_caller access_caller call_state.astate
  in
  let call_state = {call_state with astate; subst} in
  (* HACK: we should probably visit the value in the (array) access too, but since it's a value
     normally it shouldn't appear in the heap anyway so there should be nothing to visit. *)
  materialize_pre_from_address callee call_location ~pre ~addr_pre:addr_pre_dest
    (ValueHistory.get_cell_id_exn pre_hist)
    ~addr_hist_caller:addr_hist_dest_caller path call_state


let materialize_pre_from_array_indices callee call_location ~pre call_state =
  let path = LazyHeapPath.unsupported in
  let+ call_state =
    PulseResult.list_fold call_state.array_indices_to_visit ~init:call_state
      ~f:(fun call_state array_index_to_translate ->
        materialize_pre_from_array_index callee call_location ~pre array_index_to_translate path
          call_state )
  in
  {call_state with array_indices_to_visit= []}


let callee_deref_non_c_struct addr typ astate =
  match typ.Typ.desc with
  | Tstruct _ ->
      Some (addr, None)
  | _ ->
      UnsafeMemory.find_edge_opt addr Dereference astate
      |> Option.map ~f:(fun (value, hist) -> (value, ValueHistory.get_cell_id_exn hist))


(** materialize subgraph of [pre] rooted at the address represented by a [formal] parameter that has
    been instantiated with the corresponding [actual] into the current state [call_state.astate] *)
let materialize_pre_from_actual callee call_location ~pre ~formal:(formal, typ) ~actual:(actual, _)
    call_state =
  let path = LazyHeapPath.from_pvar formal in
  let formal = Var.of_pvar formal in
  L.d_printfln "Materializing PRE from [%a <- %a]" Var.pp formal AbstractValue.pp (fst actual) ;
  (let open IOption.Let_syntax in
   let* addr_formal_pre, _ = UnsafeStack.find_opt formal pre.BaseDomain.stack in
   let+ formal_pre, cell_id = callee_deref_non_c_struct addr_formal_pre typ pre.BaseDomain.heap in
   materialize_pre_from_address callee call_location ~pre ~addr_pre:formal_pre cell_id
     ~addr_hist_caller:actual path call_state )
  |> function Some result -> result | None -> Ok call_state


let materialize_pre_for_captured_vars callee call_location ~pre ~captured_formals ~captured_actuals
    call_state =
  match
    PulseResult.list_fold2 captured_formals captured_actuals ~init:call_state
      ~f:(fun call_state formal actual ->
        materialize_pre_from_actual callee call_location ~pre ~formal ~actual call_state )
  with
  | Unequal_lengths ->
      raise_notrace (Contradiction (CapturedFormalActualLength {captured_formals; captured_actuals}))
  | Ok result ->
      result


let materialize_pre_for_parameters callee call_location ~pre ~formals ~actuals call_state =
  (* For each [(formal, actual)] pair, resolve them to addresses in their respective states then
     call [materialize_pre_from] on them.  Give up if calling the function introduces aliasing.
  *)
  match
    PulseResult.list_fold2 formals actuals ~init:call_state ~f:(fun call_state formal actual ->
        materialize_pre_from_actual callee call_location ~pre ~formal ~actual call_state )
  with
  | Unequal_lengths ->
      raise_notrace (Contradiction (FormalActualLength {formals; actuals}))
  | Ok result ->
      result


let materialize_pre_for_globals path callee call_location ~pre call_state =
  fold_globals_of_callee_stack path call_location pre.BaseDomain.stack call_state
    ~f:(fun pvar ~stack_value:(addr_pre, pre_hist) ~addr_hist_caller call_state ->
      let path = LazyHeapPath.from_pvar pvar in
      materialize_pre_from_address callee call_location ~pre ~addr_pre
        (ValueHistory.get_cell_id_exn pre_hist)
        ~addr_hist_caller path call_state )


let conjoin_callee_arith callee_path_condition call_state =
  L.d_printfln "applying callee path condition: (%a)[%a]" Formula.pp callee_path_condition
    pp_to_caller_subst call_state.subst ;
  let subst, path_condition, new_eqs =
    Formula.and_callee_formula
      (raw_map_of_to_caller_subst call_state.subst)
      call_state.astate.path_condition ~callee:callee_path_condition
    |> raise_if_unsat PathCondition
  in
  let astate = AbductiveDomain.set_path_condition path_condition call_state.astate in
  let call_state = {call_state with astate; subst= to_caller_subst_of_raw_map subst} in
  incorporate_new_eqs new_eqs call_state |> raise_if_unsat PathCondition


let caller_attrs_of_callee_attrs timestamp callee_proc_name call_location caller_history call_state
    callee_attrs =
  let subst_ref = ref call_state.subst in
  let f_subst v =
    let subst, (v', _hist) =
      subst_find_or_new call_state.astate !subst_ref v ~default_hist_caller:ValueHistory.epoch
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
    materialize_pre_for_parameters callee_proc_name call_location ~pre:callee_precondition ~formals
      ~actuals call_state
    >>= materialize_pre_for_captured_vars callee_proc_name call_location ~pre:callee_precondition
          ~captured_formals ~captured_actuals
    >>= materialize_pre_for_globals path callee_proc_name call_location ~pre:callee_precondition
    >>= (* ...then relational arithmetic constraints in the callee's attributes will make sense in
           terms of the caller's values *)
    conjoin_callee_arith (AbductiveDomain.Summary.get_path_condition callee_summary)
    >>= materialize_pre_from_array_indices callee_proc_name call_location ~pre:callee_precondition
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
              let subst, access =
                translate_access_to_caller call_state.astate subst access_callee
              in
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
      ~f:(fun (subst, translated_edges) (access_callee, (addr_callee, hist_post)) ->
        let subst, (addr_curr, hist_curr) =
          subst_find_or_new call_state.astate subst addr_callee ~default_hist_caller:hist_caller
        in
        let hist_caller =
          let open Option.Monad_infix in
          ValueHistory.get_cell_ids hist_post
          >>= ValueHistory.of_cell_ids_in_map call_state.hist_map
          |> Option.value ~default:hist_curr
        in
        let subst, access = translate_access_to_caller call_state.astate subst access_callee in
        let translated_edges =
          UnsafeMemory.Edges.add access
            ( addr_curr
            , ValueHistory.sequence ~context:path.conditions
                (Call {f= Call callee_proc_name; location= call_loc; in_call= hist_post; timestamp})
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
      match to_caller_value call_state addr_callee with
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


let report_mutual_recursion_cycle
    ({InterproceduralAnalysis.add_errlog; proc_desc; err_log} as analysis_data) cycle =
  let proc_name = Procdesc.get_proc_name proc_desc in
  PulseMutualRecursion.iter_rotations cycle ~f:(fun cycle ->
      let inner_call = PulseMutualRecursion.get_inner_call cycle in
      let location = PulseMutualRecursion.get_outer_location cycle in
      match Procdesc.load inner_call with
      | None ->
          (* cannot happen but also if the procedure is undefined then we shouldn't report
             anyway *)
          ()
      | Some inner_proc_desc ->
          let is_foreign_procedure = not (Procname.equal inner_call proc_name) in
          let report_analysis_data, err_log =
            if is_foreign_procedure then
              let err_log = Errlog.empty () in
              (InterproceduralAnalysis.for_procedure inner_proc_desc err_log analysis_data, err_log)
            else (analysis_data, err_log)
          in
          L.d_printfln "reporting on procedure %a (foreign=%b) at %a" Procname.pp inner_call
            is_foreign_procedure Location.pp_file_pos location ;
          PulseReport.report report_analysis_data ~is_suppressed:false ~latent:false
            (MutualRecursionCycle {cycle; location}) ;
          if is_foreign_procedure then add_errlog inner_call err_log )


let report_recursive_calls ({InterproceduralAnalysis.proc_desc} as analysis_data) cycles =
  PulseMutualRecursion.Set.iter
    (fun cycle ->
      if
        Procname.equal
          (PulseMutualRecursion.get_inner_call cycle)
          (Procdesc.get_proc_name proc_desc)
      then report_mutual_recursion_cycle analysis_data cycle )
    cycles


let record_recursive_calls analysis_data callee_proc_name call_loc callee_summary call_state =
  let callee_recursive_calls =
    PulseMutualRecursion.Set.map
      (PulseMutualRecursion.add_call callee_proc_name call_loc)
      (AbductiveDomain.Summary.get_recursive_calls callee_summary)
  in
  report_recursive_calls analysis_data callee_recursive_calls ;
  let astate = AbductiveDomain.add_recursive_calls callee_recursive_calls call_state.astate in
  {call_state with astate}


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


let record_transitive_info {InterproceduralAnalysis.tenv} callee_proc_name call_location
    callee_summary call_state =
  if PulseTransitiveAccessChecker.should_skip_call tenv callee_proc_name then call_state
  else
    let astate =
      AbductiveDomain.transfer_transitive_info_to_caller callee_proc_name call_location
        callee_summary call_state.astate
    in
    {call_state with astate}


let apply_unknown_effects callee_summary call_state =
  let open IOption.Let_syntax in
  L.d_printfln "Applying unknown effects, call_state before = %a" pp_call_state call_state ;
  let is_modified_by_call addr_caller access =
    match to_callee_addr call_state addr_caller with
    | None ->
        false
    | Some (addr_callee, _) ->
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
         let+ addr_caller, _ = to_caller_value call_state (CanonValue.downcast addr_callee) in
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
        let return_caller, default_return_caller_hist =
          match to_caller_value call_state return_callee with
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
            (let open Option.Monad_infix in
             ValueHistory.get_cell_ids return_callee_hist
             >>= ValueHistory.of_cell_ids_in_map call_state.hist_map
             |> Option.value ~default:default_return_caller_hist )
        in
        ( call_state
        , Some
            (Formula.get_var_repr call_state.astate.path_condition return_caller, return_caller_hist)
        ) )


let apply_post analysis_data path callee_proc_name call_location callee_summary call_state =
  PerfEvent.(log (fun logger -> log_begin_event logger ~name:"pulse call post" ())) ;
  let r =
    call_state
    |> apply_unknown_effects callee_summary
    |> apply_post_from_callee_pre path callee_proc_name call_location callee_summary
    >>= apply_post_from_callee_post path callee_proc_name call_location callee_summary
    >>| add_attributes `Post path callee_proc_name call_location
          (AbductiveDomain.Summary.get_post callee_summary).attrs
    >>| record_recursive_calls analysis_data callee_proc_name call_location callee_summary
    >>| record_skipped_calls callee_proc_name call_location callee_summary
    >>| record_transitive_info analysis_data callee_proc_name call_location callee_summary
    >>| read_return_value path callee_proc_name call_location callee_summary
  in
  PerfEvent.(log (fun logger -> log_end_event logger ())) ;
  r


let check_all_valid path callee_proc_name call_location ~pre call_state =
  (* collect all the checks to perform then do each check in timestamp order to make sure we report
     the first issue if any *)
  let addresses_to_check =
    to_caller_subst_fold_constant_astate call_state.astate
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
  to_caller_subst_fold_astate_result
    (fun addr_pre addr_hist astate ->
      Option.value_map (UnsafeAttributes.get_used_as_branch_cond addr_pre pre_attrs)
        ~default:(Ok astate) ~f:(fun (pname_using_config, branch_location, trace) ->
          PulseOperations.check_used_as_branch_cond addr_hist ~pname_using_config ~branch_location
            ~location trace astate ) )
    subst astate


let check_all_taint_valid path callee_proc_name call_location callee_summary astate call_state =
  to_caller_subst_fold_astate_result
    (fun addr_pre ((_, hist_caller) as addr_hist_caller) astate ->
      let sinks =
        UnsafeAttributes.get_must_not_be_tainted addr_pre
          (AbductiveDomain.Summary.get_pre callee_summary).attrs
      in
      let trace_via_call trace =
        Trace.ViaCall
          {in_call= trace; f= Call callee_proc_name; location= call_location; history= hist_caller}
      in
      Attribute.TaintSinkMap.fold
        (fun kind Attribute.TaintSink.{sink; trace} astate_result ->
          let* astate = astate_result in
          let sink_and_trace =
            ({TaintItem.kinds= [kind]; value_tuple= sink}, trace_via_call trace)
          in
          let+ astate =
            PulseTaintOperations.check_flows_wrt_sink path call_location ~sink:sink_and_trace
              ~source:addr_hist_caller astate
          in
          astate )
        sinks (Ok astate) )
    call_state.subst astate


(* - read all the pre, assert validity of addresses and materializes *everything* (to throw stuff
   in the *current* pre as appropriate so that callers of the current procedure will also know
   about the deeper reads)

   - for each actual, write the post for that actual

   - if aliasing is introduced at any time then give up *)
let apply_summary analysis_data path ~callee_proc_name call_location ~callee_summary
    ~captured_formals ~captured_actuals ~formals ~actuals astate =
  let aux () =
    let empty_call_state =
      { astate
      ; subst= empty_to_caller_subst
      ; rev_subst= AddressMap.empty
      ; hist_map= CellId.Map.empty
      ; visited= AddressSet.empty
      ; array_indices_to_visit= []
      ; first_error= None
      ; aliases= HeapPath.Map.empty }
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
    | Ok {aliases} when HeapPath.Map.is_empty aliases |> not ->
        L.d_printfln ~color:Orange "Aliases found: %a@\n"
          (HeapPath.Map.pp ~pp_value:HeapPath.Set.pp)
          aliases ;
        let aliases =
          HeapPath.Map.fold
            (fun path set aliases ->
              let new_alias_group = HeapPath.Set.add path set |> HeapPath.Set.elements in
              new_alias_group :: aliases )
            aliases []
        in
        (Unsat, Some (AliasingWithAllAliases aliases))
    | result -> (
      try
        let pre_astate, pre_subst =
          match result with
          | Ok call_state | Recoverable (call_state, _) ->
              (call_state.astate, call_state.subst)
          | FatalError (error, _) ->
              (AccessResult.astate_of_error error, empty_to_caller_subst)
        in
        let res =
          let* call_state = result in
          L.d_printfln "Pre applied successfully, call_state after = %a" pp_call_state call_state ;
          let pre = AbductiveDomain.Summary.get_pre callee_summary in
          let* astate =
            check_all_valid path callee_proc_name call_location ~pre call_state
            |> AccessResult.of_result
          in
          (* if at that stage [call_state.first_error] is set but we haven't error'd then there is
             a problem; give up because something is wrong *)
          ( match call_state.first_error with
          | None ->
              ()
          | Some v ->
              L.internal_error
                "huho, we found an error on accessing invalid address %a when applying the \
                 precondition but did not actually report an error. Abort!"
                AbstractValue.pp v ;
              (* HACK: abuse [PathCondition], sorry *)
              raise (Contradiction PathCondition) ) ;
          let* astate = check_config_usage_at_call call_location ~pre call_state.subst astate in
          (* reset [visited] *)
          let call_state = {call_state with astate; visited= AddressSet.empty} in
          (* apply the postcondition *)
          let* call_state, return_caller =
            apply_post analysis_data path callee_proc_name call_location callee_summary call_state
          in
          let astate =
            if Topl.is_active () then
              (* normalize the substitution so TOPL has accurate information; we could also modify
                 TOPL to keep the normalization lazy/on-the-fly instead *)
              let substitution = normalize_to_caller_subst call_state.astate call_state.subst in
              let callee_is_manifest = PulseArithmetic.is_manifest callee_summary in
              AbductiveDomain.Topl.large_step ~call_location ~callee_proc_name ~substitution
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
          (astate, return_caller, raw_map_of_to_caller_subst call_state.subst, call_state.hist_map)
        in
        let contradiciton =
          let callee_heap_paths =
            AbductiveDomain.Summary.heap_paths_that_need_dynamic_type_specialization callee_summary
          in
          if HeapPath.Map.is_empty callee_heap_paths then None
          else
            let caller_heap_paths =
              HeapPath.Map.fold
                (fun heap_path addr map ->
                  match to_caller_value_ pre_astate pre_subst addr with
                  | Some (addr_in_caller, _) ->
                      L.d_printfln
                        "dynamic type is required for address %a reachable from heap path %a in \
                         callee (%a in caller)"
                        AbstractValue.pp addr HeapPath.pp heap_path AbstractValue.pp addr_in_caller ;
                      HeapPath.Map.add heap_path addr_in_caller map
                  | None ->
                      L.d_printfln
                        "dynamic type is required for address %a reachable from heap path %a in \
                         callee (not found in caller)"
                        AbstractValue.pp addr HeapPath.pp heap_path ;
                      map )
                callee_heap_paths HeapPath.Map.empty
            in
            Some (DynamicTypeNeeded caller_heap_paths)
        in
        (Sat res, contradiciton)
      with Contradiction reason ->
        L.d_printfln "Cannot apply post-condition: %a" pp_contradiction reason ;
        log_contradiction reason ;
        (Unsat, Some reason) )
  in
  let pp_formals = Pp.seq ~sep:"," (fun f (var, _) -> Var.pp f (Var.of_pvar var)) in
  let pp_summary =
    Pp.html_collapsible_block ~name:"Show/hide the summary" HTML AbductiveDomain.Summary.pp
  in
  L.with_indent ~collapsible:true "Applying pre/post for %a(%a):" Procname.pp callee_proc_name
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
        HeapPath.Map.fold
          (fun path addr map ->
            (* we may end up in a strange situation where [path] is bound in both
               [heapmap1] and [heapmap2], but since [path] is a heap path in the
               precondition space, we do not expect the binding to matter *)
            HeapPath.Map.add path addr map )
          heapmap2 heapmap1
      in
      Some (DynamicTypeNeeded heapmap)
  | Some (DynamicTypeNeeded heapmap), _ | _, Some (DynamicTypeNeeded heapmap) ->
      Some (DynamicTypeNeeded heapmap)
  | contradiction, _ ->
      contradiction
