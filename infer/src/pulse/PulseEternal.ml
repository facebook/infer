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
module AddressSet = AbstractValue.Set
module AddressMap = AbstractValue.Map

(* TODO: this is a copy/paste of PulseInterproc's "materialize the pre-condition" phase with little
   modifications besides deleting code we don't need and doing the following renamings:
   - [callee] -> [rhs]
   - [caller] -> [lhs]
   - [call_state] -> [unification]

   The TODO part is that we don't need to "materialize" anything in the "current" (LHS) state based
   on the RHS so there is more code/functionality to remove (or refactor PulseInterproc to avoid the
   code duplication).  *)

type rhs_index_to_visit =
  { addr_rhs_dest: AbstractValue.t
  ; addr_rhs: ValueHistory.t
  ; access_rhs: Access.t
  ; addr_hist_lhs: AbstractValue.t * ValueHistory.t }

type to_lhs_subst = AbstractValue.t AddressMap.t

let pp_to_lhs_subst fmt subst =
  AddressMap.pp ~pp_value:(fun fmt addr -> AbstractValue.pp fmt addr) fmt subst


let empty_to_lhs_subst = AddressMap.empty

let add_to_lhs_subst = AddressMap.add

let to_lhs_subst_fold_constant_astate _astate f subst init =
  AddressMap.fold (fun addr_rhs addr_lhs acc -> f addr_rhs addr_lhs acc) subst init


let to_lhs_value_ _astate subst x =
  (* TODO: this used to [canon_fst] the result, look into this *) AddressMap.find_opt x subst


type unification =
  { astate: AbductiveDomain.t
  ; subst: to_lhs_subst
  ; rev_subst: AbstractValue.t AddressMap.t
  ; visited: AddressSet.t
  ; array_indices_to_visit: rhs_index_to_visit list }

let incorporate_new_eqs new_eqs unification =
  let open PulseOperationResult.Import in
  let++ astate =
    let open SatUnsat.Import in
    AbductiveDomain.incorporate_new_eqs new_eqs unification.astate
    >>| AccessResult.of_abductive_result
  in
  (* TODO: is the comment below still relevant? *)
  (* we need to update [unification.rev_subst] so it always has canonical values in its domain

       no need to update the *range* of [unification.susbt] (which are also lhs values) similarly
       because values get normalized on the fly when we read from the map, eg in [to_lhs_value]

       no need to update the range of [unification.rev_subst] or the domain of [unification.subst]
       because we never learn new equalities about rhs variables (which have been normalized
       during summary creation) *)
  RevList.to_list new_eqs
  |> List.fold ~init:{unification with astate} ~f:(fun unification new_eq ->
         match (new_eq : Formula.new_eq) with
         | EqZero _ ->
             unification
         | Equal (v_old, v_new) -> (
           match AddressMap.find_opt v_old unification.rev_subst with
           | None ->
               unification
           | Some v_rhs_old ->
               (* TODO: there could already be a binding for [v_new], we should do something
                    similar to [visit] if so *)
               let rev_subst =
                 AddressMap.remove v_old unification.rev_subst |> AddressMap.add v_new v_rhs_old
               in
               {unification with rev_subst} ) )


let to_lhs_value unification x = to_lhs_value_ unification.astate unification.subst x

let pp_unification fmt
    ({astate; subst; rev_subst; visited; array_indices_to_visit}
     [@warning "+missing-record-field-pattern"] ) =
  let pp_value_and_path fmt value = F.fprintf fmt "[value %a]" AbstractValue.pp value in
  F.fprintf fmt
    "@[<v>{ astate=@[%a@];@,\
    \ subst=@[%a@];@,\
    \ rev_subst=@[%a@];@,\
    \ visited=@[%a@]@,\
    \ array_indices_to_visit=@[%a@]@}@]"
    AbductiveDomain.pp astate pp_to_lhs_subst subst
    (AddressMap.pp ~pp_value:pp_value_and_path)
    rev_subst AddressSet.pp visited
    (Pp.seq (fun _ _ -> ()))
    array_indices_to_visit


let pp_unification = Pp.html_collapsible_block ~name:"Show/hide the call state" HTML pp_unification

let to_rhs_addr unification x = AddressMap.find_opt x unification.rev_subst

type contradiction =
  | Aliasing of
      { addr_lhs: AbstractValue.t
      ; addr_rhs: AbstractValue.t
      ; addr_rhs': AbstractValue.t
      ; unification: unification }
  | PathCondition of unsat_info
  | InvalidAccess

let pp_contradiction fmt = function
  | Aliasing {addr_lhs; addr_rhs; addr_rhs'; unification} ->
      F.fprintf fmt
        "address %a in lhs already bound to %a, not %a@\nnote: current call state was %a"
        AbstractValue.pp addr_lhs AbstractValue.pp addr_rhs' AbstractValue.pp addr_rhs
        pp_unification unification
  | PathCondition unsat_info ->
      F.fprintf fmt "path condition evaluates to false: %a" SatUnsat.pp_unsat_info unsat_info
  | InvalidAccess ->
      F.fprintf fmt "InvalidAccess"


exception Contradiction of contradiction

let raise_if_unsat = function
  | Sat x ->
      x
  | Unsat unsat_info ->
      raise_notrace (Contradiction (PathCondition unsat_info))


let fold_rhs_stack stack unification ~f =
  PulseResult.container_fold
    ~fold:(IContainer.fold_of_pervasives_map_fold (AbductiveDomain.Stack.fold ~pre_or_post:`Post))
    stack ~init:unification
    ~f:(fun unification (var, vo_rhs) ->
      match (var : Var.t) with
      | ProgramVar pvar ->
          let unification, vo_lhs =
            let astate, var_value = Stack.eval ValueHistory.epoch var unification.astate in
            if phys_equal astate unification.astate then (unification, var_value)
            else ({unification with astate}, var_value)
          in
          f pvar ~addr_hist_rhs:(ValueOrigin.addr_hist vo_rhs)
            ~addr_hist_lhs:(ValueOrigin.addr_hist vo_lhs) unification
      | LogicalVar _ ->
          Ok unification )


let and_aliasing_arith ~addr_rhs ~addr_lhs0 unification =
  match to_lhs_value unification addr_rhs with
  | Some addr_lhs' when not (AbstractValue.equal addr_lhs' addr_lhs0) ->
      let path_condition, new_eqs =
        Formula.and_equal_vars addr_lhs0 addr_lhs' unification.astate.AbductiveDomain.path_condition
        |> raise_if_unsat
      in
      let+ unification = incorporate_new_eqs new_eqs unification |> raise_if_unsat in
      {unification with astate= AbductiveDomain.set_path_condition path_condition unification.astate}
  | _ ->
      Ok unification


let and_restricted_arith ~addr_rhs ~addr_lhs unification =
  if AbstractValue.is_restricted addr_rhs && AbstractValue.is_unrestricted addr_lhs then
    (* [addr_rhs] is implicitly [≥0] but [addr_lhs] isn't, we need to propagate that fact to
       the lhs address (no need to do anything in all other cases) *)
    let+ astate =
      PulseArithmetic.prune_nonnegative ~depth:1 addr_lhs unification.astate |> raise_if_unsat
    in
    {unification with astate}
  else Ok unification


let visit unification ~rhs ~addr_rhs ~addr_hist_lhs =
  let addr_lhs = fst addr_hist_lhs in
  let check_if_alias =
    match to_rhs_addr unification addr_lhs with
    | Some addr_rhs' when not (AbstractValue.equal addr_rhs addr_rhs') ->
        if
          (* [addr_lhs] corresponds to several values in the rhs, see if that's a problem for
             unification, i.e. if both values are addresses in the rhs's heap, which means they must
             be disjoint. If so, raise a contradiction, but if not then continue as it just means
             that the rhs doesn't care about the value of these variables, but record that they are
             equal. *)
          UnsafeMemory.mem addr_rhs rhs.BaseDomain.heap
          && UnsafeMemory.mem addr_rhs' rhs.BaseDomain.heap
        then raise_notrace (Contradiction (Aliasing {addr_lhs; addr_rhs; addr_rhs'; unification}))
        else `NoAliasFound (and_aliasing_arith ~addr_rhs:addr_rhs' ~addr_lhs0:addr_lhs unification)
    | _ ->
        `NoAliasFound (Ok unification)
  in
  match check_if_alias with
  | `NoAliasFound unification ->
      let* unification in
      let* unification = and_aliasing_arith ~addr_rhs ~addr_lhs0:addr_lhs unification in
      let+ unification = and_restricted_arith ~addr_rhs ~addr_lhs unification in
      if AddressSet.mem addr_rhs unification.visited then (`AlreadyVisited, unification)
      else
        ( `NotAlreadyVisited
        , { unification with
            visited= AddressSet.add addr_rhs unification.visited
          ; subst= add_to_lhs_subst addr_rhs addr_lhs unification.subst
          ; rev_subst= AddressMap.add addr_lhs addr_rhs unification.rev_subst } )
  | `AliasFound unification ->
      Ok (`AlreadyVisited, unification)


(** HACK: we don't need to update the [rev_subst] of a call state when generating a fresh value for
    the lhs because there's no chance that value appears anywhere else in the lhs state, hence we
    cannot possibly get clashes about that lhs value, which is the only thing [rev_subst] is used
    for. This is why this function is allowed to take only [subst] as argument and not a full call
    state. *)
let subst_find_or_new astate subst addr_rhs =
  match to_lhs_value_ astate subst addr_rhs with
  | None ->
      (* map restricted (≥0) values to restricted values to preserve their semantics *)
      let addr_lhs = AbstractValue.mk_fresh_same_kind addr_rhs in
      L.d_printfln "new subst %a <-> %a (fresh)" AbstractValue.pp addr_rhs AbstractValue.pp addr_lhs ;
      (add_to_lhs_subst addr_rhs addr_lhs subst, addr_lhs)
  | Some addr_hist_lhs ->
      (subst, addr_hist_lhs)


let translate_access_to_lhs astate subst (access_rhs : Access.t) : _ * Access.t =
  match access_rhs with
  | ArrayAccess (typ, val_rhs) ->
      let subst, val_lhs = subst_find_or_new astate subst val_rhs in
      (subst, ArrayAccess (typ, val_lhs))
  | FieldAccess _ | Dereference ->
      (subst, access_rhs)


(* TODO: what's this for? *)
let check_dict_keys ~rhs unification =
  let keys_to_check =
    to_lhs_subst_fold_constant_astate unification.astate
      (fun addr_rhs addr_hist_lhs keys_to_check ->
        match UnsafeAttributes.get_dict_read_const_keys addr_rhs rhs.BaseDomain.attrs with
        | None ->
            keys_to_check
        | Some keys ->
            (addr_hist_lhs, keys) :: keys_to_check )
      unification.subst []
  in
  PulseResult.list_fold keys_to_check ~init:unification.astate ~f:(fun astate (addr_lhs, keys) ->
      PulseResult.container_fold
        ~fold:(IContainer.fold_of_pervasives_map_fold Attribute.ConstKeys.fold) keys ~init:astate
        ~f:(fun astate (key, (timestamp, trace)) ->
          PulseOperations.add_dict_read_const_key timestamp trace addr_lhs key astate ) )


(** Unify the (abstract memory) subgraph of [rhs] reachable from [addr_rhs] in [unification.astate]
    starting from address [addr_lhs]. Report an error if some invalid addresses are traversed in the
    process. *)
let rec unify_rhs_from_address ~rhs ~addr_rhs ~addr_hist_lhs unification =
  let* visited_status, unification = visit unification ~rhs ~addr_rhs ~addr_hist_lhs in
  match visited_status with
  | `AlreadyVisited ->
      Ok unification
  | `NotAlreadyVisited -> (
      L.d_printfln "visiting from address %a <-> %a" AbstractValue.pp addr_rhs AbstractValue.pp
        (fst addr_hist_lhs) ;
      match UnsafeMemory.find_opt addr_rhs rhs.BaseDomain.heap with
      | None ->
          Ok unification
      | Some edges_rhs -> (
        match
          BaseAddressAttributes.check_valid
            (AbductiveDomain.CanonValue.canon' unification.astate (fst addr_hist_lhs))
            (unification.astate.post :> BaseDomain.t).attrs
        with
        | Error _ ->
            raise_notrace (Contradiction InvalidAccess)
        | Ok () ->
            let* astate = check_dict_keys ~rhs unification in
            PulseResult.container_fold ~fold:UnsafeMemory.Edges.fold ~init:{unification with astate}
              edges_rhs ~f:(fun unification (access_rhs, (addr_rhs_dest, addr_rhs)) ->
                match (access_rhs : Access.t) with
                | ArrayAccess _ ->
                    Ok
                      { unification with
                        array_indices_to_visit=
                          {addr_rhs_dest; addr_rhs; access_rhs; addr_hist_lhs}
                          :: unification.array_indices_to_visit }
                | FieldAccess _ | Dereference ->
                    (* only array accessess depend on abstract values and need translation *)
                    let access_lhs = access_rhs in
                    let astate, addr_hist_dest_lhs =
                      Memory.eval_edge addr_hist_lhs access_lhs unification.astate
                    in
                    let unification = {unification with astate} in
                    unify_rhs_from_address ~rhs ~addr_rhs:addr_rhs_dest
                      ~addr_hist_lhs:addr_hist_dest_lhs unification ) ) )


let unify_rhs_from_array_index ~rhs {addr_rhs_dest; access_rhs; addr_hist_lhs} unification =
  let subst, access_lhs = translate_access_to_lhs unification.astate unification.subst access_rhs in
  let astate, addr_hist_dest_lhs = Memory.eval_edge addr_hist_lhs access_lhs unification.astate in
  let unification = {unification with astate; subst} in
  (* HACK: we should probably visit the value in the (array) access too, but since it's a value
     normally it shouldn't appear in the heap anyway so there should be nothing to visit. *)
  unify_rhs_from_address ~rhs ~addr_rhs:addr_rhs_dest ~addr_hist_lhs:addr_hist_dest_lhs unification


let unify_rhs_from_array_indices ~rhs unification =
  let+ unification =
    PulseResult.list_fold unification.array_indices_to_visit ~init:unification
      ~f:(fun unification array_index_to_translate ->
        unify_rhs_from_array_index ~rhs array_index_to_translate unification )
  in
  {unification with array_indices_to_visit= []}


let unify_rhs_from_stack ~rhs unification =
  fold_rhs_stack rhs unification
    ~f:(fun _pvar ~addr_hist_rhs:(addr_rhs, _pre_hist) ~addr_hist_lhs unification ->
      unify_rhs_from_address
        ~rhs:(rhs.AbductiveDomain.post :> BaseDomain.t)
        ~addr_rhs ~addr_hist_lhs unification )


let unify astate_rhs unification =
  let rhs_post = (astate_rhs.AbductiveDomain.post :> BaseDomain.t) in
  unify_rhs_from_stack ~rhs:astate_rhs unification >>= unify_rhs_from_array_indices ~rhs:rhs_post


let implies (astate_lhs : AbductiveDomain.t) (astate_rhs : AbductiveDomain.t) =
  L.d_printfln_escaped
    "Eternal Infinite Loop Check: Does this implication hold?@\n  @[%a@\n⊢@\n%a@]"
    AbductiveDomain.pp astate_lhs AbductiveDomain.pp astate_rhs ;
  (* TODO: it would be better to reset the "next fresh" abstract value to avoid polluting it with
     this implication state (where we throw away all the freshly-generated variables at the end) *)
  let empty_unification =
    { astate= astate_lhs
    ; subst= empty_to_lhs_subst
    ; rev_subst= AddressMap.empty
    ; visited= AddressSet.empty
    ; array_indices_to_visit= [] }
  in
  match unify astate_rhs empty_unification with
  | exception Contradiction contradiction ->
      L.d_printfln "Contradiction when unifying: %a" pp_contradiction contradiction ;
      false
  | FatalError _ ->
      L.d_printfln "Fatal error when unifying." ;
      false
  | Recoverable (unification, _) | Ok unification -> (
    match
      PulseFormula.implies_conditions_up_to ~subst:unification.subst astate_lhs.path_condition
        ~implies:astate_lhs.path_condition
    with
    | Ok () ->
        true
    | Error atom ->
        L.d_printfln_escaped "implication failed:@\n  @[%a@\n⊬ %a@]" PulseFormula.pp
          astate_lhs.path_condition
          (PulseFormulaAtom.pp_with_pp_var AbstractValue.pp)
          atom ;
        false )
