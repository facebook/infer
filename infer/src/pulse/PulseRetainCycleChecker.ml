(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
open PulseBasicInterface
open PulseDomainInterface
open PulseOperationResult.Import
module F = Format

type cycle_data =
  { addr: AbstractValue.t
  ; hist: ValueHistory.t
  ; access: Access.t
  ; access_type: PulseRefCounting.access_type }

let pp_cycle_data fmt cycle_data =
  F.fprintf fmt "addr= %a, access= %a, access_type= %a" AbstractValue.pp cycle_data.addr Access.pp
    cycle_data.access PulseRefCounting.pp_access_type cycle_data.access_type


let is_ref_counted_or_block astate addr =
  AddressAttributes.get_static_type addr astate
  |> Option.exists ~f:(fun typ_name -> Typ.Name.is_objc_class typ_name)
  ||
  match PulseArithmetic.get_dynamic_type addr astate with
  | Some {typ= {desc= Tstruct typ_name}} ->
      Typ.Name.is_objc_class typ_name || Typ.Name.is_objc_block typ_name
  | _ ->
      false


let is_captured_function_pointer_or_block access =
  match access with
  | Access.FieldAccess fieldname ->
      Fieldname.is_capture_field_function_pointer fieldname
  | _ ->
      false


let rec crop_seen_to_cycle seen_list other_addr =
  match seen_list with
  | [] ->
      []
  | {addr} :: rest ->
      if AbstractValue.equal addr other_addr then seen_list else crop_seen_to_cycle rest addr


let has_static_dynamic_type astate v =
  let has_static_type = AddressAttributes.get_static_type v astate |> Option.is_some in
  has_static_type || PulseArithmetic.get_dynamic_type v astate |> Option.is_some


let remove_non_objc_objects cycle astate =
  (* do not remove the object if this will make the cycle length < 2 and make the message unclear *)
  if List.length cycle > 2 then
    List.filter ~f:(fun {addr} -> not (has_static_dynamic_type astate addr)) cycle
  else cycle


let get_assignment_trace astate addr =
  AddressAttributes.get_written_to addr astate |> Option.map ~f:snd


let create_values astate cycle =
  let values =
    List.map
      ~f:(fun {addr} ->
        let value = Decompiler.find addr astate in
        let trace = get_assignment_trace astate addr in
        let location = Option.map ~f:Trace.get_outer_location trace in
        {Diagnostic.expr= value; location; trace} )
      cycle
  in
  let sorted_values =
    List.sort
      ~compare:(fun {Diagnostic.location= loc1} {Diagnostic.location= loc2} ->
        Option.compare Location.compare loc1 loc2 )
      values
  in
  List.map
    ~f:(fun {Diagnostic.expr; location; trace} ->
      let location =
        if DecompilerExpr.includes_captured_variable expr || DecompilerExpr.includes_block expr then
          None
        else location
      in
      {Diagnostic.expr; location; trace} )
    sorted_values


let should_report_cycle astate cycle =
  let is_objc_or_block {addr; access} =
    match access with Access.FieldAccess _ -> is_ref_counted_or_block astate addr | _ -> false
  in
  let addr_in_retain_cycle {addr; access} =
    let not_previously_reported = not (AddressAttributes.is_in_reported_retain_cycle addr astate) in
    let path_condition = astate.AbductiveDomain.path_condition in
    let is_not_null = not (PulseFormula.is_known_zero path_condition addr) in
    let value = Decompiler.find addr astate in
    let is_known = not (DecompilerExpr.is_unknown value) in
    let is_objc_or_block_if_field_access =
      match access with Access.FieldAccess _ -> is_ref_counted_or_block astate addr | _ -> true
    in
    not_previously_reported && is_not_null && is_known && is_objc_or_block_if_field_access
  in
  List.exists ~f:is_objc_or_block cycle && List.for_all ~f:addr_in_retain_cycle cycle


let cycle_include_unknown_weak cycle =
  List.exists ~f:(fun {access_type} -> PulseRefCounting.equal_access_type access_type Unknown) cycle


(* A retain cycle is a memory path from an address to itself, following only
   strong references. From that definition, detecting them can be made
   trivial:
   Given an address and a list of adresses seen on the path, if the adress is
   part of the path then it is part of a retain cycle. Otherwise add that
   adress to the path and reiterate for each strongly referenced adresses
   there is from that adress. Explore paths starting from a given address.
   To improve on that simple algorithm, we can keep track of another marker
   to indicate adresses that have already been explored to indicate there
   will not be any retain cycle to be found down that path and skip it.
   This is handled by [check_retain_cycle] which will recursively explore
   paths from a given adress and mark explored adresses in the [checked]
   list. This function is called over a given address.

   When reporting a retain cycle, we want to give the location of its
   creation, therefore we need to remember location of the latest assignement
   in the cycle *)
let check_retain_cycles tenv location addresses orig_astate =
  (* remember explored adresses to avoid reexploring path without retain cycles *)
  let checked = ref AbstractValue.Set.empty in
  let is_seen l other_addr = List.exists ~f:(fun {addr} -> AbstractValue.equal addr other_addr) l in
  let check_retain_cycle src_addr =
    let rec contains_cycle ~(rev_seen : cycle_data list) (addr, hist) astate =
      if AbstractValue.Set.mem addr !checked then Ok astate
      else if is_seen rev_seen addr then
        let seen = List.rev rev_seen in
        let cycle = crop_seen_to_cycle seen addr in
        if should_report_cycle astate cycle then (
          Logging.d_printfln "Found cycle:\n \t%a" (Pp.seq ~sep:" -> \n\t" pp_cycle_data) cycle ;
          let unknown_access_type = cycle_include_unknown_weak cycle in
          let cycle = remove_non_objc_objects cycle astate in
          let values = create_values astate cycle in
          if List.exists ~f:(fun {Diagnostic.trace} -> Option.is_some trace) values then
            match List.rev values with
            | {Diagnostic.trace} :: _ ->
                let astate =
                  List.fold ~init:astate
                    ~f:(fun astate {addr} -> AddressAttributes.in_reported_retain_cycle addr astate)
                    seen
                in
                let location =
                  Option.value_map trace
                    ~f:(fun trace -> Trace.get_outer_location trace)
                    ~default:location
                in
                let diagnostic = Diagnostic.RetainCycle {values; location; unknown_access_type} in
                Recoverable (astate, [ReportableError {astate; diagnostic}])
            | [] ->
                Ok astate
          else Ok astate )
        else Ok astate
      else
        let res =
          AbductiveDomain.Memory.fold_edges ~init:(Ok astate) addr astate
            ~f:(fun acc (access, (accessed_addr, accessed_hist)) ->
              match acc with
              | Recoverable _ | FatalError _ ->
                  acc
              | Ok astate ->
                  let access_type = PulseRefCounting.get_access_type tenv access in
                  if
                    (not (PulseRefCounting.equal_access_type access_type PulseRefCounting.Weak))
                    && not (is_captured_function_pointer_or_block access)
                  then
                    (* This is needed to update the decompiler and be able to get good values when printing the path (above).
                        We don't want to return those changes in the decompiler to the rest of the analysis though, that was
                       changing some tests. So this checker only returns errors, but not the changes to the state. *)
                    let astate =
                      let value = Decompiler.find addr astate in
                      if DecompilerExpr.is_unknown value then
                        Memory.eval_edge (addr, hist) access astate |> fst
                      else astate
                    in
                    let rev_seen = {addr; hist; access; access_type} :: rev_seen in
                    contains_cycle ~rev_seen (accessed_addr, accessed_hist) astate
                  else Ok astate )
        in
        (* all paths down [addr] have been explored *)
        checked := AbstractValue.Set.add addr !checked ;
        res
    in
    contains_cycle ~rev_seen:[] src_addr orig_astate
  in
  Logging.d_printfln "checking retain cycles, addresses to check are %a"
    (Pp.comma_seq AbstractValue.pp) (List.map ~f:fst addresses) ;
  List.fold addresses ~init:(Ok orig_astate) ~f:(fun acc (addr, hist) ->
      match acc with Recoverable _ | FatalError _ -> acc | Ok _ -> check_retain_cycle (addr, hist) )


let check_retain_cycles_call tenv location func_args ret_opt astate =
  let actuals =
    let func_args = ValueOrigin.addr_hist_args func_args in
    List.map func_args ~f:(fun {ProcnameDispatcher.Call.FuncArg.arg_payload= addr_hist} ->
        addr_hist )
  in
  let addresses = Option.value_map ~default:actuals ~f:(fun ret -> ret :: actuals) ret_opt in
  if Language.curr_language_is Language.Clang && IssueType.retain_cycle.enabled then
    check_retain_cycles tenv location addresses astate
  else Ok astate


let check_retain_cycles_store tenv location addr astate =
  if Language.curr_language_is Language.Clang && IssueType.retain_cycle.enabled then
    check_retain_cycles tenv location [addr] astate
  else Ok astate
