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

let is_ref_counted_or_block addr astate =
  AddressAttributes.get_static_type addr astate
  |> Option.exists ~f:(fun typ_name -> Typ.Name.is_objc_class typ_name)
  ||
  match AddressAttributes.get_dynamic_type addr astate with
  | Some {Attribute.typ= {desc= Tstruct typ_name}} ->
      Typ.Name.is_objc_class typ_name || Typ.Name.is_objc_block typ_name
  | _ ->
      false


let rec crop_seen_to_cycle seen_list addr =
  match seen_list with
  | [] ->
      []
  | hd :: rest ->
      if AbstractValue.equal hd addr then seen_list else crop_seen_to_cycle rest addr


let remove_non_objc_objects cycle astate =
  let has_static_dynamic_type astate v =
    let has_static_type = AddressAttributes.get_static_type v astate |> Option.is_some in
    has_static_type || AddressAttributes.get_dynamic_type v astate |> Option.is_some
  in
  List.filter ~f:(fun v -> not (has_static_dynamic_type astate v)) cycle


let get_assignment_trace astate addr =
  AddressAttributes.get_written_to addr astate |> Option.map ~f:snd


let create_values astate cycle =
  let values =
    List.map
      ~f:(fun v ->
        let value = Decompiler.find v astate in
        let trace = get_assignment_trace astate v in
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
  let check_retain_cycle src_addr =
    let rec contains_cycle ~seen (addr, hist) astate =
      (* [seen] tracks addresses met in the current path
         [addr] is the address to explore
      *)
      if AbstractValue.Set.mem addr !checked then Ok astate
      else
        let value = Decompiler.find addr astate in
        let is_known = not (DecompilerExpr.is_unknown value) in
        let is_seen = List.mem ~equal:AbstractValue.equal seen addr in
        let is_ref_counted_or_block = is_ref_counted_or_block addr astate in
        let not_previously_reported =
          not (AddressAttributes.is_in_reported_retain_cycle addr astate)
        in
        if is_known && is_seen && is_ref_counted_or_block && not_previously_reported then
          let seen = List.rev seen in
          let cycle = crop_seen_to_cycle seen addr in
          let cycle = remove_non_objc_objects cycle astate in
          let values = create_values astate cycle in
          if List.exists ~f:(fun {Diagnostic.trace} -> Option.is_some trace) values then
            match List.rev values with
            | {Diagnostic.trace} :: _ ->
                let astate =
                  List.fold ~init:astate
                    ~f:(fun astate addr -> AddressAttributes.in_reported_retain_cycle addr astate)
                    seen
                in
                let location =
                  Option.value_map trace
                    ~f:(fun trace -> Trace.get_outer_location trace)
                    ~default:location
                in
                let diagnostic = Diagnostic.RetainCycle {values; location} in
                Recoverable (astate, [ReportableError {astate; diagnostic}])
            | [] ->
                Ok astate
          else Ok astate
        else (
          if is_seen && ((not is_known) || not is_ref_counted_or_block) then
            (* add the `UNKNOWN` address at which we have found a cycle to the [checked]
               list in case we would have a cycle of `UNKNOWN` addresses, to avoid
               looping forever. Also add the not ref_counted addresses to checked, since
               we could loop forever otherwise *)
            checked := AbstractValue.Set.add addr !checked ;
          let res =
            AbductiveDomain.Memory.fold_edges ~init:(Ok astate) addr astate
              ~f:(fun acc (access, (accessed_addr, accessed_hist)) ->
                match acc with
                | Recoverable _ | FatalError _ ->
                    acc
                | Ok astate ->
                    if PulseRefCounting.is_strong_access tenv access then
                      (* This is needed to update the decompiler and be able to get good values when printing the path (above).
                          We don't want to return those changes in the decompiler to the rest of the analysis though, that was
                         changing some tests. So this checker only returns errors, but not the changes to the state. *)
                      let astate =
                        if not is_known then Memory.eval_edge (addr, hist) access astate |> fst
                        else astate
                      in
                      let seen = addr :: seen in
                      contains_cycle ~seen (accessed_addr, accessed_hist) astate
                    else Ok astate )
          in
          (* all paths down [addr] have been explored *)
          checked := AbstractValue.Set.add addr !checked ;
          res )
    in
    contains_cycle ~seen:[] src_addr orig_astate
  in
  List.fold addresses ~init:(Ok orig_astate) ~f:(fun acc (addr, hist) ->
      match acc with
      | Recoverable _ | FatalError _ ->
          acc
      | Ok astate ->
          if is_ref_counted_or_block addr orig_astate then check_retain_cycle (addr, hist)
          else Ok astate )


let check_retain_cycles_call tenv location func_args ret_opt astate =
  let actuals =
    let func_args = ValueOrigin.addr_hist_args func_args in
    List.map func_args ~f:(fun {ProcnameDispatcher.Call.FuncArg.arg_payload= addr_hist} ->
        addr_hist )
  in
  let addresses = Option.value_map ~default:actuals ~f:(fun ret -> ret :: actuals) ret_opt in
  if Language.curr_language_is Language.Clang then
    check_retain_cycles tenv location addresses astate
  else Ok astate


let check_retain_cycles_store tenv location addr astate =
  if Language.curr_language_is Language.Clang then check_retain_cycles tenv location [addr] astate
  else Ok astate
