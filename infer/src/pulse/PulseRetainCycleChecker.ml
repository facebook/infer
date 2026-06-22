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


(* A Swift enum is a value type and can never participate in a retain cycle. Swift's name
   mangling encodes the type kind with a single-letter suffix on the mangled name: [C] for
   class, [O] for enum, [V] for struct, [P] for protocol. When the dynamic-type stamp on an
   abstract address is a [SwiftClass _] whose mangled name ends in [O], treat it as a value
   type for cycle classification — otherwise the cycle checker walks through enum payloads as
   if they were ref-counted heap edges and reports bogus 2-cycles like [FilterValue → FilterValue]
   in pattern-match sites. *)
let is_swift_enum_class typ_name =
  match (typ_name : Typ.Name.t) with
  | SwiftClass scn ->
      String.is_suffix (SwiftClassName.mangled scn) ~suffix:"O"
  | _ ->
      false


let is_ref_counted_or_block astate addr =
  let is_refcounted_swift_or_objc typ_name =
    (Typ.Name.is_objc_class typ_name || Typ.Name.is_swift_class typ_name)
    && not (is_swift_enum_class typ_name)
  in
  AddressAttributes.get_static_type addr astate |> Option.exists ~f:is_refcounted_swift_or_objc
  ||
  match PulseArithmetic.get_dynamic_type addr astate with
  | Some {typ= {desc= Tstruct typ_name}} ->
      is_refcounted_swift_or_objc typ_name || Typ.Name.is_objc_block typ_name
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


let remove_non_objc_swift_objects cycle astate =
  (* do not remove the object if this will make the cycle length < 2 and make the message unclear *)
  if List.length cycle > 2 then
    List.filter ~f:(fun {addr} -> not (has_static_dynamic_type astate addr)) cycle
  else cycle


let get_assignment_trace astate addr =
  AddressAttributes.get_written_to addr astate |> Option.map ~f:snd


(* Swift stdlib mnemonic shorthand (Si, SS, Sb, Sd, Sf, Su) — and Optional
   wrapping via the [Sg] suffix — that the length-prefix-based heuristic
   demangler in [SwiftClassName.demangle_mangled] can't reach because there
   is no [<digit>+<name>] token to extract. Render-time only: does not
   modify the underlying [SwiftClassName.t], so frontend code that
   pattern-matches on the stdlib suffixes (e.g. [Sg] for Optional
   classification, [Sa] for Array, etc.) continues to see the raw mangled
   token. Returns [None] for shapes outside this small whitelist so the
   caller can fall back to whichever rendering it was going to use. *)
let demangle_swift_stdlib_mnemonic mangled =
  let strip_t s = String.chop_prefix s ~prefix:"T" |> Option.value ~default:s in
  let rec aux s =
    match s with
    | "Si" ->
        Some "Int"
    | "Su" ->
        Some "UInt"
    | "SS" ->
        Some "String"
    | "Sb" ->
        Some "Bool"
    | "Sd" ->
        Some "Double"
    | "Sf" ->
        Some "Float"
    | _ when String.is_suffix s ~suffix:"Sg" ->
        let prefix = String.chop_suffix_exn s ~suffix:"Sg" in
        Option.map (aux prefix) ~f:(fun inner -> inner ^ "?")
    | _ ->
        None
  in
  aux (strip_t mangled)


let get_expr_with_fallback astate addr =
  let is_ptr_elt_name typ_name =
    String.is_substring (F.asprintf "%a" Typ.Name.pp typ_name) ~substring:"ptr_elt"
  in
  (* Helper: Extract type, explicitly filtering out 'ptr_elt' placeholders *)
  let get_type v =
    let dyn_type_opt =
      match PulseArithmetic.get_dynamic_type v astate with
      | Some {typ= {desc= Tstruct typ_name}} ->
          Some typ_name
      | Some {typ= {desc= Tptr ({desc= Tstruct typ_name}, _)}} ->
          Some typ_name
      | _ ->
          None
    in
    match dyn_type_opt with
    | Some typ_name when not (is_ptr_elt_name typ_name) ->
        Some typ_name
    | _ -> (
      match AddressAttributes.get_static_type v astate with
      | Some typ_name when not (is_ptr_elt_name typ_name) ->
          Some typ_name
      | _ ->
          None )
  in
  (* Search up to 5 edges deep, but ONLY through strictly safe structural boundaries *)
  let rec find_type_in_graph depth current_addr =
    match get_type current_addr with
    | Some t ->
        Some t
    | None when depth > 0 ->
        let current_is_box =
          lazy
            ( match AddressAttributes.get_static_type current_addr astate with
            | Some typ_name ->
                is_ptr_elt_name typ_name
            | None ->
                false )
        in
        AbductiveDomain.Memory.fold_edges current_addr astate ~init:None
          ~f:(fun acc (access, (accessed_addr, _)) ->
            match acc with
            | Some _ ->
                acc
            | None ->
                let is_safe =
                  match access with
                  | Access.Dereference ->
                      true
                  | Access.FieldAccess fn ->
                      (* ONLY traverse tuple fields if the parent is a compiler box *)
                      Lazy.force current_is_box
                      && String.is_prefix (Fieldname.get_field_name fn)
                           ~prefix:"__infer_tuple_field_"
                  | _ ->
                      false
                in
                if is_safe then find_type_in_graph (depth - 1) accessed_addr else None )
    | _ ->
        None
  in
  (* Names that the LLVM/Llair frontend assigns to compiler-introduced SSA
     temporaries: bare [varN], [__TEMP N], [getelementptr_offset_N], and
     fields prefixed [__infer_tuple_field_]. These leak into retain-cycle
     traces as e.g. [var21->field_1] or [swift_allocObject().__infer_tuple_field_3],
     which are unactionable to a product engineer reading the report. Treat
     such decompiled values as effectively unknown so the type-recovery
     fallback below runs and produces a more informative label like
     [object of type Foo] or [Swift closure]. *)
  let is_synthetic_temp_pvar pvar =
    let name = Mangled.to_string (Pvar.get_name pvar) in
    String.is_prefix name ~prefix:"var"
    && String.length name > 3
    && String.for_all (String.sub name ~pos:3 ~len:(String.length name - 3)) ~f:Char.is_digit
    || String.is_prefix name ~prefix:"__TEMP"
    || String.is_prefix name ~prefix:"getelementptr_offset_"
  in
  let is_synthetic_decompiled value =
    match (value : DecompilerExpr.t) with
    | SourceExpr ((DecompilerExpr.PVar pvar, accesses), _) ->
        is_synthetic_temp_pvar pvar
        || List.exists accesses ~f:(function
          | DecompilerExpr.FieldAccess fn ->
              String.is_prefix (Fieldname.get_field_name fn) ~prefix:"__infer_tuple_field_"
          | _ ->
              false )
    | _ ->
        false
  in
  let value = Decompiler.find addr astate in
  if DecompilerExpr.is_unknown value || is_synthetic_decompiled value then
    (* Execute the safe bounded search *)
    let type_name_opt = find_type_in_graph 5 addr in
    (* Format the type cleanly, catching closures explicitly *)
    let string_name =
      match type_name_opt with
      | None ->
          "dynamically allocated object"
      | Some (Typ.SwiftClass name)
        when SwiftClassName.equal name SwiftClassName.swift_alloc_unknown_type ->
          "dynamically allocated object"
      | Some typ_name -> (
          let type_str = F.asprintf "%a" Typ.Name.pp typ_name in
          let is_tuple_class =
            match typ_name with
            | Typ.SwiftClass name ->
                String.equal (SwiftClassName.mangled name) "__infer_tuple_class"
            | _ ->
                false
          in
          let stdlib_mnemonic_name =
            match typ_name with
            | Typ.SwiftClass name ->
                demangle_swift_stdlib_mnemonic (SwiftClassName.mangled name)
            | _ ->
                None
          in
          if String.is_substring type_str ~substring:"swift::function" then "Swift closure"
          else if is_tuple_class then
            (* Synthetic [Textual.swift_tuple_class_name]. Reached only when args
               contain no [swift::function] — tuple-wrapped closures take the
               [Swift closure] branch above. *)
            "Swift tuple"
          else
            match stdlib_mnemonic_name with
            | Some name ->
                "object of type " ^ name
            | None ->
                "object of type " ^ type_str )
    in
    (* Create a LOCAL variable Pvar using a dummy function context *)
    let dummy_procname = Procname.from_string_c_fun "" in
    let pvar = Pvar.mk (Mangled.from_string string_name) dummy_procname in
    (* Use Dereference to get the value, which strips the '&' address-of operator *)
    DecompilerExpr.SourceExpr ((DecompilerExpr.PVar pvar, [DecompilerExpr.Dereference]), Some addr)
  else value


(* True iff [addr] is currently typed as a Swift closure tuple — i.e. an
   [__infer_tuple_class<…, swift::function<…>>] instance — using whichever of
   [dynamic_type] / [static_type] is available. The closure-holder shapes in
   [PulseModelsSwift] stamp this on the parent of a captured-environment access. *)
(* The [__infer_tuple_class] synthetic class is emitted by the Swift frontend
   for the calling-convention tuple that backs every escaping closure
   ([(function_pointer, captured_environment)]). It is occasionally also used
   for source-level Swift tuples, but those are value types whose elements
   can't independently participate in a reference cycle, so encountering
   [__infer_tuple_field_1] of a [__infer_tuple_class] inside a detected cycle
   is, in practice, the captured-environment slot of a closure. We don't
   additionally require the args to mention [swift::function] because the
   frontend recovery of those args is unreliable (prod sometimes surfaces them
   as e.g. [<int,int>]). *)
let is_swift_closure_tuple astate addr =
  let typ_name_opt =
    match PulseArithmetic.get_dynamic_type addr astate with
    | Some {typ= {desc= Tstruct typ_name}} ->
        Some typ_name
    | _ ->
        AddressAttributes.get_static_type addr astate
  in
  match typ_name_opt with
  | Some (Typ.SwiftClass name) ->
      String.equal (SwiftClassName.mangled name) "__infer_tuple_class"
  | _ ->
      false


(* The cycle is a circular list: [cycle.(i).access] is the edge taken FROM
   [cycle.(i)] TO reach [cycle.((i + 1) mod n)]. So when [cycle.(i).access] is
   [__infer_tuple_field_1] the captured-environment node is [cycle.((i + 1) mod
   n)] and its closure-tuple parent is [cycle.(i)]. Returns an address-keyed
   lookup so that [remove_non_objc_swift_objects] (run before [create_values])
   doesn't break the index-based pairing. *)
let captured_env_flags_for_cycle astate (cycle : cycle_data list) =
  let cycle_arr = Array.of_list cycle in
  let n = Array.length cycle_arr in
  if Int.equal n 0 then AbstractValue.Map.empty
  else
    Array.foldi cycle_arr ~init:AbstractValue.Map.empty ~f:(fun i acc {addr= parent_addr; access} ->
        match access with
        | Access.FieldAccess fn
          when String.equal (Fieldname.get_field_name fn) "__infer_tuple_field_1"
               && is_swift_closure_tuple astate parent_addr ->
            let captured_env_addr = cycle_arr.((i + 1) mod n).addr in
            AbstractValue.Map.add captured_env_addr true acc
        | _ ->
            acc )


(* A retain-cycle participant that is a bare program variable (e.g. [self], or an
   ObjC notification [observer] token) is decompiled as the *address* of that variable
   and renders as [&self] / [&observer]. The address-of is an implementation detail
   here -- the cycle is between the *objects*, not the stack slots -- and it is also
   inconsistent with how field participants ([self->field]) and ObjC [self] are shown.
   Append a [Dereference] so it renders as [self] / [observer]. (Same trick the
   type-recovery fallback in [get_expr_with_fallback] already uses to strip the [&].) *)
let strip_leading_address_of (expr : DecompilerExpr.t) : DecompilerExpr.t =
  match expr with
  | SourceExpr (((DecompilerExpr.PVar _ as base), []), v) ->
      SourceExpr ((base, [DecompilerExpr.Dereference]), v)
  | _ ->
      expr


let create_values astate ~captured_env_flags (cycle : cycle_data list) =
  let values : Diagnostic.retain_cycle_data list =
    List.map cycle ~f:(fun {addr} ->
        let value = get_expr_with_fallback astate addr |> strip_leading_address_of in
        let trace = get_assignment_trace astate addr in
        let location = Option.map ~f:Trace.get_outer_location trace in
        let is_captured_env_of_closure =
          AbstractValue.Map.find_opt addr captured_env_flags |> Option.value ~default:false
        in
        {Diagnostic.expr= value; location; trace; is_captured_env_of_closure} )
  in
  let sorted_values =
    List.sort
      ~compare:(fun
          ({location= loc1} : Diagnostic.retain_cycle_data)
          ({location= loc2} : Diagnostic.retain_cycle_data)
        -> Option.compare Location.compare loc1 loc2 )
      values
  in
  List.map
    ~f:(fun {Diagnostic.expr; location; trace; is_captured_env_of_closure} ->
      let location =
        if DecompilerExpr.includes_captured_variable expr || DecompilerExpr.includes_block expr then
          None
        else location
      in
      {Diagnostic.expr; location; trace; is_captured_env_of_closure} )
    sorted_values


let should_report_cycle astate cycle =
  let is_objc_swift_or_block {addr; access} =
    match access with Access.FieldAccess _ -> is_ref_counted_or_block astate addr | _ -> false
  in
  let addr_in_retain_cycle {addr; access} =
    let not_previously_reported = not (AddressAttributes.is_in_reported_retain_cycle addr astate) in
    let path_condition = astate.AbductiveDomain.path_condition in
    let is_not_null = not (PulseFormula.is_known_zero path_condition addr) in
    let is_objc_swift_or_block_if_field_access =
      match access with Access.FieldAccess _ -> is_ref_counted_or_block astate addr | _ -> true
    in
    let blocklisted =
      Option.value_map Config.pulse_retain_cycle_blocklist_pattern ~default:false ~f:(fun re ->
          let value = Decompiler.find addr astate in
          let expr_str = F.asprintf "%a" DecompilerExpr.pp value in
          Str.string_match re expr_str 0 )
    in
    not_previously_reported && is_not_null && is_objc_swift_or_block_if_field_access
    && not blocklisted
  in
  List.exists ~f:is_objc_swift_or_block cycle && List.for_all ~f:addr_in_retain_cycle cycle


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
          let captured_env_flags = captured_env_flags_for_cycle astate cycle in
          let cycle = remove_non_objc_swift_objects cycle astate in
          let values = create_values astate ~captured_env_flags cycle in
          if List.exists ~f:(fun {Diagnostic.trace} -> Option.is_some trace) values then
            match List.rev values with
            | {Diagnostic.trace} :: _ ->
                let astate =
                  List.fold ~init:astate
                    ~f:(fun astate {addr} -> AddressAttributes.in_reported_retain_cycle addr astate)
                    seen
                in
                let location =
                  (* The trace's outer location points at the writing instruction. When
                     the write lives in a compiler-generated thunk (Swift partial-apply
                     / autoclosure body / async continuation / ObjC bridging thunk /
                     overlay init / witness-table accessor), that location's file is a
                     [SourceFile.compiler_generated] sentinel (one per bitcode) — not
                     useful to the product engineer reading the report. Fall back to
                     the cycle's detection site (the analyzed procedure's store / call
                     location) so the report lands in a real user file. *)
                  match trace with
                  | None ->
                      location
                  | Some trace ->
                      let trace_loc = Trace.get_outer_location trace in
                      if SourceFile.is_compiler_generated trace_loc.Location.file then location
                      else trace_loc
                in
                (* Same idea applied per-step: an individual [ViaCall]/[Immediate] node
                   (or any [ValueHistory] event nested under it) whose location lives in
                   compiler-generated code would render in the Errlog as an unactionable
                   [<bitcode_id>:compiler-generated:0:0] entry in the IDE / Phabricator
                   inline lint view. Replace each such location with the closest enclosing
                   non-sentinel location, falling back to the report's primary [location]
                   only at the outermost level. This keeps trace-step navigation coherent:
                   clicking a synthetic step lands the cursor at the user-code site that
                   called into the synthetic frame, not back at the headline. *)
                let values =
                  List.map values ~f:(fun (v : Diagnostic.retain_cycle_data) ->
                      let trace =
                        Option.map
                          ~f:(Trace.redact_compiler_generated_locations ~fallback:location)
                          v.trace
                      in
                      {v with trace} )
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


let should_run_retain_cycle_check () =
  (Language.curr_language_is Language.Clang || Language.curr_language_is Language.Swift)
  && IssueType.retain_cycle.enabled


let check_retain_cycles_call tenv location func_args ret_opt astate =
  let actuals =
    let func_args = ValueOrigin.addr_hist_args func_args in
    List.map func_args ~f:(fun {FuncArg.arg_payload= addr_hist} -> addr_hist)
  in
  let addresses = Option.value_map ~default:actuals ~f:(fun ret -> ret :: actuals) ret_opt in
  if should_run_retain_cycle_check () then check_retain_cycles tenv location addresses astate
  else Ok astate


let check_retain_cycles_store tenv location addr astate =
  if should_run_retain_cycle_check () then check_retain_cycles tenv location [addr] astate
  else Ok astate
