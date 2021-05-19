(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module AccessExpression = HilExp.AccessExpression
module F = Format
module L = Logging

type analysis_data =
  {interproc: RacerDDomain.summary InterproceduralAnalysis.t; formals: FormalMap.t}

module TransferFunctions (CFG : ProcCfg.S) = struct
  module CFG = CFG
  module Domain = RacerDDomain

  type nonrec analysis_data = analysis_data

  let do_call_acquiring_ownership ret_access_exp astate =
    let open Domain in
    let ownership =
      OwnershipDomain.add ret_access_exp OwnershipAbstractValue.owned astate.ownership
    in
    {astate with ownership}


  let process_call_without_summary tenv ret_access_exp callee_pname actuals astate =
    let open Domain in
    if RacerDModels.is_synchronized_container_constructor tenv callee_pname actuals then
      apply_to_first_actual actuals astate ~f:(fun receiver ->
          let attribute_map = AttributeMapDomain.add receiver Synchronized astate.attribute_map in
          {astate with attribute_map} )
    else if RacerDModels.is_converter_to_synchronized_container tenv callee_pname actuals then
      let attribute_map = AttributeMapDomain.add ret_access_exp Synchronized astate.attribute_map in
      {astate with attribute_map}
    else if RacerDModels.is_box callee_pname then
      apply_to_first_actual actuals astate ~f:(fun actual_access_expr ->
          if AttributeMapDomain.is_functional astate.attribute_map actual_access_expr then
            (* TODO: check for constants, which are functional? *)
            let attribute_map =
              AttributeMapDomain.add ret_access_exp Functional astate.attribute_map
            in
            {astate with attribute_map}
          else astate )
    else do_call_acquiring_ownership ret_access_exp astate


  let process_for_unannotated_interface_call tenv formals call_flags callee_pname actuals loc astate
      =
    if RacerDModels.should_flag_interface_call tenv actuals call_flags callee_pname then
      Domain.add_unannotated_call_access formals callee_pname actuals loc astate
    else astate


  let process_for_thread_assert_effect ret_access_exp callee_pname (astate : Domain.t) =
    let open Domain in
    match ConcurrencyModels.get_thread_assert_effect callee_pname with
    | BackgroundThread ->
        {astate with threads= ThreadsDomain.AnyThread}
    | MainThread ->
        {astate with threads= ThreadsDomain.AnyThreadButSelf}
    | MainThreadIfTrue ->
        let attribute_map =
          AttributeMapDomain.add ret_access_exp Attribute.OnMainThread astate.attribute_map
        in
        {astate with attribute_map}
    | UnknownThread ->
        astate


  let process_call_summary analyze_dependency tenv formals ret_access_exp callee_pname actuals loc
      astate =
    match analyze_dependency callee_pname with
    | Some (callee_proc_desc, summary) ->
        Domain.integrate_summary formals ~callee_proc_desc summary ret_access_exp callee_pname
          actuals loc astate
    | None ->
        process_call_without_summary tenv ret_access_exp callee_pname actuals astate


  let process_lock_effect_or_summary analyze_dependency tenv formals ret_access_exp callee_pname
      actuals loc (astate : Domain.t) =
    let open Domain in
    match ConcurrencyModels.get_lock_effect callee_pname actuals with
    | Lock _ | GuardLock _ | GuardConstruct {acquire_now= true} ->
        { astate with
          locks= LockDomain.acquire_lock astate.locks
        ; threads= ThreadsDomain.update_for_lock_use astate.threads }
    | Unlock _ | GuardDestroy _ | GuardUnlock _ ->
        { astate with
          locks= LockDomain.release_lock astate.locks
        ; threads= ThreadsDomain.update_for_lock_use astate.threads }
    | LockedIfTrue _ | GuardLockedIfTrue _ ->
        let attribute_map =
          AttributeMapDomain.add ret_access_exp Attribute.LockHeld astate.attribute_map
        in
        {astate with attribute_map; threads= ThreadsDomain.update_for_lock_use astate.threads}
    | GuardConstruct {acquire_now= false} ->
        astate
    | NoEffect ->
        process_call_summary analyze_dependency tenv formals ret_access_exp callee_pname actuals loc
          astate


  let process_for_functional_values tenv ret_access_exp callee_pname (astate : Domain.t) =
    let open Domain in
    if PatternMatch.override_exists RacerDModels.is_functional tenv callee_pname then
      let attribute_map = AttributeMapDomain.add ret_access_exp Functional astate.attribute_map in
      {astate with attribute_map}
    else astate


  let process_for_onwership_acquisition tenv ret_access_exp callee_pname astate =
    if
      PatternMatch.override_exists
        (RacerDModels.has_return_annot Annotations.ia_is_returns_ownership)
        tenv callee_pname
    then do_call_acquiring_ownership ret_access_exp astate
    else astate


  let do_proc_call ret_base callee_pname actuals call_flags loc
      {interproc= {tenv; analyze_dependency}; formals} (astate : Domain.t) =
    let ret_access_exp = AccessExpression.base ret_base in
    process_for_unannotated_interface_call tenv formals call_flags callee_pname actuals loc astate
    |> process_for_thread_assert_effect ret_access_exp callee_pname
    |> process_lock_effect_or_summary analyze_dependency tenv formals ret_access_exp callee_pname
         actuals loc
    |> process_for_functional_values tenv ret_access_exp callee_pname
    |> process_for_onwership_acquisition tenv ret_access_exp callee_pname


  let do_assignment lhs_access_exp rhs_exp loc {interproc= {tenv}; formals} (astate : Domain.t) =
    let open Domain in
    let astate = add_access tenv formals loc ~is_write:false astate rhs_exp in
    let rhs_access_exprs = HilExp.get_access_exprs rhs_exp in
    let is_functional =
      (not (List.is_empty rhs_access_exprs))
      && List.for_all rhs_access_exprs ~f:(AttributeMapDomain.is_functional astate.attribute_map)
      &&
      match AccessExpression.get_typ lhs_access_exp tenv with
      | Some {Typ.desc= Typ.Tint ILong | Tfloat FDouble} ->
          (* writes to longs and doubles are not guaranteed to be atomic in Java
             (http://docs.oracle.com/javase/specs/jls/se7/html/jls-17.html#jls-17.7), so there
             can be a race even if the RHS is functional *)
          false
      | _ ->
          true
    in
    let astate =
      if is_functional then
        (* we want to forget about writes to @Functional fields altogether, otherwise we'll
           report spurious read/write races *)
        astate
      else
        add_access tenv formals loc ~is_write:true astate (HilExp.AccessExpression lhs_access_exp)
    in
    let ownership = OwnershipDomain.propagate_assignment lhs_access_exp rhs_exp astate.ownership in
    let attribute_map =
      AttributeMapDomain.propagate_assignment lhs_access_exp rhs_exp astate.attribute_map
    in
    {astate with ownership; attribute_map}


  let do_assume formals assume_exp loc tenv (astate : Domain.t) =
    let open Domain in
    let apply_choice bool_value (acc : Domain.t) = function
      | Attribute.LockHeld ->
          let locks =
            if bool_value then LockDomain.acquire_lock acc.locks
            else LockDomain.release_lock acc.locks
          in
          {acc with locks}
      | Attribute.OnMainThread ->
          let threads =
            if bool_value then ThreadsDomain.AnyThreadButSelf else ThreadsDomain.AnyThread
          in
          {acc with threads}
      | Attribute.(Functional | Nothing | Synchronized) ->
          acc
    in
    let astate = add_access tenv formals loc ~is_write:false astate assume_exp in
    match HilExp.get_access_exprs assume_exp with
    | [access_expr] ->
        HilExp.eval_boolean_exp access_expr assume_exp
        |> Option.value_map ~default:astate ~f:(fun bool_value ->
               (* prune (prune_exp) can only evaluate to true if the choice is [bool_value].
                  add the constraint that the choice must be [bool_value] to the state *)
               AttributeMapDomain.get access_expr astate.attribute_map
               |> apply_choice bool_value astate )
    | _ ->
        astate


  let exec_instr astate ({interproc= {proc_desc; tenv}; formals} as analysis_data) _ _ instr =
    match (instr : HilInstr.t) with
    | Call (ret_base, Direct callee_pname, actuals, call_flags, loc) ->
        let astate = Domain.add_reads_of_hilexps tenv formals actuals loc astate in
        if RacerDModels.acquires_ownership callee_pname tenv then
          do_call_acquiring_ownership (AccessExpression.base ret_base) astate
        else if RacerDModels.is_container_write tenv callee_pname then
          Domain.add_container_access tenv formals ~is_write:true ret_base callee_pname actuals loc
            astate
        else if RacerDModels.is_container_read tenv callee_pname then
          Domain.add_container_access tenv formals ~is_write:false ret_base callee_pname actuals loc
            astate
        else do_proc_call ret_base callee_pname actuals call_flags loc analysis_data astate
    | Call (_, Indirect _, _, _, _) ->
        if Procname.is_java (Procdesc.get_proc_name proc_desc) then
          L.(die InternalError) "Unexpected indirect call instruction %a" HilInstr.pp instr
        else astate
    | Assign (lhs_access_expr, rhs_exp, loc) ->
        do_assignment lhs_access_expr rhs_exp loc analysis_data astate
    | Assume (assume_exp, _, _, loc) ->
        do_assume formals assume_exp loc tenv astate
    | Metadata _ ->
        astate


  let pp_session_name _node fmt = F.pp_print_string fmt "racerd"
end

module Analyzer = LowerHil.MakeAbstractInterpreter (TransferFunctions (ProcCfg.Normal))

(** Compute the attributes (of static variables) set up by the class initializer. *)
let set_class_init_attributes interproc (astate : RacerDDomain.t) =
  let open RacerDDomain in
  let attribute_map =
    ConcurrencyUtils.get_java_class_initializer_summary_of interproc
    |> Option.value_map ~default:AttributeMapDomain.top ~f:(fun summary -> summary.attributes)
  in
  ({astate with attribute_map} : t)


(** Compute the attributes of instance variables that all constructors agree on. *)
let set_constructor_attributes ({InterproceduralAnalysis.proc_desc} as interproc)
    (astate : RacerDDomain.t) =
  let open RacerDDomain in
  let procname = Procdesc.get_proc_name proc_desc in
  (* make a local [this] variable, for replacing all constructor attribute map keys' roots *)
  let local_this = Pvar.mk Mangled.this procname |> Var.of_pvar in
  let make_local exp =
    (* contract here matches that of [StarvationDomain.summary_of_astate] *)
    let var, typ = HilExp.AccessExpression.get_base exp in
    if Var.is_global var then
      (* let expressions rooted at globals unchanged, these are probably from class initialiser *)
      exp
    else (
      assert (Var.is_this var) ;
      HilExp.AccessExpression.replace_base ~remove_deref_after_base:false (local_this, typ) exp )
  in
  let localize_attrs attributes =
    AttributeMapDomain.(fold (fun exp attr acc -> add (make_local exp) attr acc) attributes empty)
  in
  let attribute_map =
    ConcurrencyUtils.get_java_constructor_summaries_of interproc
    (* make instances of [this] local to the current procedure and select only the attributes *)
    |> List.map ~f:(fun (summary : summary) -> localize_attrs summary.attributes)
    (* join all the attribute maps together *)
    |> List.reduce ~f:AttributeMapDomain.join
    |> Option.value ~default:AttributeMapDomain.top
  in
  {astate with attribute_map}


let set_initial_attributes ({InterproceduralAnalysis.proc_desc} as interproc) astate =
  let procname = Procdesc.get_proc_name proc_desc in
  match procname with
  | Procname.Java java_pname when Procname.Java.is_class_initializer java_pname ->
      (* we are analyzing the class initializer, don't go through on-demand again *)
      astate
  | Procname.Java java_pname when Procname.Java.(is_constructor java_pname || is_static java_pname)
    ->
      (* analyzing a constructor or static method, so we need the attributes established by the
         class initializer *)
      set_class_init_attributes interproc astate
  | Procname.Java _ ->
      (* we are analyzing an instance method, so we need constructor-established attributes
         which will include those by the class initializer *)
      set_constructor_attributes interproc astate
  | _ ->
      astate


let analyze ({InterproceduralAnalysis.proc_desc; tenv} as interproc) =
  let open RacerDDomain in
  let proc_name = Procdesc.get_proc_name proc_desc in
  let open ConcurrencyModels in
  let add_owned_formal acc base = OwnershipDomain.add base OwnershipAbstractValue.owned acc in
  let add_conditionally_owned_formal =
    let is_owned_formal {Annot.class_name} =
      (* [@InjectProp] allocates a fresh object to bind to the parameter *)
      String.is_suffix ~suffix:Annotations.inject_prop class_name
    in
    let method_annotation = (Procdesc.get_attributes proc_desc).method_annotation in
    let is_inject_prop = Annotations.ma_has_annotation_with method_annotation is_owned_formal in
    fun acc formal formal_index ->
      let ownership_value =
        if is_inject_prop then OwnershipAbstractValue.owned
        else OwnershipAbstractValue.make_owned_if formal_index
      in
      OwnershipDomain.add formal ownership_value acc
  in
  if RacerDModels.should_analyze_proc tenv proc_name then
    let locks =
      if Procdesc.is_java_synchronized proc_desc || Procdesc.is_csharp_synchronized proc_desc then
        LockDomain.(acquire_lock bottom)
      else LockDomain.bottom
    in
    let threads =
      if runs_on_ui_thread tenv proc_name || RacerDModels.is_thread_confined_method tenv proc_name
      then ThreadsDomain.AnyThreadButSelf
      else if
        Procdesc.is_java_synchronized proc_desc
        || Procdesc.is_csharp_synchronized proc_desc
        || RacerDModels.is_marked_thread_safe proc_name tenv
      then ThreadsDomain.AnyThread
      else ThreadsDomain.NoThread
    in
    let ownership =
      let is_initializer = RacerDModels.is_initializer tenv proc_name in
      let is_injected =
        is_initializer && Annotations.pdesc_has_return_annot proc_desc Annotations.ia_is_inject
      in
      Procdesc.get_formals proc_desc
      |> List.foldi ~init:OwnershipDomain.empty ~f:(fun index acc (name, typ) ->
             let base =
               AccessPath.base_of_pvar (Pvar.mk name proc_name) typ |> AccessExpression.base
             in
             if is_injected then
               (* if a constructor is called via DI, all of its formals will be freshly allocated and
                  therefore owned. we assume that constructors annotated with [@Inject] will only be
                  called via DI or using fresh parameters. *)
               add_owned_formal acc base
             else if is_initializer && Int.equal 0 index then
               (* express that the constructor owns [this] *)
               add_owned_formal acc base
             else add_conditionally_owned_formal acc base index )
    in
    let initial = set_initial_attributes interproc {bottom with ownership; threads; locks} in
    let formals = FormalMap.make proc_desc in
    let analysis_data = {interproc; formals} in
    Analyzer.compute_post analysis_data ~initial proc_desc
    |> Option.map ~f:(astate_to_summary proc_desc formals)
  else Some empty_summary
