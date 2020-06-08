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
module MF = MarkupFormatter

type analysis_data =
  {interproc: RacerDDomain.summary InterproceduralAnalysis.t; formals: FormalMap.t}

module TransferFunctions (CFG : ProcCfg.S) = struct
  module CFG = CFG
  module Domain = RacerDDomain

  type nonrec analysis_data = analysis_data

  let rec get_access_exp = function
    | HilExp.AccessExpression access_expr ->
        Some access_expr
    | HilExp.Cast (_, e) | HilExp.Exception e ->
        get_access_exp e
    | _ ->
        None


  let get_first_actual actuals = List.hd actuals |> Option.bind ~f:get_access_exp

  let apply_to_first_actual ~f astate actuals =
    get_first_actual actuals |> Option.value_map ~default:astate ~f


  let add_access formals loc ~is_write_access locks threads ownership tenv access_domain exp =
    let open Domain in
    let rec add_field_accesses prefix_path acc = function
      | [] ->
          acc
      | access :: access_list ->
          let prefix_path' = Option.value_exn (AccessExpression.add_access prefix_path access) in
          if
            (not (HilExp.Access.is_field_or_array_access access))
            || RacerDModels.is_safe_access access prefix_path tenv
          then add_field_accesses prefix_path' acc access_list
          else
            let is_write = List.is_empty access_list && is_write_access in
            let pre = OwnershipDomain.get_owned prefix_path ownership in
            let snapshot_opt =
              AccessSnapshot.make_access formals prefix_path' ~is_write loc locks threads pre
            in
            let access_acc' = AccessDomain.add_opt snapshot_opt acc in
            add_field_accesses prefix_path' access_acc' access_list
    in
    List.fold (HilExp.get_access_exprs exp) ~init:access_domain ~f:(fun acc access_expr ->
        let base, accesses = AccessExpression.to_accesses access_expr in
        add_field_accesses base acc accesses )


  let make_container_access {interproc= {tenv}; formals} ret_base callee_pname ~is_write receiver_ap
      callee_loc (astate : Domain.t) =
    let open Domain in
    if
      AttributeMapDomain.is_synchronized astate.attribute_map receiver_ap
      || RacerDModels.is_synchronized_container callee_pname receiver_ap tenv
    then astate
    else
      let ownership_pre = OwnershipDomain.get_owned receiver_ap astate.ownership in
      let callee_access =
        AccessSnapshot.make_container_access formals receiver_ap ~is_write callee_pname callee_loc
          astate.locks astate.threads ownership_pre
      in
      let ownership =
        OwnershipDomain.add (AccessExpression.base ret_base) ownership_pre astate.ownership
      in
      let accesses = AccessDomain.add_opt callee_access astate.accesses in
      {astate with accesses; ownership}


  let add_reads formals exps loc ({accesses; locks; threads; ownership} as astate : Domain.t) tenv =
    let accesses =
      List.fold exps ~init:accesses
        ~f:(add_access formals loc ~is_write_access:false locks threads ownership tenv)
    in
    {astate with accesses}


  let expand_actuals formals actuals accesses pdesc =
    let open Domain in
    if AccessDomain.is_empty accesses then accesses
    else
      let formal_map = FormalMap.make pdesc in
      let expand_exp exp =
        match FormalMap.get_formal_index (AccessExpression.get_base exp) formal_map with
        | Some formal_index -> (
          match List.nth actuals formal_index with
          | Some actual_exp -> (
            match get_access_exp actual_exp with
            | Some actual ->
                AccessExpression.append ~onto:actual exp |> Option.value ~default:exp
            | None ->
                exp )
          | None ->
              exp )
        | None ->
            exp
      in
      let add snapshot acc =
        let snapshot_opt' = AccessSnapshot.map_opt formals ~f:expand_exp snapshot in
        AccessDomain.add_opt snapshot_opt' acc
      in
      AccessDomain.fold add accesses AccessDomain.empty


  let add_callee_accesses formals (caller_astate : Domain.t) callee_accesses locks threads actuals
      callee_pname loc =
    let open Domain in
    let callsite = CallSite.make callee_pname loc in
    let actuals_ownership =
      (* precompute array holding ownership of each actual for fast random access *)
      Array.of_list_map actuals ~f:(fun actual_exp ->
          OwnershipDomain.ownership_of_expr actual_exp caller_astate.ownership )
    in
    let update_ownership_precondition actual_index (acc : OwnershipAbstractValue.t) =
      if actual_index >= Array.length actuals_ownership then
        (* vararg methods can result into missing actuals so simply ignore *)
        acc
      else OwnershipAbstractValue.join acc actuals_ownership.(actual_index)
    in
    let update_callee_access (snapshot : AccessSnapshot.t) acc =
      (* update precondition with caller ownership info *)
      let ownership_precondition =
        match snapshot.elem.ownership_precondition with
        | OwnedIf indexes ->
            IntSet.fold update_ownership_precondition indexes OwnershipAbstractValue.owned
        | Unowned ->
            snapshot.elem.ownership_precondition
      in
      let snapshot_opt =
        AccessSnapshot.update_callee_access formals snapshot callsite ownership_precondition threads
          locks
      in
      AccessDomain.add_opt snapshot_opt acc
    in
    AccessDomain.fold update_callee_access callee_accesses caller_astate.accesses


  let call_without_summary tenv callee_pname ret_base actuals astate =
    let open RacerDModels in
    let open RacerDDomain in
    if RacerDModels.is_synchronized_container_constructor tenv callee_pname actuals then
      apply_to_first_actual astate actuals ~f:(fun receiver ->
          let attribute_map = AttributeMapDomain.add receiver Synchronized astate.attribute_map in
          {astate with attribute_map} )
    else if RacerDModels.is_converter_to_synchronized_container tenv callee_pname actuals then
      let attribute_map =
        AttributeMapDomain.add (AccessExpression.base ret_base) Synchronized astate.attribute_map
      in
      {astate with attribute_map}
    else if is_box callee_pname then
      apply_to_first_actual astate actuals ~f:(fun actual_access_expr ->
          if AttributeMapDomain.is_functional astate.attribute_map actual_access_expr then
            (* TODO: check for constants, which are functional? *)
            let attribute_map =
              AttributeMapDomain.add (AccessExpression.base ret_base) Functional
                astate.attribute_map
            in
            {astate with attribute_map}
          else astate )
    else
      let ownership =
        OwnershipDomain.add (AccessExpression.base ret_base) OwnershipAbstractValue.owned
          astate.ownership
      in
      {astate with ownership}


  let do_call_acquiring_ownership ret_base astate =
    let open Domain in
    let ownership =
      OwnershipDomain.add (AccessExpression.base ret_base) OwnershipAbstractValue.owned
        astate.ownership
    in
    {astate with ownership}


  let do_container_access ~is_write ret_base callee_pname actuals loc analysis_data astate =
    match get_first_actual actuals with
    | Some receiver_expr ->
        make_container_access analysis_data ret_base callee_pname ~is_write receiver_expr loc astate
    | None ->
        L.internal_error "Call to %a is marked as a container access, but has no receiver"
          Procname.pp callee_pname ;
        astate


  let do_proc_call ret_base callee_pname actuals call_flags loc
      {interproc= {tenv; analyze_dependency}; formals} (astate : Domain.t) =
    let open Domain in
    let open RacerDModels in
    let open ConcurrencyModels in
    let ret_access_exp = AccessExpression.base ret_base in
    let astate =
      if RacerDModels.should_flag_interface_call tenv actuals call_flags callee_pname then
        Domain.add_unannotated_call_access formals callee_pname actuals loc astate
      else astate
    in
    let astate =
      match get_thread_assert_effect callee_pname with
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
    in
    let astate_callee =
      (* assuming that modeled procedures do not have useful summaries *)
      if is_thread_utils_method "assertMainThread" callee_pname then
        {astate with threads= ThreadsDomain.AnyThreadButSelf}
      else
        (* if we don't have any evidence about whether the current function can run in parallel
           with other threads or not, start assuming that it can. why use a lock if the function
           can't run in a multithreaded context? *)
        let update_for_lock_use = function
          | ThreadsDomain.AnyThreadButSelf ->
              ThreadsDomain.AnyThreadButSelf
          | _ ->
              ThreadsDomain.AnyThread
        in
        match get_lock_effect callee_pname actuals with
        | Lock _ | GuardLock _ | GuardConstruct {acquire_now= true} ->
            { astate with
              locks= LockDomain.acquire_lock astate.locks
            ; threads= update_for_lock_use astate.threads }
        | Unlock _ | GuardDestroy _ | GuardUnlock _ ->
            { astate with
              locks= LockDomain.release_lock astate.locks
            ; threads= update_for_lock_use astate.threads }
        | LockedIfTrue _ | GuardLockedIfTrue _ ->
            let attribute_map =
              AttributeMapDomain.add ret_access_exp Attribute.LockHeld astate.attribute_map
            in
            {astate with attribute_map; threads= update_for_lock_use astate.threads}
        | GuardConstruct {acquire_now= false} ->
            astate
        | NoEffect -> (
            let rebased_summary_opt =
              analyze_dependency callee_pname
              |> Option.map ~f:(fun (callee_proc_desc, summary) ->
                     let rebased_accesses =
                       expand_actuals formals actuals summary.accesses callee_proc_desc
                     in
                     {summary with accesses= rebased_accesses} )
            in
            match rebased_summary_opt with
            | Some {threads; locks; accesses; return_ownership; return_attribute} ->
                let locks =
                  LockDomain.integrate_summary ~caller_astate:astate.locks ~callee_astate:locks
                in
                let accesses =
                  add_callee_accesses formals astate accesses locks threads actuals callee_pname loc
                in
                let ownership =
                  OwnershipDomain.propagate_return ret_access_exp return_ownership actuals
                    astate.ownership
                in
                let attribute_map =
                  AttributeMapDomain.add ret_access_exp return_attribute astate.attribute_map
                in
                let threads =
                  ThreadsDomain.integrate_summary ~caller_astate:astate.threads
                    ~callee_astate:threads
                in
                {locks; threads; accesses; ownership; attribute_map}
            | None ->
                call_without_summary tenv callee_pname ret_base actuals astate )
    in
    let add_if_annotated predicate attribute attribute_map =
      if PatternMatch.override_exists predicate tenv callee_pname then
        AttributeMapDomain.add ret_access_exp attribute attribute_map
      else attribute_map
    in
    let attribute_map = add_if_annotated is_functional Functional astate_callee.attribute_map in
    let ownership =
      if
        PatternMatch.override_exists
          (has_return_annot Annotations.ia_is_returns_ownership)
          tenv callee_pname
      then OwnershipDomain.add ret_access_exp OwnershipAbstractValue.owned astate_callee.ownership
      else astate_callee.ownership
    in
    {astate_callee with ownership; attribute_map}


  let do_assignment lhs_access_exp rhs_exp loc {interproc= {tenv}; formals} (astate : Domain.t) =
    let open Domain in
    let rhs_accesses =
      add_access formals loc ~is_write_access:false astate.locks astate.threads astate.ownership
        tenv astate.accesses rhs_exp
    in
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
    let accesses =
      if is_functional then
        (* we want to forget about writes to @Functional fields altogether, otherwise we'll
           report spurious read/write races *)
        rhs_accesses
      else
        add_access formals loc ~is_write_access:true astate.locks astate.threads astate.ownership
          tenv rhs_accesses (HilExp.AccessExpression lhs_access_exp)
    in
    let ownership = OwnershipDomain.propagate_assignment lhs_access_exp rhs_exp astate.ownership in
    let attribute_map =
      AttributeMapDomain.propagate_assignment lhs_access_exp rhs_exp astate.attribute_map
    in
    {astate with accesses; ownership; attribute_map}


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
    let accesses =
      add_access formals loc ~is_write_access:false astate.locks astate.threads astate.ownership
        tenv astate.accesses assume_exp
    in
    let astate' =
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
    in
    {astate' with accesses}


  let exec_instr astate ({interproc= {proc_desc; tenv}; formals} as analysis_data) _ instr =
    match (instr : HilInstr.t) with
    | Call (ret_base, Direct callee_pname, actuals, call_flags, loc) ->
        let astate = add_reads formals actuals loc astate tenv in
        if RacerDModels.acquires_ownership callee_pname tenv then
          do_call_acquiring_ownership ret_base astate
        else if RacerDModels.is_container_write tenv callee_pname then
          do_container_access ~is_write:true ret_base callee_pname actuals loc analysis_data astate
        else if RacerDModels.is_container_read tenv callee_pname then
          do_container_access ~is_write:false ret_base callee_pname actuals loc analysis_data astate
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


let analyze_procedure ({InterproceduralAnalysis.proc_desc; tenv} as interproc) =
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
      if Procdesc.is_java_synchronized proc_desc then LockDomain.(acquire_lock bottom)
      else LockDomain.bottom
    in
    let threads =
      if runs_on_ui_thread tenv proc_name || RacerDModels.is_thread_confined_method tenv proc_name
      then ThreadsDomain.AnyThreadButSelf
      else if
        Procdesc.is_java_synchronized proc_desc || RacerDModels.is_marked_thread_safe proc_name tenv
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


type conflict = RacerDDomain.AccessSnapshot.t

type report_kind =
  | GuardedByViolation
  | WriteWriteRace of conflict option  (** one of conflicting access, if there are any *)
  | ReadWriteRace of conflict  (** one of several conflicting accesses *)
  | UnannotatedInterface

(** Explain why we are reporting this access, in Java *)
let get_reporting_explanation_java report_kind tenv pname thread =
  let open RacerDModels in
  (* best explanation is always that the current class or method is annotated thread-safe. try for
     that first. *)
  let annotation_explanation_opt =
    if is_thread_safe_method pname tenv then
      Some
        (F.asprintf
           "@\n Reporting because current method is annotated %a or overrides an annotated method."
           MF.pp_monospaced "@ThreadSafe")
    else
      match FbThreadSafety.get_fbthreadsafe_class_annot pname tenv with
      | Some (qual, annot) ->
          Some (FbThreadSafety.message_fbthreadsafe_class qual annot)
      | None -> (
        match get_current_class_and_threadsafe_superclasses tenv pname with
        | Some (current_class, (thread_safe_class :: _ as thread_safe_annotated_classes)) ->
            Some
              ( if List.mem ~equal:Typ.Name.equal thread_safe_annotated_classes current_class then
                F.asprintf "@\n Reporting because the current class is annotated %a"
                  MF.pp_monospaced "@ThreadSafe"
              else
                F.asprintf "@\n Reporting because a superclass %a is annotated %a"
                  (MF.wrap_monospaced Typ.Name.pp) thread_safe_class MF.pp_monospaced "@ThreadSafe"
              )
        | _ ->
            None )
  in
  match (report_kind, annotation_explanation_opt) with
  | GuardedByViolation, _ ->
      ( IssueType.guardedby_violation_racerd
      , F.asprintf "@\n Reporting because field is annotated %a" MF.pp_monospaced "@GuardedBy" )
  | UnannotatedInterface, Some threadsafe_explanation ->
      (IssueType.interface_not_thread_safe, F.asprintf "%s." threadsafe_explanation)
  | UnannotatedInterface, None ->
      Logging.die InternalError
        "Reporting non-threadsafe interface call, but can't find a @ThreadSafe annotation"
  | _, Some threadsafe_explanation when RacerDDomain.ThreadsDomain.is_any thread ->
      ( IssueType.thread_safety_violation
      , F.asprintf
          "%s, so we assume that this method can run in parallel with other non-private methods in \
           the class (including itself)."
          threadsafe_explanation )
  | _, Some threadsafe_explanation ->
      ( IssueType.thread_safety_violation
      , F.asprintf
          "%s. Although this access is not known to run on a background thread, it may happen in \
           parallel with another access that does."
          threadsafe_explanation )
  | _, None ->
      (* failed to explain based on @ThreadSafe annotation; have to justify using background thread *)
      if RacerDDomain.ThreadsDomain.is_any thread then
        ( IssueType.thread_safety_violation
        , F.asprintf "@\n Reporting because this access may occur on a background thread." )
      else
        ( IssueType.thread_safety_violation
        , F.asprintf
            "@\n\
            \ Reporting because another access to the same memory occurs on a background thread, \
             although this access may not." )


(** Explain why we are reporting this access, in C++ *)
let get_reporting_explanation_cpp = (IssueType.lock_consistency_violation, "")

(** Explain why we are reporting this access *)
let get_reporting_explanation report_kind tenv pname thread =
  if Procname.is_java pname then get_reporting_explanation_java report_kind tenv pname thread
  else get_reporting_explanation_cpp


let describe_exp = MF.wrap_monospaced RacerDDomain.pp_exp

let describe_pname = MF.wrap_monospaced (Procname.pp_simplified_string ~withclass:true)

let pp_access fmt (t : RacerDDomain.AccessSnapshot.t) =
  match t.elem.access with
  | Read {exp} | Write {exp} ->
      describe_exp fmt exp
  | ContainerRead {exp; pname} | ContainerWrite {exp; pname} ->
      F.fprintf fmt "container %a via call to %a" describe_exp exp describe_pname pname
  | InterfaceCall _ as access ->
      RacerDDomain.Access.pp fmt access


let make_trace ~report_kind original_exp =
  let open RacerDDomain in
  let loc_trace_of_path path = AccessSnapshot.make_loc_trace path in
  let original_trace = loc_trace_of_path original_exp in
  let get_end_loc trace = Option.map (List.last trace) ~f:(function {Errlog.lt_loc} -> lt_loc) in
  let original_end = get_end_loc original_trace in
  let make_with_conflicts conflict_sink original_trace ~label1 ~label2 =
    (* create a trace for one of the conflicts and append it to the trace for the original sink *)
    let conflict_trace = loc_trace_of_path conflict_sink in
    let conflict_end = get_end_loc conflict_trace in
    ( Errlog.concat_traces [(label1, original_trace); (label2, conflict_trace)]
    , original_end
    , conflict_end )
  in
  match report_kind with
  | ReadWriteRace conflict ->
      make_with_conflicts conflict original_trace ~label1:"<Read trace>" ~label2:"<Write trace>"
  | WriteWriteRace (Some conflict) ->
      make_with_conflicts conflict original_trace ~label1:"<Write on unknown thread>"
        ~label2:"<Write on background thread>"
  | GuardedByViolation | WriteWriteRace None | UnannotatedInterface ->
      (original_trace, original_end, None)


let log_issue current_pname ~issue_log ~loc ~ltr ~access issue_type error_message =
  Reporting.log_issue_external current_pname Warning ~issue_log ~loc ~ltr ~access issue_type
    error_message


type reported_access =
  { threads: RacerDDomain.ThreadsDomain.t
  ; snapshot: RacerDDomain.AccessSnapshot.t
  ; tenv: Tenv.t
  ; procname: Procname.t }

let report_thread_safety_violation ~make_description ~report_kind
    ({threads; snapshot; tenv; procname= pname} : reported_access) issue_log =
  let open RacerDDomain in
  let final_pname = List.last snapshot.trace |> Option.value_map ~default:pname ~f:CallSite.pname in
  let final_sink_site = CallSite.make final_pname snapshot.loc in
  let initial_sink_site = CallSite.make pname (AccessSnapshot.get_loc snapshot) in
  let loc = CallSite.loc initial_sink_site in
  let ltr, original_end, conflict_end = make_trace ~report_kind snapshot in
  (* what the potential bug is *)
  let description = make_description pname final_sink_site initial_sink_site snapshot in
  (* why we are reporting it *)
  let issue_type, explanation = get_reporting_explanation report_kind tenv pname threads in
  let error_message = F.sprintf "%s%s" description explanation in
  let end_locs = Option.to_list original_end @ Option.to_list conflict_end in
  let access = IssueAuxData.encode end_locs in
  log_issue pname ~issue_log ~loc ~ltr ~access RacerD issue_type error_message


let report_unannotated_interface_violation reported_pname reported_access issue_log =
  match reported_pname with
  | Procname.Java java_pname ->
      let class_name = Procname.Java.get_class_name java_pname in
      let make_description _ _ _ _ =
        F.asprintf
          "Unprotected call to method %a of un-annotated interface %a. Consider annotating the \
           interface with %a or adding a lock."
          describe_pname reported_pname MF.pp_monospaced class_name MF.pp_monospaced "@ThreadSafe"
      in
      report_thread_safety_violation ~make_description ~report_kind:UnannotatedInterface
        reported_access issue_log
  | _ ->
      (* skip reporting on C++ *)
      issue_log


let make_unprotected_write_description pname final_sink_site initial_sink_site final_sink =
  Format.asprintf "Unprotected write. Non-private method %a%s %s %a outside of synchronization."
    describe_pname pname
    (if CallSite.equal final_sink_site initial_sink_site then "" else " indirectly")
    ( if RacerDDomain.AccessSnapshot.is_container_write final_sink then "mutates"
    else "writes to field" )
    pp_access final_sink


let make_guardedby_violation_description pname final_sink_site initial_sink_site final_sink =
  Format.asprintf
    "GuardedBy violation. Non-private method %a%s accesses %a outside of synchronization."
    describe_pname pname
    (if CallSite.equal final_sink_site initial_sink_site then "" else " indirectly")
    pp_access final_sink


let make_read_write_race_description ~read_is_sync (conflict : reported_access) pname
    final_sink_site initial_sink_site final_sink =
  let pp_conflict fmt {procname} =
    F.pp_print_string fmt (Procname.to_simplified_string ~withclass:true procname)
  in
  let conflicts_description =
    Format.asprintf "Potentially races with%s write in method %a"
      (if read_is_sync then " unsynchronized" else "")
      (MF.wrap_monospaced pp_conflict) conflict
  in
  Format.asprintf "Read/Write race. Non-private method %a%s reads%s from %a. %s." describe_pname
    pname
    (if CallSite.equal final_sink_site initial_sink_site then "" else " indirectly")
    (if read_is_sync then " with synchronization" else " without synchronization")
    pp_access final_sink conflicts_description


module ReportedSet : sig
  (** Type for deduplicating and storing reports. *)
  type t

  val reset : t -> t
  (** Reset recorded writes and reads, while maintaining the same [IssueLog.t]. *)

  val empty_of_issue_log : IssueLog.t -> t
  (** Create a set of reports containing the given [IssueLog.t] but otherwise having no records of
      previous reports. *)

  val to_issue_log : t -> IssueLog.t
  (** Recover deduplicated [IssueLog.t] from [t]. *)

  val deduplicate : f:(reported_access -> IssueLog.t -> IssueLog.t) -> reported_access -> t -> t
  (** Deduplicate [f]. *)
end = struct
  type reported_set =
    { sites: CallSite.Set.t
    ; writes: Procname.Set.t
    ; reads: Procname.Set.t
    ; unannotated_calls: Procname.Set.t }

  let empty_reported_set =
    { sites= CallSite.Set.empty
    ; reads= Procname.Set.empty
    ; writes= Procname.Set.empty
    ; unannotated_calls= Procname.Set.empty }


  type t = reported_set * IssueLog.t

  let empty_of_issue_log issue_log = (empty_reported_set, issue_log)

  let to_issue_log = snd

  let reset (reported_set, issue_log) =
    ({reported_set with writes= Procname.Set.empty; reads= Procname.Set.empty}, issue_log)


  let is_duplicate {snapshot; procname} (reported_set, _) =
    let call_site = CallSite.make procname (RacerDDomain.AccessSnapshot.get_loc snapshot) in
    CallSite.Set.mem call_site reported_set.sites
    ||
    match snapshot.elem.access with
    | Write _ | ContainerWrite _ ->
        Procname.Set.mem procname reported_set.writes
    | Read _ | ContainerRead _ ->
        Procname.Set.mem procname reported_set.reads
    | InterfaceCall _ ->
        Procname.Set.mem procname reported_set.unannotated_calls


  let update {snapshot; procname} (reported_set, issue_log) =
    let call_site = CallSite.make procname (RacerDDomain.AccessSnapshot.get_loc snapshot) in
    let sites = CallSite.Set.add call_site reported_set.sites in
    let reported_set = {reported_set with sites} in
    let reported_set =
      match snapshot.elem.access with
      | Write _ | ContainerWrite _ ->
          {reported_set with writes= Procname.Set.add procname reported_set.writes}
      | Read _ | ContainerRead _ ->
          {reported_set with reads= Procname.Set.add procname reported_set.reads}
      | InterfaceCall _ ->
          { reported_set with
            unannotated_calls= Procname.Set.add procname reported_set.unannotated_calls }
    in
    (reported_set, issue_log)


  let deduplicate ~f reported_access ((reported_set, issue_log) as acc) =
    if Config.deduplicate && is_duplicate reported_access acc then acc
    else update reported_access (reported_set, f reported_access issue_log)
end

(** Map containing reported accesses, which groups them in lists, by abstract location. The
    equivalence relation used for grouping them is equality of access paths. This is slightly
    complicated because local variables contain the pname of the function declaring them. Here we
    want a purely name-based comparison, and in particular that [this == this] regardless the method
    declaring it. Hence the redefined comparison functions. *)
module ReportMap : sig
  type t

  val empty : t

  val add : reported_access -> t -> t

  val fold : (reported_access list -> 'a -> 'a) -> t -> 'a -> 'a
end = struct
  module PathModuloThis : Caml.Map.OrderedType with type t = AccessPath.t = struct
    type t = AccessPath.t

    type var_ = Var.t

    let compare_var_ = Var.compare_modulo_this

    let compare = [%compare: (var_ * Typ.t) * AccessPath.access list]
  end

  module Key = struct
    type t = Location of PathModuloThis.t | Container of PathModuloThis.t | Call of Procname.t
    [@@deriving compare]

    let of_access (access : RacerDDomain.Access.t) =
      match access with
      | Read {exp} | Write {exp} ->
          Location (AccessExpression.to_access_path exp)
      | ContainerRead {exp} | ContainerWrite {exp} ->
          Container (AccessExpression.to_access_path exp)
      | InterfaceCall {pname} ->
          Call pname
  end

  module M = Caml.Map.Make (Key)

  type t = reported_access list M.t

  let empty = M.empty

  let add (rep : reported_access) map =
    let access = rep.snapshot.elem.access in
    let k = Key.of_access access in
    M.update k (function None -> Some [rep] | Some reps -> Some (rep :: reps)) map


  let fold f map a =
    let f _ v acc = f v acc in
    M.fold f map a
end

let should_report_on_proc tenv procdesc =
  let proc_name = Procdesc.get_proc_name procdesc in
  match proc_name with
  | Java java_pname ->
      (* return true if procedure is at an abstraction boundary or reporting has been explicitly
         requested via @ThreadSafe in java *)
      RacerDModels.is_thread_safe_method proc_name tenv
      || (not (PredSymb.equal_access (Procdesc.get_access procdesc) Private))
         && (not (Procname.Java.is_autogen_method java_pname))
         && not (Annotations.pdesc_return_annot_ends_with procdesc Annotations.visibleForTesting)
  | ObjC_Cpp {kind= CPPMethod _ | CPPConstructor _ | CPPDestructor _} ->
      not (PredSymb.equal_access (Procdesc.get_access procdesc) Private)
  | ObjC_Cpp {kind= ObjCClassMethod | ObjCInstanceMethod | ObjCInternalMethod; class_name} ->
      Tenv.lookup tenv class_name
      |> Option.exists ~f:(fun {Struct.exported_objc_methods} ->
             List.mem ~equal:Procname.equal exported_objc_methods proc_name )
  | _ ->
      false


let should_report_guardedby_violation classname ({snapshot; tenv; procname} : reported_access) =
  let is_uitthread param =
    match String.lowercase param with
    | "ui thread" | "ui-thread" | "ui_thread" | "uithread" ->
        true
    | _ ->
        false
  in
  let field_is_annotated_guardedby field_name (f, _, a) =
    Fieldname.equal f field_name
    && List.exists a ~f:(fun ((annot : Annot.t), _) ->
           Annotations.annot_ends_with annot Annotations.guarded_by
           &&
           match annot.parameters with
           | [param] ->
               not (Annot.has_matching_str_value ~pred:is_uitthread param.value)
           | _ ->
               false )
  in
  (not snapshot.elem.lock)
  && RacerDDomain.AccessSnapshot.is_write snapshot
  && Procname.is_java procname
  &&
  (* restrict check to access paths of length one *)
  match
    RacerDDomain.Access.get_access_exp snapshot.elem.access
    |> AccessExpression.to_accesses
    |> fun (base, accesses) -> (base, List.filter accesses ~f:HilExp.Access.is_field_or_array_access)
  with
  | AccessExpression.Base (_, base_type), [HilExp.Access.FieldAccess field_name] -> (
    match base_type.desc with
    | Tstruct base_name | Tptr ({desc= Tstruct base_name}, _) ->
        (* is the base class a subclass of the one containing the GuardedBy annotation? *)
        PatternMatch.is_subtype tenv base_name classname
        && Tenv.lookup tenv base_name
           |> Option.exists ~f:(fun ({fields; statics} : Struct.t) ->
                  let f fld = field_is_annotated_guardedby field_name fld in
                  List.exists fields ~f || List.exists statics ~f )
    | _ ->
        false )
  | _ ->
      false


(** Report accesses that may race with each other.

    Principles for race reporting.

    Two accesses are excluded if they are both protected by the same lock or are known to be on the
    same thread. Otherwise they are in conflict. We want to report conflicting accesses one of which
    is a write.

    To cut down on duplication noise we don't always report at both sites (line numbers) involved in
    a race.

    \-- If a protected access races with an unprotected one, we don't report the protected but we do
    report the unprotected one (and we point to the protected from the unprotected one). This way
    the report is at the line number in a race-pair where the programmer should take action.

    \-- Similarly, if a threaded and unthreaded (not known to be threaded) access race, we report at
    the unthreaded site.

    Also, we avoid reporting multiple races at the same line (which can happen a lot in an
    interprocedural scenario) or multiple accesses to the same field in a single method, expecting
    that the programmer already gets signal from one report. To report all the races with separate
    warnings leads to a lot of noise. But note, we never suppress all the potential issues in a
    class: if we don't report any races, it means we didn't find any.

    The above is tempered at the moment by abstractions of "same lock" and "same thread": we are
    currently not distinguishing different locks, and are treating "known to be confined to a
    thread" as if "known to be confined to UI thread". *)
let report_unsafe_accesses ~issue_log classname (aggregated_access_map : ReportMap.t) =
  let open RacerDDomain in
  let open RacerDModels in
  let report_thread_safety_violation ~acc ~make_description ~report_kind reported_access =
    ReportedSet.deduplicate
      ~f:(report_thread_safety_violation ~make_description ~report_kind)
      reported_access acc
  in
  let report_unannotated_interface_violation ~acc reported_pname reported_access =
    ReportedSet.deduplicate
      ~f:(report_unannotated_interface_violation reported_pname)
      reported_access acc
  in
  let report_unsafe_access accesses acc
      ({snapshot; threads; tenv; procname= pname} as reported_access) =
    match snapshot.elem.access with
    | InterfaceCall {pname= reported_pname}
      when AccessSnapshot.is_unprotected snapshot
           && ThreadsDomain.is_any threads && is_marked_thread_safe pname tenv ->
        (* un-annotated interface call + no lock in method marked thread-safe. warn *)
        report_unannotated_interface_violation ~acc reported_pname reported_access
    | InterfaceCall _ ->
        acc
    | (Write _ | ContainerWrite _) when Procname.is_java pname ->
        let conflict =
          if ThreadsDomain.is_any threads then
            (* unprotected write in method that may run in parallel with itself. warn *)
            None
          else
            (* unprotected write, but not on a method that may run in parallel with itself
               (i.e., not a self race). find accesses on a background thread this access might
               conflict with and report them *)
            List.find_map accesses ~f:(fun {snapshot= other_snapshot; threads= other_threads} ->
                if AccessSnapshot.is_write other_snapshot && ThreadsDomain.is_any other_threads then
                  Some other_snapshot
                else None )
        in
        if
          AccessSnapshot.is_unprotected snapshot
          && (Option.is_some conflict || ThreadsDomain.is_any threads)
        then
          report_thread_safety_violation ~acc ~make_description:make_unprotected_write_description
            ~report_kind:(WriteWriteRace conflict) reported_access
        else acc
    | Write _ | ContainerWrite _ ->
        (* Do not report unprotected writes for ObjC_Cpp *)
        acc
    | (Read _ | ContainerRead _) when AccessSnapshot.is_unprotected snapshot ->
        (* unprotected read. report all writes as conflicts for java. for c++ filter out
           unprotected writes *)
        let is_conflict {snapshot; threads= other_threads} =
          AccessSnapshot.is_write snapshot
          &&
          if Procname.is_java pname then
            ThreadsDomain.is_any threads || ThreadsDomain.is_any other_threads
          else not (AccessSnapshot.is_unprotected snapshot)
        in
        List.find ~f:is_conflict accesses
        |> Option.value_map ~default:acc ~f:(fun conflict ->
               let make_description =
                 make_read_write_race_description ~read_is_sync:false conflict
               in
               let report_kind = ReadWriteRace conflict.snapshot in
               report_thread_safety_violation ~acc ~make_description ~report_kind reported_access )
    | (Read _ | ContainerRead _) when Procname.is_java pname ->
        (* protected read. report unprotected writes and opposite protected writes as conflicts *)
        let can_conflict (snapshot1 : AccessSnapshot.t) (snapshot2 : AccessSnapshot.t) =
          if snapshot1.elem.lock && snapshot2.elem.lock then false
          else ThreadsDomain.can_conflict snapshot1.elem.thread snapshot2.elem.thread
        in
        let is_conflict {snapshot= other_snapshot; threads= other_threads} =
          if AccessSnapshot.is_unprotected other_snapshot then
            AccessSnapshot.is_write other_snapshot && ThreadsDomain.is_any other_threads
          else AccessSnapshot.is_write other_snapshot && can_conflict snapshot other_snapshot
        in
        List.find accesses ~f:is_conflict
        |> Option.value_map ~default:acc ~f:(fun conflict ->
               (* protected read with conflicting unprotected write(s). warn. *)
               let make_description =
                 make_read_write_race_description ~read_is_sync:true conflict
               in
               let report_kind = ReadWriteRace conflict.snapshot in
               report_thread_safety_violation ~acc ~make_description ~report_kind reported_access )
    | Read _ | ContainerRead _ ->
        (* Do not report protected reads for ObjC_Cpp *)
        acc
  in
  let report_accesses_on_location reportable_accesses init =
    (* Don't report on location if all accesses are on non-concurrent contexts *)
    if
      List.for_all reportable_accesses ~f:(fun ({threads} : reported_access) ->
          ThreadsDomain.is_any threads |> not )
    then init
    else List.fold reportable_accesses ~init ~f:(report_unsafe_access reportable_accesses)
  in
  let report_guardedby_violations_on_location grouped_accesses init =
    if Config.racerd_guardedby then
      List.fold grouped_accesses ~init ~f:(fun acc r ->
          if should_report_guardedby_violation classname r then
            report_thread_safety_violation ~acc ~report_kind:GuardedByViolation
              ~make_description:make_guardedby_violation_description r
          else acc )
    else init
  in
  let report grouped_accesses acc =
    (* reset the reported reads and writes for each memory location *)
    ReportedSet.reset acc
    |> report_guardedby_violations_on_location grouped_accesses
    |> report_accesses_on_location grouped_accesses
  in
  ReportMap.fold report aggregated_access_map (ReportedSet.empty_of_issue_log issue_log)
  |> ReportedSet.to_issue_log


(* create a map from [abstraction of a memory loc] -> accesses that
   may touch that memory loc. the abstraction of a location is an access
   path like x.f.g whose concretization is the set of memory cells
   that x.f.g may point to during execution *)
let make_results_table exe_env summaries =
  let open RacerDDomain in
  let aggregate_post tenv procname acc {threads; accesses} =
    AccessDomain.fold
      (fun snapshot acc -> ReportMap.add {threads; snapshot; tenv; procname} acc)
      accesses acc
  in
  List.fold summaries ~init:ReportMap.empty ~f:(fun acc (proc_desc, summary) ->
      let procname = Procdesc.get_proc_name proc_desc in
      let tenv = Exe_env.get_tenv exe_env procname in
      aggregate_post tenv procname acc summary )


let class_has_concurrent_method class_summaries =
  let open RacerDDomain in
  let method_has_concurrent_context (_, summary) =
    match (summary.threads : ThreadsDomain.t) with NoThread -> false | _ -> true
  in
  List.exists class_summaries ~f:method_has_concurrent_context


let should_report_on_class (classname : Typ.Name.t) class_summaries =
  match classname with
  | JavaClass _ ->
      true
  | CppClass _ | ObjcClass _ | ObjcProtocol _ | CStruct _ ->
      class_has_concurrent_method class_summaries
  | CUnion _ ->
      false


let filter_reportable_classes class_map = Typ.Name.Map.filter should_report_on_class class_map

(** aggregate all of the procedures in the file env by their declaring class. this lets us analyze
    each class individually *)
let aggregate_by_class {InterproceduralAnalysis.procedures; file_exe_env; analyze_file_dependency} =
  List.fold procedures ~init:Typ.Name.Map.empty ~f:(fun acc procname ->
      Procname.get_class_type_name procname
      |> Option.bind ~f:(fun classname ->
             analyze_file_dependency procname
             |> Option.filter ~f:(fun (pdesc, _) ->
                    let tenv = Exe_env.get_tenv file_exe_env procname in
                    should_report_on_proc tenv pdesc )
             |> Option.map ~f:(fun summary_proc_desc ->
                    Typ.Name.Map.update classname
                      (function
                        | None ->
                            Some [summary_proc_desc]
                        | Some summaries ->
                            Some (summary_proc_desc :: summaries) )
                      acc ) )
      |> Option.value ~default:acc )
  |> filter_reportable_classes


(** Gathers results by analyzing all the methods in a file, then post-processes the results to check
    an (approximation of) thread safety *)
let file_analysis ({InterproceduralAnalysis.file_exe_env} as file_t) =
  let class_map = aggregate_by_class file_t in
  Typ.Name.Map.fold
    (fun classname methods issue_log ->
      make_results_table file_exe_env methods |> report_unsafe_accesses ~issue_log classname )
    class_map IssueLog.empty
