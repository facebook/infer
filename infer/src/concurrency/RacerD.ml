(*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging
module MF = MarkupFormatter

module Payload = SummaryPayload.Make (struct
  type t = RacerDDomain.summary

  let update_payloads post (payloads : Payloads.t) = {payloads with racerd= Some post}

  let of_payloads (payloads : Payloads.t) = payloads.racerd
end)

module TransferFunctions (CFG : ProcCfg.S) = struct
  module CFG = CFG
  module Domain = RacerDDomain

  type extras = ProcData.no_extras

  let add_access loc ~is_write_access locks threads ownership (proc_data : extras ProcData.t)
      accesses exp =
    let open Domain in
    let rec add_field_accesses prefix_path access_acc = function
      | [] ->
          access_acc
      | access :: access_list -> (
          let prefix_path' = (fst prefix_path, snd prefix_path @ [access]) in
          let add_field_access pre =
            let access_acc' = AccessDomain.add_opt pre access_acc in
            add_field_accesses prefix_path' access_acc' access_list
          in
          if RacerDModels.is_safe_access access prefix_path proc_data.tenv then
            add_field_accesses prefix_path' access_acc access_list
          else
            let is_write = if List.is_empty access_list then is_write_access else false in
            let access = TraceElem.make_field_access prefix_path' ~is_write loc in
            match OwnershipDomain.get_owned prefix_path ownership with
            | OwnershipAbstractValue.OwnedIf formal_indexes ->
                let pre =
                  AccessSnapshot.make access locks threads
                    (AccessSnapshot.OwnershipPrecondition.Conjunction formal_indexes)
                    proc_data.pdesc
                in
                add_field_access pre
            | OwnershipAbstractValue.Unowned ->
                let pre = AccessSnapshot.make access locks threads False proc_data.pdesc in
                add_field_access pre )
    in
    List.fold (HilExp.get_access_exprs exp) ~init:accesses ~f:(fun acc access_expr ->
        let base, accesses = HilExp.AccessExpression.to_access_path access_expr in
        add_field_accesses (base, []) acc accesses )


  let make_container_access ret_base callee_pname ~is_write receiver_ap callee_loc tenv
      caller_pdesc (astate : Domain.t) =
    let open Domain in
    let callee_access =
      if RacerDModels.is_synchronized_container callee_pname receiver_ap tenv then None
      else
        let container_access =
          TraceElem.make_container_access receiver_ap ~is_write callee_pname callee_loc
        in
        let ownership_pre = OwnershipDomain.get_precondition receiver_ap astate.ownership in
        AccessSnapshot.make container_access astate.locks astate.threads ownership_pre caller_pdesc
    in
    (* if a container c is owned in cpp, make c[i] owned for all i *)
    let ownership_value =
      match callee_pname with
      | Typ.Procname.ObjC_Cpp _ | C _ ->
          OwnershipAbstractValue.make_owned_if 0
      | _ ->
          OwnershipAbstractValue.unowned
    in
    let ownership = OwnershipDomain.add (ret_base, []) ownership_value astate.ownership in
    let accesses = AccessDomain.add_opt callee_access astate.accesses in
    Some {astate with accesses; ownership}


  let add_reads exps loc ({accesses; locks; threads; ownership} as astate : Domain.t) proc_data =
    let accesses' =
      List.fold exps ~init:accesses
        ~f:(add_access loc ~is_write_access:false locks threads ownership proc_data)
    in
    {astate with accesses= accesses'}


  let expand_actuals actuals accesses pdesc =
    let open Domain in
    if AccessDomain.is_empty accesses then accesses
    else
      let rec get_access_path = function
        | HilExp.AccessExpression access_expr ->
            Some (HilExp.AccessExpression.to_access_path access_expr)
        | HilExp.Cast (_, e) | HilExp.Exception e ->
            get_access_path e
        | _ ->
            None
      in
      let formal_map = FormalMap.make pdesc in
      let expand_path ((base, accesses) as path) =
        match FormalMap.get_formal_index base formal_map with
        | Some formal_index -> (
          match List.nth actuals formal_index with
          | Some actual_exp -> (
            match get_access_path actual_exp with
            | Some actual ->
                AccessPath.append actual accesses
            | None ->
                path )
          | None ->
              path )
        | None ->
            path
      in
      let add snapshot acc =
        let access' = TraceElem.map ~f:expand_path snapshot.AccessSnapshot.access in
        let snapshot_opt' = AccessSnapshot.make_from_snapshot access' snapshot in
        AccessDomain.add_opt snapshot_opt' acc
      in
      AccessDomain.fold add accesses AccessDomain.empty


  let add_callee_accesses (caller_astate : Domain.t) callee_accesses locks threads actuals
      callee_pname pdesc loc =
    let open Domain in
    let conjoin_ownership_precondition actual_indexes actual_exp :
        AccessSnapshot.OwnershipPrecondition.t =
      match actual_exp with
      | HilExp.Constant _ ->
          (* the actual is a constant, so it's owned in the caller. *)
          Conjunction actual_indexes
      | HilExp.AccessExpression access_expr -> (
          let actual_access_path = HilExp.AccessExpression.to_access_path access_expr in
          match OwnershipDomain.get_owned actual_access_path caller_astate.ownership with
          | OwnedIf formal_indexes ->
              (* access path conditionally owned if [formal_indexes] are owned *)
              Conjunction (IntSet.union formal_indexes actual_indexes)
          | Unowned ->
              (* access path not rooted in a formal and not conditionally owned *)
              False )
      | _ ->
          (* couldn't find access path, don't know if it's owned. assume not *)
          False
    in
    let update_ownership_precondition actual_index (acc : AccessSnapshot.OwnershipPrecondition.t) =
      match acc with
      | False ->
          (* precondition can't be satisfied *)
          acc
      | Conjunction actual_indexes ->
          List.nth actuals actual_index
          (* optional args can result into missing actuals so simply ignore *)
          |> Option.value_map ~default:acc ~f:(conjoin_ownership_precondition actual_indexes)
    in
    let update_callee_access (snapshot : AccessSnapshot.t) acc =
      let access = TraceElem.with_callsite snapshot.access (CallSite.make callee_pname loc) in
      let locks = if snapshot.lock then LocksDomain.acquire_lock locks else locks in
      let thread =
        ThreadsDomain.integrate_summary ~callee_astate:snapshot.thread ~caller_astate:threads
      in
      (* update precondition with caller ownership info *)
      let ownership_precondition =
        match snapshot.ownership_precondition with
        | Conjunction indexes ->
            let empty_precondition =
              AccessSnapshot.OwnershipPrecondition.Conjunction IntSet.empty
            in
            IntSet.fold update_ownership_precondition indexes empty_precondition
        | False ->
            snapshot.ownership_precondition
      in
      if AccessSnapshot.OwnershipPrecondition.is_true ownership_precondition then
        (* discard accesses to owned memory *)
        acc
      else
        let snapshot_opt = AccessSnapshot.make access locks thread ownership_precondition pdesc in
        AccessDomain.add_opt snapshot_opt acc
    in
    AccessDomain.fold update_callee_access callee_accesses caller_astate.accesses


  let call_without_summary callee_pname ret_access_path call_flags actuals astate =
    let open RacerDModels in
    let open RacerDDomain in
    let should_assume_returns_ownership (call_flags : CallFlags.t) actuals =
      (not call_flags.cf_interface) && List.is_empty actuals
    in
    let is_abstract_getthis_like callee =
      Ondemand.get_proc_desc callee
      |> Option.exists ~f:(fun callee_pdesc ->
             (Procdesc.get_attributes callee_pdesc).ProcAttributes.is_abstract
             &&
             match Procdesc.get_formals callee_pdesc with
             | [(_, typ)] when Typ.equal typ (ret_access_path |> fst |> snd) ->
                 true
             | _ ->
                 false )
    in
    if is_box callee_pname then
      match actuals with
      | HilExp.AccessExpression actual_access_expr :: _ ->
          let actual_ap = HilExp.AccessExpression.to_access_path actual_access_expr in
          if AttributeMapDomain.has_attribute actual_ap Functional astate.attribute_map then
            (* TODO: check for constants, which are functional? *)
            let attribute_map =
              AttributeMapDomain.add_attribute ret_access_path Functional astate.attribute_map
            in
            {astate with attribute_map}
          else astate
      | _ ->
          astate
    else if should_assume_returns_ownership call_flags actuals then
      (* assume non-interface methods with no summary and no parameters return ownership *)
      let ownership =
        OwnershipDomain.add ret_access_path OwnershipAbstractValue.owned astate.ownership
      in
      {astate with ownership}
    else if is_abstract_getthis_like callee_pname then
      (* assume abstract, single-parameter methods whose return type is equal to that of the first
         formal return conditional ownership -- an example is getThis in Litho *)
      let ownership =
        OwnershipDomain.add ret_access_path
          (OwnershipAbstractValue.make_owned_if 0)
          astate.ownership
      in
      {astate with ownership}
    else astate


  let treat_call_acquiring_ownership ret_base procname actuals loc ({ProcData.tenv} as proc_data)
      astate () =
    let open Domain in
    if RacerDModels.acquires_ownership procname tenv then
      let astate = add_reads actuals loc astate proc_data in
      let ownership =
        OwnershipDomain.add (ret_base, []) OwnershipAbstractValue.owned astate.ownership
      in
      Some {astate with ownership}
    else None


  let treat_container_accesses ret_base callee_pname actuals loc {ProcData.tenv; pdesc} astate () =
    let open RacerDModels in
    Option.bind (get_container_access callee_pname tenv) ~f:(fun container_access ->
        match List.hd actuals with
        | Some (HilExp.AccessExpression receiver_expr) ->
            let receiver_ap = HilExp.AccessExpression.to_access_path receiver_expr in
            let is_write =
              match container_access with ContainerWrite -> true | ContainerRead -> false
            in
            make_container_access ret_base callee_pname ~is_write receiver_ap loc tenv pdesc astate
        | _ ->
            L.internal_error "Call to %a is marked as a container write, but has no receiver"
              Typ.Procname.pp callee_pname ;
            None )


  let do_proc_call ret_base callee_pname actuals call_flags loc {ProcData.tenv; pdesc}
      (astate : Domain.t) () =
    let open Domain in
    let open RacerDModels in
    let open ConcurrencyModels in
    let ret_access_path = (ret_base, []) in
    let astate =
      if RacerDModels.should_flag_interface_call tenv actuals call_flags callee_pname then
        Domain.add_unannotated_call_access callee_pname loc pdesc astate
      else astate
    in
    let astate =
      match get_thread callee_pname with
      | BackgroundThread ->
          {astate with threads= ThreadsDomain.AnyThread}
      | MainThread ->
          {astate with threads= ThreadsDomain.AnyThreadButSelf}
      | MainThreadIfTrue ->
          let attribute_map =
            AttributeMapDomain.add_attribute ret_access_path (Choice Choice.OnMainThread)
              astate.attribute_map
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
              locks= LocksDomain.acquire_lock astate.locks
            ; threads= update_for_lock_use astate.threads }
        | Unlock _ | GuardDestroy _ | GuardUnlock _ ->
            { astate with
              locks= LocksDomain.release_lock astate.locks
            ; threads= update_for_lock_use astate.threads }
        | LockedIfTrue _ | GuardLockedIfTrue _ ->
            let attribute_map =
              AttributeMapDomain.add_attribute ret_access_path (Choice Choice.LockHeld)
                astate.attribute_map
            in
            {astate with attribute_map; threads= update_for_lock_use astate.threads}
        | GuardConstruct {acquire_now= false} ->
            astate
        | NoEffect -> (
            let rebased_summary_opt =
              Payload.read pdesc callee_pname
              |> Option.map ~f:(fun summary ->
                     let rebased_accesses =
                       Ondemand.get_proc_desc callee_pname
                       |> Option.fold ~init:summary.accesses ~f:(expand_actuals actuals)
                     in
                     {summary with accesses= rebased_accesses} )
            in
            match rebased_summary_opt with
            | Some {threads; locks; accesses; return_ownership; return_attributes} ->
                let locks =
                  LocksDomain.integrate_summary ~caller_astate:astate.locks ~callee_astate:locks
                in
                let accesses =
                  add_callee_accesses astate accesses locks threads actuals callee_pname pdesc loc
                in
                let ownership =
                  OwnershipDomain.propagate_return ret_access_path return_ownership actuals
                    astate.ownership
                in
                let attribute_map =
                  AttributeMapDomain.add ret_access_path return_attributes astate.attribute_map
                in
                let threads =
                  ThreadsDomain.integrate_summary ~caller_astate:astate.threads
                    ~callee_astate:threads
                in
                {locks; threads; accesses; ownership; attribute_map}
            | None ->
                call_without_summary callee_pname ret_access_path call_flags actuals astate )
    in
    let add_if_annotated predicate attribute attribute_map =
      if PatternMatch.override_exists predicate tenv callee_pname then
        AttributeMapDomain.add_attribute ret_access_path attribute attribute_map
      else attribute_map
    in
    let attribute_map = add_if_annotated is_functional Functional astate_callee.attribute_map in
    let ownership =
      if
        PatternMatch.override_exists
          (has_return_annot Annotations.ia_is_returns_ownership)
          tenv callee_pname
      then OwnershipDomain.add ret_access_path OwnershipAbstractValue.owned astate_callee.ownership
      else astate_callee.ownership
    in
    {astate_callee with ownership; attribute_map}


  let do_assignment lhs_access_expr rhs_exp loc ({ProcData.tenv} as proc_data) (astate : Domain.t)
      =
    let open Domain in
    let lhs_access_path = HilExp.AccessExpression.to_access_path lhs_access_expr in
    let rhs_accesses =
      add_access loc ~is_write_access:false astate.locks astate.threads astate.ownership proc_data
        astate.accesses rhs_exp
    in
    let rhs_access_paths =
      HilExp.AccessExpression.to_access_paths (HilExp.get_access_exprs rhs_exp)
    in
    let is_functional =
      (not (List.is_empty rhs_access_paths))
      && List.for_all rhs_access_paths ~f:(fun access_path ->
             AttributeMapDomain.has_attribute access_path Functional astate.attribute_map )
      &&
      match AccessPath.get_typ lhs_access_path tenv with
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
        add_access loc ~is_write_access:true astate.locks astate.threads astate.ownership proc_data
          rhs_accesses (HilExp.AccessExpression lhs_access_expr)
    in
    let ownership =
      OwnershipDomain.propagate_assignment lhs_access_path rhs_exp astate.ownership
    in
    let attribute_map =
      AttributeMapDomain.propagate_assignment lhs_access_path rhs_exp astate.attribute_map
    in
    {astate with accesses; ownership; attribute_map}


  let rec eval_binop op var e1 e2 =
    match (eval_bexp var e1, eval_bexp var e2) with
    | Some b1, Some b2 ->
        Some (op b1 b2)
    | _ ->
        None


  (* return Some bool_value if the given boolean expression evaluates to bool_value when
           [var] is set to true. return None if it has free variables that stop us from
           evaluating it *)
  and eval_bexp var = function
    | HilExp.AccessExpression access_expr ->
        if AccessPath.equal (HilExp.AccessExpression.to_access_path access_expr) var then Some true
        else None
    | HilExp.Constant c ->
        Some (not (Const.iszero_int_float c))
    | HilExp.UnaryOperator (Unop.LNot, e, _) ->
        let b_opt = eval_bexp var e in
        Option.map ~f:not b_opt
    | HilExp.BinaryOperator (Binop.LAnd, e1, e2) ->
        eval_binop ( && ) var e1 e2
    | HilExp.BinaryOperator (Binop.LOr, e1, e2) ->
        eval_binop ( || ) var e1 e2
    | HilExp.BinaryOperator (Binop.Eq, e1, e2) ->
        eval_binop Bool.equal var e1 e2
    | HilExp.BinaryOperator (Binop.Ne, e1, e2) ->
        eval_binop ( <> ) var e1 e2
    | _ ->
        (* non-boolean expression; can't evaluate it *)
        None


  let do_assume assume_exp loc proc_data (astate : Domain.t) =
    let open Domain in
    let add_choice bool_value (acc : Domain.t) = function
      | Choice.LockHeld ->
          let locks =
            if bool_value then LocksDomain.acquire_lock acc.locks
            else LocksDomain.release_lock acc.locks
          in
          {acc with locks}
      | Choice.OnMainThread ->
          let threads =
            if bool_value then ThreadsDomain.AnyThreadButSelf else ThreadsDomain.AnyThread
          in
          {acc with threads}
    in
    let accesses =
      add_access loc ~is_write_access:false astate.locks astate.threads astate.ownership proc_data
        astate.accesses assume_exp
    in
    let astate' =
      match HilExp.get_access_exprs assume_exp with
      | [access_expr] ->
          let access_path = HilExp.AccessExpression.to_access_path access_expr in
          eval_bexp access_path assume_exp
          |> Option.fold ~init:astate ~f:(fun init bool_value ->
                 let choices = AttributeMapDomain.get_choices access_path astate.attribute_map in
                 (* prune (prune_exp) can only evaluate to true if the choice is [bool_value].
                     add the constraint that the choice must be [bool_value] to the state *)
                 List.fold ~f:(add_choice bool_value) ~init choices )
      | _ ->
          astate
    in
    {astate' with accesses}


  let if_none_then = IOption.value_default_f

  let if_none_do ~f x = match x with None -> f () | Some _ -> x

  let exec_instr (astate : Domain.t) ({ProcData.pdesc} as proc_data) _ (instr : HilInstr.t) =
    match instr with
    | Call (ret_base, Direct callee_pname, actuals, call_flags, loc) ->
        let astate = add_reads actuals loc astate proc_data in
        treat_call_acquiring_ownership ret_base callee_pname actuals loc proc_data astate ()
        |> if_none_do
             ~f:(treat_container_accesses ret_base callee_pname actuals loc proc_data astate)
        |> if_none_then
             ~f:(do_proc_call ret_base callee_pname actuals call_flags loc proc_data astate)
    | Call (_, Indirect _, _, _, _) ->
        if Typ.Procname.is_java (Procdesc.get_proc_name pdesc) then
          L.(die InternalError) "Unexpected indirect call instruction %a" HilInstr.pp instr
        else astate
    | Assign (lhs_access_expr, rhs_exp, loc) ->
        do_assignment lhs_access_expr rhs_exp loc proc_data astate
    | Assume (assume_exp, _, _, loc) ->
        do_assume assume_exp loc proc_data astate
    | ExitScope _ ->
        astate


  let pp_session_name _node fmt = F.pp_print_string fmt "racerd"
end

module Analyzer = LowerHil.MakeAbstractInterpreter (TransferFunctions (ProcCfg.Normal))

let analyze_procedure {Callbacks.proc_desc; tenv; summary} =
  let open RacerDModels in
  let open ConcurrencyModels in
  let method_annotation = (Procdesc.get_attributes proc_desc).method_annotation in
  let is_initializer tenv proc_name =
    Typ.Procname.is_constructor proc_name || FbThreadSafety.is_custom_init tenv proc_name
  in
  let open RacerDDomain in
  if should_analyze_proc proc_desc tenv then
    let formal_map = FormalMap.make proc_desc in
    let proc_data = ProcData.make proc_desc tenv ProcData.empty_extras in
    let initial =
      let threads =
        if
          runs_on_ui_thread ~attrs_of_pname:Summary.proc_resolve_attributes tenv proc_desc
          |> Option.is_some
          || is_thread_confined_method tenv proc_desc
        then ThreadsDomain.AnyThreadButSelf
        else if Procdesc.is_java_synchronized proc_desc || is_marked_thread_safe proc_desc tenv
        then ThreadsDomain.AnyThread
        else ThreadsDomain.NoThread
      in
      let add_owned_local acc (var_data : ProcAttributes.var_data) =
        let pvar = Pvar.mk var_data.name (Procdesc.get_proc_name proc_desc) in
        let base = AccessPath.base_of_pvar pvar var_data.typ in
        OwnershipDomain.add (base, []) OwnershipAbstractValue.owned acc
      in
      (* Add ownership to local variables. In cpp, stack-allocated local
         variables cannot be raced on as every thread has its own stack.
          More generally, we will never be confident that a race exists on a local/temp. *)
      let own_locals =
        List.fold ~f:add_owned_local (Procdesc.get_locals proc_desc) ~init:OwnershipDomain.empty
      in
      let is_owned_formal {Annot.class_name} =
        (* @InjectProp allocates a fresh object to bind to the parameter *)
        String.is_suffix ~suffix:Annotations.inject_prop class_name
      in
      let add_conditional_owned_formal acc (formal, formal_index) =
        let ownership_value =
          if Annotations.ma_has_annotation_with method_annotation is_owned_formal then
            OwnershipAbstractValue.owned
          else OwnershipAbstractValue.make_owned_if formal_index
        in
        OwnershipDomain.add (formal, []) ownership_value acc
      in
      if is_initializer tenv (Procdesc.get_proc_name proc_desc) then
        let add_owned_formal acc formal_index =
          match FormalMap.get_formal_base formal_index formal_map with
          | Some base ->
              OwnershipDomain.add (base, []) OwnershipAbstractValue.owned acc
          | None ->
              acc
        in
        let ownership =
          (* if a constructer is called via DI, all of its formals will be freshly allocated and
             therefore owned. we assume that constructors annotated with @Inject will only be
             called via DI or using fresh parameters. *)
          if Annotations.pdesc_has_return_annot proc_desc Annotations.ia_is_inject then
            List.mapi ~f:(fun i _ -> i) (Procdesc.get_formals proc_desc)
            |> List.fold ~f:add_owned_formal ~init:own_locals
          else
            (* express that the constructor owns [this] *)
            let init = add_owned_formal own_locals 0 in
            FormalMap.get_formals_indexes formal_map
            |> List.filter ~f:(fun (_, index) -> not (Int.equal 0 index))
            |> List.fold ~init ~f:add_conditional_owned_formal
        in
        {RacerDDomain.bottom with ownership; threads}
      else
        (* add Owned(formal_index) predicates for each formal to indicate that each one is owned if
           it is owned in the caller *)
        let ownership =
          List.fold ~init:own_locals ~f:add_conditional_owned_formal
            (FormalMap.get_formals_indexes formal_map)
        in
        {RacerDDomain.bottom with ownership; threads}
    in
    match Analyzer.compute_post proc_data ~initial with
    | Some {threads; locks; accesses; ownership; attribute_map} ->
        let return_var_ap =
          AccessPath.of_pvar
            (Pvar.get_ret_pvar (Procdesc.get_proc_name proc_desc))
            (Procdesc.get_ret_type proc_desc)
        in
        let return_ownership = OwnershipDomain.get_owned return_var_ap ownership in
        let return_attributes =
          try AttributeMapDomain.find return_var_ap attribute_map with Caml.Not_found ->
            AttributeSetDomain.empty
        in
        let post = {threads; locks; accesses; return_ownership; return_attributes} in
        Payload.update_summary post summary
    | None ->
        summary
  else Payload.update_summary empty_summary summary


type conflict = RacerDDomain.TraceElem.t

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
           "@\n\
           \ Reporting because current method is annotated %a or overrides an annotated method."
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
          "%s, so we assume that this method can run in parallel with other non-private methods \
           in the class (including itself)."
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
  if Typ.Procname.is_java pname then get_reporting_explanation_java report_kind tenv pname thread
  else get_reporting_explanation_cpp


let pp_container_access fmt (access_path, access_pname) =
  F.fprintf fmt "container %a via call to %s"
    (MF.wrap_monospaced AccessPath.pp)
    access_path
    (MF.monospaced_to_string (Typ.Procname.get_method access_pname))


let pp_access fmt (t : RacerDDomain.TraceElem.t) =
  match t.elem with
  | Read {path} | Write {path} ->
      (MF.wrap_monospaced AccessPath.pp) fmt path
  | ContainerRead {path; pname} | ContainerWrite {path; pname} ->
      pp_container_access fmt (path, pname)
  | InterfaceCall _ as access ->
      RacerDDomain.Access.pp fmt access


let make_trace ~report_kind original_path =
  let open RacerDDomain in
  let loc_trace_of_path path = TraceElem.make_loc_trace path in
  let original_trace = loc_trace_of_path original_path in
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


let log_issue current_pname ~loc ~ltr ~access issue_type error_message =
  Reporting.log_issue_external current_pname Exceptions.Error ~loc ~ltr ~access issue_type
    error_message


type reported_access =
  { threads: RacerDDomain.ThreadsDomain.t
  ; snapshot: RacerDDomain.AccessSnapshot.t
  ; tenv: Tenv.t
  ; procdesc: Procdesc.t }

let report_thread_safety_violation ~make_description ~report_kind
    ({threads; snapshot; tenv; procdesc} : reported_access) =
  let open RacerDDomain in
  let pname = Procdesc.get_proc_name procdesc in
  let access = snapshot.access in
  let final_pname = List.last access.trace |> Option.value_map ~default:pname ~f:CallSite.pname in
  let final_sink_site = CallSite.make final_pname access.loc in
  let initial_sink_site = CallSite.make pname (TraceElem.get_loc access) in
  let loc = CallSite.loc initial_sink_site in
  let ltr, original_end, conflict_end = make_trace ~report_kind access in
  (* what the potential bug is *)
  let description = make_description pname final_sink_site initial_sink_site access in
  (* why we are reporting it *)
  let issue_type, explanation = get_reporting_explanation report_kind tenv pname threads in
  let error_message = F.sprintf "%s%s" description explanation in
  let end_locs = Option.to_list original_end @ Option.to_list conflict_end in
  let access = IssueAuxData.encode end_locs in
  log_issue pname ~loc ~ltr ~access issue_type error_message


let report_unannotated_interface_violation (reported_access : reported_access) =
  let reported_pname = Procdesc.get_proc_name reported_access.procdesc in
  match reported_pname with
  | Typ.Procname.Java java_pname ->
      let class_name = Typ.Procname.Java.get_class_name java_pname in
      let make_description _ _ _ _ =
        F.asprintf
          "Unprotected call to method %a of un-annotated interface %s. Consider annotating the \
           class with %a, adding a lock, or using an interface that is known to be thread-safe."
          Typ.Procname.pp reported_pname class_name MF.pp_monospaced "@ThreadSafe"
      in
      report_thread_safety_violation ~make_description ~report_kind:UnannotatedInterface
        reported_access
  | _ ->
      (* skip reporting on C++ *)
      ()


let make_unprotected_write_description pname final_sink_site initial_sink_site final_sink =
  Format.asprintf "Unprotected write. Non-private method %a%s %s %a outside of synchronization."
    (MF.wrap_monospaced Typ.Procname.pp)
    pname
    (if CallSite.equal final_sink_site initial_sink_site then "" else " indirectly")
    (if RacerDDomain.TraceElem.is_container_write final_sink then "mutates" else "writes to field")
    pp_access final_sink


let make_guardedby_violation_description pname final_sink_site initial_sink_site final_sink =
  Format.asprintf
    "GuardedBy violation. Non-private method %a%s accesses %a outside of synchronization."
    (MF.wrap_monospaced Typ.Procname.pp)
    pname
    (if CallSite.equal final_sink_site initial_sink_site then "" else " indirectly")
    pp_access final_sink


let make_read_write_race_description ~read_is_sync (conflict : reported_access) pname
    final_sink_site initial_sink_site final_sink =
  let pp_conflict fmt {procdesc} =
    F.pp_print_string fmt
      (Typ.Procname.to_simplified_string ~withclass:true (Procdesc.get_proc_name procdesc))
  in
  let conflicts_description =
    Format.asprintf "Potentially races with%s write in method %a"
      (if read_is_sync then " unsynchronized" else "")
      (MF.wrap_monospaced pp_conflict) conflict
  in
  Format.asprintf "Read/Write race. Non-private method %a%s reads%s from %a. %s."
    (MF.wrap_monospaced Typ.Procname.pp)
    pname
    (if CallSite.equal final_sink_site initial_sink_site then "" else " indirectly")
    (if read_is_sync then " with synchronization" else " without synchronization")
    pp_access final_sink conflicts_description


(** type for remembering what we have already reported to avoid duplicates. our policy is to report
    each kind of access (read/write) to the same field reachable from the same procedure only once.
    in addition, if a call to a procedure (transitively) accesses multiple fields, we will only
    report one of each kind of access *)
type reported =
  { reported_sites: CallSite.Set.t
  ; reported_writes: Typ.Procname.Set.t
  ; reported_reads: Typ.Procname.Set.t
  ; reported_unannotated_calls: Typ.Procname.Set.t }

let empty_reported =
  let reported_sites = CallSite.Set.empty in
  let reported_writes = Typ.Procname.Set.empty in
  let reported_reads = Typ.Procname.Set.empty in
  let reported_unannotated_calls = Typ.Procname.Set.empty in
  {reported_sites; reported_reads; reported_writes; reported_unannotated_calls}


(* decide if we should throw away a path before doing safety analysis
   for now, just check for whether the access is within a switch-map
   that is auto-generated by Java. *)
let should_filter_access path_opt =
  let check_access = function
    | AccessPath.ArrayAccess _ ->
        false
    | AccessPath.FieldAccess fld ->
        String.is_substring ~substring:"$SwitchMap" (Typ.Fieldname.to_string fld)
  in
  Option.exists path_opt ~f:(fun (_, path) -> List.exists path ~f:check_access)


(**
  Map containing reported accesses, which groups them in lists, by abstract location.
  The equivalence relation used for grouping them is equality of access paths.
  This is slightly complicated because local variables contain the pname of the function declaring
  them.  Here we want a purely name-based comparison, and in particular that [this == this]
  regardless the method declaring it.  Hence the redefined comparison functions.
*)
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
    type t =
      | Location of PathModuloThis.t
      | Container of PathModuloThis.t
      | Call of Typ.Procname.t
    [@@deriving compare]

    let of_access (access : RacerDDomain.Access.t) =
      match access with
      | Read {path} | Write {path} ->
          Location path
      | ContainerRead {path} | ContainerWrite {path} ->
          Container path
      | InterfaceCall pn ->
          Call pn
  end

  module M = Caml.Map.Make (Key)

  type t = reported_access list M.t

  let empty = M.empty

  let add (rep : reported_access) map =
    let access = rep.snapshot.access.elem in
    if RacerDDomain.Access.get_access_path access |> should_filter_access then map
    else
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
      || Procdesc.get_access procdesc <> PredSymb.Private
         && (not (Typ.Procname.Java.is_autogen_method java_pname))
         && not (Annotations.pdesc_return_annot_ends_with procdesc Annotations.visibleForTesting)
  | ObjC_Cpp {kind; class_name} ->
      ( match kind with
      | CPPMethod _ | CPPConstructor _ | CPPDestructor _ ->
          Procdesc.get_access procdesc <> PredSymb.Private
      | ObjCClassMethod | ObjCInstanceMethod | ObjCInternalMethod ->
          Tenv.lookup tenv class_name
          |> Option.exists ~f:(fun {Typ.Struct.exported_objc_methods} ->
                 List.mem ~equal:Typ.Procname.equal exported_objc_methods proc_name ) )
      &&
      let matcher = ConcurrencyModels.cpp_lock_types_matcher in
      Option.exists (Tenv.lookup tenv class_name) ~f:(fun class_str ->
          (* check if the class contains a lock member *)
          List.exists class_str.Typ.Struct.fields ~f:(fun (_, ft, _) ->
              Option.exists (Typ.name ft) ~f:(fun name ->
                  QualifiedCppName.Match.match_qualifiers matcher (Typ.Name.qual_name name) ) ) )
  | _ ->
      false


let should_report_guardedby_violation ({snapshot; tenv} : reported_access) =
  (not snapshot.lock)
  && RacerDDomain.Access.get_access_path snapshot.access.elem
     |> Option.exists ~f:(fun path ->
            AccessPath.get_field_and_annotation path tenv
            |> Option.exists ~f:(fun (_, annotlist) ->
                   List.exists annotlist ~f:(fun (annot, _) ->
                       Annotations.annot_ends_with annot Annotations.guarded_by ) ) )


(** Report accesses that may race with each other.

    Principles for race reporting.

    Two accesses are excluded if they are both protected by the same lock or are known to be on the
    same thread. Otherwise they are in conflict. We want to report conflicting accesses one of which
    is a write.

    To cut down on duplication noise we don't always report at both sites (line numbers) involved in
    a race.

    -- If a protected access races with an unprotected one, we don't report the protected but we do
       report the unprotected one (and we point to the protected from the unprotected one).  This
       way the report is at the line number in a race-pair where the programmer should take action.

    -- Similarly, if a threaded and unthreaded (not known to be threaded) access race, we report at
       the unthreaded site.

    Also, we avoid reporting multiple races at the same line (which can happen a lot in an
    interprocedural scenario) or multiple accesses to the same field in a single method, expecting
    that the programmer already gets signal from one report. To report all the races with separate
    warnings leads to a lot of noise.  But note, we never suppress all the potential issues in a
    class: if we don't report any races, it means we didn't find any.

    The above is tempered at the moment by abstractions of "same lock" and "same thread": we are
    currently not distinguishing different locks, and are treating "known to be confined to a
    thread" as if "known to be confined to UI thread".
*)
let report_unsafe_accesses (aggregated_access_map : ReportMap.t) =
  let open RacerDDomain in
  let open RacerDModels in
  let is_duplicate_report ({snapshot; procdesc} : reported_access)
      {reported_sites; reported_writes; reported_reads; reported_unannotated_calls} =
    let pname = Procdesc.get_proc_name procdesc in
    let call_site = CallSite.make pname (TraceElem.get_loc snapshot.access) in
    if Config.filtering then
      CallSite.Set.mem call_site reported_sites
      ||
      match snapshot.access.TraceElem.elem with
      | Access.Write _ | Access.ContainerWrite _ ->
          Typ.Procname.Set.mem pname reported_writes
      | Access.Read _ | Access.ContainerRead _ ->
          Typ.Procname.Set.mem pname reported_reads
      | Access.InterfaceCall _ ->
          Typ.Procname.Set.mem pname reported_unannotated_calls
    else false
  in
  let update_reported ({snapshot; procdesc} : reported_access) reported =
    if Config.filtering then
      let pname = Procdesc.get_proc_name procdesc in
      let call_site = CallSite.make pname (TraceElem.get_loc snapshot.access) in
      let reported_sites = CallSite.Set.add call_site reported.reported_sites in
      match snapshot.access.TraceElem.elem with
      | Access.Write _ | Access.ContainerWrite _ ->
          let reported_writes = Typ.Procname.Set.add pname reported.reported_writes in
          {reported with reported_writes; reported_sites}
      | Access.Read _ | Access.ContainerRead _ ->
          let reported_reads = Typ.Procname.Set.add pname reported.reported_reads in
          {reported with reported_reads; reported_sites}
      | Access.InterfaceCall _ ->
          let reported_unannotated_calls =
            Typ.Procname.Set.add pname reported.reported_unannotated_calls
          in
          {reported with reported_unannotated_calls; reported_sites}
    else reported
  in
  let report_unsafe_access accesses reported_acc
      ({snapshot; threads; tenv; procdesc} as reported_access) =
    let pname = Procdesc.get_proc_name procdesc in
    if is_duplicate_report reported_access reported_acc then reported_acc
    else
      match snapshot.access.elem with
      | Access.InterfaceCall _
        when AccessSnapshot.is_unprotected snapshot
             && ThreadsDomain.is_any threads
             && is_marked_thread_safe procdesc tenv ->
          (* un-annotated interface call + no lock in method marked thread-safe. warn *)
          report_unannotated_interface_violation reported_access ;
          update_reported reported_access reported_acc
      | Access.InterfaceCall _ ->
          reported_acc
      | (Access.Write _ | ContainerWrite _) when Typ.Procname.is_java pname ->
          let conflict =
            if ThreadsDomain.is_any threads then
              (* unprotected write in method that may run in parallel with itself. warn *)
              None
            else
              (* unprotected write, but not on a method that may run in parallel with itself
                   (i.e., not a self race). find accesses on a background thread this access might
                   conflict with and report them *)
              List.find_map accesses ~f:(fun {snapshot= other_snapshot; threads= other_threads} ->
                  if TraceElem.is_write other_snapshot.access && ThreadsDomain.is_any other_threads
                  then Some other_snapshot.access
                  else None )
          in
          if
            AccessSnapshot.is_unprotected snapshot
            && (Option.is_some conflict || ThreadsDomain.is_any threads)
          then (
            report_thread_safety_violation ~make_description:make_unprotected_write_description
              ~report_kind:(WriteWriteRace conflict) reported_access ;
            update_reported reported_access reported_acc )
          else reported_acc
      | Access.Write _ | ContainerWrite _ ->
          (* Do not report unprotected writes for ObjC_Cpp *)
          reported_acc
      | (Access.Read _ | ContainerRead _) when AccessSnapshot.is_unprotected snapshot ->
          (* unprotected read. report all writes as conflicts for java. for c++ filter out
             unprotected writes *)
          let is_conflict {snapshot; threads= other_threads} =
            TraceElem.is_write snapshot.access
            &&
            if Typ.Procname.is_java pname then
              ThreadsDomain.is_any threads || ThreadsDomain.is_any other_threads
            else not (AccessSnapshot.is_unprotected snapshot)
          in
          List.find ~f:is_conflict accesses
          |> Option.value_map ~default:reported_acc ~f:(fun conflict ->
                 let make_description =
                   make_read_write_race_description ~read_is_sync:false conflict
                 in
                 let report_kind = ReadWriteRace conflict.snapshot.access in
                 report_thread_safety_violation ~make_description ~report_kind reported_access ;
                 update_reported reported_access reported_acc )
      | Access.Read _ | ContainerRead _ ->
          (* protected read. report unprotected writes and opposite protected writes as conflicts *)
          let can_conflict (snapshot1 : AccessSnapshot.t) (snapshot2 : AccessSnapshot.t) =
            if snapshot1.lock && snapshot2.lock then false
            else ThreadsDomain.can_conflict snapshot1.thread snapshot2.thread
          in
          let is_conflict {snapshot= other_snapshot; threads= other_threads} =
            if AccessSnapshot.is_unprotected other_snapshot then
              TraceElem.is_write other_snapshot.access && ThreadsDomain.is_any other_threads
            else TraceElem.is_write other_snapshot.access && can_conflict snapshot other_snapshot
          in
          List.find accesses ~f:is_conflict
          |> Option.value_map ~default:reported_acc ~f:(fun conflict ->
                 (* protected read with conflicting unprotected write(s). warn. *)
                 let make_description =
                   make_read_write_race_description ~read_is_sync:true conflict
                 in
                 let report_kind = ReadWriteRace conflict.snapshot.access in
                 report_thread_safety_violation ~make_description ~report_kind reported_access ;
                 update_reported reported_access reported_acc )
  in
  let report_accesses_on_location (reportable_accesses : reported_access list) init =
    (* Don't report on location if all accesses are on non-concurrent contexts *)
    if
      List.for_all reportable_accesses ~f:(fun ({threads} : reported_access) ->
          ThreadsDomain.is_any threads |> not )
    then init
    else List.fold reportable_accesses ~init ~f:(report_unsafe_access reportable_accesses)
  in
  let report_guardedby_violations_on_location grouped_accesses init =
    List.fold grouped_accesses ~init ~f:(fun acc r ->
        if should_report_guardedby_violation r && not (is_duplicate_report r acc) then (
          report_thread_safety_violation ~make_description:make_guardedby_violation_description
            ~report_kind:GuardedByViolation r ;
          update_reported r acc )
        else acc )
  in
  let report grouped_accesses reported_acc =
    (* reset the reported reads and writes for each memory location *)
    let reported_acc =
      { reported_acc with
        reported_writes= Typ.Procname.Set.empty; reported_reads= Typ.Procname.Set.empty }
    in
    report_guardedby_violations_on_location grouped_accesses reported_acc
    |> report_accesses_on_location grouped_accesses
  in
  ReportMap.fold report aggregated_access_map empty_reported |> ignore


(* create a map from [abstraction of a memory loc] -> accesses that
   may touch that memory loc. the abstraction of a location is an access
   path like x.f.g whose concretization is the set of memory cells
   that x.f.g may point to during execution *)
let make_results_table file_env =
  let open RacerDDomain in
  let aggregate_post tenv procdesc acc {threads; accesses} =
    AccessDomain.fold
      (fun snapshot acc -> ReportMap.add {threads; snapshot; tenv; procdesc} acc)
      accesses acc
  in
  List.fold file_env ~init:ReportMap.empty ~f:(fun acc (tenv, proc_desc) ->
      Procdesc.get_proc_name proc_desc |> Payload.read proc_desc
      |> Option.fold ~init:acc ~f:(aggregate_post tenv proc_desc) )


(* aggregate all of the procedures in the file env by their declaring
   class. this lets us analyze each class individually *)
let aggregate_by_class file_env =
  List.fold file_env ~init:String.Map.empty ~f:(fun acc ((tenv, pdesc) as proc) ->
      if should_report_on_proc tenv pdesc then
        Procdesc.get_proc_name pdesc |> Typ.Procname.get_class_name
        |> Option.fold ~init:acc ~f:(fun acc classname ->
               String.Map.add_multi acc ~key:classname ~data:proc )
      else acc )


(* Gathers results by analyzing all the methods in a file, then
   post-processes the results to check an (approximation of) thread
   safety *)
let file_analysis {Callbacks.procedures; source_file} =
  aggregate_by_class procedures
  |> String.Map.iter ~f:(fun class_env -> report_unsafe_accesses (make_results_table class_env)) ;
  IssueLog.store Config.racerd_issues_dir_name source_file
