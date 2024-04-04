(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging

(** add Abstract instructions into the IR to give hints about when abstraction should be performed *)
module AddAbstractionInstructions = struct
  let process pdesc =
    let open Procdesc in
    (* true if there is a succ node s.t.: it is an exit node, or the succ of >1 nodes *)
    let converging_node node =
      let is_exit node = match Node.get_kind node with Node.Exit_node -> true | _ -> false in
      let succ_nodes = Node.get_succs node in
      if List.exists ~f:is_exit succ_nodes then true
      else
        match succ_nodes with [] -> false | [h] -> List.length (Node.get_preds h) > 1 | _ -> false
    in
    let node_requires_abstraction node =
      match Node.get_kind node with
      | Node.Start_node | Node.Join_node ->
          false
      | Node.Exit_node | Node.Stmt_node _ | Node.Prune_node _ | Node.Skip_node _ ->
          converging_node node
    in
    let do_node node =
      let loc = Node.get_last_loc node in
      if node_requires_abstraction node then Node.append_instrs node [Sil.Metadata (Abstract loc)]
    in
    Procdesc.iter_nodes do_node pdesc
end

let objc_get_first_arg_typ = function
  | [(_, {Typ.desc= Tptr ({desc= Tstruct ((ObjcClass _ | ObjcProtocol _) as objc_class)}, _)})] ->
      Some objc_class
  | _ ->
      None


(** In ObjC, [NSObject.copy] returns the object returned by [copyWithZone:] on the given class. This
    method must be implemented if the class complies with [NSCopying] protocol. Since we don't have
    access to NSObject's code, to follow calls into [copyWithZone:], we replace such [copy] calls
    with calls to [copyWithZone] when i) such a method exists in the class and 2) class conforms to
    NSCopying protocol.

    TODO: handle calls into superclasses.

    Even though [NSObject] doesn't itself conform to [NSCopying], it supports the above pattern.
    Hence, we consider all subclasses that extend it to conform to the protocol. Similarly for:
    [mutableCopy] -> [mutableCopyWithZone:] for classes implementing [NSMutableCopying] protocol. *)
module ReplaceObjCCopy = struct
  type copy_kind =
    {protocol: string; method_name: string; method_with_zone: string; is_mutable: bool}

  let get_copy_kind_opt pname =
    let matches_nsobject_proc method_name =
      String.equal (Procname.get_method pname) method_name
      && Procname.get_class_type_name pname
         |> Option.exists ~f:(fun type_name -> String.equal (Typ.Name.name type_name) "NSObject")
    in
    if matches_nsobject_proc "copy" then
      Some
        { protocol= "NSCopying"
        ; method_name= "copy"
        ; method_with_zone= "copyWithZone:"
        ; is_mutable= false }
    else if matches_nsobject_proc "mutableCopy" then
      Some
        { protocol= "NSMutableCopying"
        ; method_name= "mutableCopy"
        ; method_with_zone= "mutableCopyWithZone:"
        ; is_mutable= true }
    else None


  let method_exists_in_sources pdesc ~method_name ~class_name =
    let pname = Procdesc.get_proc_name pdesc in
    let procs = SourceFiles.get_procs_in_file pname in
    List.exists procs ~f:(fun pn ->
        let class_name_opt = Procname.get_class_name pn in
        String.equal method_name (Procname.get_method pn)
        && Option.exists class_name_opt ~f:(String.equal class_name) )


  let get_replaced_instr {protocol; method_name; method_with_zone; is_mutable} pdesc tenv args
      ret_id_typ loc flags =
    match objc_get_first_arg_typ args with
    | Some cl ->
        let class_name = Typ.Name.name cl in
        if
          ( PatternMatch.ObjectiveC.conforms_to ~protocol tenv class_name
          || PatternMatch.ObjectiveC.implements "NSObject" tenv class_name )
          && method_exists_in_sources pdesc ~method_name:method_with_zone ~class_name
        then (
          let pname = Procname.make_objc_copyWithZone cl ~is_mutable in
          let function_exp = Exp.Const (Const.Cfun pname) in
          (* Zone parameter is ignored: Memory zones are no longer
             used by Objective-C. We still need to satisfy the
             signature though. *)
          L.(debug Capture Verbose) "REPLACING %s with '%s'@\n" method_name method_with_zone ;
          Some
            (Sil.Call
               ( ret_id_typ
               , function_exp
               , args @ [(Exp.null, StdTyp.Objc.pointer_to_nszone)]
               , loc
               , flags ) ) )
        else None
    | _ ->
        None


  let process tenv pdesc ret_id_typ callee args loc flags =
    get_copy_kind_opt callee
    |> Option.bind ~f:(fun copy_kind ->
           get_replaced_instr copy_kind pdesc tenv args ret_id_typ loc flags )
end

module ReplaceObjCOverridden = struct
  let may_be_super_call class_name_opt object_name =
    Option.exists class_name_opt ~f:(Typ.Name.equal object_name)


  let get_overridden_method_opt tenv ~caller_class_name ~callee args =
    let open IOption.Let_syntax in
    let* sup_class_name = Procname.get_class_type_name callee in
    let* sub_class_name = objc_get_first_arg_typ args in
    if
      PatternMatch.is_subtype tenv sub_class_name sup_class_name
      && not (may_be_super_call caller_class_name sub_class_name)
    then
      let callee' = Procname.replace_class callee sub_class_name in
      if Option.is_some (Procdesc.load callee') then Some callee' else None
    else None


  let process tenv caller ret_id_typ callee args loc flags =
    get_overridden_method_opt tenv
      ~caller_class_name:(Procname.get_class_type_name caller)
      ~callee args
    |> Option.map ~f:(fun overridden_method ->
           Logging.d_printfln_escaped "Replace overridden method %a to %a" Procname.pp callee
             Procname.pp overridden_method ;
           Sil.Call (ret_id_typ, Const (Cfun overridden_method), args, loc, flags) )
end

module ReplaceObjCMethodCall = struct
  let process tenv pdesc caller =
    let replace_method instr =
      match (instr : Sil.instr) with
      | Call (ret_id_typ, Const (Cfun callee), args, loc, flags) ->
          IOption.if_none_evalopt
            (ReplaceObjCCopy.process tenv pdesc ret_id_typ callee args loc flags) ~f:(fun () ->
              ReplaceObjCOverridden.process tenv caller ret_id_typ callee args loc flags )
          |> Option.value ~default:instr
      | _ ->
          instr
    in
    Procdesc.replace_instrs pdesc ~f:(fun node instr ->
        NodePrinter.with_session node ~kind:`ComputePre
          ~pp_name:(fun fmt -> Format.pp_print_string fmt "Replace ObjC method")
          ~f:(fun () -> replace_method instr) )
    |> ignore
end

(** Find synthetic (including access and bridge) Java methods in the procedure and inline them in
    the cfg.

    This is a horrible hack that inlines only *one* instruction ouf of the callee. This works only
    on some synthetic methods that have a particular shape. *)
module InlineJavaSyntheticMethods = struct
  (** Inline a synthetic (access or bridge) method. *)
  let inline_synthetic_method ((ret_id, _) as ret) etl pdesc loc_call : Sil.instr option =
    let found instr instr' =
      L.debug Analysis Verbose
        "inline_synthetic_method translated the call %a as %a (original instr %a)@\n" Procname.pp
        (Procdesc.get_proc_name pdesc)
        (Sil.pp_instr ~print_types:true Pp.text)
        instr'
        (Sil.pp_instr ~print_types:true Pp.text)
        instr ;
      Some instr'
    in
    let do_instr instr =
      match (instr, etl) with
      | Sil.Load {e= Exp.Lfield (Exp.Var _, fn, ft); typ}, [(* getter for fields *) (e1, _)] ->
          let instr' = Sil.Load {id= ret_id; e= Exp.Lfield (e1, fn, ft); typ; loc= loc_call} in
          found instr instr'
      | Sil.Load {e= Exp.Lfield (Exp.Lvar pvar, fn, ft); typ}, [] when Pvar.is_global pvar ->
          (* getter for static fields *)
          let instr' =
            Sil.Load {id= ret_id; e= Exp.Lfield (Exp.Lvar pvar, fn, ft); typ; loc= loc_call}
          in
          found instr instr'
      | Sil.Store {e1= Exp.Lfield (_, fn, ft); typ}, [(* setter for fields *) (e1, _); (e2, _)] ->
          let instr' = Sil.Store {e1= Exp.Lfield (e1, fn, ft); typ; e2; loc= loc_call} in
          found instr instr'
      | Sil.Store {e1= Exp.Lfield (Exp.Lvar pvar, fn, ft); typ}, [(e1, _)] when Pvar.is_global pvar
        ->
          (* setter for static fields *)
          let instr' =
            Sil.Store {e1= Exp.Lfield (Exp.Lvar pvar, fn, ft); typ; e2= e1; loc= loc_call}
          in
          found instr instr'
      | Sil.Call (_, Exp.Const (Const.Cfun pn), etl', _, cf), _
        when Int.equal (List.length etl') (List.length etl) ->
          let instr' = Sil.Call (ret, Exp.Const (Const.Cfun pn), etl, loc_call, cf) in
          found instr instr'
      | Sil.Call (_, Exp.Const (Const.Cfun pn), etl', _, cf), _
        when Int.equal (List.length etl' + 1) (List.length etl) ->
          let etl1 =
            match List.rev etl with
            (* remove last element *)
            | _ :: l ->
                List.rev l
            | [] ->
                assert false
          in
          let instr' = Sil.Call (ret, Exp.Const (Const.Cfun pn), etl1, loc_call, cf) in
          found instr instr'
      | _ ->
          None
    in
    Procdesc.find_map_instrs ~f:do_instr pdesc


  let process pdesc =
    let is_generated_for_lambda proc_name =
      let method_name = Procname.get_method proc_name in
      String.is_substring ~substring:Config.java_lambda_marker_infix_generated_by_javalib
        method_name
      || String.is_prefix ~prefix:Config.java_lambda_marker_prefix_generated_by_javac method_name
    in
    let should_inline proc_name =
      (not (is_generated_for_lambda proc_name))
      &&
      match Attributes.load proc_name with
      | None ->
          false
      | Some attributes ->
          let is_access =
            match proc_name with
            | Procname.Java java_proc_name ->
                Procname.Java.is_access_method java_proc_name
            | _ ->
                false
          in
          let is_synthetic = attributes.is_synthetic_method in
          let is_bridge = attributes.is_bridge_method in
          is_access || is_bridge || is_synthetic
    in
    let instr_inline_synthetic_method _node (instr : Sil.instr) =
      match instr with
      | Call (ret_id_typ, Const (Cfun pn), etl, loc, _) when should_inline pn ->
          Option.bind (Procdesc.load pn) ~f:(fun proc_desc_callee ->
              inline_synthetic_method ret_id_typ etl proc_desc_callee loc )
          |> Option.value ~default:instr
      | _ ->
          instr
    in
    Procdesc.replace_instrs pdesc ~f:instr_inline_synthetic_method |> ignore
end

(** perform liveness analysis and insert Nullify/Remove_temps instructions into the IR to make it
    easy for analyses to do abstract garbage collection *)
module Liveness = struct
  module VarDomain = Liveness.Domain

  (** computes the non-nullified reaching definitions at the end of each node by building on the
      results of a liveness analysis to be precise, what we want to compute is:

      to_nullify := (live_before U non_nullifed_reaching_defs) - live_after

      non_nullified_reaching_defs := non_nullified_reaching_defs - to_nullify

      Note that this can't be done with by combining the results of reaching definitions and
      liveness after the fact, nor can it be done with liveness alone. We will insert nullify
      instructions for each pvar in to_nullify afer we finish the analysis. Nullify instructions
      speed up the analysis by enabling it to GC state that will no longer be read. *)
  module NullifyTransferFunctions = struct
    (** (reaching non-nullified vars) * (vars to nullify) *)
    module Domain = AbstractDomain.Pair (VarDomain) (VarDomain)

    module CFG = ProcCfg.Exceptional

    type analysis_data = Liveness.t

    let postprocess ((reaching_defs, _) as astate) node liveness_inv_map =
      let node_id = Procdesc.Node.get_id (CFG.Node.underlying_node node) in
      match
        (Liveness.live_before node_id liveness_inv_map, Liveness.live_after node_id liveness_inv_map)
      with
      | Some live_before, Some live_after ->
          let to_nullify = VarDomain.diff (VarDomain.union live_before reaching_defs) live_after in
          let reaching_defs' = VarDomain.diff reaching_defs to_nullify in
          (reaching_defs', to_nullify)
      | _, _ ->
          astate


    let cache_node = ref (Procdesc.Node.dummy (Procname.from_string_c_fun ""))

    let cache_instr = ref Sil.skip_instr

    let last_instr_in_node node =
      let get_last_instr () =
        CFG.instrs node |> Instrs.last |> Option.value ~default:Sil.skip_instr
      in
      if phys_equal node !cache_node then !cache_instr
      else
        let last_instr = get_last_instr () in
        cache_node := node ;
        cache_instr := last_instr ;
        last_instr


    let is_last_instr_in_node instr node = phys_equal (last_instr_in_node node) instr

    let exec_instr ((active_defs, to_nullify) as astate) liveness_inv_map node _ (instr : Sil.instr)
        =
      let astate' =
        match instr with
        | Load {id= lhs_id} ->
            (VarDomain.add (Var.of_id lhs_id) active_defs, to_nullify)
        | Call ((id, _), _, actuals, _, {CallFlags.cf_assign_last_arg}) ->
            let active_defs = VarDomain.add (Var.of_id id) active_defs in
            let active_defs =
              if cf_assign_last_arg then
                match IList.split_last_rev actuals with
                | Some ((Exp.Lvar pvar, _), _) ->
                    VarDomain.add (Var.of_pvar pvar) active_defs
                | _ ->
                    active_defs
              else active_defs
            in
            (active_defs, to_nullify)
        | Store {e1= Exp.Lvar lhs_pvar} ->
            (VarDomain.add (Var.of_pvar lhs_pvar) active_defs, to_nullify)
        | Metadata (VariableLifetimeBegins {pvar}) ->
            (VarDomain.add (Var.of_pvar pvar) active_defs, to_nullify)
        | Store _
        | Prune _
        | Metadata
            (Abstract _ | CatchEntry _ | EndBranches | ExitScope _ | Skip | TryEntry _ | TryExit _)
          ->
            astate
        | Metadata (Nullify _) ->
            L.die InternalError
              "%a: found NULLIFY instructions while doing the nullify pre-analysis; did the \
               pre-analysis run twice?"
              Procname.pp
              (Procdesc.Node.get_proc_name node)
      in
      if is_last_instr_in_node instr node then postprocess astate' node liveness_inv_map
      else astate'


    let pp_session_name _node fmt = Format.pp_print_string fmt "nullify"
  end

  module NullifyAnalysis = AbstractInterpreter.MakeRPO (NullifyTransferFunctions)

  let add_nullify_instrs proc_desc liveness_inv_map =
    let address_taken_vars =
      if Procname.is_java (Procdesc.get_proc_name proc_desc) then AddressTaken.Domain.empty
        (* can't take the address of a variable in Java *)
      else
        let initial = AddressTaken.Domain.empty in
        match AddressTaken.Analyzer.compute_post () ~initial proc_desc with
        | Some post ->
            post
        | None ->
            AddressTaken.Domain.empty
    in
    let nullify_proc_cfg = ProcCfg.Exceptional.from_pdesc proc_desc in
    let initial = (VarDomain.bottom, VarDomain.bottom) in
    let nullify_inv_map = NullifyAnalysis.exec_cfg nullify_proc_cfg liveness_inv_map ~initial in
    (* only nullify pvars that are local; don't nullify those that can escape *)
    let is_local pvar = not (Liveness.is_always_in_scope proc_desc pvar) in
    let prepend_node_nullify_instructions loc pvars instrs =
      List.fold pvars ~init:instrs ~f:(fun instrs pvar ->
          if is_local pvar && not (Pvar.is_artificial pvar) then
            Sil.Metadata (Nullify (pvar, loc)) :: instrs
          else instrs )
    in
    let node_deadvars_instruction loc vars =
      let local_vars =
        List.rev_filter vars ~f:(function
          | Var.ProgramVar pvar ->
              is_local pvar
          | Var.LogicalVar _ ->
              true )
      in
      if List.is_empty local_vars then None else Some (Sil.Metadata (ExitScope (local_vars, loc)))
    in
    Container.iter nullify_proc_cfg ~fold:ProcCfg.Exceptional.fold_nodes ~f:(fun node ->
        match NullifyAnalysis.extract_post (ProcCfg.Exceptional.Node.id node) nullify_inv_map with
        | Some (_, to_nullify) ->
            let dead_vars, pvars_to_nullify =
              VarDomain.fold
                (fun var (dead_vars, pvars_to_nullify) ->
                  let dead_vars = if Var.is_artificial var then dead_vars else var :: dead_vars in
                  let pvars_to_nullify =
                    match Var.get_pvar var with
                    | Some pvar when not (AddressTaken.Domain.mem pvar address_taken_vars) ->
                        (* We nullify all address taken variables at the end of the procedure. This is
                           to avoid setting heap values to 0 that may be aliased somewhere else. *)
                        pvar :: pvars_to_nullify
                    | _ ->
                        pvars_to_nullify
                  in
                  (dead_vars, pvars_to_nullify) )
                to_nullify ([], [])
            in
            let loc = Procdesc.Node.get_last_loc node in
            Option.to_list (node_deadvars_instruction loc dead_vars)
            |> prepend_node_nullify_instructions loc pvars_to_nullify
            |> Procdesc.Node.append_instrs node
        | None ->
            () ) ;
    (* nullify all address taken variables at the end of the procedure *)
    if not (AddressTaken.Domain.is_empty address_taken_vars) then
      let exit_node = ProcCfg.Exceptional.exit_node nullify_proc_cfg in
      let exit_loc = Procdesc.Node.get_last_loc exit_node in
      prepend_node_nullify_instructions exit_loc
        (AddressTaken.Domain.elements address_taken_vars)
        []
      |> Procdesc.Node.append_instrs exit_node


  let process proc_desc =
    let liveness_inv_map = Liveness.compute proc_desc in
    add_nullify_instrs proc_desc liveness_inv_map
end

(** pre-analysis to cut control flow after calls to functions whose type indicates they do not
    return *)
module NoReturn = struct
  let has_noreturn_call tenv node =
    Procdesc.Node.get_instrs node
    |> Instrs.exists ~f:(fun (instr : Sil.instr) ->
           match instr with
           | Call (_, Const (Cfun proc_name), _, _, _) ->
               Attributes.is_no_return proc_name
               || NoReturnModels.dispatch tenv proc_name |> Option.value ~default:false
           | _ ->
               false )


  let process tenv proc_desc =
    let exit_node = Procdesc.get_exit_node proc_desc in
    Procdesc.iter_nodes
      (fun node ->
        if has_noreturn_call tenv node then
          Procdesc.set_succs node ~normal:(Some [exit_node]) ~exn:None )
      proc_desc
end

module InjectTraitSinit = struct
  let update_ident_generator pdesc =
    let idents =
      Procdesc.fold_instrs pdesc ~init:Ident.Set.empty ~f:(fun acc _ instr ->
          List.fold (Sil.exps_of_instr instr) ~init:acc ~f:(fun acc exp ->
              Sequence.fold (Exp.free_vars exp) ~init:acc ~f:(fun acc ident ->
                  Ident.Set.add ident acc ) ) )
    in
    Ident.update_name_generator (Ident.Set.elements idents)


  let inject_trait_sinit tenv pdesc =
    let traits =
      let pname = Procdesc.get_proc_name pdesc in
      Option.value_map (Procname.get_class_type_name pname) ~default:[] ~f:(fun name ->
          Tenv.get_hack_direct_used_traits tenv name )
    in
    if not (List.is_empty traits) then
      match Procdesc.get_pvar_formals pdesc with
      | (this_pvar, this_typ) :: _ ->
          update_ident_generator pdesc ;
          let entry_node = Procdesc.get_start_node pdesc in
          let instrs =
            let loc = Procdesc.Node.get_loc entry_node in
            let this_id = Ident.create_fresh Ident.knormal in
            let this_load = Sil.Load {id= this_id; e= Lvar this_pvar; typ= this_typ; loc} in
            let sinit_calls =
              let ret_typ = Procdesc.get_ret_type pdesc in
              let arg = [(Exp.Var this_id, this_typ)] in
              List.map traits ~f:(fun trait ->
                  let ret_id = Ident.create_none () in
                  let sinit = Procname.get_hack_static_init ~is_trait:true trait in
                  Sil.Call ((ret_id, ret_typ), Const (Cfun sinit), arg, loc, CallFlags.default) )
            in
            this_load :: sinit_calls
          in
          let succs = Procdesc.Node.get_succs entry_node in
          List.iter succs ~f:(fun succ -> Procdesc.Node.prepend_instrs succ instrs)
      | [] ->
          L.internal_error "Error loading the `$this` formal from sinit"


  let process tenv pdesc =
    NodePrinter.with_session (Procdesc.get_start_node pdesc) ~kind:`ComputePre
      ~pp_name:(fun fmt -> Format.pp_print_string fmt "Inject trait sinit")
      ~f:(fun () -> inject_trait_sinit tenv pdesc)
end

let do_preanalysis tenv pdesc =
  if not Config.preanalysis_html then NodePrinter.print_html := false ;
  let proc_name = Procdesc.get_proc_name pdesc in
  if Procname.is_java proc_name || Procname.is_csharp proc_name then
    InlineJavaSyntheticMethods.process pdesc ;
  (* NOTE: It is important that this preanalysis stays before Liveness *)
  if not (Procname.is_java proc_name || Procname.is_csharp proc_name) then
    (* Apply dynamic selection of copy and overriden methods *)
    ReplaceObjCMethodCall.process tenv pdesc proc_name ;
  if Procname.is_hack_sinit proc_name then InjectTraitSinit.process tenv pdesc ;
  Liveness.process pdesc ;
  AddAbstractionInstructions.process pdesc ;
  if Procname.is_java proc_name then Devirtualizer.process pdesc tenv ;
  NoReturn.process tenv pdesc ;
  NodePrinter.print_html := true ;
  ()
