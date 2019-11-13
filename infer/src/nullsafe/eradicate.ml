(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Eradicate NPEs. *)

module L = Logging
module F = Format
open Dataflow

(* ERADICATE CHECKER. TODOS:*)
(* 1) add support for constructors for anonymous inner classes (currently not checked) *)

(** Type for a module that provides a main callback function *)
module type CallBackT = sig
  val callback : TypeCheck.checks -> Callbacks.proc_callback_t
end

(* CallBackT *)

(** Extension to the type checker. *)
module type ExtensionT = sig
  val update_payloads : TypeState.t option -> Payloads.t -> Payloads.t
end

(** Create a module with the toplevel callback. *)
module MkCallback (Extension : ExtensionT) : CallBackT = struct
  let callback1 tenv find_canonical_duplicate calls_this checks idenv curr_pname curr_pdesc
      annotated_signature linereader proc_loc : bool * TypeState.t option =
    let add_formal typestate (param_signature : AnnotatedSignature.param_signature) =
      let pvar = Pvar.mk param_signature.mangled curr_pname in
      let formal_name = param_signature.mangled in
      let origin =
        if Mangled.is_this formal_name then
          (* `this` is technically an implicit method param, but from syntactic and semantic points of view it
             has very special meaning and nullability limitations (it can never be null).
          *)
          TypeOrigin.This
        else TypeOrigin.MethodParameter param_signature
      in
      let inferred_nullability = InferredNullability.create origin in
      TypeState.add pvar (param_signature.param_annotated_type.typ, inferred_nullability) typestate
    in
    let get_initial_typestate () =
      let typestate_empty = TypeState.empty in
      List.fold ~f:add_formal ~init:typestate_empty annotated_signature.AnnotatedSignature.params
    in
    (* Check the nullable flag computed for the return value and report inconsistencies. *)
    let check_return find_canonical_duplicate exit_node final_typestate annotated_signature loc :
        unit =
      let AnnotatedSignature.{ret_annotated_type} = annotated_signature.AnnotatedSignature.ret in
      let ret_pvar = Procdesc.get_ret_var curr_pdesc in
      let ret_range = TypeState.lookup_pvar ret_pvar final_typestate in
      let typ_found_opt =
        match ret_range with Some (typ_found, _) -> Some typ_found | None -> None
      in
      (* TODO(T54088319): model this in AnnotatedNullability *)
      let ret_implicitly_nullable =
        String.equal (PatternMatch.get_type_name ret_annotated_type.typ) "java.lang.Void"
      in
      State.set_node exit_node ;
      if checks.TypeCheck.check_ret_type <> [] then
        List.iter
          ~f:(fun f -> f curr_pname curr_pdesc ret_annotated_type.typ typ_found_opt loc)
          checks.TypeCheck.check_ret_type ;
      if checks.TypeCheck.eradicate then
        EradicateChecks.check_return_annotation tenv find_canonical_duplicate curr_pdesc ret_range
          annotated_signature ret_implicitly_nullable loc
    in
    let do_before_dataflow initial_typestate =
      if Config.eradicate_verbose then
        L.result "Initial Typestate@\n%a@." TypeState.pp initial_typestate
    in
    let do_after_dataflow find_canonical_duplicate final_typestate =
      let exit_node = Procdesc.get_exit_node curr_pdesc in
      check_return find_canonical_duplicate exit_node final_typestate annotated_signature proc_loc
    in
    let module DFTypeCheck = MakeDF (struct
      type t = TypeState.t

      let equal = TypeState.equal

      let join = TypeState.join

      let pp_name fmt = F.pp_print_string fmt "eradicate"

      let do_node tenv node typestate =
        NodePrinter.with_session ~pp_name node ~f:(fun () ->
            State.set_node node ;
            let typestates_succ, typestates_exn =
              TypeCheck.typecheck_node tenv calls_this checks idenv curr_pname curr_pdesc
                find_canonical_duplicate annotated_signature typestate node linereader
            in
            if Config.write_html then (
              let d_typestate ts = L.d_printfln "%a" TypeState.pp ts in
              L.d_strln "before:" ;
              d_typestate typestate ;
              L.d_strln "after:" ;
              List.iter ~f:d_typestate typestates_succ ) ;
            (typestates_succ, typestates_exn) )


      let proc_throws _ = DontKnow
    end) in
    let initial_typestate = get_initial_typestate () in
    do_before_dataflow initial_typestate ;
    let transitions = DFTypeCheck.run tenv curr_pdesc initial_typestate in
    match transitions (Procdesc.get_exit_node curr_pdesc) with
    | DFTypeCheck.Transition (final_typestate, _, _) ->
        do_after_dataflow find_canonical_duplicate final_typestate ;
        (!calls_this, Some final_typestate)
    | DFTypeCheck.Dead_state ->
        (!calls_this, None)


  let callback2 tenv curr_pname calls_this checks {Callbacks.summary; get_procs_in_file}
      annotated_signature linereader proc_loc : unit =
    let curr_pdesc = Summary.get_proc_desc summary in
    let idenv = Idenv.create curr_pdesc in
    let find_duplicate_nodes = State.mk_find_duplicate_nodes curr_pdesc in
    let find_canonical_duplicate node =
      let duplicate_nodes = find_duplicate_nodes node in
      try Procdesc.NodeSet.min_elt duplicate_nodes with Caml.Not_found -> node
    in
    let typecheck_proc do_checks pname pdesc proc_details_opt =
      let ann_sig, loc, idenv_pn =
        match proc_details_opt with
        | Some (ann_sig, loc, idenv_pn) ->
            (ann_sig, loc, idenv_pn)
        | None ->
            let ann_sig =
              Models.get_modelled_annotated_signature tenv (Procdesc.get_attributes pdesc)
            in
            let loc = Procdesc.get_loc pdesc in
            (ann_sig, loc, Idenv.create pdesc)
      in
      let checks', calls_this' =
        if do_checks then (checks, calls_this)
        else ({TypeCheck.eradicate= false; check_ret_type= []}, ref false)
      in
      callback1 tenv find_canonical_duplicate calls_this' checks' idenv_pn pname pdesc ann_sig
        linereader loc
    in
    let do_final_typestate typestate_opt calls_this =
      let do_typestate typestate =
        let start_node = Procdesc.get_start_node curr_pdesc in
        if
          (not calls_this)
          (* if 'this(...)' is called, no need to check initialization *)
          && checks.TypeCheck.eradicate
        then (
          let typestates_for_curr_constructor_and_all_initializer_methods =
            Initializers.final_initializer_typestates_lazy tenv curr_pname curr_pdesc
              get_procs_in_file typecheck_proc
          in
          let typestates_for_all_constructors_incl_current =
            Initializers.final_constructor_typestates_lazy tenv curr_pname get_procs_in_file
              typecheck_proc
          in
          EradicateChecks.check_constructor_initialization tenv find_canonical_duplicate curr_pname
            curr_pdesc start_node ~typestates_for_curr_constructor_and_all_initializer_methods
            ~typestates_for_all_constructors_incl_current proc_loc ;
          if Config.eradicate_verbose then L.result "Final Typestate@\n%a@." TypeState.pp typestate
          )
      in
      match typestate_opt with None -> () | Some typestate -> do_typestate typestate
    in
    TypeErr.reset () ;
    let calls_this, final_typestate_opt =
      typecheck_proc true curr_pname curr_pdesc (Some (annotated_signature, proc_loc, idenv))
    in
    do_final_typestate final_typestate_opt calls_this ;
    if checks.TypeCheck.eradicate then
      EradicateChecks.check_overridden_annotations find_canonical_duplicate tenv curr_pname
        curr_pdesc annotated_signature ;
    TypeErr.report_forall_checks_and_reset tenv (EradicateCheckers.report_error tenv) curr_pdesc ;
    ()


  (** Entry point for the eradicate-based checker infrastructure. *)
  let callback checks ({Callbacks.summary} as callback_args) : Summary.t =
    let proc_desc = Summary.get_proc_desc summary in
    let proc_name = Procdesc.get_proc_name proc_desc in
    let calls_this = ref false in
    let curr_pname = Summary.get_proc_name summary in
    let tenv = Exe_env.get_tenv callback_args.exe_env curr_pname in
    let filter_special_cases () =
      if
        ( match proc_name with
        | Typ.Procname.Java java_pname ->
            Typ.Procname.Java.is_access_method java_pname
            || Typ.Procname.Java.is_external java_pname
        | _ ->
            false )
        || (Procdesc.get_attributes proc_desc).ProcAttributes.is_bridge_method
      then None
      else
        let annotated_signature =
          Models.get_modelled_annotated_signature tenv (Procdesc.get_attributes proc_desc)
        in
        Some annotated_signature
    in
    ( match filter_special_cases () with
    | None ->
        ()
    | Some annotated_signature ->
        let loc = Procdesc.get_loc proc_desc in
        let linereader = Printer.LineReader.create () in
        if Config.eradicate_verbose then
          L.result "%a@." (AnnotatedSignature.pp proc_name) annotated_signature ;
        callback2 tenv curr_pname calls_this checks callback_args annotated_signature linereader loc
    ) ;
    summary
end

(* MkCallback *)

module EmptyExtension : ExtensionT = struct
  let update_payloads typestate_opt (payloads : Payloads.t) =
    {payloads with typestate= typestate_opt}
end

module Main = struct
  module Callback = MkCallback (EmptyExtension)

  let callback = Callback.callback
end

(** Eradicate checker for Java @Nullable annotations. *)
let callback_eradicate =
  let checks = {TypeCheck.eradicate= true; check_ret_type= []} in
  Main.callback checks


(** Call the given check_return_type at the end of every procedure. *)
let callback_check_return_type check_return_type callback_args =
  let checks = {TypeCheck.eradicate= false; check_ret_type= [check_return_type]} in
  Main.callback checks callback_args
