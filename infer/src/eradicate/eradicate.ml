(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

(** Eradicate NPEs. *)

module L = Logging
module F = Format
open Dataflow

(* ERADICATE CHECKER. TODOS:*)
(* 1) add support for constructors for anonymous inner classes (currently not checked) *)

(* check that nonnullable fields are initialized in constructors *)
let check_field_initialization = true

type parameters = TypeState.parameters


(** Type for a module that provides a main callback function *)
module type CallBackT =
sig
  val callback : TypeCheck.checks -> Callbacks.proc_callback_args -> unit
end (* CallBackT *)

(** Extension to the type checker. *)
module type ExtensionT = sig
  type extension
  val ext : extension TypeState.ext
  val update_payload : extension TypeState.t option -> Specs.payload -> Specs.payload
end

(** Create a module with the toplevel callback. *)
module MkCallback
    (Extension : ExtensionT)
  : CallBackT =
struct
  (** Update the summary with stats from the checker. *)
  let update_summary proc_name proc_desc final_typestate_opt =
    match Specs.get_summary proc_name with
    | Some old_summ ->
        let nodes = IList.map (fun n -> Procdesc.Node.get_id n) (Procdesc.get_nodes proc_desc) in
        let method_annotation =
          (Specs.pdesc_resolve_attributes proc_desc).ProcAttributes.method_annotation in
        let new_summ =
          {
            old_summ with
            Specs.nodes = nodes;
            Specs.payload =
              Extension.update_payload final_typestate_opt old_summ.Specs.payload;
            Specs.attributes =
              {
                old_summ.Specs.attributes with
                ProcAttributes.loc = Procdesc.get_loc proc_desc;
                method_annotation;
              };
          } in
        Specs.add_summary proc_name new_summ
    | None -> ()

  let callback1
      tenv find_canonical_duplicate calls_this checks get_proc_desc idenv curr_pname
      curr_pdesc annotated_signature linereader proc_loc
    : bool * Extension.extension TypeState.t option =
    let mk s = Pvar.mk s curr_pname in
    let add_formal typestate (s, ia, typ) =
      let pvar = mk s in
      let ta =
        let origin = TypeOrigin.Formal s in
        TypeAnnotation.from_item_annotation ia origin in
      TypeState.add pvar (typ, ta, []) typestate in
    let get_initial_typestate () =
      let typestate_empty = TypeState.empty Extension.ext in
      List.fold
        ~f:add_formal
        ~init:typestate_empty
        annotated_signature.AnnotatedSignature.params in

    (* Check the nullable flag computed for the return value and report inconsistencies. *)
    let check_return find_canonical_duplicate exit_node final_typestate ret_ia ret_type loc : unit =
      let ret_pvar = Procdesc.get_ret_var curr_pdesc in
      let ret_range = TypeState.lookup_pvar ret_pvar final_typestate in
      let typ_found_opt = match ret_range with
        | Some (typ_found, _, _) -> Some typ_found
        | None -> None in
      let ret_implicitly_nullable =
        String.equal (PatternMatch.get_type_name ret_type) "java.lang.Void" in
      State.set_node exit_node;

      if checks.TypeCheck.check_ret_type <> [] then
        IList.iter
          (fun f -> f curr_pname curr_pdesc ret_type typ_found_opt loc)
          checks.TypeCheck.check_ret_type;
      if checks.TypeCheck.eradicate then
        EradicateChecks.check_return_annotation tenv
          find_canonical_duplicate curr_pdesc ret_range
          ret_ia ret_implicitly_nullable loc in

    let do_before_dataflow initial_typestate =
      if Config.eradicate_verbose then
        L.stdout "Initial Typestate@\n%a@."
          (TypeState.pp Extension.ext) initial_typestate in

    let do_after_dataflow find_canonical_duplicate final_typestate =
      let exit_node = Procdesc.get_exit_node curr_pdesc in
      let ia, ret_type = annotated_signature.AnnotatedSignature.ret in
      check_return find_canonical_duplicate exit_node final_typestate ia ret_type proc_loc in

    let module DFTypeCheck = MakeDF(struct
        type t = Extension.extension TypeState.t
        let equal = TypeState.equal
        let join = TypeState.join Extension.ext
        let do_node tenv node typestate =
          NodePrinter.start_session node;
          State.set_node node;
          let typestates_succ, typestates_exn =
            TypeCheck.typecheck_node
              tenv Extension.ext calls_this checks idenv get_proc_desc curr_pname curr_pdesc
              find_canonical_duplicate annotated_signature typestate node linereader in

          if Config.write_html then
            begin
              let d_typestate ts =
                L.d_strln (F.asprintf "%a" (TypeState.pp Extension.ext) ts) in
              L.d_strln "before:";
              d_typestate typestate;
              L.d_strln "after:";
              IList.iter d_typestate typestates_succ
            end;

          NodePrinter.finish_session node;
          typestates_succ, typestates_exn
        let proc_throws _ = DontKnow
      end) in
    let initial_typestate = get_initial_typestate () in
    do_before_dataflow initial_typestate;
    let transitions = DFTypeCheck.run tenv curr_pdesc initial_typestate in
    match transitions (Procdesc.get_exit_node curr_pdesc) with
    | DFTypeCheck.Transition (final_typestate, _, _) ->
        do_after_dataflow find_canonical_duplicate final_typestate;
        !calls_this, Some final_typestate
    | DFTypeCheck.Dead_state ->
        !calls_this, None

  let callback2
      calls_this checks
      {
        Callbacks.proc_desc = curr_pdesc;
        proc_name = curr_pname;
        get_proc_desc;
        idenv;
        tenv;
        get_procs_in_file;
      }
      annotated_signature linereader proc_loc : unit =

    let find_duplicate_nodes = State.mk_find_duplicate_nodes curr_pdesc in
    let find_canonical_duplicate node =
      let duplicate_nodes = find_duplicate_nodes node in
      try Procdesc.NodeSet.min_elt duplicate_nodes with
      | Not_found -> node in

    let typecheck_proc do_checks pname pdesc proc_details_opt =
      let ann_sig, loc, idenv_pn = match proc_details_opt with
        | Some (ann_sig, loc, idenv_pn) ->
            (ann_sig, loc, idenv_pn)
        | None ->
            let ann_sig =
              Models.get_modelled_annotated_signature (Procdesc.get_attributes pdesc) in
            let loc = Procdesc.get_loc pdesc in
            let idenv_pn = Idenv.create_from_idenv idenv pdesc in
            (ann_sig, loc, idenv_pn) in
      let checks', calls_this' =
        if do_checks then checks, calls_this
        else
          {
            TypeCheck.eradicate = false;
            check_extension = false;
            check_ret_type = [];
          }, ref false in
      callback1
        tenv find_canonical_duplicate calls_this' checks' get_proc_desc idenv_pn
        pname pdesc ann_sig linereader loc in

    let module Initializers = struct
      type init = Procname.t * Procdesc.t

      let equal_class_opt = [%compare.equal : string option]

      let final_typestates initializers_current_class =
        (* Get the private methods, from the same class, directly called by the initializers. *)
        let get_private_called (initializers : init list) : init list =
          let res = ref [] in
          let do_proc (init_pn, init_pd) =
            let filter callee_pn callee_attributes =
              let is_private =
                PredSymb.equal_access callee_attributes.ProcAttributes.access PredSymb.Private in
              let same_class =
                let get_class_opt pn = match pn with
                  | Procname.Java pn_java ->
                      Some (Procname.java_get_class_name pn_java)
                  | _ ->
                      None in
                equal_class_opt (get_class_opt init_pn) (get_class_opt callee_pn) in
              is_private && same_class in
            let private_called = PatternMatch.proc_calls
                Specs.proc_resolve_attributes init_pd filter in
            let do_called (callee_pn, _) =
              match get_proc_desc callee_pn with
              | Some callee_pd ->
                  res := (callee_pn, callee_pd) :: !res
              | None -> () in
            IList.iter do_called private_called in
          IList.iter do_proc initializers;
          !res in

        (* Get the initializers recursively called by computing a fixpoint.
           Start from the initializers of the current class and the current procedure. *)
        let initializers_recursive : init list =
          let initializers_base_case = initializers_current_class in

          let res = ref [] in
          let seen = ref Procname.Set.empty in
          let mark_seen (initializers : init list) : unit =
            IList.iter (fun (pn, _) -> seen := Procname.Set.add pn !seen) initializers;
            res := !res @ initializers in

          let rec fixpoint initializers_old =
            let initializers_new = get_private_called initializers_old in
            let initializers_new' =
              List.filter ~f:(fun (pn, _) -> not (Procname.Set.mem pn !seen)) initializers_new in
            mark_seen initializers_new';
            if initializers_new' <> [] then fixpoint initializers_new' in

          mark_seen initializers_base_case;
          fixpoint initializers_base_case;
          !res in

        (* Get the final typestates of all the initializers. *)
        let final_typestates = ref [] in
        let get_final_typestate (pname, pdesc) =
          match typecheck_proc false pname pdesc None with
          | _, Some final_typestate ->
              final_typestates := (pname, final_typestate) :: !final_typestates
          | _, None -> () in
        IList.iter get_final_typestate initializers_recursive;
        IList.rev !final_typestates

      let pname_and_pdescs_with f =
        let res = ref [] in
        let filter pname = match Specs.proc_resolve_attributes pname with
          | Some proc_attributes -> f (pname, proc_attributes)
          | None -> false in
        let do_proc pname =
          if filter pname then
            match get_proc_desc pname with
            | Some pdesc ->
                res := (pname, pdesc) :: !res
            | None -> () in
        IList.iter do_proc (get_procs_in_file curr_pname);
        IList.rev !res

      let get_class pn = match pn with
        | Procname.Java pn_java ->
            Some (Procname.java_get_class_name pn_java)
        | _ ->
            None

      (** Typestates after the current procedure and all initializer procedures. *)
      let final_initializer_typestates_lazy = lazy
        begin
          let is_initializer proc_attributes =
            PatternMatch.method_is_initializer tenv proc_attributes ||
            let ia, _ =
              (Models.get_modelled_annotated_signature proc_attributes).AnnotatedSignature.ret in
            Annotations.ia_is_initializer ia in
          let initializers_current_class =
            pname_and_pdescs_with
              (function (pname, proc_attributes) ->
                 is_initializer proc_attributes &&
                 equal_class_opt (get_class pname) (get_class curr_pname)) in
          final_typestates
            ((curr_pname, curr_pdesc) :: initializers_current_class)
        end

      (** Typestates after all constructors. *)
      let final_constructor_typestates_lazy = lazy
        begin
          let constructors_current_class =
            pname_and_pdescs_with
              (fun (pname, _) ->
                 Procname.is_constructor pname &&
                 equal_class_opt (get_class pname) (get_class curr_pname)) in
          final_typestates constructors_current_class
        end

    end (* Initializers *) in

    let do_final_typestate typestate_opt calls_this =
      let do_typestate typestate =
        let start_node = Procdesc.get_start_node curr_pdesc in
        if not calls_this && (* if 'this(...)' is called, no need to check initialization *)
           check_field_initialization &&
           checks.TypeCheck.eradicate
        then begin
          EradicateChecks.check_constructor_initialization tenv
            find_canonical_duplicate
            curr_pname
            curr_pdesc
            start_node
            Initializers.final_initializer_typestates_lazy
            Initializers.final_constructor_typestates_lazy
            proc_loc
        end;
        if Config.eradicate_verbose then
          L.stdout "Final Typestate@\n%a@."
            (TypeState.pp Extension.ext) typestate in
      match typestate_opt with
      | None -> ()
      | Some typestate -> do_typestate typestate in

    TypeErr.reset ();

    let calls_this, final_typestate_opt =
      typecheck_proc true curr_pname curr_pdesc (Some (annotated_signature, proc_loc, idenv)) in
    do_final_typestate final_typestate_opt calls_this;
    if checks.TypeCheck.eradicate then
      EradicateChecks.check_overridden_annotations
        find_canonical_duplicate
        tenv curr_pname curr_pdesc
        annotated_signature;

    TypeErr.report_forall_checks_and_reset tenv (Checkers.ST.report_error tenv) curr_pdesc;
    update_summary curr_pname curr_pdesc final_typestate_opt

  (** Entry point for the eradicate-based checker infrastructure. *)
  let callback checks ({ Callbacks.proc_desc; proc_name } as callback_args) =
    let calls_this = ref false in

    let filter_special_cases () =
      if Procname.java_is_access_method proc_name ||
         (Specs.pdesc_resolve_attributes proc_desc).ProcAttributes.is_bridge_method
      then None
      else
        begin
          let annotated_signature =
            Models.get_modelled_annotated_signature (Specs.pdesc_resolve_attributes proc_desc) in
          Some annotated_signature
        end in
    match filter_special_cases () with
    | None -> ()
    | Some annotated_signature ->
        let loc = Procdesc.get_loc proc_desc in
        let linereader = Printer.LineReader.create () in
        if Config.eradicate_verbose then
          L.stdout "%a@." (AnnotatedSignature.pp proc_name) annotated_signature;

        callback2
          calls_this checks callback_args annotated_signature linereader loc

end (* MkCallback *)

(** Given an extension to the typestate with a check, call the check on each instruction. *)
module Build
    (Extension : ExtensionT)
  : CallBackT =
struct
  module Callback = MkCallback(Extension)
  let callback = Callback.callback
end (* Build *)

module EmptyExtension : ExtensionT =
struct
  type extension = unit
  let ext =
    let empty = () in
    let check_instr _ _ _ _ ext _ _ = ext in
    let join () () = () in
    let pp _ () = () in
    {
      TypeState.empty = empty;
      check_instr = check_instr;
      join = join;
      pp = pp;
    }
  let update_payload typestate_opt payload =
    { payload with
      Specs.typestate = typestate_opt; }
end

module Main =
  Build(EmptyExtension)

(** Eradicate checker for Java @Nullable annotations. *)
let callback_eradicate
    ({ Callbacks.get_proc_desc; idenv; proc_name } as callback_args) =
  let checks =
    {
      TypeCheck.eradicate = true;
      check_extension = false;
      check_ret_type = [];
    } in
  let callbacks =
    let analyze_ondemand _ pdesc =
      let idenv_pname = Idenv.create_from_idenv idenv pdesc in
      Main.callback checks
        { callback_args with
          Callbacks.idenv = idenv_pname;
          proc_name = (Procdesc.get_proc_name pdesc);
          proc_desc = pdesc; } in
    {
      Ondemand.analyze_ondemand;
      get_proc_desc;
    } in

  if Ondemand.procedure_should_be_analyzed proc_name
  then
    begin
      Ondemand.set_callbacks callbacks;
      Main.callback checks callback_args;
      Ondemand.unset_callbacks ()
    end

(** Call the given check_return_type at the end of every procedure. *)
let callback_check_return_type check_return_type callback_args =
  let checks =
    {
      TypeCheck.eradicate = false;
      check_extension = false;
      check_ret_type = [check_return_type];
    } in
  Main.callback checks callback_args
