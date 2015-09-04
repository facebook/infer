(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

(** Eradicate NPEs. *)

module L = Logging
module F = Format
open Utils
open Dataflow

(* ERADICATE CHECKER. TODOS:*)
(* 1) add support for constructors for anonymous inner classes (currently not checked) *)

(* print initial and final typestates *)
let verbose = Config.from_env_variable "ERADICATE_TYPINGS"

(* print step-by-step tracing information *)
let trace = Config.from_env_variable "ERADICATE_TRACE"

let check_field_initialization = true (* check that nonnullable fields are initialized in constructors *)

type parameters = TypeState.parameters


(** Type for a module that provides a main callback function *)
module type CallBackT =
sig
  val callback :
    TypeCheck.checks -> Procname.t list -> TypeCheck.get_proc_desc ->
    Idenv.t -> Sil.tenv -> Procname.t ->
    Cfg.Procdesc.t -> unit
end (* CallBackT *)

(** Extension to the type checker. *)
module type ExtensionT = sig
  type extension
  val ext : extension TypeState.ext
  val mkpayload : extension TypeState.t option -> Specs.payload
end

(** Create a module with the toplevel callback. *)
module MkCallback
    (Extension : ExtensionT)
  : CallBackT =
struct
  (** Update the summary with stats from the checker. *)
  let update_summary proc_name proc_desc final_typestate_opt =
    let old_summ = Specs.get_summary_unsafe proc_name in
    let nodes = list_map (fun n -> Cfg.Node.get_id n) (Cfg.Procdesc.get_nodes proc_desc) in
    let method_annotation =
      (Specs.pdesc_resolve_attributes proc_desc).ProcAttributes.method_annotation in
    let new_summ =
      {
        old_summ with
        Specs.nodes = nodes;
        Specs.payload = Extension.mkpayload final_typestate_opt;
        Specs.attributes =
          {
            old_summ.Specs.attributes with
            ProcAttributes.loc = Cfg.Procdesc.get_loc proc_desc;
            method_annotation;
          };
      } in
    Specs.add_summary proc_name new_summ

  let callback1
      find_canonical_duplicate calls_this checks get_proc_desc idenv tenv curr_pname
      curr_pdesc annotated_signature linereader proc_loc : bool * 'a TypeState.t option =
    let mk_pvar s = Sil.mk_pvar (Mangled.from_string s) curr_pname in
    let add_formal typestate (s, ia, typ) =
      let pvar = mk_pvar s in
      let ta =
        let origin = TypeOrigin.Formal s in
        TypeAnnotation.from_item_annotation ia origin in
      TypeState.add_pvar pvar (typ, ta, []) typestate in
    let get_initial_typestate () =
      let typestate_empty = TypeState.empty Extension.ext in
      list_fold_left add_formal typestate_empty annotated_signature.Annotations.params in

    (** Check the nullable flag computed for the return value and report inconsistencies. *)
    let check_return find_canonical_duplicate exit_node final_typestate ret_ia ret_type loc : unit =
      let ret_pvar = Cfg.Procdesc.get_ret_var curr_pdesc in
      let ret_range = TypeState.lookup_pvar ret_pvar final_typestate in
      let typ_found_opt = match ret_range with
        | Some (typ_found, _, _) -> Some typ_found
        | None -> None in
      let ret_implicitly_nullable =
        string_equal (PatternMatch.get_type_name ret_type) "java.lang.Void" in
      State.set_node exit_node;

      if checks.TypeCheck.check_ret_type <> [] then
        list_iter
          (fun f -> f curr_pname curr_pdesc ret_type typ_found_opt loc)
          checks.TypeCheck.check_ret_type;
      if checks.TypeCheck.eradicate then
        EradicateChecks.check_return_annotation
          find_canonical_duplicate curr_pname curr_pdesc exit_node ret_range
          ret_ia ret_implicitly_nullable loc in

    let do_before_dataflow initial_typestate =
      if verbose then
        L.stdout "Initial Typestate@\n%a@."
          (TypeState.pp Extension.ext) initial_typestate in

    let do_after_dataflow find_canonical_duplicate final_typestate =
      let exit_node = Cfg.Procdesc.get_exit_node curr_pdesc in
      let ia, ret_type = annotated_signature.Annotations.ret in
      check_return find_canonical_duplicate exit_node final_typestate ia ret_type proc_loc in

    let module DFTypeCheck = MakeDF(struct
        type t = Extension.extension TypeState.t
        let initial = TypeState.empty Extension.ext
        let equal = TypeState.equal
        let join = TypeState.join Extension.ext
        let do_node node typestate =
          State.set_node node;
          let typestates_succ, typestates_exn =
            TypeCheck.typecheck_node
              Extension.ext calls_this checks idenv get_proc_desc curr_pname curr_pdesc
              find_canonical_duplicate annotated_signature typestate node linereader in
          if trace then
            list_iter (fun typestate_succ ->
                L.stdout
                  "Typestate After Node %a@\n%a@."
                  Cfg.Node.pp node
                  (TypeState.pp Extension.ext) typestate_succ)
              typestates_succ;
          typestates_succ, typestates_exn
        let proc_throws pn = DontKnow
      end) in
    let initial_typestate = get_initial_typestate () in
    do_before_dataflow initial_typestate;
    let transitions = DFTypeCheck.run curr_pdesc initial_typestate in
    match transitions (Cfg.Procdesc.get_exit_node curr_pdesc) with
    | DFTypeCheck.Transition (final_typestate, _, _) ->
        do_after_dataflow find_canonical_duplicate final_typestate;
        !calls_this, Some final_typestate
    | DFTypeCheck.Dead_state ->
        !calls_this, None

  let callback2
      calls_this checks all_procs get_proc_desc idenv tenv curr_pname
      curr_pdesc annotated_signature linereader proc_loc : unit =

    let find_duplicate_nodes = State.mk_find_duplicate_nodes curr_pdesc in
    let find_canonical_duplicate node =
      let duplicate_nodes = find_duplicate_nodes node in
      try Cfg.NodeSet.min_elt duplicate_nodes with
      | Not_found -> node in

    let typecheck_proc do_checks idenv_pn pname pdesc ann_sig loc =
      let checks', calls_this' =
        if do_checks then checks, calls_this
        else
          {
            TypeCheck.eradicate = false;
            check_extension = false;
            check_ret_type = [];
          }, ref false in
      callback1
        find_canonical_duplicate calls_this' checks' get_proc_desc idenv_pn
        tenv pname pdesc ann_sig linereader loc in

    let module Initializers = struct
      type init = Procname.t * Cfg.Procdesc.t

      let final_typestates initializers_current_class =
        (** Get the private methods, from the same class, directly called by the initializers. *)
        let get_private_called (initializers : init list) : init list =
          let res = ref [] in
          let do_proc (init_pn, init_pd) =
            let filter callee_pn callee_pd =
              let is_private =
                let attr = Specs.pdesc_resolve_attributes callee_pd in
                attr.ProcAttributes.access = Sil.Private in
              let same_class =
                let get_class_opt pn =
                  if Procname.is_java pn then Some (Procname.java_get_class pn)
                  else None in
                get_class_opt init_pn = get_class_opt callee_pn in
              is_private && same_class in
            let private_called = PatternMatch.proc_calls get_proc_desc init_pn init_pd filter in
            res := private_called @ !res in
          list_iter do_proc initializers;
          !res in

        (** Get the initializers recursively called by computing a fixpoint.
            Start from the initializers of the current class and the current procedure. *)
        let initializers_recursive : init list =
          let initializers_base_case = initializers_current_class in

          let res = ref [] in
          let seen = ref Procname.Set.empty in
          let mark_seen (initializers : init list) : unit =
            list_iter (fun (pn, _) -> seen := Procname.Set.add pn !seen) initializers;
            res := !res @ initializers in

          let rec fixpoint initializers_old =
            let initializers_new = get_private_called initializers_old in
            let initializers_new' =
              list_filter (fun (pn, _) -> not (Procname.Set.mem pn !seen)) initializers_new in
            mark_seen initializers_new';
            if initializers_new' <> [] then fixpoint initializers_new' in

          mark_seen initializers_base_case;
          fixpoint initializers_base_case;
          !res in

        (** Get the final typestates of all the initializers. *)
        let final_typestates = ref [] in
        let get_final_typestate (pname, pdesc) =
          let ann_sig =
            Models.get_modelled_annotated_signature (Cfg.Procdesc.get_attributes pdesc) in
          let loc = Cfg.Procdesc.get_loc pdesc in
          let idenv_pn = Idenv.create_from_idenv idenv pdesc in
          match typecheck_proc false idenv_pn pname pdesc ann_sig loc with
          | _, Some final_typestate ->
              final_typestates := (pname, final_typestate) :: !final_typestates
          | _, None -> () in
        list_iter get_final_typestate initializers_recursive;
        list_rev !final_typestates

      let pname_and_pdescs_with f =
        list_map
          (fun n -> match get_proc_desc n with
             | Some d -> [(n, d)]
             | None -> [])
          all_procs
        |> list_flatten
        |> list_filter f

      (** Typestates after the current procedure and all initializer procedures. *)
      let final_initializer_typestates_lazy = lazy
        begin
          let is_initializer pdesc pname =
            PatternMatch.method_is_initializer tenv (Cfg.Procdesc.get_attributes pdesc) ||
            let ia, _ =
              (Models.get_modelled_annotated_signature
                 (Cfg.Procdesc.get_attributes pdesc)).Annotations.ret in
            Annotations.ia_is_initializer ia in
          let initializers_current_class =
            pname_and_pdescs_with
              (fun (pname, pdesc) ->
                 is_initializer pdesc pname &&
                 Procname.java_get_class pname = Procname.java_get_class curr_pname) in
          final_typestates ((curr_pname, curr_pdesc):: initializers_current_class)
        end

      (** Typestates after all constructors. *)
      let final_constructor_typestates_lazy = lazy
        begin
          let constructors_current_class =
            pname_and_pdescs_with
              (fun (n, d) ->
                 Procname.is_constructor n &&
                 Procname.java_get_class n = Procname.java_get_class curr_pname) in
          final_typestates constructors_current_class
        end

    end (* Initializers *) in

    let do_final_typestate typestate_opt calls_this =
      let do_typestate typestate =
        let start_node = Cfg.Procdesc.get_start_node curr_pdesc in
        if not calls_this && (* if 'this(...)' is called, no need to check initialization *)
           check_field_initialization &&
           checks.TypeCheck.eradicate
        then begin
          EradicateChecks.check_constructor_initialization
            find_canonical_duplicate
            curr_pname
            curr_pdesc
            start_node
            typestate
            Initializers.final_initializer_typestates_lazy
            Initializers.final_constructor_typestates_lazy
            proc_loc
        end;
        if verbose then
          L.stdout "Final Typestate@\n%a@."
            (TypeState.pp Extension.ext) typestate in
      match typestate_opt with
      | None -> ()
      | Some typestate -> do_typestate typestate in

    TypeErr.reset ();

    let calls_this, final_typestate_opt =
      typecheck_proc true idenv curr_pname curr_pdesc annotated_signature proc_loc in
    do_final_typestate final_typestate_opt calls_this;
    if checks.TypeCheck.eradicate then
      EradicateChecks.check_overridden_annotations
        find_canonical_duplicate get_proc_desc
        tenv curr_pname curr_pdesc
        annotated_signature;

    TypeErr.report_forall_checks_and_reset Checkers.ST.report_error curr_pname;
    update_summary curr_pname curr_pdesc final_typestate_opt

  (** Entry point for the eradicate-based checker infrastructure. *)
  let callback checks all_procs get_proc_desc idenv tenv proc_name proc_desc =
    let calls_this = ref false in

    let filter_special_cases () =
      if Procname.java_is_access_method proc_name ||
         (Specs.pdesc_resolve_attributes proc_desc).ProcAttributes.is_bridge_method
      then None
      else
        begin
          let annotated_signature =
            Models.get_modelled_annotated_signature (Specs.pdesc_resolve_attributes proc_desc) in
          if (Specs.pdesc_resolve_attributes proc_desc).ProcAttributes.is_abstract then
            begin
              if Models.infer_library_return &&
                 EradicateChecks.classify_procedure (Cfg.Procdesc.get_attributes proc_desc) = "L"
              then
                (let ret_is_nullable = (* get the existing annotation *)
                   let ia, _ = annotated_signature.Annotations.ret in
                   Annotations.ia_is_nullable ia in
                 EradicateChecks.pp_inferred_return_annotation ret_is_nullable proc_name);
              Some annotated_signature
            end
          else
            Some annotated_signature
        end in
    match filter_special_cases () with
    | None -> ()
    | Some annotated_signature ->
        let loc = Cfg.Procdesc.get_loc proc_desc in
        let linereader = Printer.LineReader.create () in
        if verbose then
          L.stdout "%a@."
            (Annotations.pp_annotated_signature proc_name)
            annotated_signature;

        callback2
          calls_this checks all_procs get_proc_desc idenv tenv
          proc_name proc_desc annotated_signature linereader loc

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
    let check_instr get_proc_desc proc_name proc_desc node ext instr param = ext in
    let join () () = () in
    let pp fmt () = () in
    {
      TypeState.empty = empty;
      check_instr = check_instr;
      join = join;
      pp = pp;
    }
  let mkpayload typestate_opt = Specs.TypeState typestate_opt
end

module Main =
  Build(EmptyExtension)

(** Eradicate checker for Java @Nullable annotations. *)
let callback_eradicate all_procs get_proc_desc idenv tenv proc_name1 proc_desc1 =
  let checks =
    {
      TypeCheck.eradicate = true;
      check_extension = false;
      check_ret_type = [];
    } in
  Main.callback checks all_procs get_proc_desc idenv tenv proc_name1 proc_desc1

(** Call the given check_return_type at the end of every procedure. *)
let callback_check_return_type check_return_type all_procs
    get_proc_desc idenv tenv proc_name1 proc_desc1 =
  let checks =
    {
      TypeCheck.eradicate = false;
      check_extension = false;
      check_ret_type = [check_return_type];
    } in
  Main.callback checks all_procs get_proc_desc idenv tenv proc_name1 proc_desc1

(** Export the ability to add nullable annotations. *)
let field_add_nullable_annotation fieldname =
  Models.Inference.field_add_nullable_annotation fieldname
