(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type init = Typ.Procname.t * Procdesc.t

let equal_class_opt = [%compare.equal: string option]

let final_typestates initializers_current_class tenv typecheck_proc =
  (* Get the private methods, from the same class, directly called by the initializers. *)
  let get_private_called (initializers : init list) : init list =
    let res = ref [] in
    let do_proc (init_pn, init_pd) =
      let filter callee_pn callee_attributes =
        let is_private =
          PredSymb.equal_access callee_attributes.ProcAttributes.access PredSymb.Private
        in
        let same_class =
          let get_class_opt pn =
            match pn with
            | Typ.Procname.Java pn_java ->
                Some (Typ.Procname.Java.get_class_name pn_java)
            | _ ->
                None
          in
          equal_class_opt (get_class_opt init_pn) (get_class_opt callee_pn)
        in
        is_private && same_class
      in
      let private_called =
        PatternMatch.proc_calls (PatternMatch.lookup_attributes tenv) init_pd filter
      in
      let do_called (callee_pn, _) =
        match Ondemand.get_proc_desc callee_pn with
        | Some callee_pd ->
            res := (callee_pn, callee_pd) :: !res
        | None ->
            ()
      in
      List.iter ~f:do_called private_called
    in
    List.iter ~f:do_proc initializers ; !res
  in
  (* Get the initializers recursively called by computing a fixpoint.
     Start from the initializers of the current class and the current procedure. *)
  let initializers_recursive : init list =
    let initializers_base_case = initializers_current_class in
    let res = ref [] in
    let seen = ref Typ.Procname.Set.empty in
    let mark_seen (initializers : init list) : unit =
      List.iter ~f:(fun (pn, _) -> seen := Typ.Procname.Set.add pn !seen) initializers ;
      res := !res @ initializers
    in
    let rec fixpoint initializers_old =
      let initializers_new = get_private_called initializers_old in
      let initializers_new' =
        List.filter ~f:(fun (pn, _) -> not (Typ.Procname.Set.mem pn !seen)) initializers_new
      in
      mark_seen initializers_new' ;
      if initializers_new' <> [] then fixpoint initializers_new'
    in
    mark_seen initializers_base_case ; fixpoint initializers_base_case ; !res
  in
  (* Get the final typestates of all the initializers. *)
  let final_typestates = ref [] in
  let get_final_typestate (pname, pdesc) =
    match typecheck_proc false pname pdesc None with
    | _, Some final_typestate ->
        final_typestates := (pname, final_typestate) :: !final_typestates
    | _, None ->
        ()
  in
  List.iter ~f:get_final_typestate initializers_recursive ;
  List.rev !final_typestates


let pname_and_pdescs_with tenv curr_pname get_procs_in_file f =
  let res = ref [] in
  let filter pname =
    match PatternMatch.lookup_attributes tenv pname with
    | Some proc_attributes ->
        f (pname, proc_attributes)
    | None ->
        false
  in
  let do_proc pname =
    if filter pname then
      match Ondemand.get_proc_desc pname with
      | Some pdesc ->
          res := (pname, pdesc) :: !res
      | None ->
          ()
  in
  List.iter ~f:do_proc (get_procs_in_file curr_pname) ;
  List.rev !res


let get_class pn =
  match pn with
  | Typ.Procname.Java pn_java ->
      Some (Typ.Procname.Java.get_class_name pn_java)
  | _ ->
      None


(** Typestates after the current procedure and all initializer procedures. *)
let final_initializer_typestates_lazy tenv curr_pname curr_pdesc get_procs_in_file typecheck_proc =
  lazy
    (let is_initializer proc_attributes =
       PatternMatch.method_is_initializer tenv proc_attributes
       ||
       let ia =
         (Models.get_modelled_annotated_signature tenv proc_attributes).AnnotatedSignature.ret
           .ret_annotation_deprecated
       in
       Annotations.ia_is_initializer ia
     in
     let initializers_current_class =
       pname_and_pdescs_with tenv curr_pname get_procs_in_file (function pname, proc_attributes ->
           is_initializer proc_attributes
           && equal_class_opt (get_class pname) (get_class curr_pname) )
     in
     final_typestates ((curr_pname, curr_pdesc) :: initializers_current_class) tenv typecheck_proc
    )


(** Typestates after all constructors. *)
let final_constructor_typestates_lazy tenv curr_pname get_procs_in_file typecheck_proc =
  lazy
    (let constructors_current_class =
       pname_and_pdescs_with tenv curr_pname get_procs_in_file (fun (pname, _) ->
           Typ.Procname.is_constructor pname
           && equal_class_opt (get_class pname) (get_class curr_pname) )
     in
     final_typestates constructors_current_class tenv typecheck_proc )
