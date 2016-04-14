(*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils

open Javalib_pack
open Sawja_pack

module L = Logging


let add_edges context start_node exn_node exit_nodes method_body_nodes impl super_call =
  let cfg = (JContext.get_icfg context).cfg in
  let pc_nb = Array.length method_body_nodes in
  let last_pc = pc_nb - 1 in
  let is_last pc = (pc = last_pc) in
  let rec get_body_nodes pc =
    let current_nodes = method_body_nodes.(pc) in
    match current_nodes with
    | JTrans.Skip when (is_last pc) && not (JContext.is_goto_jump context pc) ->
        exit_nodes
    | JTrans.Skip -> direct_successors pc
    | JTrans.Instr node -> [node]
    | JTrans.Prune (node_true, node_false) -> [node_true; node_false]
    | JTrans.Loop (join_node, _, _) -> [join_node]
  and direct_successors pc =
    if is_last pc && not (JContext.is_goto_jump context pc) then
      exit_nodes
    else
      match JContext.get_goto_jump context pc with
      | JContext.Next -> get_body_nodes (pc + 1)
      | JContext.Jump goto_pc ->
          if pc = goto_pc then [] (* loop in goto *)
          else get_body_nodes goto_pc
      | JContext.Exit -> exit_nodes in
  let get_succ_nodes node pc =
    match JContext.get_if_jump context node with
    | None -> direct_successors pc
    | Some jump_pc -> get_body_nodes jump_pc in
  let get_exn_nodes =
    if super_call then (fun _ -> exit_nodes)
    else JTransExn.create_exception_handlers context [exn_node] get_body_nodes impl in
  let connect node pc =
    Cfg.Node.set_succs_exn cfg node (get_succ_nodes node pc) (get_exn_nodes pc) in
  let connect_nodes pc translated_instruction =
    match translated_instruction with
    | JTrans.Skip -> ()
    | JTrans.Instr node -> connect node pc
    | JTrans.Prune (node_true, node_false) ->
        connect node_true pc;
        connect node_false pc
    | JTrans.Loop (join_node, node_true, node_false) ->
        Cfg.Node.set_succs_exn cfg join_node [node_true; node_false] [];
        connect node_true pc;
        connect node_false pc in
  let first_nodes =
    (* deals with the case of an empty array *)
    direct_successors (-1) in

  (* the exceptions edges here are going directly to the exit node *)
  Cfg.Node.set_succs_exn cfg start_node first_nodes exit_nodes;

  if not super_call then
    (* the exceptions node is just before the exit node *)
    Cfg.Node.set_succs_exn cfg exn_node exit_nodes exit_nodes;
  Array.iteri connect_nodes method_body_nodes

(** Add a concrete method. *)
let add_cmethod never_null_matcher program icfg node cm is_static =
  let cfg = icfg.JContext.cfg in
  let tenv = icfg.JContext.tenv in
  let cn, ms = JBasics.cms_split cm.Javalib.cm_class_method_signature in
  let is_clinit = JBasics.ms_equal ms JBasics.clinit_signature in
  if !JTrans.no_static_final = false
  && is_clinit
  && not (JTransStaticField.has_static_final_fields node) then
    JUtils.log "\t\tskipping class initializer: %s@." (JBasics.ms_name ms)
  else
    match JTrans.get_method_procdesc program cfg tenv cn ms is_static with
    | JTrans.Defined procdesc when JClasspath.is_model (Cfg.Procdesc.get_proc_name procdesc) ->
        (* do not capture the method if there is a model for it *)
        JUtils.log "Skipping method with a model: %s@." (Procname.to_string (Cfg.Procdesc.get_proc_name procdesc));
    | JTrans.Defined procdesc ->
        let start_node = Cfg.Procdesc.get_start_node procdesc in
        let exit_node = Cfg.Procdesc.get_exit_node procdesc in
        let exn_node =
          match JContext.get_exn_node procdesc with
          | Some node -> node
          | None -> assert false in
        let impl = JTrans.get_implementation cm in
        let instrs, meth_kind =
          if is_clinit && not !JTrans.no_static_final then
            let instrs = JTransStaticField.static_field_init node cn (JBir.code impl) in
            (instrs, JContext.Init)
          else (JBir.code impl), JContext.Normal in
        let context =
          JContext.create_context
            never_null_matcher icfg procdesc impl cn meth_kind node program in
        let method_body_nodes = Array.mapi (JTrans.instruction context) instrs in
        let procname = Cfg.Procdesc.get_proc_name procdesc in
        add_edges context start_node exn_node [exit_node] method_body_nodes impl false;
        Cg.add_defined_node icfg.JContext.cg procname;
        if Procname.is_constructor procname then Cfg.set_procname_priority cfg procname
    | JTrans.Called _ -> ()


(** Add an abstract method. *)
let add_amethod program icfg am is_static =
  let cfg = icfg.JContext.cfg in
  let tenv = icfg.JContext.tenv in
  let cn, ms = JBasics.cms_split am.Javalib.am_class_method_signature in
  match JTrans.get_method_procdesc program cfg tenv cn ms is_static with
  | JTrans.Defined procdesc when (JClasspath.is_model (Cfg.Procdesc.get_proc_name procdesc)) ->
      (* do not capture the method if there is a model for it *)
      JUtils.log "Skipping method with a model: %s@." (Procname.to_string (Cfg.Procdesc.get_proc_name procdesc));
  | JTrans.Defined procdesc ->
      Cg.add_defined_node icfg.JContext.cg (Cfg.Procdesc.get_proc_name procdesc)
  | JTrans.Called _ -> ()


let path_of_cached_classname cn =
  let root_path = Filename.concat !Config.results_dir "classnames" in
  let package_path = IList.fold_left Filename.concat root_path (JBasics.cn_package cn) in
  Filename.concat package_path ((JBasics.cn_simple_name cn)^".java")


let cache_classname cn =
  let path = path_of_cached_classname cn in
  let splitted_root_dir =
    let rec split l p =
      match p with
      | p when p = Filename.current_dir_name -> l
      | p when p = Filename.dir_sep -> l
      | p -> split ((Filename.basename p):: l) (Filename.dirname p) in
    split [] (Filename.dirname path) in
  let rec mkdir l p =
    let () =
      if not (Sys.file_exists p) then
        Unix.mkdir p 493 in
    match l with
    | [] -> ()
    | d:: tl -> mkdir tl (Filename.concat p d) in
  mkdir splitted_root_dir Filename.dir_sep;
  let file_out = open_out(path) in
  output_string file_out (string_of_float (Unix.time ()));
  close_out file_out

let is_classname_cached cn =
  Sys.file_exists (path_of_cached_classname cn)

(* Given a source file and a class, translates the code of this class.
   In init - mode, finds out whether this class contains initializers at all,
   in this case translates it. In standard mode, all methods are translated *)
let create_icfg never_null_matcher linereader program icfg cn node =
  JUtils.log "\tclassname: %s@." (JBasics.cn_name cn);
  cache_classname cn;
  let cfg = icfg.JContext.cfg in
  let tenv = icfg.JContext.tenv in
  begin
    Javalib.m_iter (JTrans.create_local_procdesc program linereader cfg tenv node) node;
    Javalib.m_iter (fun m ->
        (* each procedure has different scope: start names from id 0 *)
        Ident.NameGenerator.reset ();
        let method_kind = JTransType.get_method_kind m in
        match m with
        | Javalib.ConcreteMethod cm ->
            add_cmethod never_null_matcher program icfg node cm method_kind
        | Javalib.AbstractMethod am ->
            add_amethod program icfg am method_kind
      ) node
  end

(*
This type definition is for a future improvement of the capture where in one pass, the frontend will
translate things differently whether a source file is found for a given class
type capture_status =
  | With_source of string
  | Library of string
  | Unknown
*)

(* returns true for the set of classes that are selected to be translated *)
let should_capture classes package_opt source_basename node =
  let classname = Javalib.get_name node in
  let match_package pkg cn =
    match JTransType.package_to_string (JBasics.cn_package cn) with
    | None -> pkg = ""
    | Some found_pkg -> found_pkg = pkg in
  if JBasics.ClassSet.mem classname classes then
    begin
      match Javalib.get_sourcefile node with
      | None -> false
      | Some found_basename ->
          begin
            match package_opt with
            | None -> found_basename = source_basename
            | Some pkg ->
                match_package pkg classname
                && found_basename = source_basename
          end
    end
  else false


(* Computes the control - flow graph and call - graph of a given source file.
   In the standard - mode, it translated all the classes that correspond to this
   source file. *)
let compute_source_icfg
    never_null_matcher linereader classes program tenv
    source_basename package_opt =
  let icfg =
    { JContext.cg = Cg.create ();
      JContext.cfg = Cfg.Node.create_cfg ();
      JContext.tenv = tenv } in
  let select test procedure cn node =
    if test node then
      try
        procedure cn node
      with
      | Bir.Subroutine -> ()
      | e -> raise e in
  let () =
    JBasics.ClassMap.iter
      (select
         (should_capture classes package_opt source_basename)
         (create_icfg never_null_matcher linereader program icfg))
      (JClasspath.get_classmap program) in
  (icfg.JContext.cg, icfg.JContext.cfg)

let compute_class_icfg never_null_matcher linereader program tenv node =
  let icfg =
    { JContext.cg = Cg.create ();
      JContext.cfg = Cfg.Node.create_cfg ();
      JContext.tenv = tenv } in
  begin
    try
      create_icfg
        never_null_matcher linereader program icfg (Javalib.get_name node) node
    with
    | Bir.Subroutine -> ()
    | e -> raise e
  end;
  (icfg.JContext.cg, icfg.JContext.cfg)
