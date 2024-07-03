(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open Javalib_pack
open Sawja_pack
module L = Logging

let add_edges (context : JContext.t) start_node exn_node exit_nodes method_body_nodes impl
    super_call =
  let pc_nb = Array.length method_body_nodes in
  let last_pc = pc_nb - 1 in
  let is_last pc = Int.equal pc last_pc in
  let is_a_throw pc =
    let code = JBir.code impl in
    if pc < 0 || pc >= Array.length code then false
    else match code.(pc) with JBir.Throw _ -> true | _ -> false
  in
  let rec get_body_nodes_ pc visited =
    let current_nodes = method_body_nodes.(pc) in
    match current_nodes with
    | JTrans.Skip when is_last pc && not (JContext.is_goto_jump context pc) ->
        exit_nodes
    | JTrans.Skip ->
        direct_successors pc (Int.Set.add visited pc)
    | JTrans.Instr node ->
        [node]
    | JTrans.Prune (node_true, node_false) ->
        [node_true; node_false]
    | JTrans.Loop (join_node, _, _) ->
        [join_node]
  and direct_successors pc visited =
    if is_last pc && not (JContext.is_goto_jump context pc) then exit_nodes
    else
      match JContext.get_goto_jump context pc with
      | JContext.Next ->
          let next_pc = pc + 1 in
          if Int.Set.mem visited next_pc || is_a_throw pc then []
          else get_body_nodes_ next_pc visited
      | JContext.Jump goto_pc when Int.Set.mem visited goto_pc ->
          [] (* loop in goto *)
      | JContext.Jump goto_pc ->
          get_body_nodes_ goto_pc visited
      | JContext.Exit ->
          exit_nodes
  in
  let get_body_nodes pc = get_body_nodes_ pc Int.Set.empty in
  let get_succ_nodes node pc =
    match JContext.get_if_jump context node with
    | None ->
        direct_successors pc Int.Set.empty
    | Some jump_pc ->
        get_body_nodes jump_pc
  in
  let get_exn_nodes =
    if super_call then fun _ -> exit_nodes
    else JTransExn.create_exception_handlers context [exn_node] get_body_nodes impl
  in
  let connect node pc =
    Procdesc.node_set_succs context.procdesc node ~normal:(get_succ_nodes node pc)
      ~exn:(get_exn_nodes pc)
  in
  let connect_nodes pc translated_instruction =
    match translated_instruction with
    | JTrans.Skip ->
        ()
    | JTrans.Instr node ->
        connect node pc
    | JTrans.Prune (node_true, node_false) ->
        connect node_true pc ;
        connect node_false pc
    | JTrans.Loop (join_node, node_true, node_false) ->
        Procdesc.node_set_succs context.procdesc join_node ~normal:[node_true; node_false] ~exn:[] ;
        connect node_true pc ;
        connect node_false pc
  in
  let first_nodes =
    (* deals with the case of an empty array *)
    direct_successors (-1) Int.Set.empty
  in
  (* the exceptions edges here are going directly to the exit node *)
  Procdesc.node_set_succs context.procdesc start_node ~normal:first_nodes ~exn:exit_nodes ;
  if not super_call then
    (* the exceptions node is just before the exit node *)
    Procdesc.node_set_succs context.procdesc exn_node ~normal:exit_nodes ~exn:exit_nodes ;
  Array.iteri ~f:connect_nodes method_body_nodes


(** Add a concrete method. *)
let add_cmethod source_file program icfg cm proc_name =
  let cn, _ = JBasics.cms_split cm.Javalib.cm_class_method_signature in
  if
    (not Config.kotlin_capture)
    && SourceFile.has_extension source_file ~ext:Config.kotlin_source_extension
  then ignore (JTrans.create_empty_procdesc source_file program icfg cm proc_name)
  else
    match JTrans.create_cm_procdesc source_file program icfg cm proc_name with
    | None ->
        ()
    | Some (procdesc, start_node, exit_node, exn_node, jbir_code) ->
        let context = JContext.create_context icfg procdesc jbir_code cn source_file program in
        let method_body_nodes = Array.mapi ~f:(JTrans.instruction context) (JBir.code jbir_code) in
        add_edges context start_node exn_node [exit_node] method_body_nodes jbir_code false


let classname_path cn =
  let package_path = List.fold ~f:Filename.concat ~init:Filename.root (JBasics.cn_package cn) in
  Filename.concat package_path (JBasics.cn_simple_name cn ^ ".class")


let cache_classname, is_classname_cached =
  let translated_classnames = ref JBasics.ClassSet.empty in
  let cache cn = translated_classnames := JBasics.ClassSet.add cn !translated_classnames
  and is_cached cn = JBasics.ClassSet.mem cn !translated_classnames in
  (cache, is_cached)


let test_source_file_location source_file program cn node =
  let is_synthetic = function
    | Javalib.JInterface _ ->
        false
    | Javalib.JClass jc ->
        jc.Javalib.c_synthetic
  in
  if not (is_synthetic node) then
    match JProgramDesc.get_java_location program cn with
    | None ->
        L.(debug Capture Verbose)
          "WARNING SOURCE FILE PARSER: location not found for class %s in source file %s \n"
          (JBasics.cn_name cn)
          (SourceFile.to_abs_path source_file)
    | Some _ ->
        ()


(* Given a source file and a class, translates the code of this class.
   In init - mode, finds out whether this class contains initializers at all,
   in this case translates it. In standard mode, all methods are translated *)
let create_icfg source_file program tenv icfg cn node =
  L.(debug Capture Verbose) "\tclassname: %s@." (JBasics.cn_name cn) ;
  if Config.dependency_mode && not (is_classname_cached cn) then cache_classname cn ;
  test_source_file_location source_file program cn node ;
  let translate m =
    let proc_name = JTransType.translate_method_name program tenv m in
    JProgramDesc.set_callee_translated program proc_name ;
    if BiabductionModels.mem proc_name then
      (* do not translate the method if there is a model for it *)
      L.debug Capture Verbose "Skipping method with a model: %a@." Procname.pp proc_name
    else
      try
        (* each procedure has different scope: start names from id 0 *)
        Ident.NameGenerator.reset () ;
        match m with
        | Javalib.AbstractMethod am ->
            ignore (JTrans.create_am_procdesc source_file program icfg am proc_name)
        | Javalib.ConcreteMethod cm when JTrans.is_java_native cm ->
            ignore (JTrans.create_native_procdesc source_file program icfg cm proc_name)
        | Javalib.ConcreteMethod cm ->
            add_cmethod source_file program icfg cm proc_name
      with JBasics.Class_structure_error error ->
        L.internal_error "create_icfg raised JBasics.Class_structure_error %s on %a@." error
          Procname.pp proc_name
  in
  Javalib.m_iter translate node


(* returns true for the set of classes that are selected to be translated in the given
   program *)
let should_capture package_opt source_basename classname node =
  match Javalib.get_sourcefile node with
  | None ->
      false
  | Some found_basename -> (
      String.equal found_basename source_basename
      &&
      match package_opt with
      | None ->
          true
      | Some pkg ->
          List.equal String.equal pkg (JBasics.cn_package classname) )


(* Computes the control - flow graph and call - graph of a given source file.
   In the standard - mode, it translated all the classes of [program] that correspond to this
   source file. *)
let compute_source_icfg program tenv source_basename package_opt source_file =
  let icfg = {JContext.cfg= Cfg.create (); tenv} in
  let select test procedure cn =
    match JProgramDesc.lookup_node cn program with
    | None ->
        ()
    | Some node -> (
        if test cn node then try procedure cn node with Bir.Subroutine -> () )
  in
  (* we must set the java location for all classes in the source file before translation *)
  if Config.java_source_parser_experimental then
    JSourceLocations.collect_class_location program source_file
  else JSourceFileInfo.collect_class_location program source_file ;
  let create =
    select (should_capture package_opt source_basename) (create_icfg source_file program tenv icfg)
  in
  List.iter ~f:create (JProgramDesc.get_matching_class_names program source_basename) ;
  icfg.JContext.cfg


let compute_class_icfg source_file program tenv node =
  let icfg = {JContext.cfg= Cfg.create (); tenv} in
  ( try create_icfg source_file program tenv icfg (Javalib.get_name node) node
    with Bir.Subroutine -> () ) ;
  icfg.JContext.cfg
