(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PolyVariantEqual
open Javalib_pack
open Sawja_pack
module L = Logging

let add_edges (context : JContext.t) start_node exn_node exit_nodes method_body_nodes impl
    super_call =
  let pc_nb = Array.length method_body_nodes in
  let last_pc = pc_nb - 1 in
  let is_last pc = Int.equal pc last_pc in
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
          if Int.Set.mem visited next_pc then [] else get_body_nodes_ next_pc visited
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
    Procdesc.node_set_succs_exn context.procdesc node (get_succ_nodes node pc) (get_exn_nodes pc)
  in
  let connect_nodes pc translated_instruction =
    match translated_instruction with
    | JTrans.Skip ->
        ()
    | JTrans.Instr node ->
        connect node pc
    | JTrans.Prune (node_true, node_false) ->
        connect node_true pc ; connect node_false pc
    | JTrans.Loop (join_node, node_true, node_false) ->
        Procdesc.node_set_succs_exn context.procdesc join_node [node_true; node_false] [] ;
        connect node_true pc ;
        connect node_false pc
  in
  let first_nodes =
    (* deals with the case of an empty array *)
    direct_successors (-1) Int.Set.empty
  in
  (* the exceptions edges here are going directly to the exit node *)
  Procdesc.node_set_succs_exn context.procdesc start_node first_nodes exit_nodes ;
  if not super_call then
    (* the exceptions node is just before the exit node *)
    Procdesc.node_set_succs_exn context.procdesc exn_node exit_nodes exit_nodes ;
  Array.iteri ~f:connect_nodes method_body_nodes


(** Add a concrete method. *)
let add_cmethod source_file program linereader icfg cm proc_name =
  let cn, _ = JBasics.cms_split cm.Javalib.cm_class_method_signature in
  if Inferconfig.skip_implementation_matcher source_file proc_name then
    ignore (JTrans.create_empty_procdesc source_file program linereader icfg cm proc_name)
  else
    match JTrans.create_cm_procdesc source_file program linereader icfg cm proc_name with
    | None ->
        ()
    | Some (procdesc, start_node, exit_node, exn_node, jbir_code) ->
        let context = JContext.create_context icfg procdesc jbir_code cn source_file program in
        let method_body_nodes = Array.mapi ~f:(JTrans.instruction context) (JBir.code jbir_code) in
        add_edges context start_node exn_node [exit_node] method_body_nodes jbir_code false


let path_of_cached_classname cn =
  let root_path = Config.(results_dir ^/ classnames_dir_name) in
  let package_path = List.fold ~f:Filename.concat ~init:root_path (JBasics.cn_package cn) in
  Filename.concat package_path (JBasics.cn_simple_name cn ^ ".java")


let cache_classname cn =
  let path = path_of_cached_classname cn in
  let splitted_root_dir =
    let rec split l p =
      match p with
      | p when String.equal p Filename.current_dir_name ->
          l
      | p when String.equal p Filename.dir_sep ->
          l
      | p ->
          split (Filename.basename p :: l) (Filename.dirname p)
    in
    split [] (Filename.dirname path)
  in
  let rec mkdir l p =
    let () = if Sys.file_exists p <> `Yes then Unix.mkdir p ~perm:493 in
    match l with [] -> () | d :: tl -> mkdir tl (Filename.concat p d)
  in
  mkdir splitted_root_dir Filename.dir_sep ;
  let file_out = Out_channel.create path in
  Out_channel.output_string file_out (string_of_float (Unix.time ())) ;
  Out_channel.close file_out


let is_classname_cached cn = Sys.file_exists (path_of_cached_classname cn) = `Yes

(* Given a source file and a class, translates the code of this class.
   In init - mode, finds out whether this class contains initializers at all,
   in this case translates it. In standard mode, all methods are translated *)
let create_icfg source_file linereader program tenv icfg cn node =
  L.(debug Capture Verbose) "\tclassname: %s@." (JBasics.cn_name cn) ;
  if Config.dependency_mode && not (is_classname_cached cn) then cache_classname cn ;
  let translate m =
    let proc_name = JTransType.translate_method_name program tenv m in
    JClasspath.set_callee_translated program proc_name ;
    if JClasspath.is_model proc_name then
      (* do not translate the method if there is a model for it *)
      L.(debug Capture Verbose)
        "Skipping method with a model: %s@."
        (Typ.Procname.to_string proc_name)
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
            add_cmethod source_file program linereader icfg cm proc_name
      with JBasics.Class_structure_error error ->
        L.internal_error "create_icfg raised JBasics.Class_structure_error %s on %a@." error
          Typ.Procname.pp proc_name
  in
  Javalib.m_iter translate node


(* returns true for the set of classes that are selected to be translated *)
let should_capture classes package_opt source_basename node =
  let classname = Javalib.get_name node in
  let match_package pkg cn =
    match JTransType.package_to_string (JBasics.cn_package cn) with
    | None ->
        String.equal pkg ""
    | Some found_pkg ->
        String.equal found_pkg pkg
  in
  if JBasics.ClassSet.mem classname classes then
    match Javalib.get_sourcefile node with
    | None ->
        false
    | Some found_basename -> (
      match package_opt with
      | None ->
          String.equal found_basename source_basename
      | Some pkg ->
          match_package pkg classname && String.equal found_basename source_basename )
  else false


(* Computes the control - flow graph and call - graph of a given source file.
   In the standard - mode, it translated all the classes that correspond to this
   source file. *)
let compute_source_icfg linereader classes program tenv source_basename package_opt source_file =
  let icfg = {JContext.cfg= Cfg.create (); tenv} in
  let select test procedure cn node =
    if test node then try procedure cn node with Bir.Subroutine -> ()
  in
  let () =
    JBasics.ClassMap.iter
      (select
         (should_capture classes package_opt source_basename)
         (create_icfg source_file linereader program tenv icfg))
      (JClasspath.get_classmap program)
  in
  icfg.JContext.cfg


let compute_class_icfg source_file linereader program tenv node =
  let icfg = {JContext.cfg= Cfg.create (); tenv} in
  ( try create_icfg source_file linereader program tenv icfg (Javalib.get_name node) node
    with Bir.Subroutine -> () ) ;
  icfg.JContext.cfg
