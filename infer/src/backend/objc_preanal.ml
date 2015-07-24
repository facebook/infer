(*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open Utils

type procedure_type =
  | ALL
  | DEFINED
  | OBJC_GENERATED

let print_map procname_map =
  Procname.Hash.iter
    (fun pname redefined ->
       print_endline ((Procname.to_string pname)^" "^(string_of_bool redefined)))
    procname_map

let process_all_cfgs process_function default_value =
  let source_dirs = DB.find_source_dirs () in
  let process_dir source_dir value =
    let cfg_name = DB.source_dir_get_internal_file source_dir ".cfg" in
    let cfg_opt = Cfg.load_cfg_from_file cfg_name in
    match cfg_opt with
    | None -> value
    | Some cfg -> process_function cfg source_dir in
  list_fold_right process_dir source_dirs default_value

let process_procedures process_function default_value procedure_type =
  let process_cfg_procedures cfg source_dir =
    let procdescs =
      match procedure_type with
      | DEFINED -> Cfg.get_defined_procs cfg
      | ALL -> Cfg.get_all_procs cfg
      | OBJC_GENERATED -> Cfg.get_objc_generated_procs cfg in
    list_fold_right (process_function cfg source_dir) procdescs default_value in
  process_all_cfgs process_cfg_procedures default_value

let process_all_procedures process_function default_value =
  process_procedures process_function default_value ALL

let process_defined_procedures process_function default_value =
  process_procedures process_function default_value DEFINED

let process_objc_generated_procedures process_function default_value =
  process_procedures process_function default_value OBJC_GENERATED

(* first run to fill the map. The bool that indicates whether the method *)
(* has a real implementation is false by default *)
let fill_generated_proc_map generated_proc_map =
  let add_generated_pname_to_map cfg source_dir procdesc () =
    let pname = Cfg.Procdesc.get_proc_name procdesc in
    Procname.Hash.replace generated_proc_map pname false in
  process_objc_generated_procedures add_generated_pname_to_map ()

(* second run to update the map. Now we check whether there is a real *)
(* implementation for the generated methods *)
let update_generate_proc_map generated_proc_map =
  let update_generated_pname_to_map cfg source_dir procdesc () =
    let cfg_pname = Cfg.Procdesc.get_proc_name procdesc in
    if not (Cfg.Procdesc.get_attributes procdesc).Sil.is_generated then
      try ignore(Procname.Hash.find generated_proc_map cfg_pname);
        Procname.Hash.replace generated_proc_map cfg_pname true
      with Not_found -> () in
  process_defined_procedures update_generated_pname_to_map ()

(* third run to change the cfgs according to the map. The generated methods *)
(* that have implementations  get deleted. *)
let update_cfgs generated_proc_map =
  let update_cfg cfg source_dir =
    let generated_procs = Cfg.get_objc_generated_procs cfg in
    let cfg_name = DB.source_dir_get_internal_file source_dir ".cfg" in
    let cg_name = DB.source_dir_get_internal_file source_dir ".cg" in
    let cg_opt = Cg.load_from_file cg_name in
    match cg_opt with
    | None -> assert false
    | Some cg ->
        let update_cfg_procdesc procdesc need_updating =
          let pname = Cfg.Procdesc.get_proc_name procdesc in
          let is_redefined =
            try Procname.Hash.find generated_proc_map pname
            with Not_found -> assert false in
          if is_redefined then
            (Cfg.Procdesc.remove cfg pname true;
             Cg.node_set_defined cg pname false;
             true)
          else need_updating in
        let need_updating = list_fold_right update_cfg_procdesc generated_procs false in
        if need_updating then
          (Cfg.store_cfg_to_file cfg_name false cfg;
           Cg.store_to_file cg_name cg) in
  process_all_cfgs update_cfg ()

let do_objc_preanalysis () =
  let generated_proc_map = Procname.Hash.create 100 in
  fill_generated_proc_map generated_proc_map;
  update_generate_proc_map generated_proc_map;
  update_cfgs generated_proc_map
