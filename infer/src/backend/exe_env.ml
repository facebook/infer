(*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

(** Support for Execution environments *)

open Utils
module L = Logging

(** per-file data: type environment and cfg *)
type file_data =
  { source: DB.source_file;
    nLOC : int;
    tenv_file: DB.filename;
    mutable tenv: Sil.tenv option;
    cfg_file: DB.filename;
    mutable cfg: Cfg.cfg option;
  }


(** get the path to the tenv file, which either one tenv file per source file or a global tenv file *)
let tenv_filename file_base =
  let per_source_tenv_filename = DB.filename_add_suffix file_base ".tenv" in
  if Sys.file_exists (DB.filename_to_string per_source_tenv_filename) then
    per_source_tenv_filename
  else
    DB.global_tenv_fname ()

(** create a new file_data *)
let new_file_data source nLOC cg_fname =
  let file_base = DB.chop_extension cg_fname in
  let tenv_file = tenv_filename file_base in
  let cfg_file = DB.filename_add_suffix file_base ".cfg" in
  { source = source;
    nLOC = nLOC;
    tenv_file = tenv_file;
    tenv = None; (* Sil.load_tenv_from_file tenv_file *)
    cfg_file = cfg_file;
    cfg = None; (* Cfg.load_cfg_from_file cfg_file *)
  }


(** execution environment *)
type t =
  { cg: Cg.t; (** global call graph *)
    proc_map: file_data Procname.Hash.t; (** map from procedure name to file data *)
    file_map: (DB.source_file, file_data) Hashtbl.t; (** map from filaname to file data *)
    mutable active_opt : Procname.Set.t option; (** if not None, restrict the active procedures to the given set *)
    mutable procs_defined_in_several_files : Procname.Set.t; (** Procedures defined in more than one file *)
  }

(** initial state, used to add cg's *)
type initial = t

(** freeze the execution environment, so it can be queried *)
let freeze exe_env = exe_env (* TODO: unclear what this function is used for *)

(** create a new execution environment *)
let create procset_opt =
  { cg = Cg.create ();
    proc_map = Procname.Hash.create 17;
    file_map = Hashtbl.create 17;
    active_opt = procset_opt;
    procs_defined_in_several_files = Procname.Set.empty;
  }

(** check if a procedure is marked as active *)
let proc_is_active exe_env proc_name =
  match exe_env.active_opt with
  | None -> true
  | Some procset -> Procname.Set.mem proc_name procset

(** add a procedure to the set of active procedures *)
let add_active_proc exe_env proc_name =
  match exe_env.active_opt with
  | None -> ()
  | Some procset -> exe_env.active_opt <- Some (Procname.Set.add proc_name procset)

(** like add_cg, but use exclude_fun to determine files to be excluded *)
let add_cg_exclude_fun (exe_env: t) (source_dir : DB.source_dir) exclude_fun =
  let cg_fname = DB.source_dir_get_internal_file source_dir ".cg" in
  let cg = match Cg.load_from_file cg_fname with
    | None -> (L.err "cannot load %s@." (DB.filename_to_string cg_fname); assert false)
    | Some cg ->
        Cg.restrict_defined cg exe_env.active_opt;
        cg in
  let source = Cg.get_source cg in
  if exclude_fun source then None
  else
    let nLOC = Cg.get_nLOC cg in
    Cg.extend exe_env.cg cg;
    let file_data = new_file_data source nLOC cg_fname in
    let defined_procs = Cg.get_defined_nodes cg in
    IList.iter (fun pname ->
        let should_update =
          if Procname.Hash.mem exe_env.proc_map pname then
            let old_source = (Procname.Hash.find exe_env.proc_map pname).source in
            exe_env.procs_defined_in_several_files <- Procname.Set.add pname exe_env.procs_defined_in_several_files;
            (* L.err "Warning: procedure %a is defined in both %s and %s@." Procname.pp pname (DB.source_file_to_string source) (DB.source_file_to_string old_source); *)
            source < old_source (* when a procedure is defined in several files, map to the first alphabetically *)
          else true in
        if should_update then Procname.Hash.replace exe_env.proc_map pname file_data) defined_procs;
    Hashtbl.add exe_env.file_map source file_data;
    Some cg

(** add call graph from fname in the spec db, with relative tenv and cfg, to the execution environment *)
let add_cg exe_env (source_dir : DB.source_dir) =
  add_cg_exclude_fun exe_env source_dir (fun _ -> false)

(** add a new source file -> file data mapping. arguments are the components of the file_data
  * record *)
let add_file_mapping exe_env source nLOC tenv_file tenv cfg_file cfg =
  let file_data =
    { source = source;
      nLOC = nLOC;
      tenv_file = tenv_file;
      tenv = tenv;
      cfg_file = cfg_file;
      cfg = cfg;
    } in
  Hashtbl.add exe_env.file_map source file_data

(** get the procedures defined in more than one file *)
let get_procs_defined_in_several_files exe_env =
  exe_env.procs_defined_in_several_files

(** get the global call graph *)
let get_cg exe_env =
  exe_env.cg

let get_file_data exe_env pname =
  try
    Procname.Hash.find exe_env.proc_map pname
  with Not_found ->
    begin
      match AttributesTable.load_attributes pname with
      | None ->
          L.err "can't find tenv_cfg_object for %a@." Procname.pp pname;
          raise Not_found
      | Some proc_attributes ->
          let loc = proc_attributes.ProcAttributes.loc in
          let source_file = loc.Location.file in
          let nLOC = loc.Location.nLOC in
          let source_dir = DB.source_dir_from_source_file source_file in
          let cg_fname = DB.source_dir_get_internal_file source_dir ".cg" in
          let file_data =
            try Hashtbl.find exe_env.file_map source_file with
            | Not_found ->
                let file_data = new_file_data source_file nLOC cg_fname in
                Hashtbl.replace exe_env.file_map source_file file_data;
                file_data in
          Procname.Hash.replace exe_env.proc_map pname file_data;
          file_data
    end

(** return the source file associated to the procedure *)
let get_source exe_env pname =
  (get_file_data exe_env pname).source

let file_data_to_tenv file_data =
  if file_data.tenv == None then file_data.tenv <- Sil.load_tenv_from_file file_data.tenv_file;
  match file_data.tenv with
  | None ->
      L.err "Cannot find tenv for %s@." (DB.filename_to_string file_data.tenv_file);
      assert false
  | Some tenv -> tenv

(** return the procs enabled: active and not shadowed, plus the procs they call directly *)
let procs_enabled exe_env source =
  let is_not_shadowed proc_name = (* not shadowed by a definition in another file *)
    DB.source_file_equal (get_source exe_env proc_name) source in
  match exe_env.active_opt with
  | Some pset ->
      let res = ref Procname.Set.empty in
      let do_pname proc_name = (* add any proc which is not shadowed, and all the procs it calls *)
        if is_not_shadowed proc_name then
          let pset' = Cg.get_all_children exe_env.cg proc_name in
          let pset'' = Procname.Set.add proc_name pset' in
          res := Procname.Set.union pset'' !res in
      Procname.Set.iter do_pname pset;
      Some !res
  | None -> None

let file_data_to_cfg exe_env file_data =
  match file_data.cfg with
  | None ->
      let cfg = match Cfg.load_cfg_from_file file_data.cfg_file with
        | None ->
            L.err "Cannot find cfg for %s@." (DB.filename_to_string file_data.cfg_file);
            assert false
        | Some cfg -> cfg in
      file_data.cfg <- Some cfg;
      cfg
  | Some cfg -> cfg

(** return the type environment associated to the procedure *)
let get_tenv exe_env pname =
  let file_data = get_file_data exe_env pname in
  file_data_to_tenv file_data

(** return the cfg associated to the procedure *)
let get_cfg exe_env pname =
  let file_data = get_file_data exe_env pname in
  file_data_to_cfg exe_env file_data

(** [iter_files f exe_env] applies [f] to the filename and tenv and cfg for each file in [exe_env] *)
let iter_files f exe_env =
  let do_file fname file_data =
    DB.current_source := fname;
    Config.nLOC := file_data.nLOC;
    f fname (file_data_to_tenv file_data) (file_data_to_cfg exe_env file_data) in
  Hashtbl.iter do_file exe_env.file_map

(** [fold_files f exe_env] folds f through the source file, tenv, and cfg for each file in [exe_env] *)
let fold_files f acc exe_env =
  let fold_file fname file_data acc =
    DB.current_source := fname;
    Config.nLOC := file_data.nLOC;
    f fname (file_data_to_tenv file_data) (file_data_to_cfg exe_env file_data) acc in
  Hashtbl.fold fold_file exe_env.file_map acc
