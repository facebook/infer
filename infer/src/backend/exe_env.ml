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

module L = Logging

(** per-file data: type environment and cfg *)
type file_data =
  { source: DB.source_file;
    nLOC : int;
    tenv_file: DB.filename;
    mutable tenv: Tenv.t option;
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
    mutable source_files : DB.SourceFileSet.t; (** Source files in the execution environment *)
  }

(** initial state, used to add cg's *)
type initial = t

(** freeze the execution environment, so it can be queried *)
let freeze exe_env = exe_env (* TODO: unclear what this function is used for *)

(** create a new execution environment *)
let create () =
  { cg = Cg.create ();
    proc_map = Procname.Hash.create 17;
    source_files = DB.SourceFileSet.empty;
  }

(** add call graph from fname in the spec db,
    with relative tenv and cfg, to the execution environment *)
let add_cg (exe_env: t) (source_dir : DB.source_dir) =
  let cg_fname = DB.source_dir_get_internal_file source_dir ".cg" in
  match Cg.load_from_file cg_fname with
  | None ->
      L.stderr "cannot load %s@." (DB.filename_to_string cg_fname);
      None
  | Some cg ->
      let source = Cg.get_source cg in
      exe_env.source_files <- DB.SourceFileSet.add source exe_env.source_files;
      let nLOC = Cg.get_nLOC cg in
      Cg.extend exe_env.cg cg;
      let file_data = new_file_data source nLOC cg_fname in
      let defined_procs = Cg.get_defined_nodes cg in
      IList.iter (fun pname ->
          let should_update =
            if Procname.Hash.mem exe_env.proc_map pname then
              let old_source =
                (Procname.Hash.find exe_env.proc_map pname).source in
              (* when a procedure is defined in several files, *)
              (* map to the first alphabetically *)
              source < old_source
            else true in
          if should_update
          then Procname.Hash.replace exe_env.proc_map pname file_data)
        defined_procs;
      Some cg

(** get the global call graph *)
let get_cg exe_env =
  exe_env.cg

let get_file_data exe_env pname =
  try
    Some (Procname.Hash.find exe_env.proc_map pname)
  with Not_found ->
    begin
      match AttributesTable.load_attributes pname with
      | None ->
          L.err "can't find tenv_cfg_object for %a@." Procname.pp pname;
          None
      | Some proc_attributes ->
          let loc = proc_attributes.ProcAttributes.loc in
          let source_file = loc.Location.file in
          let nLOC = loc.Location.nLOC in
          let source_dir = DB.source_dir_from_source_file source_file in
          let cg_fname = DB.source_dir_get_internal_file source_dir ".cg" in
          let file_data = new_file_data source_file nLOC cg_fname in
          Procname.Hash.replace exe_env.proc_map pname file_data;
          Some file_data
    end

(** return the source file associated to the procedure *)
let get_source exe_env pname =
  Option.map
    (fun file_data -> file_data.source)
    (get_file_data exe_env pname)

let file_data_to_tenv file_data =
  if file_data.tenv == None
  then file_data.tenv <- Tenv.load_from_file file_data.tenv_file;
  file_data.tenv

let file_data_to_cfg file_data =
  if file_data.cfg = None
  then file_data.cfg <- Cfg.load_cfg_from_file file_data.cfg_file;
  file_data.cfg

(** return the type environment associated to the procedure *)
let get_tenv exe_env proc_name : Tenv.t =
  match get_file_data exe_env proc_name with
  | None ->
      failwith ("get_tenv: file_data not found for" ^ Procname.to_string proc_name)
  | Some file_data ->
      begin
        match file_data_to_tenv file_data with
        | Some tenv ->
            tenv
        | None ->
            failwith ("get_tenv: tenv not found for" ^ Procname.to_string proc_name)
      end

(** return the cfg associated to the procedure *)
let get_cfg exe_env pname =
  match get_file_data exe_env pname with
  | None ->
      None
  | Some file_data ->
      file_data_to_cfg file_data

(** return the proc desc associated to the procedure *)
let get_proc_desc exe_env pname =
  match get_cfg exe_env pname with
  | Some cfg ->
      Cfg.Procdesc.find_from_name cfg pname
  | None ->
      None

(** [iter_files f exe_env] applies [f] to the filename and tenv and cfg for each file in [exe_env] *)
let iter_files f exe_env =
  let do_file _ file_data seen_files_acc =
    let fname = file_data.source in
    if DB.SourceFileSet.mem fname seen_files_acc ||
       (* only files added with add_cg* functions *)
       not (DB.SourceFileSet.mem fname exe_env.source_files)
    then seen_files_acc
    else
      begin
        DB.current_source := fname;
        Config.nLOC := file_data.nLOC;
        Option.may (fun cfg -> f fname cfg) (file_data_to_cfg file_data);
        DB.SourceFileSet.add fname seen_files_acc
      end in
  ignore (Procname.Hash.fold do_file exe_env.proc_map DB.SourceFileSet.empty)
