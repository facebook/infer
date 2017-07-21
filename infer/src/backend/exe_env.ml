(*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd
open! PVariant
module Hashtbl = Caml.Hashtbl

(** Support for Execution environments *)

module L = Logging
module F = Format

(** per-file data: type environment and cfg *)
type file_data =
  { source: SourceFile.t
  ; tenv_file: DB.filename
  ; mutable tenv: Tenv.t option
  ; cfg_file: DB.filename
  ; mutable cfg: Cfg.cfg option }

(** get the path to the tenv file, which either one tenv file per source file or a global tenv file *)
let tenv_filename file_base =
  let per_source_tenv_filename = DB.filename_add_suffix file_base ".tenv" in
  if Sys.file_exists (DB.filename_to_string per_source_tenv_filename) = `Yes then
    per_source_tenv_filename
  else DB.global_tenv_fname

module FilenameHash = Hashtbl.Make (struct
  type t = DB.filename

  let equal file1 file2 = DB.equal_filename file1 file2

  let hash = Hashtbl.hash
end)

(** create a new file_data *)
let new_file_data source cg_fname =
  let file_base = DB.chop_extension cg_fname in
  let tenv_file = tenv_filename file_base in
  let cfg_file = DB.filename_add_suffix file_base ".cfg" in
  { source
  ; tenv_file
  ; tenv= None
  ; (* Sil.load_tenv_from_file tenv_file *)
  cfg_file
  ; cfg= None (* Cfg.load_cfg_from_file cfg_file *) }

let create_file_data table source cg_fname =
  match FilenameHash.find table cg_fname with
  | file_data
   -> file_data
  | exception Not_found
   -> let file_data = new_file_data source cg_fname in
      FilenameHash.add table cg_fname file_data ; file_data

(** execution environment *)
type t =
  { cg: Cg.t  (** global call graph *)
  ; proc_map: file_data Typ.Procname.Hash.t  (** map from procedure name to file data *)
  ; file_map: file_data FilenameHash.t  (** map from cg fname to file data *)
  ; mutable source_files: SourceFile.Set.t  (** Source files in the execution environment *) }

(** initial state, used to add cg's *)
type initial = t

(** create a new execution environment *)
let create () =
  { cg= Cg.create (SourceFile.invalid __FILE__)
  ; proc_map= Typ.Procname.Hash.create 17
  ; file_map= FilenameHash.create 1
  ; source_files= SourceFile.Set.empty }

(** add call graph from fname in the spec db,
    with relative tenv and cfg, to the execution environment *)
let add_cg (exe_env: t) (source_dir: DB.source_dir) =
  let cg_fname = DB.source_dir_get_internal_file source_dir ".cg" in
  match Cg.load_from_file cg_fname with
  | None
   -> L.internal_error "Error: cannot load %s@." (DB.filename_to_string cg_fname)
  | Some cg
   -> let source = Cg.get_source cg in
      exe_env.source_files <- SourceFile.Set.add source exe_env.source_files ;
      let defined_procs = Cg.get_defined_nodes cg in
      let duplicate_procs_to_print =
        List.filter_map defined_procs ~f:(fun pname ->
            match AttributesTable.find_file_capturing_procedure ~cache:false pname with
            | None
             -> None
            | Some (source_captured, origin)
             -> let multiply_defined = SourceFile.compare source source_captured <> 0 in
                if multiply_defined then Cg.remove_node_defined cg pname ;
                if multiply_defined && origin <> `Include then Some (pname, source_captured)
                else None )
      in
      if Config.dump_duplicate_symbols then
        Out_channel.with_file (Config.results_dir ^/ Config.duplicates_filename) ~append:true
          ~perm:0o666 ~f:(fun outc ->
            let fmt = F.formatter_of_out_channel outc in
            List.iter duplicate_procs_to_print ~f:(fun (pname, source_captured) ->
                F.fprintf fmt "@.DUPLICATE_SYMBOLS source: %a source_captured:%a pname:%a@."
                  SourceFile.pp source SourceFile.pp source_captured Typ.Procname.pp pname ) ) ;
      Cg.extend exe_env.cg cg

(** get the global call graph *)
let get_cg exe_env = exe_env.cg

let get_file_data exe_env pname =
  try Some (Typ.Procname.Hash.find exe_env.proc_map pname)
  with Not_found ->
    let source_file_opt =
      match AttributesTable.load_attributes ~cache:true pname with
      | None
       -> L.(debug Analysis Medium) "can't find tenv_cfg_object for %a@." Typ.Procname.pp pname ;
          None
      | Some proc_attributes when Config.reactive_capture
       -> let get_captured_file {ProcAttributes.source_file_captured} = source_file_captured in
          OndemandCapture.try_capture proc_attributes |> Option.map ~f:get_captured_file
      | Some proc_attributes
       -> Some proc_attributes.ProcAttributes.source_file_captured
    in
    let get_file_data_for_source source_file =
      let source_dir = DB.source_dir_from_source_file source_file in
      let cg_fname = DB.source_dir_get_internal_file source_dir ".cg" in
      let file_data = create_file_data exe_env.file_map source_file cg_fname in
      Typ.Procname.Hash.replace exe_env.proc_map pname file_data ; file_data
    in
    Option.map ~f:get_file_data_for_source source_file_opt

(** return the source file associated to the procedure *)
let get_source exe_env pname =
  Option.map ~f:(fun file_data -> file_data.source) (get_file_data exe_env pname)

let file_data_to_tenv file_data =
  if is_none file_data.tenv then file_data.tenv <- Tenv.load_from_file file_data.tenv_file ;
  file_data.tenv

let file_data_to_cfg file_data =
  if is_none file_data.cfg then file_data.cfg <- Cfg.load_cfg_from_file file_data.cfg_file ;
  file_data.cfg

let java_global_tenv =
  ( lazy
  ( match Tenv.load_from_file DB.global_tenv_fname with
  | None
   -> failwithf "Could not load the global tenv at path %s@."
        (DB.filename_to_string DB.global_tenv_fname)
  | Some tenv
   -> tenv ) )

(** return the type environment associated to the procedure *)
let get_tenv exe_env proc_name =
  match proc_name with
  | Typ.Procname.Java _
   -> Lazy.force java_global_tenv
  | _ ->
    match get_file_data exe_env proc_name with
    | Some file_data -> (
      match file_data_to_tenv file_data with
      | Some tenv
       -> tenv
      | None
       -> failwithf "get_tenv: tenv not found for %a in file %s" Typ.Procname.pp proc_name
            (DB.filename_to_string file_data.tenv_file) )
    | None
     -> failwithf "get_tenv: file_data not found for %a" Typ.Procname.pp proc_name

(** return the cfg associated to the procedure *)
let get_cfg exe_env pname =
  match get_file_data exe_env pname with
  | None
   -> None
  | Some file_data
   -> file_data_to_cfg file_data

(** return the proc desc associated to the procedure *)
let get_proc_desc exe_env pname =
  match get_cfg exe_env pname with
  | Some cfg
   -> Cfg.find_proc_desc_from_name cfg pname
  | None
   -> None

(** Create an exe_env from a source dir *)
let from_cluster cluster =
  let exe_env = create () in
  add_cg exe_env cluster ; exe_env

(** [iter_files f exe_env] applies [f] to the filename and tenv and cfg for each file in [exe_env] *)
let iter_files f exe_env =
  let do_file _ file_data seen_files_acc =
    let fname = file_data.source in
    if SourceFile.Set.mem fname seen_files_acc
       || (* only files added with add_cg* functions *)
          not (SourceFile.Set.mem fname exe_env.source_files)
    then seen_files_acc
    else (
      Option.iter ~f:(fun cfg -> f fname cfg) (file_data_to_cfg file_data) ;
      SourceFile.Set.add fname seen_files_acc )
  in
  ignore (Typ.Procname.Hash.fold do_file exe_env.proc_map SourceFile.Set.empty)
