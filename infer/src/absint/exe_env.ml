(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

(** Execution environments: basically a cache of where procedures are and what is their CFG and type
    environment *)

module L = Logging

(** per-file data: type environment and integer widths *)
type file_data = {tenv: Tenv.t option Lazy.t; integer_type_widths: IntegerWidths.t option Lazy.t}

(** create a new file_data *)
let[@alert "-tenv"] new_file_data source =
  (* exceptionally allow [Tenv.load] as it goes through a cache *)
  { tenv= lazy (Tenv.load source)
  ; integer_type_widths=
      lazy (Option.first_some (IntegerWidths.load source) (Some IntegerWidths.java)) }


type t = {proc_map: SourceFile.t Procname.Hash.t; file_map: file_data SourceFile.Hash.t}

let file_data_of_source {file_map} source =
  match SourceFile.Hash.find_opt file_map source with
  | Some file_data ->
      file_data
  | None ->
      let file_data = new_file_data source in
      SourceFile.Hash.add file_map source file_data ;
      file_data


let get_file_data exe_env pname =
  match Procname.Hash.find_opt exe_env.proc_map pname with
  | Some source ->
      Some (file_data_of_source exe_env source)
  | None -> (
    match Attributes.load pname with
    | None ->
        L.debug Analysis Medium "can't find attributes for %a@." Procname.pp pname ;
        None
    | Some proc_attributes ->
        let source_file = proc_attributes.ProcAttributes.translation_unit in
        let file_data = file_data_of_source exe_env source_file in
        Procname.Hash.add exe_env.proc_map pname source_file ;
        Some file_data )


let file_data_to_tenv file_data = Lazy.force file_data.tenv

let file_data_to_integer_type_widths file_data = Lazy.force file_data.integer_type_widths

let java_global_tenv =
  lazy
    ( match Tenv.load_global () with
    | None ->
        L.(die InternalError) "Could not load the global tenv"
    | Some tenv ->
        tenv )


let load_java_global_tenv _exe_env =
  (* We don't use exe_env, though require it as a param.
     This is to make API consistent with other methods and to allow for future implementation changes.
     If somebody wants to fetch java global environment, they'd better have exe_env instance provided.
     If they don't they are probably doing something wrong.
  *)
  Lazy.force java_global_tenv


let get_column_value ~value_on_java ~file_data_to_value ~column_name exe_env proc_name =
  let pp_loc_opt f = function
    | Some loc ->
        F.fprintf f " in file '%a' at %a" SourceFile.pp loc.Location.file Location.pp loc
    | None ->
        ()
  in
  match proc_name with
  | Procname.Java _ ->
      Lazy.force value_on_java
  | _ -> (
    match get_file_data exe_env proc_name with
    | Some file_data -> (
      match file_data_to_value file_data with
      | Some v ->
          v
      | None when Config.log_missing_deps ->
          raise MissingDependencyException.MissingDependencyException
      | None ->
          let loc_opt = AnalysisState.get_loc () in
          L.die InternalError "get_column_value: %s not found for %a%a" column_name Procname.pp
            proc_name pp_loc_opt loc_opt )
    | None when Config.log_missing_deps ->
        raise MissingDependencyException.MissingDependencyException
    | None ->
        let loc_opt = AnalysisState.get_loc () in
        L.die InternalError "get_column_value: file_data not found for %a%a" Procname.pp proc_name
          pp_loc_opt loc_opt )


(** return the type environment associated to the procedure *)
let get_proc_tenv =
  get_column_value ~value_on_java:java_global_tenv ~file_data_to_value:file_data_to_tenv
    ~column_name:"tenv"


let get_source_tenv exe_env source =
  let file_data = file_data_of_source exe_env source in
  Lazy.force file_data.tenv


(** return the integer type widths associated to the procedure *)
let get_integer_type_widths =
  get_column_value
    ~value_on_java:(lazy IntegerWidths.java)
    ~file_data_to_value:file_data_to_integer_type_widths ~column_name:"integer type widths"


let mk () = {proc_map= Procname.Hash.create 17; file_map= SourceFile.Hash.create 1}
