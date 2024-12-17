(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging

type t =
  {tenvs_map: Tenv.t option SourceFile.Hash.t; int_widths_map: IntegerWidths.t SourceFile.Hash.t}

let mk () = {tenvs_map= SourceFile.Hash.create 1; int_widths_map= SourceFile.Hash.create 1}

let[@alert "-tenv"] get_source_tenv exe_env source =
  match SourceFile.Hash.find_opt exe_env.tenvs_map source with
  | Some tenv_opt ->
      tenv_opt
  | None -> (
    match Tenv.load source with
    | None when Config.log_missing_deps ->
        raise MissingDependencyException.MissingDependencyException
    | v ->
        SourceFile.Hash.add exe_env.tenvs_map source v ;
        v )


let get_proc_source pname =
  match Attributes.load pname with
  | Some proc_attributes ->
      proc_attributes.ProcAttributes.translation_unit
  | None when Config.log_missing_deps ->
      raise MissingDependencyException.MissingDependencyException
  | None ->
      L.die InternalError "exe_env: could not find procedure attributes for %a@\n" Procname.pp pname


let load_java_global_tenv _exe_env =
  (* We don't use exe_env, though require it as a param.
     This is to make API consistent with other methods and to allow for future implementation changes.
     If somebody wants to fetch java global environment, they'd better have exe_env instance provided.
     If they don't they are probably doing something wrong.
  *)
  match Tenv.load_global () with
  | None ->
      L.die InternalError "Could not load the global tenv"
  | Some tenv ->
      tenv


let get_proc_tenv exe_env pname =
  match pname with
  | Procname.Java _ ->
      load_java_global_tenv exe_env
  | _ ->
      get_proc_source pname |> get_source_tenv exe_env
      |> Option.value_or_thunk ~default:(fun () ->
             L.die InternalError "exe_env: could not find tenv for %a@\n" Procname.pp pname )


let get_integer_type_widths exe_env pname =
  let source = get_proc_source pname in
  match SourceFile.Hash.find_opt exe_env.int_widths_map source with
  | Some widths ->
      widths
  | None -> (
    match IntegerWidths.load source with
    | Some widths ->
        SourceFile.Hash.add exe_env.int_widths_map source widths ;
        widths
    | None ->
        IntegerWidths.java )
