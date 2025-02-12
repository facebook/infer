(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging

let tenvs_map = SourceFile.Cache.create ~name:"tenvs"

let int_widths_map = SourceFile.Cache.create ~name:"int_widths"

let clear_caches () =
  SourceFile.Cache.clear tenvs_map ;
  SourceFile.Cache.clear int_widths_map


let[@alert "-tenv"] get_source_tenv source =
  match SourceFile.Cache.lookup tenvs_map source with
  | Some tenv_opt ->
      tenv_opt
  | None -> (
    match Tenv.load source with
    | None when Config.log_missing_deps ->
        raise MissingDependencyException.MissingDependencyException
    | v ->
        SourceFile.Cache.add tenvs_map source v ;
        v )


let get_proc_source pname =
  match Attributes.load pname with
  | Some proc_attributes ->
      proc_attributes.ProcAttributes.translation_unit
  | None when Config.log_missing_deps ->
      raise MissingDependencyException.MissingDependencyException
  | None ->
      L.die InternalError "exe_env: could not find procedure attributes for %a@\n" Procname.pp pname


let get_proc_tenv pname =
  match pname with
  | Procname.Java _ ->
      Tenv.Global.load () |> Option.value_exn
  | _ ->
      get_proc_source pname |> get_source_tenv
      |> Option.value_or_thunk ~default:(fun () ->
             L.die InternalError "exe_env: could not find tenv for %a@\n" Procname.pp pname )


let get_integer_type_widths pname =
  let source = get_proc_source pname in
  match SourceFile.Cache.lookup int_widths_map source with
  | Some widths ->
      widths
  | None -> (
    match IntegerWidths.load source with
    | Some widths ->
        SourceFile.Cache.add int_widths_map source widths ;
        widths
    | None ->
        IntegerWidths.java )


let set_lru_limit ~lru_limit =
  SourceFile.Cache.set_lru_mode tenvs_map ~lru_limit ;
  SourceFile.Cache.set_lru_mode int_widths_map ~lru_limit
