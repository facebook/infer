(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging

let models_jar = ref None

let get_models_jar_filename () = !models_jar

module StringHash = Caml.Hashtbl.Make (String)

let models_specs_filenames = StringHash.create 1

let collect_specs_filenames jar_filename =
  let zip_channel = Zip.open_in jar_filename in
  let collect entry =
    let filename = entry.Zip.filename in
    if Filename.check_suffix filename Config.specs_files_suffix then
      let proc_filename = Filename.chop_extension (Filename.basename filename) in
      StringHash.replace models_specs_filenames proc_filename ()
  in
  List.iter ~f:collect (Zip.entries zip_channel) ;
  Zip.close_in zip_channel


let set_models ~jar_filename =
  match !models_jar with
  | None when match Sys.file_exists jar_filename with `Yes -> false | _ -> true ->
      L.die InternalError "Java model file not found@."
  | None ->
      models_jar := Some jar_filename ;
      collect_specs_filenames jar_filename
  | Some filename when String.equal filename jar_filename ->
      ()
  | Some filename ->
      L.die InternalError "Asked to load a 2nd models jar (%s) when %s was loaded.@." jar_filename
        filename


let is_model procname = StringHash.mem models_specs_filenames (Procname.to_filename procname)
