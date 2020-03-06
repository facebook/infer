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

let specs_file_extension = String.chop_prefix_exn ~prefix:"." Config.specs_files_suffix

let collect_specs_filenames jar_filename =
  let f () filename =
    let proc_filename = Filename.basename filename in
    StringHash.replace models_specs_filenames proc_filename ()
  in
  Utils.zip_fold_filenames ~init:() ~f ~chop_extension:specs_file_extension
    ~zip_filename:jar_filename


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
