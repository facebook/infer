(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open Javalib_pack
module L = Logging

let models_jar = ref None

let models_classmap = ref JBasics.ClassMap.empty

let collect_specs_filenames jar_filename =
  (* version of Javalib.get_class that does not spam stderr *)
  let javalib_get_class = Utils.suppress_stderr2 Javalib.get_class in
  let classpath = Javalib.class_path jar_filename in
  let f classmap _ zip_entry =
    match Filename.split_extension zip_entry.Zip.filename with
    | filename, Some extension when String.equal extension "class" -> (
        let cn = JBasics.make_cn (String.map ~f:(function '/' -> '.' | c -> c) filename) in
        try JBasics.ClassMap.add cn (javalib_get_class classpath cn) classmap
        with JBasics.Class_structure_error _ -> classmap )
    | _ ->
        classmap
  in
  models_classmap := Utils.zip_fold ~init:JBasics.ClassMap.empty ~f ~zip_filename:jar_filename ;
  Javalib.close_class_path classpath


let load_models ~jar_filename =
  match !models_jar with
  | None when not (ISys.file_exists jar_filename) ->
      L.die InternalError "Java model file not found@."
  | None ->
      models_jar := Some jar_filename ;
      collect_specs_filenames jar_filename
  | Some filename when String.equal filename jar_filename ->
      ()
  | Some filename ->
      L.die InternalError "Asked to load a 2nd models jar (%s) when %s was loaded.@." jar_filename
        filename


let get_classmap () = !models_classmap
