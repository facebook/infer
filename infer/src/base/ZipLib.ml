(*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PolyVariantEqual

type zip_library = {zip_filename: string; zip_channel: Zip.in_file Lazy.t}

let load_from_zip serializer zip_path zip_library =
  let (lazy zip_channel) = zip_library.zip_channel in
  let deserialize = Serialization.read_from_string serializer in
  match deserialize (Zip.read_entry zip_channel (Zip.find_entry zip_channel zip_path)) with
  | Some data ->
      Some data
  | None ->
      None
  | exception Caml.Not_found ->
      None


let load_data serializer path zip_library =
  let zip_path = Filename.concat Config.default_in_zip_results_dir path in
  load_from_zip serializer zip_path zip_library


(** list of the zip files to search for specs files *)
let zip_libraries =
  (* delay until load is called, to avoid stating/opening files at init time *)
  lazy
    (let mk_zip_lib zip_filename = {zip_filename; zip_channel= lazy (Zip.open_in zip_filename)} in
     let zip_libs =
       let load_zip fname =
         if Filename.check_suffix fname ".jar" then
           (* fname is a zip of specs *)
           Some (mk_zip_lib fname)
         else (* fname is a dir of specs *)
           None
       in
       (* Order matters: jar files should appear in the order in which they should be searched for
            specs files. [Config.specs_library] is in reverse order of appearance on the command
            line. *)
       List.rev_filter_map Config.specs_library ~f:load_zip
     in
     if Config.biabduction && (not Config.models_mode) && Sys.file_exists Config.models_jar = `Yes
     then mk_zip_lib Config.models_jar :: zip_libs
     else zip_libs)


(** Search path in the list of zip libraries and use a cache directory to save already deserialized
    data *)
let load serializer path =
  let rec loop = function
    | [] ->
        None
    | zip_library :: other_libraries ->
        let opt = load_data serializer path zip_library in
        if Option.is_some opt then opt else loop other_libraries
  in
  loop (Lazy.force zip_libraries)
