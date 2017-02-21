(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd
open! PVariant

module L = Logging


type zip_library = {
  zip_filename: string;
  zip_channel: Zip.in_file Lazy.t;
  models: bool;
}


let get_cache_dir infer_cache zip_filename =
  let basename = Filename.basename zip_filename in
  let key = basename ^ Utils.string_crc_hex32 zip_filename in
  Filename.concat infer_cache key

let load_from_cache serializer zip_path cache_dir zip_library =
  let absolute_path = Filename.concat cache_dir zip_path in
  let deserialize = Serialization.from_file serializer in
  let extract to_path =
    if (Sys.file_exists to_path) <> `Yes then
      begin
        Unix.mkdir_p (Filename.dirname to_path);
        let lazy zip_channel = zip_library.zip_channel in
        let entry = Zip.find_entry zip_channel zip_path in
        Zip.copy_entry_to_file zip_channel entry to_path
      end;
    DB.filename_from_string to_path in
  match deserialize (extract absolute_path) with
  | Some data -> Some data
  | None -> None
  | exception Not_found -> None

let load_from_zip serializer zip_path zip_library =
  let lazy zip_channel = zip_library.zip_channel in
  let deserialize = Serialization.from_string serializer in
  match deserialize (Zip.read_entry zip_channel (Zip.find_entry zip_channel zip_path)) with
  | Some data -> Some data
  | None -> None
  | exception Not_found -> None

let load_data serializer path zip_library =
  let zip_path = Filename.concat Config.default_in_zip_results_dir path in
  match Config.infer_cache with
  | None ->
      load_from_zip serializer zip_path zip_library
  | Some infer_cache ->
      let cache_dir = get_cache_dir infer_cache zip_library.zip_filename in
      load_from_cache serializer zip_path cache_dir zip_library

(** list of the zip files to search for specs files *)
let zip_libraries =
  (* delay until load is called, to avoid stating/opening files at init time *)
  lazy (
    let mk_zip_lib models zip_filename =
      { models; zip_filename; zip_channel = lazy (Zip.open_in zip_filename) } in
    let zip_libs =
      if Config.use_jar_cache && Config.infer_cache <> None then
        []
      else
        (* Order matters, jar files should appear in the order in which they should be searched for
           specs files. Config.specs_library is in reverse order of appearance on command line. *)
        let add_zip zip_libs fname =
          if Filename.check_suffix fname ".jar" then
            (* fname is a zip of specs *)
            (mk_zip_lib false fname) :: zip_libs
          else
            (* fname is a dir of specs *)
            zip_libs in
        List.fold ~f:add_zip ~init:[] Config.specs_library in
    if Config.checkers then
      zip_libs
    else if (Sys.file_exists Config.models_jar) = `Yes then
      (mk_zip_lib true Config.models_jar) :: zip_libs
    else
      zip_libs
  )

(* Search path in the list of zip libraries and use a cache directory to save already
   deserialized data *)
let load serializer path =
  let rec loop = function
    | [] -> None
    | zip_library :: other_libraries ->
        let opt = load_data serializer path zip_library in
        if Option.is_some opt then opt
        else loop other_libraries in
  loop (Lazy.force zip_libraries)
