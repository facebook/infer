(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils

module L = Logging


let get_cache_dir infer_cache zip_filename =
  let basename = Filename.basename zip_filename in
  let key = basename ^ string_crc_hex32 zip_filename in
  Filename.concat infer_cache key

let load_from_cache serializer zip_path cache_dir zip_library =
  let absolute_path = Filename.concat cache_dir zip_path in
  let deserialize = Serialization.from_file serializer in
  let extract to_path =
    if not (Sys.file_exists to_path) then
      begin
        DB.create_path (Filename.dirname to_path);
        let lazy zip_channel = zip_library.Config.zip_channel in
        let entry = Zip.find_entry zip_channel zip_path in
        Zip.copy_entry_to_file zip_channel entry to_path
      end;
    DB.filename_from_string to_path in
  match deserialize (extract absolute_path) with
  | Some data when zip_library.Config.models -> Some (data, DB.Models)
  | Some data -> Some (data, DB.Spec_lib)
  | None -> None
  | exception Not_found -> None

let load_from_zip serializer zip_path zip_library =
  let lazy zip_channel = zip_library.Config.zip_channel in
  let deserialize = Serialization.from_string serializer in
  match deserialize (Zip.read_entry zip_channel (Zip.find_entry zip_channel zip_path)) with
  | Some data when zip_library.Config.models -> Some (data, DB.Models)
  | Some data -> Some (data, DB.Spec_lib)
  | None -> None
  | exception Not_found -> None

let load_data serializer path zip_library =
  let zip_path = Filename.concat Config.default_in_zip_results_dir path in
  match Config.infer_cache with
  | None ->
      load_from_zip serializer zip_path zip_library
  | Some infer_cache ->
      let cache_dir = get_cache_dir infer_cache zip_library.Config.zip_filename in
      load_from_cache serializer zip_path cache_dir zip_library

(* Search path in the list of zip libraries and use a cache directory to save already
   deserialized data *)
let load serializer path =
  let rec loop = function
    | [] -> None
    | zip_library :: other_libraries ->
        let opt = load_data serializer path zip_library in
        if Option.is_some opt then opt
        else loop other_libraries in
  loop Config.zip_libraries
