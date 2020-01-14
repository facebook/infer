(*
 * Copyright (c) Facebook, Inc. and its affiliates.
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
     if
       Config.biabduction
       && (not Config.biabduction_models_mode)
       && Sys.file_exists Config.biabduction_models_jar = `Yes
     then Some (mk_zip_lib Config.biabduction_models_jar)
     else None )


let load serializer path =
  (* NOTE: This and [zib_libraries] used to also work with a list of where to find "zip libraries"
     but now this only handles at most one such library: the biabduction models. There's a chance
     that this code looks weirder than it should as a result. *)
  Option.bind (Lazy.force zip_libraries) ~f:(fun zip_library ->
      load_data serializer path zip_library )
