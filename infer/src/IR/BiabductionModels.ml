(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

let scan_models () =
  let rec next_entry index dir =
    match Unix.readdir_opt dir with
    | None ->
        Unix.closedir dir ;
        index
    | Some entry -> (
      match String.chop_suffix entry ~suffix:Config.specs_files_suffix with
      | Some file_proc_name ->
          next_entry (String.Set.add index file_proc_name) dir
      | None ->
          next_entry index dir )
  in
  match Unix.opendir Config.biabduction_models_dir with
  | dir ->
      next_entry String.Set.empty dir
  | exception Unix.Unix_error ((ENOTDIR | ENOENT), _, _) ->
      String.Set.empty


let models_index =
  lazy (if not Config.biabduction_models_mode then scan_models () else String.Set.empty)


let mem proc_name = String.Set.mem (Lazy.force models_index) (Procname.to_filename proc_name)
