(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

type load_error = {filename: string; parsing_error: ThirdPartyAnnotationInfo.file_parsing_error}

let pp_load_error fmt {filename; parsing_error} =
  Format.fprintf fmt "Could not parse %s: %a" filename ThirdPartyAnnotationInfo.pp_parsing_error
    parsing_error


let add_from_file storage ~path_to_repo_dir ~sig_file =
  let lines = In_channel.read_lines (path_to_repo_dir ^ "/" ^ sig_file) in
  ThirdPartyAnnotationInfo.add_from_signature_file storage ~lines
  |> Result.map_error ~f:(fun parsing_error -> {filename= sig_file; parsing_error})
  |> Result.bind ~f:(fun _ -> Ok storage)


let load ~path_to_repo_dir =
  (* Sequentally load information from all .sig files *)
  Sys.ls_dir path_to_repo_dir
  |> List.filter ~f:(String.is_suffix ~suffix:".sig")
  |> List.fold_result ~init:(ThirdPartyAnnotationInfo.create_storage ())
       ~f:(fun accumulated_storage sig_file ->
         add_from_file accumulated_storage ~path_to_repo_dir ~sig_file )
