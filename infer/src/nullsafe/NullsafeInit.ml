(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

let load_third_party_repo ~absolute_path_to_repo_dir =
  match Sys.is_directory absolute_path_to_repo_dir with
  | `Yes -> (
    match ThirdPartyAnnotationInfoLoader.load ~path_to_repo_dir:absolute_path_to_repo_dir with
    | Ok storage ->
        storage
    | Error error ->
        Logging.die Logging.InternalError "Error while reading 3rd party annotation repo: %a"
          ThirdPartyAnnotationInfoLoader.pp_load_error error )
  | _ ->
      Logging.die Logging.InternalError
        "Could not locate 3rd party annotation repository: expected location %s"
        absolute_path_to_repo_dir


let get_absolute_path_to_repo_dir path_to_repo_dir =
  if Filename.is_absolute path_to_repo_dir then
    (* By agreement, this means absolute path *)
    path_to_repo_dir
  else
    (* By agreement, this means path relative to inferconfig dir *)
    match Config.inferconfig_dir with
    | None ->
        Logging.die Logging.InternalError
          "Could not locate .inferconfig directory, which is required for resolving the path to \
           third party annotation repository"
    | Some inferconfig_dir ->
        inferconfig_dir ^/ path_to_repo_dir


let create_global_storage () =
  match Config.nullsafe_third_party_signatures with
  | Some path_to_repo_dir ->
      load_third_party_repo
        ~absolute_path_to_repo_dir:(get_absolute_path_to_repo_dir path_to_repo_dir)
  (* Create empty *)
  | None ->
      ThirdPartyAnnotationInfo.create_storage ()


let init () =
  let global_storage = create_global_storage () in
  ThirdPartyAnnotationGlobalRepo.initialize global_storage
