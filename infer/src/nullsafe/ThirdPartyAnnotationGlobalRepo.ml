(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type repo_with_metadata =
  {repo: ThirdPartyAnnotationInfo.storage; absolute_path_to_repo: string option}

let repo_with_metadata = ref None

let initialize ~absolute_path_to_repo repo =
  match !repo_with_metadata with
  | None ->
      repo_with_metadata := Some {absolute_path_to_repo; repo}
  | Some _ ->
      Logging.die Logging.InternalError "Attempt to initialize global 3rd party repository twice"


let get_repo_with_metadata () =
  match !repo_with_metadata with
  | None ->
      Logging.die Logging.InternalError "Attempt to access not initialized 3rd party repository"
  | Some repo_with_metadata ->
      repo_with_metadata


let get_repo () = (get_repo_with_metadata ()).repo

let get_absolute_path_to_repo () = (get_repo_with_metadata ()).absolute_path_to_repo

let get_last_2_dirs path =
  let dirs = String.split path ~on:'/' in
  let last_2_dirs = List.drop dirs (List.length dirs - 2) in
  String.concat last_2_dirs ~sep:"/"


let get_user_friendly_third_party_sig_file_name ~filename =
  (* Enough details that will hint the user to where to look for the file. *)
  let absolute_path_opt = get_absolute_path_to_repo () in
  (* If we got file_name from somewhere, it definitely means the repo exists. *)
  let absolute_path = Option.value_exn absolute_path_opt in
  (* Take last 2 dirs: the last one is one with the folder itself, and the previous will indicate
     the location *)
  get_last_2_dirs absolute_path ^ "/" ^ filename
