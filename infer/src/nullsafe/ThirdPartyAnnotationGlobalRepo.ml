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


let get_from_absolute_path () =
  (* If we got file_name from somewhere, it definitely means the repo exists. *)
  Option.value_exn (get_absolute_path_to_repo ())
  (* Enough details to have an idea where to look *)
  |> get_last_2_dirs


let get_user_friendly_third_party_sig_file_name ~filename =
  let path =
    Config.nullsafe_third_party_location_for_messaging_only
    |> Option.value ~default:(get_from_absolute_path ())
  in
  path ^ "/" ^ filename
