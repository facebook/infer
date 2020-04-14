(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

type id = Temporary [@@deriving enumerate]

type cleanup_action = Delete | Keep [@@deriving equal]

type entry_kind = Directory

type t =
  { rel_path: string  (** path inside infer-out/ *)
  ; kind: entry_kind
  ; before_incremental_analysis: cleanup_action
        (** whether this should be deleted before an incremental analysis *)
  ; before_caching_capture: cleanup_action
        (** whether this should be deleted before sending to a remote cache for the capture phase,
            e.g., a distributed Buck cache. *) }

let of_id = function
  | Temporary ->
      { rel_path= "tmp"
      ; kind= Directory
      ; before_incremental_analysis= Keep
      ; before_caching_capture= Delete }


let path_of_entry ~results_dir {rel_path; _} = results_dir ^/ rel_path

let get_path ~results_dir id = path_of_entry ~results_dir (of_id id)

let get_filtered_paths ~results_dir ~f =
  List.filter_map all_of_id ~f:(fun id ->
      let entry = of_id id in
      if f entry then Some (path_of_entry ~results_dir entry) else None )


let to_delete_before_incremental_capture_and_analysis ~results_dir =
  get_filtered_paths ~results_dir ~f:(fun {before_incremental_analysis; _} ->
      equal_cleanup_action before_incremental_analysis Delete )


let to_delete_before_caching_capture ~results_dir =
  get_filtered_paths ~results_dir ~f:(fun {before_caching_capture; _} ->
      equal_cleanup_action before_caching_capture Delete )
