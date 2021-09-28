(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging

type t =
  | HashedBuckOut of string
      (** source file only exists during build under some non-deterministic prefix; however, the
          value here has been post processed to remove non-determinism *)
  | Invalid of {ml_source_file: string}
  | Absolute of string
  | RelativeProjectRoot of string  (** path of the source file relative to the project root *)
  | RelativeProjectRootAndWorkspace of
      { workspace_rel_root: string
            (** path relative to the workspace of the project root with respect to which the source
                file was captured *)
      ; rel_path: string  (** path of the source file relative to the project root *) }
[@@deriving compare, equal, sexp_of]

module OrderedSourceFile = struct
  type nonrec t = t [@@deriving compare]
end

module Map = Caml.Map.Make (OrderedSourceFile)
module Set = Caml.Set.Make (OrderedSourceFile)

module Hash = Caml.Hashtbl.Make (struct
  type nonrec t = t

  let equal = equal

  let hash = Caml.Hashtbl.hash
end)

let project_root_real = Utils.realpath Config.project_root

let workspace_real = Option.map ~f:Utils.realpath Config.workspace

let workspace_rel_root_opt =
  Option.bind workspace_real ~f:(fun workspace_real ->
      Utils.filename_to_relative ~root:workspace_real project_root_real )


let sanitise_buck_out_gen_hashed_path =
  let hash_pattern = List.init 8 ~f:(fun _ -> "[a-f0-9]") |> String.concat in
  let prefix_pattern = "^" ^ Config.buck_out_gen ^/ hash_pattern ^/ "" in
  let regex = Re.Str.regexp prefix_pattern in
  fun path ->
    if not Config.buck_cache_mode then None
    else
      let path_without_hashed_prefix = Re.Str.replace_first regex "" path in
      if String.equal path path_without_hashed_prefix then None else Some path_without_hashed_prefix


let from_abs_path ?(warn_on_error = true) fname =
  if Filename.is_relative fname then
    L.(die InternalError) "Path '%s' is relative, when absolute path was expected." fname ;
  (* try to get realpath of source file. Use original if it fails *)
  let fname_real = try Utils.realpath ~warn_on_error fname with Unix.Unix_error _ -> fname in
  match
    Utils.filename_to_relative ~backtrack:Config.relative_path_backtrack ~root:project_root_real
      fname_real
  with
  | None when Config.buck_cache_mode && Filename.check_suffix fname_real "java" ->
      L.die InternalError "%s is not relative to %s" fname_real project_root_real
  | None ->
      (* fname_real is absolute already *)
      Absolute fname_real
  | Some rel_path -> (
    match sanitise_buck_out_gen_hashed_path rel_path with
    | Some sanitised_path ->
        HashedBuckOut sanitised_path
    | None -> (
      match workspace_rel_root_opt with
      | Some workspace_rel_root ->
          RelativeProjectRootAndWorkspace {workspace_rel_root; rel_path}
      | None ->
          RelativeProjectRoot rel_path ) )


let die_missing_workspace ~rel_path ~foreign_rel_project_root =
  L.die UserError
    "Missing workspace: please provide the --workspace option. A file (relative path: '%s') was \
     encountered whose project root at the time of capture is relative to a workspace (project \
     root: '%s'). The same workspace must be specified now."
    rel_path foreign_rel_project_root


let reroot_rel_path ~foreign_rel_project_root rel_path =
  match (workspace_real, foreign_rel_project_root) with
  | None, Some foreign_rel_project_root ->
      die_missing_workspace ~rel_path ~foreign_rel_project_root
  | Some workspace, foreign_offset_opt
    when not (Option.equal String.equal foreign_offset_opt workspace_rel_root_opt) ->
      (* re-root rel_path relative to the current project_root *)
      let offset_to_abs_path offset_opt =
        (* if the relative offset of the project root with respect to the workspace is None then
           assume the project root is relative to the workspace (with no offset), i.e. that the
           offset is [.] *)
        Option.value_map ~default:workspace offset_opt ~f:(fun offset -> workspace ^/ offset)
      in
      let abs_project_root = offset_to_abs_path workspace_rel_root_opt in
      let foreign_abs_project_root = offset_to_abs_path foreign_offset_opt in
      Option.value_exn
        (Utils.filename_to_relative ~force_full_backtrack:true ~root:abs_project_root
           foreign_abs_project_root )
      ^/ rel_path
  | _ ->
      rel_path


(** given a relative [path], find a [<hash>] such that [Config.buck_out_gen ^/ <hash> ^/ path]
    exists. return that absolute path *)
let recreate_hashed_buck_out_path_abs =
  let last_matched_hash_path = (* cached last existing absolute path to hash *) ref None in
  let buck_out_gen_abs = project_root_real ^/ Config.buck_out_gen in
  let find_file path =
    try
      Sys.readdir buck_out_gen_abs
      |> Array.find_map ~f:(fun entry ->
             let root = buck_out_gen_abs ^/ entry in
             let full_path = root ^/ path in
             if ISys.file_exists full_path then (
               last_matched_hash_path := Some root ;
               Some full_path )
             else None )
    with Sys_error _ -> None
  in
  fun path ->
    match !last_matched_hash_path with
    | Some root when ISys.file_exists (root ^/ path) ->
        (* there may exist several hashes, just pick one that works *)
        Some (root ^/ path)
    | _ ->
        find_file path


let recreate_hashed_buck_out_path_rel rel_path =
  recreate_hashed_buck_out_path_abs rel_path
  |> Option.bind ~f:(Utils.filename_to_relative ~root:project_root_real)


let to_string ?(force_relative = false) fname =
  match fname with
  | Invalid {ml_source_file} ->
      "DUMMY from " ^ ml_source_file
  | RelativeProjectRootAndWorkspace {workspace_rel_root= foreign_rel_project_root; rel_path} ->
      reroot_rel_path ~foreign_rel_project_root:(Some foreign_rel_project_root) rel_path
  | HashedBuckOut rel_path -> (
    match recreate_hashed_buck_out_path_rel rel_path with
    | Some path ->
        path
    | None ->
        Config.buck_out_gen ^/ "<hash>" ^/ rel_path )
  | RelativeProjectRoot rel_path ->
      reroot_rel_path ~foreign_rel_project_root:None rel_path
  | Absolute path ->
      if force_relative then
        let open IOption.Let_syntax in
        (let* isysroot_suffix = Config.xcode_isysroot_suffix in
         let+ pos = String.substr_index path ~pattern:isysroot_suffix in
         "${XCODE_ISYSROOT}" ^ String.subo ~pos:(pos + String.length isysroot_suffix) path )
        |> IOption.if_none_eval ~f:(fun () ->
               Option.value_exn
                 (Utils.filename_to_relative ~force_full_backtrack:true ~root:project_root_real path) )
      else path


let has_extension ~ext = function
  | Invalid {ml_source_file} ->
      L.die InternalError
        "has_extension cannot be called with Invalid source file originating in %s" ml_source_file
  | RelativeProjectRootAndWorkspace {rel_path= path}
  | HashedBuckOut path
  | RelativeProjectRoot path
  | Absolute path ->
      String.is_suffix path ~suffix:ext


let pp fmt fname = Format.pp_print_string fmt (to_string fname)

let to_abs_path fname =
  match fname with
  | Invalid {ml_source_file} ->
      L.die InternalError "cannot be called with Invalid source file originating in %s"
        ml_source_file
  | HashedBuckOut rel_path -> (
    match recreate_hashed_buck_out_path_abs rel_path with
    | None ->
        L.die InternalError "Could not recreate path under `buck-out/gen/<hash>` from %s" rel_path
    | Some abs_path ->
        abs_path )
  | RelativeProjectRoot rel_path ->
      Config.project_root ^/ rel_path
  | RelativeProjectRootAndWorkspace {workspace_rel_root; rel_path} ->
      let workspace_abs =
        match Config.workspace with
        | Some workspace ->
            workspace
        | None ->
            die_missing_workspace ~rel_path ~foreign_rel_project_root:workspace_rel_root
      in
      workspace_abs ^/ workspace_rel_root ^/ rel_path
  | Absolute path ->
      path


let to_rel_path fname =
  match fname with
  | RelativeProjectRootAndWorkspace {workspace_rel_root= foreign_rel_project_root; rel_path} ->
      reroot_rel_path ~foreign_rel_project_root:(Some foreign_rel_project_root) rel_path
  | RelativeProjectRoot rel_path ->
      reroot_rel_path ~foreign_rel_project_root:None rel_path
  | HashedBuckOut rel_path ->
      recreate_hashed_buck_out_path_rel rel_path |> Option.value ~default:rel_path
  | Absolute _ | Invalid _ ->
      to_abs_path fname


let invalid ml_source_file = Invalid {ml_source_file}

let is_invalid = function Invalid _ -> true | _ -> false

let is_under_project_root = function
  | Invalid {ml_source_file} ->
      L.die InternalError "cannot be called with Invalid source file from %s" ml_source_file
  | RelativeProjectRoot _ | RelativeProjectRootAndWorkspace _ | HashedBuckOut _ ->
      true
  | Absolute _ ->
      false


let exists_cache = String.Table.create ~size:256 ()

let path_exists abs_path =
  try String.Table.find_exn exists_cache abs_path
  with Not_found_s _ | Caml.Not_found ->
    let result = ISys.file_exists abs_path in
    String.Table.set exists_cache ~key:abs_path ~data:result ;
    result


let of_header ?(warn_on_error = true) header_file =
  let abs_path = to_abs_path header_file in
  let source_exts = ["c"; "cc"; "cpp"; "cxx"; "m"; "mm"] in
  let header_exts = ["h"; "hh"; "hpp"; "hxx"] in
  match Filename.split_extension abs_path with
  | file_no_ext, Some ext when List.mem ~equal:String.equal header_exts ext ->
      List.find_map source_exts ~f:(fun ext ->
          let possible_file = file_no_ext ^ "." ^ ext in
          if path_exists possible_file then Some (from_abs_path ~warn_on_error possible_file)
          else None )
  | _ ->
      None


let create ?(warn_on_error = true) path =
  if Filename.is_relative path then
    match (sanitise_buck_out_gen_hashed_path path, workspace_rel_root_opt) with
    | Some sanitised_path, _ ->
        HashedBuckOut sanitised_path
    | None, None ->
        RelativeProjectRoot path
    | None, Some workspace_rel_root ->
        let rel_path, new_root = Utils.normalize_path_from ~root:workspace_rel_root path in
        RelativeProjectRootAndWorkspace {workspace_rel_root= new_root; rel_path}
  else from_abs_path ~warn_on_error path


let changed_sources_from_changed_files changed_files =
  List.fold changed_files ~init:Set.empty ~f:(fun changed_files_set line ->
      try
        let source_file = create line in
        let changed_files' = Set.add source_file changed_files_set in
        (* Add source corresponding to changed header if it exists *)
        match of_header source_file with
        | Some src ->
            Set.add src changed_files'
        | None ->
            changed_files'
      with _exn -> changed_files_set )


let read_config_changed_files =
  let result =
    lazy
      ( match Config.changed_files_index with
      | None ->
          None
      | Some index -> (
        match Utils.read_file index with
        | Ok lines ->
            Some (changed_sources_from_changed_files lines)
        | Error error ->
            L.external_error "Error reading the changed files index '%s': %s@." index error ;
            None ) )
  in
  fun () -> Lazy.force result


let is_matching patterns source_file =
  let path = to_rel_path source_file in
  List.exists
    ~f:(fun pattern ->
      try Int.equal (Str.search_forward pattern path 0) 0 with Caml.Not_found -> false )
    patterns


module SQLite = struct
  type nonrec t = t

  let invalid_tag = 'I'

  let absolute_tag = 'A'

  let relative_project_root_tag = 'R'

  let hashed_buck_out_tag = 'H'

  (* to encode the pair (workspace_rel_root, rel_path), we store the length of the first element
     in-between two 'W' characters, eg 'W3Wfoo/rest/of/the/path.java' *)
  let relative_project_root_and_workspace_tag = 'W'

  let serialize sourcefile =
    let tag_text tag str = Sqlite3.Data.TEXT (Printf.sprintf "%c%s" tag str) in
    match sourcefile with
    | Invalid {ml_source_file} ->
        tag_text invalid_tag ml_source_file
    | HashedBuckOut rel_path ->
        tag_text hashed_buck_out_tag rel_path
    | Absolute abs_path ->
        tag_text absolute_tag abs_path
    | RelativeProjectRoot rel_path ->
        tag_text relative_project_root_tag rel_path
    | RelativeProjectRootAndWorkspace {workspace_rel_root= prefix; rel_path} ->
        Sqlite3.Data.TEXT
          (Printf.sprintf "%c%d%c%s/%s" relative_project_root_and_workspace_tag
             (String.length prefix) relative_project_root_and_workspace_tag prefix rel_path )


  let deserialize serialized_sourcefile =
    let[@warning "-8"] (Sqlite3.Data.TEXT text) = serialized_sourcefile in
    if String.is_empty text then
      L.die InternalError "Could not deserialize sourcefile with empty representation@." ;
    let tag = text.[0] in
    let str = String.sub ~pos:1 ~len:(String.length text - 1) text in
    if Char.equal tag invalid_tag then Invalid {ml_source_file= str}
    else if Char.equal tag absolute_tag then Absolute str
    else if Char.equal tag hashed_buck_out_tag then HashedBuckOut str
    else if Char.equal tag relative_project_root_tag then RelativeProjectRoot str
    else if Char.equal tag relative_project_root_and_workspace_tag then
      let prefix_length_str, path_with_prefix =
        String.lsplit2_exn str ~on:relative_project_root_and_workspace_tag
      in
      let prefix_length = Int.of_string prefix_length_str in
      let prefix = String.prefix path_with_prefix prefix_length in
      let rel_path = String.drop_prefix path_with_prefix (prefix_length + 1) in
      RelativeProjectRootAndWorkspace {workspace_rel_root= prefix; rel_path}
    else L.die InternalError "Could not deserialize sourcefile with tag=%c, str= %s@." tag str
end

module Normalizer = HashNormalizer.Make (struct
  type nonrec t = t [@@deriving equal]

  let hash = Hashtbl.hash

  let normalize fname =
    let string_normalize = HashNormalizer.StringNormalizer.normalize in
    match fname with
    | Invalid {ml_source_file} ->
        let ml_source_file' = string_normalize ml_source_file in
        if phys_equal ml_source_file ml_source_file' then fname
        else Invalid {ml_source_file= ml_source_file'}
    | RelativeProjectRootAndWorkspace {workspace_rel_root; rel_path} ->
        let workspace_rel_root' = string_normalize workspace_rel_root in
        let rel_path' = string_normalize rel_path in
        if phys_equal workspace_rel_root workspace_rel_root' && phys_equal rel_path rel_path' then
          fname
        else
          RelativeProjectRootAndWorkspace
            {workspace_rel_root= workspace_rel_root'; rel_path= rel_path'}
    | RelativeProjectRoot rel_path ->
        let rel_path' = string_normalize rel_path in
        if phys_equal rel_path rel_path' then fname else RelativeProjectRoot rel_path'
    | Absolute path ->
        let path' = string_normalize path in
        if phys_equal path path' then fname else Absolute path'
    | HashedBuckOut path ->
        let path' = string_normalize path in
        if phys_equal path path' then fname else HashedBuckOut path'
end)
