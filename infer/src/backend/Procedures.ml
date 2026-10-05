(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging

let get_all ~filter () =
  let query_str = "SELECT proc_attributes FROM procedures" in
  let adb = Database.get_database AnalysisDatabase in
  let cdb = Database.get_database CaptureDatabase in
  let adb_stmt = Sqlite3.prepare adb query_str in
  let cdb_stmt = Sqlite3.prepare cdb query_str in
  let run_query_fold db log stmt init =
    SqliteUtils.result_fold_rows db ~log stmt ~init ~f:(fun rev_results stmt ->
        let attrs = Sqlite3.column stmt 0 |> ProcAttributes.SQLite.deserialize in
        let source_file = attrs.ProcAttributes.translation_unit in
        let proc_name = ProcAttributes.get_proc_name attrs in
        if filter source_file proc_name then proc_name :: rev_results else rev_results )
  in
  run_query_fold cdb "reading all procedure names capturedb" cdb_stmt []
  |> run_query_fold adb "reading all procedure names analysisdb" adb_stmt


let compute_procs_defined_in_changed_headers changed_files =
  let headers =
    (* a deleted header defines no procedure, but with [--suffix-match-changed-files] the entries
       can be partial paths *)
    SourceFile.Set.filter
      (fun file ->
        SourceFile.is_header file
        && (Config.suffix_match_changed_files || ISys.file_exists (SourceFile.to_abs_path file))
        && not (SourceFiles.mem file) )
      changed_files
  in
  if SourceFile.Set.is_empty headers then Procname.Map.empty
  else
    let db = Database.get_database CaptureDatabase in
    let stmt = Sqlite3.prepare db "SELECT proc_attributes FROM procedures WHERE cfg IS NOT NULL" in
    let header_procs =
      SqliteUtils.result_fold_single_column_rows db ~log:"reading procedure locations" stmt ~init:[]
        ~f:(fun procs data ->
          let attrs = ProcAttributes.SQLite.deserialize data in
          if SourceFile.is_changed ~changed_files:headers attrs.loc.file then attrs :: procs
          else procs )
    in
    let files_with_procs =
      List.map header_procs ~f:(fun {ProcAttributes.loc} -> loc.file) |> SourceFile.Set.of_list
    in
    let headers_without_procs =
      SourceFile.Set.filter
        (fun header ->
          not
            (SourceFile.Set.exists
               (SourceFile.is_changed ~changed_files:(SourceFile.Set.singleton header))
               files_with_procs ) )
        headers
      |> SourceFile.Set.elements
    in
    if not (List.is_empty headers_without_procs) then
      if List.is_empty Config.clang_compilation_dbs then
        L.debug Analysis Quiet "Changed headers that define no captured procedure: %a@\n"
          (Pp.seq ~sep:", " SourceFile.pp) headers_without_procs
      else
        L.user_warning
          "Changed headers that define no captured procedure: %a. With --compilation-database, \
           only the files in the changed files index are captured: to analyze the procedures \
           defined in a header, add a source file that includes it to the index.@."
          (Pp.seq ~sep:", " SourceFile.pp) headers_without_procs ;
    List.fold header_procs ~init:Procname.Map.empty
      ~f:(fun procs {ProcAttributes.proc_name; translation_unit} ->
        Procname.Map.add proc_name translation_unit procs )


let get_procs_defined_in_changed_headers =
  let cache = ref (SourceFile.Set.empty, Procname.Map.empty) in
  fun changed_files ->
    let cached_changed_files, procs = !cache in
    if SourceFile.Set.equal cached_changed_files changed_files then procs
    else
      let procs = compute_procs_defined_in_changed_headers changed_files in
      cache := (changed_files, procs) ;
      procs


let select_proc_names_interactive ~filter =
  let proc_names = get_all ~filter () |> List.rev in
  let proc_names_len = List.length proc_names in
  match (proc_names, Config.select) with
  | [], _ ->
      F.eprintf "No procedures found" ;
      None
  | _, Some (`Select n) when n >= proc_names_len ->
      L.die UserError "Cannot select result #%d out of only %d procedures" n proc_names_len
  | [proc_name], _ ->
      F.eprintf "Selected proc name: %a@." Procname.pp proc_name ;
      Some proc_names
  | _, Some `All ->
      Some proc_names
  | _, Some (`Select n) ->
      let proc_names_array = List.to_array proc_names in
      Some [proc_names_array.(n)]
  | _, None ->
      let proc_names_array = List.to_array proc_names in
      Array.iteri proc_names_array ~f:(fun i proc_name ->
          F.eprintf "%d: %a@\n" i Procname.pp proc_name ) ;
      let rec ask_user_input () =
        F.eprintf "Select one number (type 'a' for selecting all, 'q' for quit): " ;
        Out_channel.flush stderr ;
        let input = String.strip In_channel.(input_line_exn stdin) in
        if String.equal (String.lowercase input) "a" then Some proc_names
        else if String.equal (String.lowercase input) "q" then (
          F.eprintf "Quit interactive mode" ;
          None )
        else
          match int_of_string_opt input with
          | Some n when 0 <= n && n < Array.length proc_names_array ->
              Some [proc_names_array.(n)]
          | _ ->
              F.eprintf "Invalid input" ;
              ask_user_input ()
      in
      ask_user_input ()


let pp_all ~filter ~proc_name:proc_name_cond ~defined ~source_file:source_file_cond ~proc_attributes
    ~proc_cfg ~callees fmt () =
  let db = Database.get_database CaptureDatabase in
  let deserialize_bool_int = function
    | Sqlite3.Data.INT int64 -> (
      match Int64.to_int_exn int64 with 0 -> false | _ -> true )
    | _ ->
        L.die InternalError "deserialize_int"
  in
  let pp_if ?(new_line = false) condition title pp fmt x =
    if condition then (
      if new_line then F.fprintf fmt "@[<v2>" else F.fprintf fmt "@[<h>" ;
      F.fprintf fmt "%s:@ %a@]@;" title pp x )
  in
  let pp_column_if stmt ?new_line condition title deserialize pp fmt column =
    if condition then
      (* repeat the [condition] check so that we do not deserialize if there's nothing to do *)
      pp_if ?new_line condition title pp fmt (Sqlite3.column stmt column |> deserialize)
  in
  let pp_row stmt fmt source_file proc_name =
    let[@warning "-partial-match"] (Sqlite3.Data.TEXT proc_uid) = Sqlite3.column stmt 0 in
    let dump_cfg fmt cfg_opt =
      match cfg_opt with
      | None ->
          F.pp_print_string fmt "not found"
      | Some cfg ->
          let path = DotCfg.emit_proc_desc source_file cfg in
          F.fprintf fmt "'%s'" path
    in
    F.fprintf fmt "@[<v2>%s@,%a%a%a%a%a%a@]@\n" proc_uid
      (pp_if source_file_cond "source_file" SourceFile.pp)
      source_file
      (pp_if proc_name_cond "proc_name" Procname.pp_verbose)
      proc_name
      (pp_column_if stmt defined "defined" deserialize_bool_int Bool.pp)
      1
      (pp_column_if stmt ~new_line:true proc_attributes "attributes"
         ProcAttributes.SQLite.deserialize ProcAttributes.pp )
      2
      (pp_column_if stmt ~new_line:true proc_cfg "control-flow graph" Procdesc.SQLite.deserialize
         dump_cfg )
      3
      (pp_column_if stmt ~new_line:false callees "callees" Procname.SQLiteList.deserialize
         (Pp.seq ~sep:", " Procname.pp) )
      4
  in
  (* we could also register this statement but it's typically used only once per run so just prepare
     it inside the function *)
  Sqlite3.prepare db
    {|
       SELECT
         proc_uid,
         cfg IS NOT NULL,
         proc_attributes,
         cfg,
         callees
       FROM procedures ORDER BY proc_uid
    |}
  |> Container.iter ~fold:(SqliteUtils.result_fold_rows db ~log:"print all procedures")
       ~f:(fun stmt ->
         let attrs = Sqlite3.column stmt 2 |> ProcAttributes.SQLite.deserialize in
         let proc_name = ProcAttributes.get_proc_name attrs in
         let source_file = attrs.ProcAttributes.translation_unit in
         if filter source_file proc_name then pp_row stmt fmt source_file proc_name )
