(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging
module F = Format

module ServerSocket = struct
  let socket_name = ResultsDirEntryName.db_writer_socket_name

  let socket_addr = Unix.ADDR_UNIX socket_name

  let socket_domain = Unix.domain_of_sockaddr socket_addr

  (** Unix socket *paths* have a historical length limit of ~100 chars (!?*@&*$). However, this only
      applies to the argument passed in the system call to create the socket, not to the actual
      path. Thus a workaround is to cd into the parent dir of the socket and then use it, hence this
      function. *)
  let in_results_dir ~f = Utils.do_in_dir ~dir:Config.toplevel_results_dir ~f

  let socket_exists () = in_results_dir ~f:(fun () -> Sys.file_exists_exn socket_name)

  (* Error recuperation is done by attempting this function at module initialization time, and
     not using DbWriter at all in case it fails. See {!can_use_socket} below. *)
  let setup_socket () =
    if socket_exists () then L.die InternalError "Sqlite write daemon: socket already exists@." ;
    let socket = Unix.socket ~domain:socket_domain ~kind:SOCK_STREAM ~protocol:0 () in
    in_results_dir ~f:(fun () -> Unix.bind socket ~addr:socket_addr) ;
    (* [backlog] is (supposedly) the length of the queue for pending connections ;
       there are no rules about the implied behaviour though.  Here use optimistically
       the number of workers, though even that is a guess. *)
    Unix.listen socket ~backlog:Config.jobs ;
    socket


  let remove_socket_file () =
    in_results_dir ~f:(fun () -> if socket_exists () then Unix.unlink socket_name)


  let remove_socket socket =
    in_results_dir ~f:(fun () -> Unix.close socket) ;
    remove_socket_file ()


  (* Check whether we can create a socket to communicate with the asynchronous DBWriter process. *)
  let can_use_socket () =
    try
      (* This function is called very early, and the infer-out directory may not have been created
         yet. Ensure it exists before attempting to create the socket. *)
      Unix.mkdir_p Config.toplevel_results_dir ;
      let socket = setup_socket () in
      remove_socket socket ;
      true
    with _ -> false
end

let default_use_daemon =
  lazy
    (let is_windows =
       match Version.build_platform with Windows -> true | Linux | Darwin -> false
     in
     Config.((not is_windows) && dbwriter && (not (buck || genrule_mode)) && jobs > 1)
     &&
     (* Only the main process should try detecting whether the socket can be created.
        Otherwise, re-spawned Infer will try to create a socket on top of the existing one. *)
     if Config.is_originator then (
       let socket_ok = ServerSocket.can_use_socket () in
       if not socket_ok then
         L.user_warning
           "Cannot setup the socket to communicate with the database daemon. Performance will be \
            impacted. Do you have enough rights to create a Unix socket in directory '%s'?@."
           Config.toplevel_results_dir ;
       socket_ok )
     else ServerSocket.socket_exists () )


let override_daemon_ref = ref None

let override_use_daemon should_use = override_daemon_ref := Some should_use

let use_daemon () =
  Option.value_or_thunk !override_daemon_ref ~default:(fun () -> Lazy.force default_use_daemon)


module Implementation = struct
  let add_source_file =
    let source_file_store_statement =
      Database.register_statement CaptureDatabase
        {|
          INSERT OR REPLACE INTO source_files
          VALUES (:source, :tenv, :integer_type_widths, :proc_names, :freshly_captured)
        |}
    in
    fun ~source_file ~tenv ~integer_type_widths ~proc_names ->
      Database.with_registered_statement source_file_store_statement ~f:(fun db store_stmt ->
          Sqlite3.bind store_stmt 1 source_file
          (* :source *)
          |> SqliteUtils.check_result_code db ~log:"store bind source file" ;
          Sqlite3.bind store_stmt 2 tenv
          (* :tenv *)
          |> SqliteUtils.check_result_code db ~log:"store bind type environment" ;
          Sqlite3.bind store_stmt 3 integer_type_widths
          (* :integer_type_widths *)
          |> SqliteUtils.check_result_code db ~log:"store bind integer type widths" ;
          Sqlite3.bind store_stmt 4 proc_names
          (* :proc_names *)
          |> SqliteUtils.check_result_code db ~log:"store bind proc names" ;
          Sqlite3.bind store_stmt 5 (Sqlite3.Data.INT Int64.one)
          (* :freshly_captured *)
          |> SqliteUtils.check_result_code db ~log:"store freshness" ;
          SqliteUtils.result_unit ~finalize:false ~log:"Cfg.store" db store_stmt )


  let canonicalize () =
    Database.get_database CaptureDatabase
    |> SqliteUtils.exec ~log:"checkpointing" ~stmt:"PRAGMA wal_checkpoint(TRUNCATE)"


  let delete_all_specs () =
    let db = Database.get_database AnalysisDatabase in
    SqliteUtils.exec ~log:"drop specs table" ~stmt:"DELETE FROM specs" db ;
    SqliteUtils.exec ~log:"drop issue_logs table" ~stmt:"DELETE FROM issue_logs" db


  let delete_attributes =
    let delete_statement =
      Database.register_statement CaptureDatabase "DELETE FROM procedures WHERE proc_uid = :k"
    in
    fun ~proc_uid ->
      Database.with_registered_statement delete_statement ~f:(fun db delete_stmt ->
          Sqlite3.bind delete_stmt 1 (Sqlite3.Data.TEXT proc_uid)
          |> SqliteUtils.check_result_code db ~log:"delete attrs bind proc_uid" ;
          SqliteUtils.result_unit ~finalize:false ~log:"delete attrs" db delete_stmt )


  let delete_issue_logs =
    let delete_statement =
      Database.register_statement AnalysisDatabase "DELETE FROM issue_logs WHERE source_file = :k"
    in
    fun ~source_file ->
      Database.with_registered_statement delete_statement ~f:(fun db delete_stmt ->
          Sqlite3.bind delete_stmt 1 source_file
          |> SqliteUtils.check_result_code db ~log:"delete issue_logs bind source_file" ;
          SqliteUtils.result_unit ~finalize:false ~log:"delete issue_logs" db delete_stmt )


  let delete_specs =
    let delete_statement =
      Database.register_statement AnalysisDatabase "DELETE FROM specs WHERE proc_uid = :k"
    in
    let delete_spec ~proc_uid =
      Database.with_registered_statement delete_statement ~f:(fun db delete_stmt ->
          Sqlite3.bind delete_stmt 1 (Sqlite3.Data.TEXT proc_uid)
          |> SqliteUtils.check_result_code db ~log:"delete spec bind proc_uid" ;
          SqliteUtils.result_unit ~finalize:false ~log:"delete spec" db delete_stmt )
    in
    fun ~proc_uids ->
      SqliteUtils.transaction (Database.get_database AnalysisDatabase) ~f:(fun () ->
          List.iter proc_uids ~f:(fun proc_uid -> delete_spec ~proc_uid) )


  let mark_all_source_files_stale () =
    Database.get_database CaptureDatabase
    |> SqliteUtils.exec ~stmt:"UPDATE source_files SET freshly_captured = 0" ~log:"mark_all_stale"


  let merge_procedures_table ~db ~to_db ~db_file =
    (* Do the merge purely in SQL for great speed. The query works by doing a left join between the
       sub-table and the main one, and applying the same "more defined" logic as in [replace_attributes] in the
       cases where a proc_name is present in both the sub-table and the main one (main.proc_uid !=
       NULL). All the rows that pass this filter are inserted/updated into the main table. *)
    Database.get_database db
    |> SqliteUtils.exec
         ~log:(Printf.sprintf "copying procedures of database '%s'" db_file)
         ~stmt:
           (Printf.sprintf
              {|
                INSERT OR REPLACE INTO %s.procedures
                SELECT
                  sub.proc_uid,
                  sub.proc_attributes,
                  sub.cfg,
                  sub.callees
                FROM (
                  attached.procedures AS sub
                  LEFT OUTER JOIN %s.procedures AS main
                  ON sub.proc_uid = main.proc_uid )
                WHERE
                  main.proc_uid IS NULL
                  OR
                  (main.cfg IS NOT NULL) < (sub.cfg IS NOT NULL)
                  OR
                  ((main.cfg IS NULL) = (sub.cfg IS NULL) AND main.proc_attributes < sub.proc_attributes)
              |}
              to_db to_db )


  let merge_captures ~root ~infer_deps_file =
    let merge_source_files_table ~db_file =
      Database.get_database CaptureDatabase
      |> SqliteUtils.exec
           ~log:(Printf.sprintf "copying source_files of database '%s'" db_file)
           ~stmt:
             {|
              INSERT OR REPLACE INTO memdb.source_files
              SELECT source_file, type_environment, integer_type_widths, procedure_names, 1
              FROM attached.source_files
            |}
    in
    let merge_db infer_out_src =
      let db_file = ResultsDirEntryName.get_path ~results_dir:infer_out_src CaptureDB in
      if not (ISys.file_exists db_file) then
        L.die InternalError "Tried to merge in DB at %s but path does not exist.@\n" db_file ;
      let main_db = Database.get_database CaptureDatabase in
      SqliteUtils.with_attached_db main_db ~db_file ~db_name:"attached" ~f:(fun () ->
          merge_procedures_table ~db:CaptureDatabase ~to_db:"memdb" ~db_file ;
          merge_source_files_table ~db_file )
    in
    let copy_to_main db =
      SqliteUtils.exec db ~log:"Copying procedures into main db"
        ~stmt:"INSERT OR REPLACE INTO procedures SELECT * FROM memdb.procedures" ;
      SqliteUtils.exec db ~log:"Copying source_files into main db"
        ~stmt:"INSERT OR REPLACE INTO source_files SELECT * FROM memdb.source_files"
    in
    let main_db = Database.get_database CaptureDatabase in
    SqliteUtils.with_attached_db main_db ~db_file:":memory:" ~db_name:"memdb" ~f:(fun () ->
        Database.create_tables ~prefix:"memdb." main_db CaptureDatabase ;
        Utils.iter_infer_deps infer_deps_file ~root ~f:merge_db ;
        copy_to_main main_db )


  let merge_summaries infer_outs =
    let merge_specs_table ~db_file =
      Database.get_database AnalysisDatabase
      |> SqliteUtils.exec
           ~log:(Printf.sprintf "copying specs of database '%s'" db_file)
           ~stmt:
             (Printf.sprintf
                {|
                  INSERT OR REPLACE INTO specs
                  SELECT
                    sub.proc_uid,
                    sub.proc_name,
                    sub.report_summary,
                    sub.summary_metadata,
                    %s
                  FROM (
                    attached.specs AS sub
                    LEFT OUTER JOIN specs AS main
                    ON sub.proc_uid = main.proc_uid )
                  WHERE
                    main.proc_uid IS NULL
                  OR
                    main.report_summary >= sub.report_summary
                |}
                ( PayloadId.database_fields
                |> List.map ~f:(fun s -> "sub." ^ s)
                |> String.concat ~sep:", " ) )
    in
    let merge_issues_table ~db_file =
      Database.get_database AnalysisDatabase
      |> SqliteUtils.exec
           ~log:(Printf.sprintf "copying issues of database '%s'" db_file)
           ~stmt:
             {|
                INSERT OR REPLACE INTO issue_logs
                SELECT
                  sub.checker,
                  sub.source_file,
                  sub.issue_log
                FROM (
                  attached.issue_logs AS sub
                  LEFT OUTER JOIN issue_logs AS main
                  ON (sub.checker = main.checker AND sub.source_file = main.source_file) )
                WHERE
                  main.checker IS NULL
                OR
                  main.source_file IS NULL
                OR
                  main.issue_log >= sub.issue_log
              |}
    in
    let main_db = Database.get_database AnalysisDatabase in
    List.iter infer_outs ~f:(fun results_dir ->
        let db_file = ResultsDirEntryName.get_path ~results_dir AnalysisDB in
        SqliteUtils.with_attached_db main_db ~db_file ~db_name:"attached" ~f:(fun () ->
            merge_procedures_table ~db:AnalysisDatabase ~to_db:"main" ~db_file ;
            merge_specs_table ~db_file ;
            merge_issues_table ~db_file ) )


  let replace_attributes =
    let do_attribute_replace_statement db =
      (* The innermost SELECT returns 1 iff there is a row with the same procedure uid but which is strictly
         more defined than the one we are trying to store. This is either because the stored proc has a CFG
         and ours doesn't, or because the procedures are equally (un)defined but the attributes of the stored
         one are lexicographically smaller. The latter is used purely to impose determinism.

         The outermost operation will insert or replace the given values only if the innermost query returns
         nothing. *)
      (* TRICK: older versions of sqlite (prior to version 3.15.0 (2016-10-14)) do not support row
         values so the lexicographic ordering for (:cgf, :proc_attributes) is done by hand *)
      Database.register_statement db
        {|
          INSERT OR REPLACE INTO procedures
          SELECT :uid, :pattr, :cfg, :callees
          WHERE NOT EXISTS
          (
            SELECT 1
            FROM procedures
            WHERE proc_uid = :uid
            AND (
                  (:cfg IS NOT NULL) < (cfg IS NOT NULL)
                  OR
                  ((:cfg IS NOT NULL) = (cfg IS NOT NULL) AND :pattr < proc_attributes)
            )
          )
        |}
    in
    let attribute_replace_statement_adb = do_attribute_replace_statement AnalysisDatabase in
    let attribute_replace_statement_cdb = do_attribute_replace_statement CaptureDatabase in
    fun ~proc_uid ~proc_attributes ~cfg ~callees ~analysis ->
      let run_query stmt =
        Database.with_registered_statement stmt ~f:(fun db replace_stmt ->
            Sqlite3.bind replace_stmt 1 (* :proc_uid *) (Sqlite3.Data.TEXT proc_uid)
            |> SqliteUtils.check_result_code db ~log:"replace bind proc_uid" ;
            Sqlite3.bind replace_stmt 2 (* :pattr *) proc_attributes
            |> SqliteUtils.check_result_code db ~log:"replace bind proc proc_attributes" ;
            Sqlite3.bind replace_stmt 3 (* :cfg *) cfg
            |> SqliteUtils.check_result_code db ~log:"replace bind cfg" ;
            Sqlite3.bind replace_stmt 4 (* :callees *) callees
            |> SqliteUtils.check_result_code db ~log:"replace bind callees" ;
            SqliteUtils.result_unit db ~finalize:false ~log:"replace_attributes" replace_stmt )
      in
      if analysis then run_query attribute_replace_statement_adb
      else run_query attribute_replace_statement_cdb


  (** drop everything except reports *)
  let shrink_analysis_db () =
    let db = Database.get_database AnalysisDatabase in
    SqliteUtils.exec db ~log:"nullify analysis summaries"
      ~stmt:
        (Printf.sprintf "UPDATE specs SET summary_metadata=NULL, %s"
           (F.asprintf "%a"
              (Pp.seq ~sep:", " (fun fmt payload_name -> F.fprintf fmt "%s=NULL" payload_name))
              PayloadId.database_fields ) ) ;
    SqliteUtils.exec db ~log:"vacuum analysis database" ~stmt:"VACUUM" ;
    SqliteUtils.exec db ~log:"checkpointing" ~stmt:"PRAGMA wal_checkpoint(TRUNCATE)"


  let store_issue_log =
    let store_statement =
      Database.register_statement AnalysisDatabase
        {|
          INSERT OR REPLACE INTO issue_logs
          VALUES (:checker, :source_file, :issue_log)
        |}
    in
    fun ~source_file ~checker ~issue_log ->
      Database.with_registered_statement store_statement ~f:(fun db store_stmt ->
          Sqlite3.bind store_stmt 1 (Sqlite3.Data.TEXT checker)
          |> SqliteUtils.check_result_code db ~log:"store issuelog bind checker" ;
          Sqlite3.bind store_stmt 2 source_file
          |> SqliteUtils.check_result_code db ~log:"store issuelog bind source_file" ;
          Sqlite3.bind store_stmt 3 issue_log
          |> SqliteUtils.check_result_code db ~log:"store issuelog bind issue_log" ;
          SqliteUtils.result_unit ~finalize:false ~log:"store issuelog" db store_stmt )


  module IntHash = Caml.Hashtbl.Make (Int)

  let specs_overwrite_counts =
    (* We don't want to keep all [proc_uid]s in memory just to keep an overwrite count,
       so use a table keyed on their integer hashes; collisions will just lead to some noise. *)
    IntHash.create 10


  let select_report_summary =
    let select_statement =
      Database.register_statement AnalysisDatabase
        "SELECT report_summary FROM specs WHERE proc_uid = :proc_uid"
    in
    fun ~proc_uid ->
      Database.with_registered_statement select_statement ~f:(fun db select_stmt ->
          Sqlite3.bind_name select_stmt ":proc_uid" (TEXT proc_uid)
          |> SqliteUtils.check_result_code db ~log:"select report summary proc_uid" ;
          SqliteUtils.result_option ~finalize:false ~log:"select report summary"
            ~read_row:(fun stmt -> Sqlite3.column stmt 0)
            db select_stmt )


  let select_pulse_spec =
    let select_statement =
      Database.register_statement AnalysisDatabase
        {|
          SELECT report_summary, summary_metadata, %s FROM specs
          WHERE proc_uid = :proc_uid
        |}
        PayloadId.Variants.pulse.Variant.name
    in
    fun ~proc_uid ->
      Database.with_registered_statement select_statement ~f:(fun db select_stmt ->
          Sqlite3.bind_name select_stmt ":proc_uid" (TEXT proc_uid)
          |> SqliteUtils.check_result_code db ~log:"select pulse spec proc_uid" ;
          SqliteUtils.result_option ~finalize:false ~log:"select pulse spec"
            ~read_row:(fun stmt ->
              let report_summary = Sqlite3.column stmt 0 in
              let summary_metadata = Sqlite3.column stmt 1 in
              let pulse_payload = Sqlite3.column stmt 2 in
              (report_summary, summary_metadata, pulse_payload) )
            db select_stmt )


  let store_sql_time = ref ExecutionDuration.zero

  let store_spec =
    let insert_or_replace_statement =
      Database.register_statement AnalysisDatabase
        {|
          INSERT OR REPLACE INTO specs
          VALUES (:proc_uid, :proc_name, :report_summary, :summary_metadata, %s)
        |}
        (F.asprintf "%a"
           (Pp.seq ~sep:", " (fun fmt payload_name -> F.fprintf fmt ":%s" payload_name))
           PayloadId.database_fields )
    in
    let update_statement =
      let stmts =
        List.fold PayloadId.database_fields ~init:String.Map.empty ~f:(fun acc payload_id ->
            String.Map.add_exn ~key:payload_id
              ~data:
                (Database.register_statement AnalysisDatabase
                   {|
                     UPDATE specs SET
                       report_summary = :report_summary,
                       summary_metadata = :summary_metadata,
                       %s = :value
                     WHERE proc_uid = :proc_uid
                   |}
                   payload_id )
              acc )
      in
      fun payload_id -> String.Map.find_exn stmts (PayloadId.Variants.to_name payload_id)
    in
    fun analysis_req ~proc_uid ~proc_name ~merge_pulse_payload ~merge_report_summary
        ~merge_summary_metadata ->
      let proc_uid_hash = String.hash proc_uid in
      IntHash.find_opt specs_overwrite_counts proc_uid_hash
      |> Option.value_map ~default:0 ~f:(( + ) 1)
      (* [default] is 0 as we are only counting overwrites *)
      |> IntHash.replace specs_overwrite_counts proc_uid_hash ;
      let now = ExecutionDuration.counter () in
      let f () =
        let found_old, old_report_summary, old_summary_metadata, old_pulse_payload =
          match select_pulse_spec ~proc_uid with
          | Some (report_summary, summary_metadata, pulse_payload) ->
              (true, Some report_summary, Some summary_metadata, Some pulse_payload)
          | None ->
              (false, None, None, None)
        in
        let report_summary = merge_report_summary ~old_report_summary in
        let summary_metadata = merge_summary_metadata ~old_summary_metadata in
        let payloads = merge_pulse_payload ~old_pulse_payload in
        let stmt, binds =
          match (analysis_req : AnalysisRequest.t) with
          | One payload_id when found_old ->
              ( update_statement payload_id
              , [ report_summary
                ; summary_metadata
                ; List.nth_exn payloads (PayloadId.Variants.to_rank payload_id)
                ; Sqlite3.Data.TEXT proc_uid ] )
          | One _ | All | CheckerWithoutPayload _ ->
              ( insert_or_replace_statement
              , Sqlite3.Data.TEXT proc_uid :: proc_name :: report_summary :: summary_metadata
                :: payloads )
        in
        Database.with_registered_statement stmt ~f:(fun db store_stmt ->
            Sqlite3.bind_values store_stmt binds
            |> SqliteUtils.check_result_code db ~log:"store spec bind_values" ;
            SqliteUtils.result_unit ~finalize:false ~log:"store spec" db store_stmt )
      in
      if use_daemon () then f ()
      else SqliteUtils.transaction ~immediate:true (Database.get_database AnalysisDatabase) ~f ;
      store_sql_time := ExecutionDuration.add_duration_since !store_sql_time now


  let terminate () =
    let overwrites = IntHash.fold (fun _hash count acc -> acc + count) specs_overwrite_counts 0 in
    ScubaLogging.log_count ~label:"overwritten_specs" ~value:overwrites ;
    L.debug Analysis Quiet "Detected %d spec overwrites.@\n" overwrites


  let update_report_summary =
    let store_statement =
      Database.register_statement AnalysisDatabase
        "UPDATE specs SET report_summary = :report_summary WHERE proc_uid = :proc_uid"
    in
    fun ~proc_uid ~merge_report_summary ->
      let now = ExecutionDuration.counter () in
      let f () =
        let old_report_summary = select_report_summary ~proc_uid in
        let report_summary = merge_report_summary ~old_report_summary in
        Database.with_registered_statement store_statement ~f:(fun db store_stmt ->
            Sqlite3.bind_values store_stmt [report_summary; Sqlite3.Data.TEXT proc_uid]
            |> SqliteUtils.check_result_code db ~log:"store report summary bind_values" ;
            SqliteUtils.result_unit ~finalize:false ~log:"store report summary" db store_stmt )
      in
      if use_daemon () then f ()
      else SqliteUtils.transaction ~immediate:true (Database.get_database AnalysisDatabase) ~f ;
      store_sql_time := ExecutionDuration.add_duration_since !store_sql_time now
end

module Command = struct
  type t =
    | AddSourceFile of
        { source_file: Sqlite3.Data.t
        ; tenv: Sqlite3.Data.t
        ; integer_type_widths: Sqlite3.Data.t
        ; proc_names: Sqlite3.Data.t }
    | Checkpoint
    | DeleteAllSpecs
    | DeleteAttributes of {proc_uid: string}
    | DeleteIssueLogs of {source_file: Sqlite3.Data.t}
    | DeleteSpecs of {proc_uids: string list}
    | Handshake
    | MarkAllSourceFilesStale
    | MergeCaptures of {root: string; infer_deps_file: string}
    | MergeSummaries of {infer_outs: string list}
    | ReplaceAttributes of
        { proc_uid: string
        ; proc_attributes: Sqlite3.Data.t
        ; cfg: Sqlite3.Data.t
        ; callees: Sqlite3.Data.t
        ; analysis: bool }
    | ShrinkAnalysisDB
    | StoreIssueLog of {checker: string; source_file: Sqlite3.Data.t; issue_log: Sqlite3.Data.t}
    | StoreSpec of
        { analysis_req: AnalysisRequest.t
        ; proc_uid: string
        ; proc_name: Sqlite3.Data.t
        ; merge_pulse_payload: old_pulse_payload:Sqlite3.Data.t option -> Sqlite3.Data.t list
        ; merge_report_summary: old_report_summary:Sqlite3.Data.t option -> Sqlite3.Data.t
        ; merge_summary_metadata: old_summary_metadata:Sqlite3.Data.t option -> Sqlite3.Data.t }
    | Terminate
    | UpdateReportSummary of
        { proc_uid: string
        ; merge_report_summary: old_report_summary:Sqlite3.Data.t option -> Sqlite3.Data.t }

  let to_string = function
    | AddSourceFile _ ->
        "AddSourceFile"
    | Checkpoint ->
        "Checkpoint"
    | DeleteAllSpecs ->
        "DeleteAllSpecs"
    | DeleteAttributes _ ->
        "DeleteAttributes"
    | DeleteIssueLogs _ ->
        "DeleteIssueLogs"
    | DeleteSpecs _ ->
        "DeleteSpecs"
    | Handshake ->
        "Handshake"
    | MarkAllSourceFilesStale ->
        "MarkAllSourceFilesStale"
    | MergeCaptures _ ->
        "MergeCaptures"
    | MergeSummaries _ ->
        "MergeSummaries"
    | ReplaceAttributes _ ->
        "ReplaceAttributes"
    | ShrinkAnalysisDB ->
        "ShrinkAnalysisDB"
    | StoreIssueLog _ ->
        "StoreIssueLog"
    | StoreSpec _ ->
        "StoreSpec"
    | Terminate ->
        "Terminate"
    | UpdateReportSummary _ ->
        "UpdateReportSummary"


  let[@warning "-unused-value-declaration"] pp fmt cmd = F.pp_print_string fmt (to_string cmd)

  let execute = function
    | AddSourceFile {source_file; tenv; integer_type_widths; proc_names} ->
        Implementation.add_source_file ~source_file ~tenv ~integer_type_widths ~proc_names
    | Checkpoint ->
        Implementation.canonicalize ()
    | DeleteAllSpecs ->
        Implementation.delete_all_specs ()
    | DeleteAttributes {proc_uid} ->
        Implementation.delete_attributes ~proc_uid
    | DeleteIssueLogs {source_file} ->
        Implementation.delete_issue_logs ~source_file
    | DeleteSpecs {proc_uids} ->
        Implementation.delete_specs ~proc_uids
    | Handshake ->
        ()
    | MarkAllSourceFilesStale ->
        Implementation.mark_all_source_files_stale ()
    | MergeCaptures {root; infer_deps_file} ->
        Implementation.merge_captures ~root ~infer_deps_file
    | MergeSummaries {infer_outs} ->
        Implementation.merge_summaries infer_outs
    | ReplaceAttributes {proc_uid; proc_attributes; cfg; callees; analysis} ->
        Implementation.replace_attributes ~proc_uid ~proc_attributes ~cfg ~callees ~analysis
    | ShrinkAnalysisDB ->
        Implementation.shrink_analysis_db ()
    | StoreIssueLog {checker; source_file; issue_log} ->
        Implementation.store_issue_log ~checker ~source_file ~issue_log
    | StoreSpec
        { analysis_req
        ; proc_uid
        ; proc_name
        ; merge_pulse_payload
        ; merge_report_summary
        ; merge_summary_metadata } ->
        Implementation.store_spec analysis_req ~proc_uid ~proc_name ~merge_pulse_payload
          ~merge_report_summary ~merge_summary_metadata
    | Terminate ->
        Implementation.terminate ()
    | UpdateReportSummary {proc_uid; merge_report_summary} ->
        Implementation.update_report_summary ~proc_uid ~merge_report_summary
end

type response = Ack | Error of (exn * Caml.Printexc.raw_backtrace)

let server_pid : Pid.t option ref = ref None

module Server = struct
  (* General comment about socket/channel destruction: closing the in_channel associated with the socket
     will close the file descriptor too, so closing also the out_channel sometimes throws an exception.
     That's why in all code below only the input channel is ever closed. *)

  let log_store_pct useful_time =
    let label = "dbwriter.store_sql_pct" in
    let total_useful = ExecutionDuration.total_useful_s useful_time in
    let total_store = ExecutionDuration.total_useful_s !Implementation.store_sql_time in
    let store_pct = 100.0 *. total_store /. total_useful |> Float.round |> int_of_float in
    L.debug Analysis Quiet "%s= %d%%@\n" label store_pct ;
    ScubaLogging.log_count ~label ~value:store_pct


  let rec server_loop ?(useful_time = ExecutionDuration.zero) socket =
    let client_sock, _client = Unix.accept socket in
    let in_channel = Unix.in_channel_of_descr client_sock
    and out_channel = Unix.out_channel_of_descr client_sock in
    let now = ExecutionDuration.counter () in
    let command : Command.t = Marshal.from_channel in_channel in
    ( try
        Command.execute command ;
        Marshal.to_channel out_channel Ack []
      with exn ->
        Marshal.to_channel out_channel (Error (exn, Caml.Printexc.get_raw_backtrace ())) [] ) ;
    Out_channel.flush out_channel ;
    In_channel.close in_channel ;
    let useful_time = ExecutionDuration.add_duration_since useful_time now in
    match command with
    | Terminate ->
        L.debug Analysis Quiet "Sqlite write daemon: terminating@." ;
        ExecutionDuration.log ~prefix:"dbwriter.useful_time" Analysis useful_time ;
        ExecutionDuration.log ~prefix:"dbwriter.store_sql" Analysis !Implementation.store_sql_time ;
        log_store_pct useful_time ;
        ()
    | _ ->
        server_loop ~useful_time socket


  let server socket =
    L.debug Analysis Quiet "Sqlite write daemon: process starting, pid= %a@." Pid.pp
      (Unix.getpid ()) ;
    let finally () = ServerSocket.remove_socket socket in
    Exception.try_finally ~finally ~f:(fun () ->
        let ExecutionDuration.{execution_duration} =
          ExecutionDuration.timed_evaluate ~f:(fun () -> server_loop socket)
        in
        ExecutionDuration.log ~prefix:"dbwriter.total_time" Analysis execution_duration )


  let send cmd =
    let in_channel, out_channel =
      ServerSocket.in_results_dir ~f:(fun () -> Unix.open_connection ServerSocket.socket_addr)
    in
    Marshal.to_channel out_channel cmd [Closures] ;
    Out_channel.flush out_channel ;
    let (response : response) = Marshal.from_channel in_channel in
    ( match response with
    | Ack ->
        ()
    | Error (exn, exn_backtrace) ->
        Caml.Printexc.raise_with_backtrace exn exn_backtrace ) ;
    In_channel.close in_channel


  let start () =
    L.debug Analysis Quiet "Sqlite write daemon: starting up@." ;
    let socket = ServerSocket.setup_socket () in
    L.debug Analysis Quiet "Sqlite write daemon: set up complete, waiting for connections@." ;
    match Unix.fork () with
    | `In_the_child ->
        ForkUtils.protect ~f:server socket ;
        L.exit 0
    | `In_the_parent child_pid ->
        server_pid := Some child_pid ;
        send Command.Handshake
end

let remove_socket_file () = ServerSocket.remove_socket_file ()

let perform cmd = if use_daemon () then Server.send cmd else Command.execute cmd

let add_source_file ~source_file ~tenv ~integer_type_widths ~proc_names =
  perform (AddSourceFile {source_file; tenv; integer_type_widths; proc_names})


let canonicalize () = perform Checkpoint

let delete_all_specs () = perform DeleteAllSpecs

let delete_attributes ~proc_uid = perform (DeleteAttributes {proc_uid})

let delete_issue_logs ~source_file = perform (DeleteIssueLogs {source_file})

let delete_specs ~proc_uids = perform (DeleteSpecs {proc_uids})

let mark_all_source_files_stale () = perform MarkAllSourceFilesStale

let merge_captures ~root ~infer_deps_file = perform (MergeCaptures {root; infer_deps_file})

let merge_summaries ~infer_outs = perform (MergeSummaries {infer_outs})

let replace_attributes ~proc_uid ~proc_attributes ~cfg ~callees ~analysis =
  perform (ReplaceAttributes {proc_uid; proc_attributes; cfg; callees; analysis})


let shrink_analysis_db () = perform ShrinkAnalysisDB

let stop () =
  (try Server.send Command.Terminate with Unix.Unix_error _ -> ()) ;
  (* don't terminate the main infer process before the server has finished *)
  Option.iter !server_pid ~f:(fun server_pid ->
      L.debug Analysis Quiet "Sqlite write daemon: waiting for process %a to finish@." Pid.pp
        server_pid ;
      let pid, exit_or_signal = Unix.wait (`Pid server_pid) in
      Result.iter_error exit_or_signal ~f:(function error ->
          ( L.internal_error "ERROR: Sqlite write daemon terminated with an error: %s@\n"
              (Core_unix.Exit_or_signal.to_string_hum (Error error)) ;
            try ServerSocket.remove_socket_file () with Unix.Unix_error _ -> () ) ) ;
      L.debug Analysis Quiet "Sqlite write daemon: process %a terminated@." Pid.pp pid )


let start =
  let already_started = ref false in
  fun () ->
    if (not !already_started) && Config.is_originator then (
      remove_socket_file () ;
      if use_daemon () then (
        Server.start () ;
        Epilogues.register ~f:stop ~description:"Stop Sqlite write daemon" ;
        already_started := true ) )


let store_issue_log ~checker ~source_file ~issue_log =
  perform (StoreIssueLog {checker; source_file; issue_log})


let update_report_summary ~proc_uid ~merge_report_summary =
  perform (UpdateReportSummary {proc_uid; merge_report_summary})


let store_spec analysis_req ~proc_uid ~proc_name ~merge_pulse_payload ~merge_report_summary
    ~merge_summary_metadata =
  let counter = ExecutionDuration.counter () in
  let result =
    perform
      (StoreSpec
         { analysis_req
         ; proc_uid
         ; proc_name
         ; merge_pulse_payload
         ; merge_report_summary
         ; merge_summary_metadata } )
  in
  Stats.incr_spec_store_times counter ;
  result
