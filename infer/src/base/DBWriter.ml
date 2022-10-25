(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging
module F = Format

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
    Database.get_database AnalysisDatabase
    |> SqliteUtils.exec ~log:"checkpointing" ~stmt:"PRAGMA wal_checkpoint"


  let delete_all_specs () =
    Database.get_database AnalysisDatabase
    |> SqliteUtils.exec ~log:"drop specs table" ~stmt:"DELETE FROM specs"


  let delete_spec =
    let delete_statement =
      Database.register_statement AnalysisDatabase "DELETE FROM specs WHERE proc_uid = :k"
    in
    fun ~proc_uid ->
      Database.with_registered_statement delete_statement ~f:(fun db delete_stmt ->
          Sqlite3.bind delete_stmt 1 (Sqlite3.Data.TEXT proc_uid)
          |> SqliteUtils.check_result_code db ~log:"delete spec bind proc_uid" ;
          SqliteUtils.result_unit ~finalize:false ~log:"delete spec" db delete_stmt )


  let mark_all_source_files_stale () =
    Database.get_database CaptureDatabase
    |> SqliteUtils.exec ~stmt:"UPDATE source_files SET freshly_captured = 0" ~log:"mark_all_stale"


  let merge_captures infer_deps_file =
    let merge_procedures_table ~db_file =
      (* Do the merge purely in SQL for great speed. The query works by doing a left join between the
         sub-table and the main one, and applying the same "more defined" logic as in [replace_attributes] in the
         cases where a proc_name is present in both the sub-table and the main one (main.proc_uid !=
         NULL). All the rows that pass this filter are inserted/updated into the main table. *)
      Database.get_database CaptureDatabase
      |> SqliteUtils.exec
           ~log:(Printf.sprintf "copying procedures of database '%s'" db_file)
           ~stmt:
             {|
              INSERT OR REPLACE INTO memdb.procedures
              SELECT
                sub.proc_uid,
                sub.proc_attributes,
                sub.cfg,
                sub.callees
              FROM (
                attached.procedures AS sub
                LEFT OUTER JOIN memdb.procedures AS main
                ON sub.proc_uid = main.proc_uid )
              WHERE
                main.proc_uid IS NULL
                OR
                (main.cfg IS NOT NULL) < (sub.cfg IS NOT NULL)
                OR
                ((main.cfg IS NULL) = (sub.cfg IS NULL) AND main.proc_attributes < sub.proc_attributes)
            |}
    in
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
          merge_procedures_table ~db_file ;
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
        Utils.iter_infer_deps ~project_root:Config.project_root ~f:merge_db infer_deps_file ;
        copy_to_main main_db )


  let merge_report_summaries infer_outs =
    let merge_report_summaries_in_specs_table ~db_file =
      (* NB [NULL] is used to skip reading/writing analysis summaries *)
      Database.get_database AnalysisDatabase
      |> SqliteUtils.exec
           ~log:(Printf.sprintf "copying specs of database '%s'" db_file)
           ~stmt:
             {|
              INSERT OR REPLACE INTO specs
              SELECT
                sub.proc_uid,
                sub.proc_name,
                NULL,
                sub.report_summary
              FROM (
                attached.specs AS sub
                LEFT OUTER JOIN specs AS main
                ON sub.proc_uid = main.proc_uid )
              WHERE
                main.proc_uid IS NULL
                OR
                main.report_summary >= sub.report_summary
            |}
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
            merge_report_summaries_in_specs_table ~db_file ;
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


  let reset_capture_tables () =
    let db = Database.get_database CaptureDatabase in
    SqliteUtils.exec db ~log:"drop procedures table" ~stmt:"DROP TABLE procedures" ;
    SqliteUtils.exec db ~log:"drop source_files table" ~stmt:"DROP TABLE source_files" ;
    Database.create_tables db CaptureDatabase


  let shrink_analysis_db () =
    let db = Database.get_database AnalysisDatabase in
    SqliteUtils.exec db ~log:"nullify analysis summaries"
      ~stmt:"UPDATE specs SET analysis_summary=NULL" ;
    SqliteUtils.exec db ~log:"drop source_files table" ~stmt:"VACUUM"


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


  let store_spec =
    let store_statement =
      Database.register_statement AnalysisDatabase
        {|
          INSERT OR REPLACE INTO specs
          VALUES (:proc_uid, :proc_name, :analysis_summary, :report_summary)
        |}
    in
    fun ~proc_uid ~proc_name ~analysis_summary ~report_summary ->
      let proc_uid_hash = String.hash proc_uid in
      IntHash.find_opt specs_overwrite_counts proc_uid_hash
      |> Option.value_map ~default:0 ~f:(( + ) 1)
      (* [default] is 0 as we are only counting overwrites *)
      |> IntHash.replace specs_overwrite_counts proc_uid_hash ;
      Database.with_registered_statement store_statement ~f:(fun db store_stmt ->
          Sqlite3.bind store_stmt 1 (Sqlite3.Data.TEXT proc_uid)
          |> SqliteUtils.check_result_code db ~log:"store spec bind proc_uid" ;
          Sqlite3.bind store_stmt 2 proc_name
          |> SqliteUtils.check_result_code db ~log:"store spec bind proc_name" ;
          Sqlite3.bind store_stmt 3 analysis_summary
          |> SqliteUtils.check_result_code db ~log:"store spec bind analysis_summary" ;
          Sqlite3.bind store_stmt 4 report_summary
          |> SqliteUtils.check_result_code db ~log:"store spec bind report_summary" ;
          SqliteUtils.result_unit ~finalize:false ~log:"store spec" db store_stmt )


  let terminate () =
    let overwrites = IntHash.fold (fun _hash count acc -> acc + count) specs_overwrite_counts 0 in
    ScubaLogging.log_count ~label:"overwritten_specs" ~value:overwrites ;
    L.debug Analysis Quiet "Detected %d spec overwrittes.@\n" overwrites
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
    | DeleteSpec of {proc_uid: string}
    | Handshake
    | MarkAllSourceFilesStale
    | MergeCaptures of {infer_deps_file: string}
    | MergeReportSummaries of {infer_outs: string list}
    | ShrinkAnalysisDB
    | StoreIssueLog of {checker: string; source_file: Sqlite3.Data.t; issue_log: Sqlite3.Data.t}
    | StoreSpec of
        { proc_uid: string
        ; proc_name: Sqlite3.Data.t
        ; analysis_summary: Sqlite3.Data.t
        ; report_summary: Sqlite3.Data.t }
    | ReplaceAttributes of
        { proc_uid: string
        ; proc_attributes: Sqlite3.Data.t
        ; cfg: Sqlite3.Data.t
        ; callees: Sqlite3.Data.t
        ; analysis: bool }
    | ResetCaptureTables
    | Terminate

  let to_string = function
    | AddSourceFile _ ->
        "AddSourceFile"
    | Checkpoint ->
        "Checkpoint"
    | DeleteAllSpecs ->
        "DeleteAllSpecs"
    | DeleteSpec _ ->
        "DeleteSpec"
    | Handshake ->
        "Handshake"
    | MarkAllSourceFilesStale ->
        "MarkAllSourceFilesStale"
    | MergeCaptures _ ->
        "MergeCaptures"
    | MergeReportSummaries _ ->
        "MergeReportSummaries"
    | ReplaceAttributes _ ->
        "ReplaceAttributes"
    | ResetCaptureTables ->
        "ResetCaptureTables"
    | ShrinkAnalysisDB ->
        "ShrinkAnalysisDB"
    | StoreIssueLog _ ->
        "StoreIssueLog"
    | StoreSpec _ ->
        "StoreSpec"
    | Terminate ->
        "Terminate"


  let pp fmt cmd = F.pp_print_string fmt (to_string cmd)

  let execute = function
    | AddSourceFile {source_file; tenv; integer_type_widths; proc_names} ->
        Implementation.add_source_file ~source_file ~tenv ~integer_type_widths ~proc_names
    | Checkpoint ->
        Implementation.canonicalize ()
    | DeleteAllSpecs ->
        Implementation.delete_all_specs ()
    | DeleteSpec {proc_uid} ->
        Implementation.delete_spec ~proc_uid
    | Handshake ->
        ()
    | MarkAllSourceFilesStale ->
        Implementation.mark_all_source_files_stale ()
    | MergeCaptures {infer_deps_file} ->
        Implementation.merge_captures infer_deps_file
    | MergeReportSummaries {infer_outs} ->
        Implementation.merge_report_summaries infer_outs
    | ShrinkAnalysisDB ->
        Implementation.shrink_analysis_db ()
    | StoreIssueLog {checker; source_file; issue_log} ->
        Implementation.store_issue_log ~checker ~source_file ~issue_log
    | StoreSpec {proc_uid; proc_name; analysis_summary; report_summary} ->
        Implementation.store_spec ~proc_uid ~proc_name ~analysis_summary ~report_summary
    | ReplaceAttributes {proc_uid; proc_attributes; cfg; callees; analysis} ->
        Implementation.replace_attributes ~proc_uid ~proc_attributes ~cfg ~callees ~analysis
    | ResetCaptureTables ->
        Implementation.reset_capture_tables ()
    | Terminate ->
        Implementation.terminate ()
end

type response = Ack | Error of (string * Caml.Printexc.raw_backtrace)

module Server = struct
  (* General comment about socket/channel destruction: closing the in_channel associated with the socket
     will close the file descriptor too, so closing also the out_channel sometimes throws an exception.
     That's why in all code below only the input channel is ever closed. *)

  let socket_name = "sqlite_write_socket"

  let socket_addr = Unix.ADDR_UNIX socket_name

  let socket_domain = Unix.domain_of_sockaddr socket_addr

  (** Unix socket *paths* have a historical length limit of ~100 chars (!?*\@&*$). However, this
      only applies to the argument passed in the system call to create the socket, not to the actual
      path. Thus a workaround is to cd into the parent dir of the socket and then use it, hence this
      function. *)
  let in_results_dir ~f = Utils.do_in_dir ~dir:Config.toplevel_results_dir ~f

  let rec server_loop socket =
    let client_sock, _client = Unix.accept socket in
    let in_channel = Unix.in_channel_of_descr client_sock
    and out_channel = Unix.out_channel_of_descr client_sock in
    let command : Command.t = Marshal.from_channel in_channel in
    L.debug Analysis Verbose "Sqlite write daemon: received command %a@." Command.pp command ;
    ( try
        Command.execute command ;
        Marshal.to_channel out_channel Ack []
      with exn ->
        Marshal.to_channel out_channel
          (Error (Caml.Printexc.to_string exn, Caml.Printexc.get_raw_backtrace ()))
          [] ) ;
    Out_channel.flush out_channel ;
    In_channel.close in_channel ;
    L.debug Analysis Verbose "Sqlite write daemon: closing connection@." ;
    match command with
    | Terminate ->
        L.debug Analysis Quiet "Sqlite write daemon: terminating@." ;
        ()
    | _ ->
        server_loop socket


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
      let socket = setup_socket () in
      remove_socket socket ;
      true
    with _ -> false


  let server socket =
    let finally () = remove_socket socket in
    Exception.try_finally ~f:(fun () -> server_loop socket) ~finally


  let send cmd =
    let in_channel, out_channel = in_results_dir ~f:(fun () -> Unix.open_connection socket_addr) in
    Marshal.to_channel out_channel cmd [] ;
    Out_channel.flush out_channel ;
    let (response : response) = Marshal.from_channel in_channel in
    ( match response with
    | Ack ->
        ()
    | Error (exn_str, exn_backtrace) ->
        Caml.Printexc.raise_with_backtrace
          (Die.InferInternalError ("DBWriter raised " ^ exn_str))
          exn_backtrace ) ;
    In_channel.close in_channel


  let start () =
    L.debug Analysis Quiet "Sqlite write daemon: starting up@." ;
    let socket = setup_socket () in
    L.debug Analysis Quiet "Sqlite write daemon: set up complete, waiting for connections@." ;
    match Unix.fork () with
    | `In_the_child ->
        ForkUtils.protect ~f:server socket ;
        L.exit 0
    | `In_the_parent _child_pid ->
        send Command.Handshake
end

let remove_socket_file () = Server.remove_socket_file ()

let use_daemon =
  lazy
    (let is_windows =
       match Version.build_platform with Windows -> true | Linux | Darwin -> false
     in
     Config.((not is_windows) && dbwriter && (not (buck || genrule_mode)) && jobs > 1)
     &&
     (* Only the main process should try detecting whether the socket can be created.
        Otherwise, re-spawned Infer will try to create a socket on top of the existing one. *)
     if Config.is_originator then (
       let socket_ok = Server.can_use_socket () in
       if not socket_ok then
         L.user_warning
           "Cannot setup the socket to communicate with the database daemon. Performance will be \
            impacted. Do you have enough rights to create a Unix socket in directory '%s'?@."
           Config.toplevel_results_dir ;
       socket_ok )
     else Server.socket_exists () )


let perform cmd = if Lazy.force use_daemon then Server.send cmd else Command.execute cmd

let add_source_file ~source_file ~tenv ~integer_type_widths ~proc_names =
  perform (AddSourceFile {source_file; tenv; integer_type_widths; proc_names})


let canonicalize () = perform Checkpoint

let delete_all_specs () = perform DeleteAllSpecs

let delete_spec ~proc_uid = perform (DeleteSpec {proc_uid})

let mark_all_source_files_stale () = perform MarkAllSourceFilesStale

let merge_captures ~infer_deps_file = perform (MergeCaptures {infer_deps_file})

let merge_report_summaries ~infer_outs = perform (MergeReportSummaries {infer_outs})

let replace_attributes ~proc_uid ~proc_attributes ~cfg ~callees ~analysis =
  perform (ReplaceAttributes {proc_uid; proc_attributes; cfg; callees; analysis})


let reset_capture_tables () = perform ResetCaptureTables

let shrink_analysis_db () = perform ShrinkAnalysisDB

let start () = Server.start ()

let stop () = try Server.send Command.Terminate with Unix.Unix_error _ -> ()

let store_issue_log ~checker ~source_file ~issue_log =
  perform (StoreIssueLog {checker; source_file; issue_log})


let store_spec ~proc_uid ~proc_name ~analysis_summary ~report_summary =
  perform (StoreSpec {proc_uid; proc_name; analysis_summary; report_summary})
