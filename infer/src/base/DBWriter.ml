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
  let replace_attributes =
    let attribute_replace_statement =
      (* The innermost SELECT returns either the current attributes_kind and source_file associated with
         the given proc name, or default values of (-1,""). These default values have the property that
         they are always "less than" any legit value. More precisely, MAX ensures that some value is
         returned even if there is no row satisfying WHERE (we'll get NULL in that case, the value in
         the row otherwise). COALESCE then returns the first non-NULL value, which will be either the
         value of the row corresponding to that pname in the DB, or the default if no such row exists.

         The next (second-outermost) SELECT filters out that value if it is "more defined" than the ones
         we would like to insert (which will never be the case if the default values are returned). If
         not, it returns a trivial row (consisting solely of NULL since we don't use its values) and the
         INSERT OR REPLACE will proceed and insert or update the values stored into the DB for that
         pname. *)
      (* TRICK: use the source file to be more deterministic in case the same procedure name is defined
         in several files *)
      (* TRICK: older versions of sqlite (prior to version 3.15.0 (2016-10-14)) do not support row
         values so the lexicographic ordering for (:akind, :sfile) is done by hand *)
      ResultsDatabase.register_statement
        {|
          INSERT OR REPLACE INTO procedures
          SELECT :uid, :pname, :akind, :sfile, :pattr, :cfg, :callees
          FROM (
            SELECT NULL
            FROM (
              SELECT COALESCE(MAX(attr_kind),-1) AS attr_kind,
                    COALESCE(MAX(source_file),"") AS source_file
              FROM procedures
              WHERE proc_uid = :uid )
            WHERE attr_kind < :akind
                  OR (attr_kind = :akind AND source_file <= :sfile) )
        |}
    in
    fun ~proc_uid ~proc_name ~attr_kind ~source_file ~proc_attributes ~cfg ~callees ->
      ResultsDatabase.with_registered_statement attribute_replace_statement
        ~f:(fun db replace_stmt ->
          Sqlite3.bind replace_stmt 1 (* :proc_uid *) (Sqlite3.Data.TEXT proc_uid)
          |> SqliteUtils.check_result_code db ~log:"replace bind proc_uid" ;
          Sqlite3.bind replace_stmt 2 (* :pname *) proc_name
          |> SqliteUtils.check_result_code db ~log:"replace bind proc_name" ;
          Sqlite3.bind replace_stmt 3 (* :akind *) (Sqlite3.Data.INT attr_kind)
          |> SqliteUtils.check_result_code db ~log:"replace bind attr_kind" ;
          Sqlite3.bind replace_stmt 4 (* :sfile *) source_file
          |> SqliteUtils.check_result_code db ~log:"replace bind source source_file" ;
          Sqlite3.bind replace_stmt 5 (* :pattr *) proc_attributes
          |> SqliteUtils.check_result_code db ~log:"replace bind proc proc_attributes" ;
          Sqlite3.bind replace_stmt 6 (* :cfg *) cfg
          |> SqliteUtils.check_result_code db ~log:"replace bind cfg" ;
          Sqlite3.bind replace_stmt 7 (* :callees *) callees
          |> SqliteUtils.check_result_code db ~log:"replace bind callees" ;
          SqliteUtils.result_unit db ~finalize:false ~log:"replace_attributes" replace_stmt )


  let add_source_file =
    let source_file_store_statement =
      ResultsDatabase.register_statement
        {|
          INSERT OR REPLACE INTO source_files
          VALUES (:source, :tenv, :integer_type_widths, :proc_names, :freshly_captured)
        |}
    in
    fun ~source_file ~tenv ~integer_type_widths ~proc_names ->
      ResultsDatabase.with_registered_statement source_file_store_statement ~f:(fun db store_stmt ->
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


  let mark_all_source_files_stale () =
    ResultsDatabase.get_database ()
    |> SqliteUtils.exec ~stmt:"UPDATE source_files SET freshly_captured = 0" ~log:"mark_all_stale"


  let merge_procedures_table ~db_file =
    (* Do the merge purely in SQL for great speed. The query works by doing a left join between the
       sub-table and the main one, and applying the same "more defined" logic as in Attributes in the
       cases where a proc_name is present in both the sub-table and the main one (main.attr_kind !=
       NULL). All the rows that pass this filter are inserted/updated into the main table. *)
    ResultsDatabase.get_database ()
    |> SqliteUtils.exec
         ~log:(Printf.sprintf "copying procedures of database '%s'" db_file)
         ~stmt:
           {|
              INSERT OR REPLACE INTO memdb.procedures
              SELECT
                sub.proc_uid,
                sub.proc_name,
                sub.attr_kind,
                sub.source_file,
                sub.proc_attributes,
                sub.cfg,
                sub.callees
              FROM (
                attached.procedures AS sub
                LEFT OUTER JOIN memdb.procedures AS main
                ON sub.proc_uid = main.proc_uid )
              WHERE
                main.attr_kind IS NULL
                OR main.attr_kind < sub.attr_kind
                OR (main.attr_kind = sub.attr_kind AND main.source_file < sub.source_file)
            |}


  let merge_source_files_table ~db_file =
    ResultsDatabase.get_database ()
    |> SqliteUtils.exec
         ~log:(Printf.sprintf "copying source_files of database '%s'" db_file)
         ~stmt:
           {|
              INSERT OR REPLACE INTO memdb.source_files
              SELECT source_file, type_environment, integer_type_widths, procedure_names, 1
              FROM attached.source_files
            |}


  let copy_to_main db =
    SqliteUtils.exec db ~log:"Copying procedures into main db"
      ~stmt:"INSERT OR REPLACE INTO procedures SELECT * FROM memdb.procedures" ;
    SqliteUtils.exec db ~log:"Copying source_files into main db"
      ~stmt:"INSERT OR REPLACE INTO source_files SELECT * FROM memdb.source_files"


  let merge_db infer_out_src =
    let db_file = ResultsDirEntryName.get_path ~results_dir:infer_out_src CaptureDB in
    let main_db = ResultsDatabase.get_database () in
    SqliteUtils.exec main_db
      ~stmt:(Printf.sprintf "ATTACH '%s' AS attached" db_file)
      ~log:(Printf.sprintf "attaching database '%s'" db_file) ;
    merge_procedures_table ~db_file ;
    merge_source_files_table ~db_file ;
    SqliteUtils.exec main_db ~stmt:"DETACH attached"
      ~log:(Printf.sprintf "detaching database '%s'" db_file)


  let merge infer_deps_file =
    let main_db = ResultsDatabase.get_database () in
    SqliteUtils.exec main_db ~stmt:"ATTACH ':memory:' AS memdb" ~log:"attaching memdb" ;
    ResultsDatabase.create_tables ~prefix:"memdb." main_db ;
    Utils.iter_infer_deps ~project_root:Config.project_root ~f:merge_db infer_deps_file ;
    copy_to_main main_db ;
    SqliteUtils.exec main_db ~stmt:"DETACH memdb" ~log:"detaching memdb"


  let canonicalize () =
    ResultsDatabase.get_database () |> SqliteUtils.exec ~log:"running VACUUM" ~stmt:"VACUUM"


  let reset_capture_tables () =
    let db = ResultsDatabase.get_database () in
    SqliteUtils.exec db ~log:"drop procedures table" ~stmt:"DROP TABLE procedures" ;
    SqliteUtils.exec db ~log:"drop source_files table" ~stmt:"DROP TABLE source_files" ;
    ResultsDatabase.create_tables db


  let store_spec =
    let store_statement =
      ResultsDatabase.register_statement
        {|
          INSERT OR REPLACE INTO specs
          VALUES (:proc_uid, :proc_name, :analysis_summary, :report_summary)
        |}
    in
    fun ~proc_uid ~proc_name ~analysis_summary ~report_summary ->
      ResultsDatabase.with_registered_statement store_statement ~f:(fun db store_stmt ->
          Sqlite3.bind store_stmt 1 (Sqlite3.Data.TEXT proc_uid)
          |> SqliteUtils.check_result_code db ~log:"store spec bind proc_uid" ;
          Sqlite3.bind store_stmt 2 proc_name
          |> SqliteUtils.check_result_code db ~log:"store spec bind proc_name" ;
          Sqlite3.bind store_stmt 3 analysis_summary
          |> SqliteUtils.check_result_code db ~log:"store spec bind analysis_summary" ;
          Sqlite3.bind store_stmt 4 report_summary
          |> SqliteUtils.check_result_code db ~log:"store spec bind report_summary" ;
          SqliteUtils.result_unit ~finalize:false ~log:"store spec" db store_stmt )


  let delete_spec =
    let delete_statement =
      ResultsDatabase.register_statement "DELETE FROM specs WHERE proc_uid = :k"
    in
    fun ~proc_uid ->
      ResultsDatabase.with_registered_statement delete_statement ~f:(fun db delete_stmt ->
          Sqlite3.bind delete_stmt 1 (Sqlite3.Data.TEXT proc_uid)
          |> SqliteUtils.check_result_code db ~log:"delete spec bind proc_uid" ;
          SqliteUtils.result_unit ~finalize:false ~log:"store spec" db delete_stmt )


  let delete_all_specs () =
    ResultsDatabase.get_database ()
    |> SqliteUtils.exec ~log:"drop procedures table" ~stmt:"DELETE FROM specs"
end

module Command = struct
  type t =
    | AddSourceFile of
        { source_file: Sqlite3.Data.t
        ; tenv: Sqlite3.Data.t
        ; integer_type_widths: Sqlite3.Data.t
        ; proc_names: Sqlite3.Data.t }
    | DeleteAllSpecs
    | DeleteSpec of {proc_uid: string}
    | Handshake
    | MarkAllSourceFilesStale
    | Merge of {infer_deps_file: string}
    | StoreSpec of
        { proc_uid: string
        ; proc_name: Sqlite3.Data.t
        ; analysis_summary: Sqlite3.Data.t
        ; report_summary: Sqlite3.Data.t }
    | ReplaceAttributes of
        { proc_uid: string
        ; proc_name: Sqlite3.Data.t
        ; attr_kind: int64
        ; source_file: Sqlite3.Data.t
        ; proc_attributes: Sqlite3.Data.t
        ; cfg: Sqlite3.Data.t
        ; callees: Sqlite3.Data.t }
    | ResetCaptureTables
    | Terminate
    | Vacuum

  let to_string = function
    | AddSourceFile _ ->
        "AddSourceFile"
    | DeleteAllSpecs ->
        "DeleteAllSpecs"
    | DeleteSpec _ ->
        "DeleteSpec"
    | Handshake ->
        "Handshake"
    | MarkAllSourceFilesStale ->
        "MarkAllSourceFilesStale"
    | Merge _ ->
        "Merge"
    | ReplaceAttributes _ ->
        "ReplaceAttributes"
    | ResetCaptureTables ->
        "ResetCaptureTables"
    | StoreSpec _ ->
        "StoreSpec"
    | Terminate ->
        "Terminate"
    | Vacuum ->
        "Vacuum"


  let pp fmt cmd = F.pp_print_string fmt (to_string cmd)

  let execute = function
    | AddSourceFile {source_file; tenv; integer_type_widths; proc_names} ->
        Implementation.add_source_file ~source_file ~tenv ~integer_type_widths ~proc_names
    | DeleteAllSpecs ->
        Implementation.delete_all_specs ()
    | DeleteSpec {proc_uid} ->
        Implementation.delete_spec ~proc_uid
    | Handshake ->
        ()
    | MarkAllSourceFilesStale ->
        Implementation.mark_all_source_files_stale ()
    | Merge {infer_deps_file} ->
        Implementation.merge infer_deps_file
    | StoreSpec {proc_uid; proc_name; analysis_summary; report_summary} ->
        Implementation.store_spec ~proc_uid ~proc_name ~analysis_summary ~report_summary
    | ReplaceAttributes {proc_uid; proc_name; attr_kind; source_file; proc_attributes; cfg; callees}
      ->
        Implementation.replace_attributes ~proc_uid ~proc_name ~attr_kind ~source_file
          ~proc_attributes ~cfg ~callees
    | ResetCaptureTables ->
        Implementation.reset_capture_tables ()
    | Terminate ->
        ()
    | Vacuum ->
        Implementation.canonicalize ()
end

type response = Ack

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
    Command.execute command ;
    Marshal.to_channel out_channel Ack [] ;
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

  let server () =
    L.debug Analysis Quiet "Sqlite write daemon: starting up@." ;
    if socket_exists () then L.die InternalError "Sqlite write daemon: socket already exists@." ;
    let socket = Unix.socket ~domain:socket_domain ~kind:Unix.SOCK_STREAM ~protocol:0 () in
    in_results_dir ~f:(fun () -> Unix.bind socket ~addr:socket_addr) ;
    (* [backlog] is (supposedly) the length of the queue for pending connections ;
       there are no rules about the implied behaviour though.  Here use optimistically
       the number of workers, though even that is a guess. *)
    Unix.listen socket ~backlog:Config.jobs ;
    L.debug Analysis Quiet "Sqlite write daemon: set up complete, waiting for connections@." ;
    let shutdown () =
      in_results_dir ~f:(fun () ->
          Unix.close socket ;
          Unix.remove socket_name )
    in
    Utils.try_finally_swallow_timeout ~f:(fun () -> server_loop socket) ~finally:shutdown


  let send cmd =
    let in_channel, out_channel = in_results_dir ~f:(fun () -> Unix.open_connection socket_addr) in
    Marshal.to_channel out_channel cmd [] ;
    Out_channel.flush out_channel ;
    let (Ack : response) = Marshal.from_channel in_channel in
    In_channel.close in_channel


  (* wait for socket to appear with 0.1 sec timeout, doubling each time ;
     choice of numbers is arbitrary *)
  let initial_wait_for_socket_secs = 0.1

  let rec wait_for_server_start ~wait_secs =
    Unix.nanosleep wait_secs |> ignore ;
    if not (socket_exists ()) then (
      let wait_secs = 2.0 *. wait_secs in
      L.progress "Waiting for Sqlite write daemon to start (%f seconds)@\n" wait_secs ;
      wait_for_server_start ~wait_secs )


  let start () =
    match Unix.fork () with
    | `In_the_child ->
        ForkUtils.protect ~f:server () ;
        L.exit 0
    | `In_the_parent _child_pid ->
        wait_for_server_start ~wait_secs:initial_wait_for_socket_secs ;
        send Command.Handshake
end

let use_daemon = Config.((not (buck || genrule_mode)) && jobs > 1)

let perform cmd = if use_daemon then Server.send cmd else Command.execute cmd

let start () = Server.start ()

let stop () = Server.send Command.Terminate

let replace_attributes ~proc_uid ~proc_name ~attr_kind ~source_file ~proc_attributes ~cfg ~callees =
  perform
    (ReplaceAttributes {proc_uid; proc_name; attr_kind; source_file; proc_attributes; cfg; callees})


let add_source_file ~source_file ~tenv ~integer_type_widths ~proc_names =
  perform (AddSourceFile {source_file; tenv; integer_type_widths; proc_names})


let mark_all_source_files_stale () = perform MarkAllSourceFilesStale

let merge ~infer_deps_file = perform (Merge {infer_deps_file})

let canonicalize () = perform Vacuum

let reset_capture_tables () = perform ResetCaptureTables

let store_spec ~proc_uid ~proc_name ~analysis_summary ~report_summary =
  perform (StoreSpec {proc_uid; proc_name; analysis_summary; report_summary})


let delete_spec ~proc_uid = perform (DeleteSpec {proc_uid})

let delete_all_specs () = perform DeleteAllSpecs
