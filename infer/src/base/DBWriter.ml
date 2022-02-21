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
    if not (ISys.file_exists db_file) then
      L.die InternalError "Tried to merge in DB at %s but path does not exist.@\n" db_file ;
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
    ResultsDatabase.get_database ()
    |> SqliteUtils.exec ~log:"checkpointing" ~stmt:"PRAGMA wal_checkpoint"


  let reset_capture_tables () =
    let db = ResultsDatabase.get_database () in
    SqliteUtils.exec db ~log:"drop procedures table" ~stmt:"DROP TABLE procedures" ;
    SqliteUtils.exec db ~log:"drop source_files table" ~stmt:"DROP TABLE source_files" ;
    ResultsDatabase.create_tables db


  module IntHash = Caml.Hashtbl.Make (Int)

  let specs_overwrite_counts =
    (* We don't want to keep all [proc_uid]s in memory just to keep an overwrite count,
       so use a table keyed on their integer hashes; collisions will just lead to some noise. *)
    IntHash.create 10


  let log_specs_overwrite_counts () =
    let overwrites = IntHash.fold (fun _hash count acc -> acc + count) specs_overwrite_counts 0 in
    ScubaLogging.log_count ~label:"overwritten_specs" ~value:overwrites ;
    L.debug Analysis Quiet "Detected %d spec overwrittes.@\n" overwrites


  let store_spec =
    let store_statement =
      ResultsDatabase.register_statement
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
    | Checkpoint
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
        Implementation.log_specs_overwrite_counts ()
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
    let socket = Unix.socket ~domain:socket_domain ~kind:Unix.SOCK_STREAM ~protocol:0 () in
    in_results_dir ~f:(fun () -> Unix.bind socket ~addr:socket_addr) ;
    (* [backlog] is (supposedly) the length of the queue for pending connections ;
       there are no rules about the implied behaviour though.  Here use optimistically
       the number of workers, though even that is a guess. *)
    Unix.listen socket ~backlog:Config.jobs ;
    socket


  let remove_socket socket =
    in_results_dir ~f:(fun () ->
        Unix.close socket ;
        Unix.unlink socket_name )


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

let start () = Server.start ()

let stop () = try Server.send Command.Terminate with Unix.Unix_error _ -> ()

let replace_attributes ~proc_uid ~proc_name ~attr_kind ~source_file ~proc_attributes ~cfg ~callees =
  perform
    (ReplaceAttributes {proc_uid; proc_name; attr_kind; source_file; proc_attributes; cfg; callees})


let add_source_file ~source_file ~tenv ~integer_type_widths ~proc_names =
  perform (AddSourceFile {source_file; tenv; integer_type_widths; proc_names})


let mark_all_source_files_stale () = perform MarkAllSourceFilesStale

let merge ~infer_deps_file = perform (Merge {infer_deps_file})

let canonicalize () = perform Checkpoint

let reset_capture_tables () = perform ResetCaptureTables

let store_spec ~proc_uid ~proc_name ~analysis_summary ~report_summary =
  perform (StoreSpec {proc_uid; proc_name; analysis_summary; report_summary})


let delete_spec ~proc_uid = perform (DeleteSpec {proc_uid})

let delete_all_specs () = perform DeleteAllSpecs
