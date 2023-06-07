(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

let find =
  let select_statement db =
    Database.register_statement db "SELECT proc_attributes FROM procedures WHERE proc_uid = :k"
  in
  let select_statement_adb = select_statement AnalysisDatabase in
  let select_statement_cdb = select_statement CaptureDatabase in
  fun proc_uid ->
    let run_query select_stmt =
      Database.with_registered_statement select_stmt ~f:(fun db select_stmt ->
          Sqlite3.bind select_stmt 1 (Sqlite3.Data.TEXT proc_uid)
          |> SqliteUtils.check_result_code db ~log:"find bind proc name" ;
          SqliteUtils.result_single_column_option ~finalize:false ~log:"Attributes.find" db
            select_stmt
          |> Option.map ~f:ProcAttributes.SQLite.deserialize )
    in
    (* Since the procedure table can be updated in the analysis phase,
       we need to query both databases, analysisdb first *)
    IOption.if_none_evalopt
      ~f:(fun () -> run_query select_statement_cdb)
      (run_query select_statement_adb)


let load_from_uid, load, clear_cache, store =
  (* capture DB attribute cache: only keeps positive entries as analysis may add entries *)
  let cache : ProcAttributes.t Procname.Hash.t = Procname.Hash.create 1 in
  let load_from_uid uid =
    let result = find uid in
    Option.iter result ~f:(fun attrs ->
        Procname.Hash.add cache (ProcAttributes.get_proc_name attrs) attrs ) ;
    result
  in
  let load pname =
    Dependencies.record_pname_dep Other pname ;
    match Procname.Hash.find_opt cache pname with
    | Some _ as result ->
        result
    | None -> (
      match load_from_uid (Procname.to_unique_id pname) with
      | None ->
          MissingDependencies.record_procname pname ;
          None
      | some ->
          some )
  in
  let clear_cache () = Procname.Hash.clear cache in
  let store ~proc_desc (attr : ProcAttributes.t) ~analysis =
    if attr.is_defined && Option.is_none proc_desc then
      Logging.die InternalError "Was given DEFINED procedure without procdesc: %a@."
        ProcAttributes.pp attr ;
    if (not attr.is_defined) && Option.is_some proc_desc then
      Logging.die InternalError
        "Was given UNDEFINED procedure WITH procdesc:@\nAttributes:%a@\nProcdesc:%a@."
        ProcAttributes.pp attr (Pp.option Procdesc.pp_signature) proc_desc ;
    let pname = attr.proc_name in
    let proc_uid = Procname.to_unique_id pname in
    let cfg = Procdesc.SQLite.serialize proc_desc in
    let proc_attributes = ProcAttributes.SQLite.serialize attr in
    let callees =
      Option.value_map proc_desc ~f:Procdesc.get_static_callees ~default:[]
      |> Procname.SQLiteList.serialize
    in
    DBWriter.replace_attributes ~proc_uid ~proc_attributes ~cfg ~callees ~analysis ;
    Procname.Hash.remove cache pname
  in
  (load_from_uid, load, clear_cache, store)


let load_exn pname = Option.value_exn (load pname)

let load_formal_types pname =
  match load pname with Some {formals} -> List.map formals ~f:snd3 | None -> []


let is_no_return pname = match load pname with Some {is_no_return} -> is_no_return | _ -> false
