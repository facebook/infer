(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

let scan_model_proc_names () =
  let db = Database.get_database AnalysisDatabase in
  Sqlite3.prepare db "SELECT proc_uid FROM model_specs"
  |> SqliteUtils.result_fold_single_column_rows db ~log:"scan model procnames"
       ~init:String.Set.empty ~f:(fun acc proc_uid_sqlite ->
         let[@warning "-partial-match"] (Sqlite3.Data.TEXT proc_uid) = proc_uid_sqlite in
         String.Set.add acc proc_uid )


let models_index =
  lazy (if Config.biabduction_models_mode then String.Set.empty else scan_model_proc_names ())


let mem proc_name =
  let proc_uid = Procname.to_unique_id proc_name in
  String.Set.mem (Lazy.force models_index) proc_uid
