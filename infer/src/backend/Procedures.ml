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
  let db = ResultsDatabase.get_database () in
  let stmt = Sqlite3.prepare db "SELECT source_file, proc_name FROM procedures" in
  SqliteUtils.result_fold_rows db ~log:"reading all procedure names" stmt ~init:[]
    ~f:(fun rev_results stmt ->
      let source_file = Sqlite3.column stmt 0 |> SourceFile.SQLite.deserialize in
      let proc_name = Sqlite3.column stmt 1 |> Procname.SQLite.deserialize in
      if filter source_file proc_name then proc_name :: rev_results else rev_results )


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


let pp_all ~filter ~proc_name:proc_name_cond ~attr_kind ~source_file:source_file_cond
    ~proc_attributes ~proc_cfg fmt () =
  let db = ResultsDatabase.get_database () in
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
    let[@warning "-8"] (Sqlite3.Data.TEXT proc_uid) = Sqlite3.column stmt 1 in
    let dump_cfg fmt cfg_opt =
      match cfg_opt with
      | None ->
          F.pp_print_string fmt "not found"
      | Some cfg ->
          let path = DotCfg.emit_proc_desc source_file cfg in
          F.fprintf fmt "'%s'" path
    in
    F.fprintf fmt "@[<v2>%s@,%a%a%a%a%a@]@\n" proc_uid
      (pp_if source_file_cond "source_file" SourceFile.pp)
      source_file
      (pp_if proc_name_cond "proc_name" Procname.pp)
      proc_name
      (pp_column_if stmt attr_kind "attribute_kind" Attributes.deserialize_attributes_kind
         Attributes.pp_attributes_kind)
      2
      (pp_column_if stmt ~new_line:true proc_attributes "attributes"
         ProcAttributes.SQLite.deserialize ProcAttributes.pp)
      4
      (pp_column_if stmt ~new_line:false proc_cfg "control-flow graph" Procdesc.SQLite.deserialize
         dump_cfg)
      5
  in
  (* we could also register this statement but it's typically used only once per run so just prepare
     it inside the function *)
  Sqlite3.prepare db
    {|
       SELECT
         proc_name,
         proc_uid,
         attr_kind,
         source_file,
         proc_attributes,
         cfg
       FROM procedures ORDER BY proc_uid
    |}
  |> Container.iter ~fold:(SqliteUtils.result_fold_rows db ~log:"print all procedures")
       ~f:(fun stmt ->
         let proc_name = Sqlite3.column stmt 0 |> Procname.SQLite.deserialize in
         let source_file = Sqlite3.column stmt 3 |> SourceFile.SQLite.deserialize in
         if filter source_file proc_name then pp_row stmt fmt source_file proc_name )
