(*
 * Copyright (c) 2014 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

module L = Logging


(** Find SQL statements in string concatenations *)
let callback_sql { Callbacks.proc_desc; tenv; summary } : Specs.summary =
  let proc_name = Procdesc.get_proc_name proc_desc in
  let verbose = false in

  (* Case insensitive SQL statement patterns *)
  let sql_start =
    let _sql_start = [
      "select.*from.*";
      "insert into.*";
      "update .* set.*";
      "delete .* from.*";
    ] in
    List.map ~f:Str.regexp_case_fold _sql_start in

  (* Check for SQL string concatenations *)
  let do_instr const_map node instr =
    let do_call pn_java i1 i2 l =
      if String.equal (Typ.Procname.java_get_class_name pn_java) "java.lang.StringBuilder"
      && String.equal (Typ.Procname.java_get_method pn_java) "append"
      then
        begin
          let rvar1 = Exp.Var i1 in
          let rvar2 = Exp.Var i2 in
          begin
            let matches s r = Str.string_match r s 0 in
            match const_map node rvar1, const_map node rvar2 with
            | Some (Const.Cstr ""), Some (Const.Cstr s2) ->
                if List.exists ~f:(matches s2) sql_start then
                  begin
                    L.stdout
                      "%s%s@."
                      "Possible SQL query using string concatenation. "
                      "Please consider using a prepared statement instead.";
                    let linereader = Printer.LineReader.create () in
                    L.stdout "%a@." (Checkers.PP.pp_loc_range linereader 2 2) l
                  end
            | _ -> ()
          end
        end in

    match instr with
    | Sil.Call (_, Exp.Const (Const.Cfun pn), (Exp.Var i1, _):: (Exp.Var i2, _):: [], l, _) ->
        begin
          match pn with
          | Typ.Procname.Java pn_java ->
              do_call pn_java i1 i2 l
          | _ ->
              ()
        end
    | _ -> () in

  begin
    try
      let const_map = ConstantPropagation.build_const_map tenv proc_desc in
      if verbose then L.stdout "Analyzing %a...\n@." Typ.Procname.pp proc_name;
      Procdesc.iter_instrs (do_instr const_map) proc_desc
    with _ -> ()
  end;
  summary
