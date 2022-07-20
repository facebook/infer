(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

let is_entry_proc proc_name =
  let java_proc_name = Procname.as_java_exn proc_name ~explanation:"Only Java procdesc supported" in
  String.equal (Procname.Java.get_method java_proc_name) "main"
  && Typ.is_void (Procname.Java.get_return_typ java_proc_name)


let report_fact ({IntraproceduralAnalysis.proc_desc; err_log} as _analysis_data) fact =
  Reporting.log_issue proc_desc err_log ~loc:Location.dummy Checker.Datalog IssueType.datalog_fact
    (Fact.to_string fact)


let emit_procedure_level_facts ({IntraproceduralAnalysis.proc_desc} as analysis_data) =
  let proc_name = Procdesc.get_proc_name proc_desc in
  if is_entry_proc proc_name then report_fact analysis_data (Fact.Reachable proc_name)


let emit_facts analysis_data = emit_procedure_level_facts analysis_data
