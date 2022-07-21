(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

let analyzed_classes = Hash_set.create (module String)

let is_entry_proc proc_name =
  let java_proc_name = Procname.as_java_exn proc_name ~explanation:"Only Java procdesc supported" in
  String.equal (Procname.Java.get_method java_proc_name) "main"
  && Typ.is_void (Procname.Java.get_return_typ java_proc_name)


let report_fact {IntraproceduralAnalysis.proc_desc; err_log} fact =
  Reporting.log_issue proc_desc err_log ~loc:(Procdesc.get_loc proc_desc) Checker.Datalog
    IssueType.datalog_fact (Fact.to_string fact)


let log_fact ({IntraproceduralAnalysis.proc_desc} as analysis_data) (fact : Fact.t) =
  match fact with
  (* Class-level facts *)
  | Extends (cl, _) ->
      if
        (not (Hash_set.mem analyzed_classes (Typ.Name.name cl)))
        && Procname.is_constructor (Procdesc.get_proc_name proc_desc)
      then (
        Hash_set.add analyzed_classes (Typ.Name.name cl) ;
        report_fact analysis_data fact )
  (* Procedure-level facts *)
  | Reachable _ ->
      report_fact analysis_data fact


let emit_procedure_level_facts ({IntraproceduralAnalysis.proc_desc} as analysis_data) =
  let proc_name = Procdesc.get_proc_name proc_desc in
  if is_entry_proc proc_name then log_fact analysis_data (Fact.Reachable proc_name)


let emit_class_level_facts ({IntraproceduralAnalysis.proc_desc; tenv} as analysis_data) =
  let open IOption.Let_syntax in
  let class_facts =
    (let* class_typ = Procname.get_class_type_name (Procdesc.get_proc_name proc_desc) in
     let+ class_struct = Tenv.lookup tenv class_typ in
     List.map class_struct.Struct.supers ~f:(fun superclass -> Fact.Extends (class_typ, superclass))
    )
    |> Option.value ~default:[]
  in
  List.iter class_facts ~f:(log_fact analysis_data)


let emit_facts analysis_data =
  emit_class_level_facts analysis_data ;
  emit_procedure_level_facts analysis_data
