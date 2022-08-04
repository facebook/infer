(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

let analyzed_classes = Hash_set.create (module String)

let is_builtin = BuiltinDecl.is_declared

let is_builtin_cast = Procname.equal BuiltinDecl.__cast

let is_builtin_alloc proc =
  Procname.equal BuiltinDecl.__new proc || Procname.equal BuiltinDecl.__new_array proc


let is_entry_proc proc_name =
  let java_proc_name = Procname.as_java_exn proc_name ~explanation:"Only Java procdesc supported" in
  String.equal (Procname.Java.get_method java_proc_name) "main"
  && Typ.is_void (Procname.Java.get_return_typ java_proc_name)


let report_fact {IntraproceduralAnalysis.proc_desc; err_log} ~loc fact =
  Reporting.log_issue proc_desc err_log ~loc Checker.Datalog IssueType.datalog_fact
    (Fact.to_string fact)


(** If a specific location is not given, the location of the procdesc is used *)
let log_fact ({IntraproceduralAnalysis.proc_desc} as analysis_data) ?loc (fact : Fact.t) =
  let loc = Option.value loc ~default:(Procdesc.get_loc proc_desc) in
  match fact with
  (* Class-level facts *)
  | Extends {typ; _} ->
      if
        (not (Hash_set.mem analyzed_classes (Typ.Name.name typ)))
        && Procname.is_constructor (Procdesc.get_proc_name proc_desc)
      then (
        Hash_set.add analyzed_classes (Typ.Name.name typ) ;
        report_fact analysis_data fact ~loc )
  (* Procedure-level facts *)
  | Reachable _ | Cast _ | Alloc _ | VirtualCall _ | StaticCall _ ->
      report_fact analysis_data fact ~loc


(** [JAVA ONLY] `Foo x = new Foo();` becomes in SIL `tmp = __new(...) ; x = Foo.<init>(..., tmp)`.
    __new() is the built-in function for allocations, whereas Foo.<init> is a static call to the
    default constructor of Foo. In the fact generation, a call to __new() generates an "Alloc" fact,
    whereas a call to a constructor will generate a "StaticCall" fact. Both facts are generated when
    the `new Class()` statement is used. *)
let emit_procedure_level_facts ({IntraproceduralAnalysis.proc_desc} as analysis_data) =
  let proc_name = Procdesc.get_proc_name proc_desc in
  if is_entry_proc proc_name then log_fact analysis_data (Fact.reachable proc_name) ;
  Procdesc.iter_instrs
    (fun _ instr ->
      match instr with
      | Call ((ret_id, _), Const (Cfun call_proc), [(Var arg_id, _); (Sizeof sizeof, _)], loc, _)
        when is_builtin_cast call_proc ->
          log_fact analysis_data (Fact.cast proc_name ret_id arg_id sizeof.typ) ~loc
      | Call ((ret_id, _), Const (Cfun call_proc), [(Sizeof sizeof, _)], loc, _)
        when is_builtin_alloc call_proc ->
          log_fact analysis_data (Fact.alloc proc_name ret_id loc sizeof.typ) ~loc
      | Call ((ret_id, _), Const (Cfun call_proc), _args, loc, call_flags)
        when not (is_builtin call_proc) ->
          if call_flags.cf_virtual || call_flags.cf_interface then
            log_fact analysis_data (Fact.virtual_call proc_name loc ret_id call_proc) ~loc
          else log_fact analysis_data (Fact.static_call proc_name loc ret_id call_proc) ~loc
      | _ ->
          () )
    proc_desc


let emit_class_level_facts ({IntraproceduralAnalysis.proc_desc; tenv} as analysis_data) =
  let open IOption.Let_syntax in
  let class_facts =
    (let* class_typ = Procname.get_class_type_name (Procdesc.get_proc_name proc_desc) in
     let+ class_struct = Tenv.lookup tenv class_typ in
     List.map class_struct.Struct.supers ~f:(fun superclass -> Fact.extends class_typ superclass) )
    |> Option.value ~default:[]
  in
  List.iter class_facts ~f:(log_fact analysis_data)


let emit_facts analysis_data =
  emit_class_level_facts analysis_data ;
  emit_procedure_level_facts analysis_data
