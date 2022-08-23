(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging

let analyzed_classes = Hash_set.create (module String)

let is_builtin = BuiltinDecl.is_declared

let is_builtin_cast = Procname.equal BuiltinDecl.__cast

let is_builtin_alloc proc =
  Procname.equal BuiltinDecl.__new proc || Procname.equal BuiltinDecl.__new_array proc


let is_entry_proc proc_name =
  let java_proc_name = Procname.as_java_exn proc_name ~explanation:"Only Java procdesc supported" in
  String.equal (Procname.Java.get_method java_proc_name) "main"
  && Typ.is_void (Procname.Java.get_return_typ java_proc_name)


let die_on_instr instr loc =
  L.(die InternalError)
    "FACTGEN: Unexpected instruction '%a' at location %a.@\n"
    (Sil.pp_instr Pp.text ~print_types:true)
    instr Location.pp_file_pos loc


let report_fact {IntraproceduralAnalysis.proc_desc; err_log} ~loc fact =
  Reporting.log_issue proc_desc err_log ~loc Checker.Datalog IssueType.datalog_fact
    (Fact.to_string fact)


(** If a specific location is not given, the location of the procdesc is used *)
let log_fact ({IntraproceduralAnalysis.proc_desc} as analysis_data) ?loc fact =
  let loc = Option.value loc ~default:(Procdesc.get_loc proc_desc) in
  (* Facts that are generated once for each class *)
  match Fact.is_generated_per_class fact with
  | Some typ ->
      if
        (not (Hash_set.mem analyzed_classes (Typ.Name.name typ)))
        && Procname.is_constructor (Procdesc.get_proc_name proc_desc)
      then (
        Hash_set.add analyzed_classes (Typ.Name.name typ) ;
        report_fact analysis_data fact ~loc )
  | None ->
      report_fact analysis_data fact ~loc


let emit_call_moves analysis_data args call_proc proc_name loc ret_id =
  let java_call_proc = Procname.as_java_exn call_proc ~explanation:"Only Java procdesc supported" in
  if not (Typ.is_void (Procname.Java.get_return_typ java_call_proc)) then
    log_fact analysis_data (Fact.actual_return proc_name loc ret_id) ~loc ;
  List.iteri args ~f:(fun i (exp, _) ->
      match exp with
      | Exp.Var arg_id ->
          log_fact analysis_data (Fact.actual_arg proc_name loc ret_id i arg_id) ~loc
      | _ ->
          () )


(** [JAVA ONLY] `Foo x = new Foo();` becomes in SIL `tmp = __new(...) ; x = Foo.<init>(..., tmp)`.
    __new() is the built-in function for allocations, whereas Foo.<init> is a static call to the
    default constructor of Foo. In the fact generation, a call to __new() generates an "Alloc" fact,
    whereas a call to a constructor will generate a "StaticCall" fact. Both facts are generated when
    the `new Class()` statement is used. *)
let emit_procedure_level_facts ({IntraproceduralAnalysis.proc_desc} as analysis_data) =
  let proc_name = Procdesc.get_proc_name proc_desc in
  let proc_formal_args = List.map ~f:fst (Procdesc.get_pvar_formals proc_desc) in
  List.iteri proc_formal_args ~f:(fun i arg ->
      log_fact analysis_data (Fact.formal_arg proc_name i arg) ) ;
  if is_entry_proc proc_name then log_fact analysis_data (Fact.entrypoint proc_name) ;
  Procdesc.iter_instrs
    (fun _ instr ->
      match instr with
      | Call ((ret_id, _), Const (Cfun call_proc), [(Var arg_id, _); (Sizeof sizeof, _)], loc, _)
        when is_builtin_cast call_proc ->
          log_fact analysis_data (Fact.cast proc_name ret_id arg_id sizeof.typ) ~loc
      | Call ((ret_id, _), Const (Cfun call_proc), [(Sizeof sizeof, _)], loc, _)
        when is_builtin_alloc call_proc ->
          log_fact analysis_data (Fact.alloc proc_name ret_id loc sizeof.typ) ~loc
      (* Virtual call: the first arg contains the receiver so it always exists (e.g. in a.f() a is the receiver) *)
      | Call ((ret_id, _), Const (Cfun call_proc), (Var receiver_id, _) :: args, loc, flags)
        when (not (is_builtin call_proc)) && (flags.cf_virtual || flags.cf_interface) ->
          log_fact analysis_data (Fact.virtual_call proc_name loc ret_id call_proc receiver_id) ~loc ;
          emit_call_moves analysis_data args call_proc proc_name loc ret_id
      (* Static call *)
      | Call ((ret_id, _), Const (Cfun call_proc), args, loc, flags)
        when (not (is_builtin call_proc)) && not (flags.cf_virtual || flags.cf_interface) ->
          log_fact analysis_data (Fact.static_call proc_name loc ret_id call_proc) ~loc ;
          emit_call_moves analysis_data args call_proc proc_name loc ret_id
      | Store {e1= Lvar pvar; root_typ= _; typ= _; e2= Var ret_id; loc} when Pvar.is_return pvar ->
          log_fact analysis_data (Fact.formal_return proc_name ret_id) ~loc
      | Load {id= dest; e= Lfield (Var src, src_field, _); root_typ= _; typ= _; loc} ->
          log_fact analysis_data (Fact.load_field proc_name dest src src_field) ~loc
      | Store {e1= Lfield (Var dest, dest_field, _); root_typ= _; typ= _; e2= Var src; loc} ->
          log_fact analysis_data (Fact.store_field proc_name dest dest_field src) ~loc
      | Load {id= dest; e= Lvar src_pvar; root_typ= _; typ= _; loc} ->
          log_fact analysis_data (Fact.move_load proc_name dest src_pvar) ~loc
      | Store {e1= Lvar dest_pvar; root_typ= _; typ= _; e2= Var src; loc} ->
          log_fact analysis_data (Fact.move_store proc_name dest_pvar src) ~loc
      (* Unexpected instructions *)
      | Store {e1= Lvar _; root_typ= _; typ= _; e2= Lvar _; loc}
      | Store {e1= Lfield (Var _, _, _); root_typ= _; typ= _; e2= Lvar _; loc}
      | Store {e1= Lfield (Var _, _, _); root_typ= _; typ= _; e2= Lfield (Var _, _, _); loc} ->
          die_on_instr instr loc
      (* Ignored instructions *)
      | Prune _ | Metadata _ | _ ->
          () )
    proc_desc ;
  match Procname.get_class_type_name proc_name with
  | Some class_typ ->
      log_fact analysis_data (Fact.implem class_typ proc_name)
  | None ->
      ()


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
