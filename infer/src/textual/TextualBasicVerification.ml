(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open Textual

type error =
  | UnknownField of qualified_fieldname
  | UnknownProcdecl of qualified_procname
  | UnknownLabel of {label: NodeName.t; pname: qualified_procname}
  | WrongArgNumber of {proc: qualified_procname; args: int; formals: int; loc: Location.t}

let error_loc = function
  | UnknownField {enclosing_class; _} ->
      enclosing_class.loc
  | UnknownProcdecl proc ->
      proc.name.loc
  | UnknownLabel {label; _} ->
      label.loc
  | WrongArgNumber {loc; _} ->
      loc


let pp_error sourcefile fmt error =
  let loc = error_loc error in
  F.fprintf fmt "%a, %a: SIL consistency error: " SourceFile.pp sourcefile Location.pp loc ;
  match error with
  | UnknownField {enclosing_class; name} ->
      F.fprintf fmt "field %a.%a is not declared" TypeName.pp enclosing_class FieldName.pp name
  | UnknownProcdecl proc ->
      F.fprintf fmt "function %a is not declared" pp_qualified_procname proc
  | UnknownLabel {label; pname} ->
      F.fprintf fmt "label %a is not declared in function %a" NodeName.pp label
        pp_qualified_procname pname
  | WrongArgNumber {proc; args; formals} ->
      F.fprintf fmt "function %a called with %d arguments while declared with %d parameters"
        pp_qualified_procname proc args formals


let verify_decl ~env errors (decl : Module.decl) =
  let verify_label errors declared_labels pname label =
    if String.Set.mem declared_labels label.NodeName.value then errors
    else UnknownLabel {label; pname} :: errors
  in
  let verify_field errors field =
    if TextualDecls.is_field_declared env field then errors else UnknownField field :: errors
  in
  let verify_call loc errors proc args =
    if ProcDecl.is_not_regular_proc proc then errors
    else
      match TextualDecls.get_procname env proc with
      | None ->
          UnknownProcdecl proc :: errors
      | Some {formals_types= Some formals_types; _} ->
          let formals = List.length formals_types in
          let args = List.length args in
          if not (Int.equal args formals) then WrongArgNumber {proc; args; formals; loc} :: errors
          else errors
      | _ ->
          errors
  in
  let rec verify_exp loc errors (exp : Exp.t) =
    match exp with
    | Var _ | Lvar _ | Const _ | Typ _ ->
        errors
    | Field {exp; field} ->
        let errors = verify_field errors field in
        verify_exp loc errors exp
    | Index (e1, e2) ->
        let errors = verify_exp loc errors e1 in
        verify_exp loc errors e2
    | Call {proc; args} ->
        let errors = List.fold ~f:(verify_exp loc) ~init:errors args in
        verify_call loc errors proc args
  in
  let verify_instr errors (instr : Instr.t) =
    let loc = Instr.loc instr in
    match instr with
    | Load {exp} | Prune {exp} | Let {exp} ->
        verify_exp loc errors exp
    | Store {exp1; exp2} ->
        let errors = verify_exp loc errors exp1 in
        verify_exp loc errors exp2
  in
  let verify_procdesc errors ({procdecl; nodes} : ProcDesc.t) =
    let declared_labels =
      List.fold nodes ~init:String.Set.empty ~f:(fun set node ->
          String.Set.add set node.Node.label.value )
    in
    let verify_label errors = verify_label errors declared_labels procdecl.qualified_name in
    let verify_terminator loc errors (t : Terminator.t) =
      match t with
      | Jump l ->
          let f errors {Terminator.label} = verify_label errors label in
          List.fold ~init:errors ~f l
      | Ret e | Throw e ->
          verify_exp loc errors e
      | Unreachable ->
          errors
    in
    let verify_node errors (node : Node.t) =
      let errors = List.fold ~f:verify_instr ~init:errors node.instrs in
      verify_terminator node.last_loc errors node.last
    in
    List.fold ~f:verify_node ~init:errors nodes
  in
  match decl with
  | Global _ | Struct _ | Procdecl _ ->
      errors
  | Proc pdesc ->
      verify_procdesc errors pdesc


let run (module_ : Module.t) env =
  let f = verify_decl ~env in
  List.fold ~f ~init:[] module_.decls
