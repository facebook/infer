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
(* TODO: check that a name is not declared twice *)
(* TODO: add basic type verification *)

let error_loc = function
  | UnknownField {enclosing_class; _} ->
      enclosing_class.loc
  | UnknownProcdecl proc ->
      proc.name.loc
  | UnknownLabel {label; _} ->
      label.loc


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


let verify_decl ~is_field_declared ~is_procname_declared errors (decl : Module.decl) =
  let verify_label errors declared_labels pname label =
    if String.Set.mem declared_labels label.NodeName.value then errors
    else UnknownLabel {label; pname} :: errors
  in
  let verify_field errors field =
    if is_field_declared field then errors else UnknownField field :: errors
  in
  let verify_procname errors proc =
    if is_procname_declared proc || ProcDecl.is_not_regular_proc proc then errors
    else UnknownProcdecl proc :: errors
  in
  let rec verify_exp errors (exp : Exp.t) =
    match exp with
    | Var _ | Lvar _ | Const _ | Typ _ ->
        errors
    | Field {exp; field} ->
        let errors = verify_field errors field in
        verify_exp errors exp
    | Index (e1, e2) ->
        let errors = verify_exp errors e1 in
        verify_exp errors e2
    | Call {proc; args} ->
        let errors = verify_procname errors proc in
        List.fold ~f:verify_exp ~init:errors args
  in
  let verify_instr errors (instr : Instr.t) =
    match instr with
    | Load {exp} | Prune {exp} | Let {exp} ->
        verify_exp errors exp
    | Store {exp1; exp2} ->
        let errors = verify_exp errors exp1 in
        verify_exp errors exp2
  in
  let verify_procdesc errors ({procdecl; nodes} : ProcDesc.t) =
    let declared_labels =
      List.fold nodes ~init:String.Set.empty ~f:(fun set node ->
          String.Set.add set node.Node.label.value )
    in
    let verify_label errors = verify_label errors declared_labels procdecl.qualified_name in
    let verify_terminator errors (t : Terminator.t) =
      match t with
      | Jump l ->
          let f errors {Terminator.label} = verify_label errors label in
          List.fold ~init:errors ~f l
      | Ret e | Throw e ->
          verify_exp errors e
      | Unreachable ->
          errors
    in
    let verify_node errors ({instrs; last} : Node.t) =
      let errors = List.fold ~f:verify_instr ~init:errors instrs in
      verify_terminator errors last
    in
    List.fold ~f:verify_node ~init:errors nodes
  in
  match decl with
  | Global _ | Struct _ | Procdecl _ ->
      errors
  | Proc pdesc ->
      verify_procdesc errors pdesc


let run (module_ : Module.t) =
  let decls_env = TextualDecls.make_decls module_ in
  let is_field_declared = TextualDecls.is_field_declared decls_env in
  let is_procname_declared = TextualDecls.is_procname_declared decls_env in
  let f = verify_decl ~is_field_declared ~is_procname_declared in
  List.fold ~f ~init:[] module_.decls
