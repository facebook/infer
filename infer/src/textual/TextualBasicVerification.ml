(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open Textual

type error =
  | ProcNotImplementedButInClosure of {proc: QualifiedProcName.t}
  | UnknownField of qualified_fieldname
  (* TODO(arr): This is too specific to the Hack use-case. We should really check if there are other
     overloads and provide an error message based on this. *)
  | UnknownProc of {proc: QualifiedProcName.t; args: int}
  | UnknownLabel of {label: NodeName.t; pname: QualifiedProcName.t}
  | VariadicWrongParam of {proc: QualifiedProcName.t; in_a_trait: bool}
  | VariadicNotEnoughArgs of {proc: QualifiedProcName.t; nb_args: int; nb_formals: int}
  | WrongArgNumber of {proc: QualifiedProcName.t; args: int; formals: int; loc: Location.t}

let error_loc = function
  | UnknownField {enclosing_class; _} ->
      enclosing_class.loc
  | ProcNotImplementedButInClosure {proc} | UnknownProc {proc} ->
      proc.name.loc
  | UnknownLabel {label; _} ->
      label.loc
  | VariadicNotEnoughArgs {proc} ->
      proc.name.loc
  | VariadicWrongParam {proc} ->
      proc.name.loc
  | WrongArgNumber {loc; _} ->
      loc


let pp_error sourcefile fmt error =
  let loc = error_loc error in
  F.fprintf fmt "%a, %a: SIL consistency error: " SourceFile.pp sourcefile Location.pp loc ;
  match error with
  | ProcNotImplementedButInClosure {proc} ->
      F.fprintf fmt "function  %a is declared but not implemented: it can not be used in a closure"
        QualifiedProcName.pp proc
  | UnknownField {enclosing_class; name} ->
      F.fprintf fmt "field %a.%a is not declared" TypeName.pp enclosing_class FieldName.pp name
  | UnknownProc {proc; args} ->
      F.fprintf fmt "function %a which can be called with %d arguments is not declared"
        QualifiedProcName.pp proc args
  | UnknownLabel {label; pname} ->
      F.fprintf fmt "label %a is not declared in function %a" NodeName.pp label QualifiedProcName.pp
        pname
  | VariadicNotEnoughArgs {proc; nb_args; nb_formals} ->
      F.fprintf fmt
        "the variadic function %a is expecting at least %d arguments but is called with only %d"
        QualifiedProcName.pp proc (nb_formals - 1) nb_args
  | VariadicWrongParam {proc; in_a_trait} when in_a_trait ->
      F.fprintf fmt "variadic parameter is not in penultimate position in trait fonction %a"
        QualifiedProcName.pp proc
  | VariadicWrongParam {proc} ->
      F.fprintf fmt "variadic parameter is not in last position in fonction %a" QualifiedProcName.pp
        proc
  | WrongArgNumber {proc; args; formals} ->
      F.fprintf fmt "function %a called with %d arguments while declared with %d parameters"
        QualifiedProcName.pp proc args formals


let verify_decl ~env errors (decl : Module.decl) =
  let verify_label errors declared_labels pname label =
    if String.Set.mem declared_labels label.NodeName.value then errors
    else UnknownLabel {label; pname} :: errors
  in
  let verify_field errors field =
    if
      TypeName.equal field.enclosing_class TypeName.wildcard
      || TextualDecls.is_field_declared env field
    then errors
    else UnknownField field :: errors
  in
  let verify_call loc errors ?(must_be_implemented = false) proc nb_args =
    if ProcDecl.is_not_regular_proc proc then errors
    else
      let procsig = Exp.call_sig proc nb_args (TextualDecls.lang env) in
      match TextualDecls.get_procdecl env procsig with
      | None when QualifiedProcName.contains_wildcard proc ->
          errors
      | None ->
          UnknownProc {proc; args= nb_args} :: errors
      | Some (Variadic _, {formals_types= None}) ->
          Logging.internal_error "Textual variadic status with empty list of formals" ;
          (* unexpected situation because a procdecl will be stored as variadic only
             if one of its parameter is annotated as being variadic *)
          errors
      | Some (Variadic _, {formals_types= Some formals_types}) ->
          let nb_formals = List.length formals_types in
          if nb_args < nb_formals - 1 then
            VariadicNotEnoughArgs {proc; nb_args; nb_formals} :: errors
          else errors
      | Some (NotVariadic, {formals_types= Some formals_types}) ->
          let errors =
            if must_be_implemented && TextualDecls.get_procdecl env procsig |> Option.is_none then
              ProcNotImplementedButInClosure {proc} :: errors
            else errors
          in
          let formals = List.length formals_types in
          let args = nb_args + if TextualDecls.is_trait_method env procsig then 1 else 0 in
          if not (Int.equal args formals) then WrongArgNumber {proc; args; formals; loc} :: errors
          else errors
      | Some (NotVariadic, {formals_types= None}) ->
          errors
  in
  let rec verify_exp loc errors (exp : Exp.t) =
    match exp with
    | Var _ | Lvar _ | Const _ | Typ _ ->
        errors
    | Load {exp} ->
        verify_exp loc errors exp
    | Field {exp; field} ->
        let errors = verify_field errors field in
        verify_exp loc errors exp
    | Index (e1, e2) ->
        let errors = verify_exp loc errors e1 in
        verify_exp loc errors e2
    | Call {proc; args} ->
        let errors = List.fold ~f:(verify_exp loc) ~init:errors args in
        verify_call loc errors proc (List.length args)
    | Closure {proc; captured; params} ->
        let errors = List.fold ~f:(verify_exp loc) ~init:errors captured in
        verify_call loc errors ~must_be_implemented:true proc
          (List.length captured + List.length params)
    | Apply {closure; args} ->
        let errors = verify_exp loc errors closure in
        List.fold ~f:(verify_exp loc) ~init:errors args
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
  let verify_variadic_position errors {ProcDesc.procdecl= {qualified_name; formals_types}} =
    let contains_variadic_typ formals =
      List.exists formals ~f:(Typ.is_annotated ~f:Attr.is_variadic)
    in
    Option.value_map formals_types ~default:errors ~f:(fun formals_types ->
        let is_defined_in_a_trait = TextualDecls.is_defined_in_a_trait env qualified_name in
        match List.rev formals_types with
        | [] ->
            errors
        | _ :: others when contains_variadic_typ others && not is_defined_in_a_trait ->
            VariadicWrongParam {proc= qualified_name; in_a_trait= false} :: errors
        | self :: _ :: others when contains_variadic_typ (self :: others) && is_defined_in_a_trait
          ->
            VariadicWrongParam {proc= qualified_name; in_a_trait= true} :: errors
        | _ ->
            errors )
  in
  let verify_procdesc errors (procdesc : ProcDesc.t) =
    let declared_labels =
      List.fold procdesc.nodes ~init:String.Set.empty ~f:(fun set node ->
          String.Set.add set node.Node.label.value )
    in
    let verify_label errors =
      verify_label errors declared_labels procdesc.procdecl.qualified_name
    in
    let verify_node_call errors {Terminator.label} = verify_label errors label in
    let rec verify_terminator loc errors (t : Terminator.t) =
      match t with
      | If {then_; else_} ->
          let errors = verify_terminator loc errors then_ in
          verify_terminator loc errors else_
      | Jump l ->
          List.fold ~init:errors ~f:verify_node_call l
      | Ret e | Throw e ->
          verify_exp loc errors e
      | Unreachable ->
          errors
    in
    let verify_node errors (node : Node.t) =
      let errors = List.fold ~f:verify_instr ~init:errors node.instrs in
      verify_terminator node.last_loc errors node.last
    in
    let errors = verify_variadic_position errors procdesc in
    List.fold ~f:verify_node ~init:errors procdesc.nodes
  in
  match decl with
  | Global _ | Struct _ | Procdecl _ ->
      errors
  | Proc pdesc ->
      verify_procdesc errors pdesc


let run (module_ : Module.t) env =
  let f = verify_decl ~env in
  List.fold ~f ~init:[] module_.decls
