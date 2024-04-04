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
  | VariadicWrongParam of
      {proc: QualifiedProcName.t; in_a_trait: bool; has_reified_generics_param: bool}
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
  | VariadicWrongParam {proc; in_a_trait; has_reified_generics_param}
    when in_a_trait && has_reified_generics_param ->
      F.fprintf fmt
        "variadic parameter is not in second-to-last position in trait function %a which has a \
         reified generics parameter"
        QualifiedProcName.pp proc
  | VariadicWrongParam {proc; in_a_trait} when in_a_trait ->
      F.fprintf fmt "variadic parameter is not in penultimate position in trait function %a"
        QualifiedProcName.pp proc
  | VariadicWrongParam {proc; has_reified_generics_param} when has_reified_generics_param ->
      F.fprintf fmt
        "variadic parameter is not in penultimate position in function %a which has a reified \
         generics parameter"
        QualifiedProcName.pp proc
  | VariadicWrongParam {proc} ->
      F.fprintf fmt "variadic parameter is not in last position in function %a" QualifiedProcName.pp
        proc
  | WrongArgNumber {proc; args; formals} ->
      F.fprintf fmt "function %a called with %d arguments while declared with %d parameters"
        QualifiedProcName.pp proc args formals


let count_generics_args args generics =
  List.count args ~f:(fun exp ->
      match exp with Exp.Var id -> Ident.Set.mem id generics | _ -> false )


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
  let rec verify_call loc errors ?(must_be_implemented = false) proc nb_args nb_generics_args =
    if ProcDecl.is_not_regular_proc proc then errors
    else
      let procsig = Exp.call_sig proc nb_args (TextualDecls.lang env) in
      match TextualDecls.get_procdecl env procsig nb_args with
      | None when QualifiedProcName.contains_wildcard proc ->
          errors
      | None when nb_generics_args > 0 ->
          (* second try by removing generics args *)
          verify_call loc errors ~must_be_implemented proc (nb_args - nb_generics_args) 0
      | None ->
          UnknownProc {proc; args= nb_args} :: errors
      | Some (_, NotReified, _) when nb_generics_args > 0 ->
          (* we did not feed get_procdecl with right arity: let's retry *)
          verify_call loc errors ~must_be_implemented proc (nb_args - nb_generics_args) 0
      | Some (Variadic _, _, {formals_types= None}) ->
          Logging.internal_error "Textual variadic status with empty list of formals" ;
          (* unexpected situation because a procdecl will be stored as variadic only
             if one of its parameter is annotated as being variadic *)
          errors
      | Some (Variadic _, _, {formals_types= Some formals_types}) ->
          let nb_formals = List.length formals_types in
          if nb_args < nb_formals - 1 then
            VariadicNotEnoughArgs {proc; nb_args; nb_formals} :: errors
          else errors
      | Some (NotVariadic, _, {formals_types= Some formals_types}) ->
          let errors =
            if
              must_be_implemented && TextualDecls.get_procdecl env procsig nb_args |> Option.is_none
            then ProcNotImplementedButInClosure {proc} :: errors
            else errors
          in
          let formals = List.length formals_types in
          let args = nb_args + if TextualDecls.is_trait_method env procsig then 1 else 0 in
          if not (Int.equal args formals) then WrongArgNumber {proc; args; formals; loc} :: errors
          else errors
      | Some (NotVariadic, _, {formals_types= None}) ->
          errors
  in
  let verify_exp loc generics errors exp =
    let rec aux errors (exp : Exp.t) =
      match exp with
      | Var _ | Lvar _ | Const _ | Typ _ ->
          errors
      | Load {exp} ->
          aux errors exp
      | Field {exp; field} ->
          let errors = verify_field errors field in
          aux errors exp
      | Index (e1, e2) ->
          let errors = aux errors e1 in
          aux errors e2
      | Call {proc; args} ->
          let errors = List.fold ~f:aux ~init:errors args in
          let nb_generics_args = count_generics_args args generics in
          let nb_args = List.length args in
          verify_call loc errors proc nb_args nb_generics_args
      | Closure {proc; captured; params} ->
          let errors = List.fold ~f:aux ~init:errors captured in
          let nb_generics_args = count_generics_args captured generics in
          let nb_args = List.length captured + List.length params in
          verify_call loc ~must_be_implemented:true errors proc nb_args nb_generics_args
      | Apply {closure; args} ->
          let errors = aux errors closure in
          List.fold ~f:aux ~init:errors args
    in
    aux errors exp
  in
  let verify_instr generics errors (instr : Instr.t) =
    let loc = Instr.loc instr in
    match instr with
    | Load {exp} | Prune {exp} | Let {exp} ->
        verify_exp loc generics errors exp
    | Store {exp1; exp2} ->
        let errors = verify_exp loc generics errors exp1 in
        verify_exp loc generics errors exp2
  in
  let verify_variadic_position errors {ProcDesc.procdecl= {qualified_name; formals_types}; params} =
    let contains_variadic_typ formals =
      List.exists formals ~f:(Typ.is_annotated ~f:Attr.is_variadic)
    in
    let has_reified_generics_param = List.exists ~f:VarName.is_hack_reified_generics_param params in
    Option.value_map formals_types ~default:errors ~f:(fun formals_types ->
        let in_a_trait = TextualDecls.is_defined_in_a_trait env qualified_name in
        match List.rev formals_types with
        | [] ->
            errors
        | _ :: others
          when contains_variadic_typ others && (not in_a_trait) && not has_reified_generics_param ->
            (* basic variadic function: the variadic argument is expected to appear at the last position *)
            VariadicWrongParam
              {proc= qualified_name; in_a_trait= false; has_reified_generics_param= false}
            :: errors
        | last :: before_last :: _ :: others
          when contains_variadic_typ (last :: before_last :: others)
               && in_a_trait && has_reified_generics_param ->
            (* variadic trait function with a reified generic parameter: the variadic argument is expected
               to appear at the second-to-last position, the generic type(s) at the first-to-last position
               and the [self] parameter at the last position *)
            VariadicWrongParam
              {proc= qualified_name; in_a_trait= true; has_reified_generics_param= true}
            :: errors
        | last :: _ :: others
          when contains_variadic_typ (last :: others)
               && ( (in_a_trait && not has_reified_generics_param)
                  || ((not in_a_trait) && has_reified_generics_param) ) ->
            (* case1: variadic trait function without a reified generic parameter: the variadic argument is
               expected to appear at the first-to-last position, and the [self] parameter at the last position *)
            (* case2: variadic non-trait function with a reified generic parameter: the variadic argument is
               expected to appear at the first-to-last position, and the generic type(s) at the last position *)
            VariadicWrongParam {proc= qualified_name; in_a_trait; has_reified_generics_param}
            :: errors
        | _ ->
            errors )
  in
  let verify_procdesc errors (procdesc : ProcDesc.t) =
    (* we expect the frontend to encapsulate each generic argument with an intermediate
       [id = __sil_generics(arg)] call. The [generics] set will contain all this identifiers. *)
    let generics : Ident.Set.t =
      List.fold procdesc.nodes ~init:Ident.Set.empty ~f:(fun init node ->
          List.fold node.Node.instrs ~init ~f:(fun idents instr ->
              match instr with
              | Instr.Let {id; exp= Call {proc}} when ProcDecl.is_generics_constructor_builtin proc
                ->
                  Ident.Set.add id idents
              | _ ->
                  idents ) )
    in
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
          verify_exp loc generics errors e
      | Unreachable ->
          errors
    in
    let verify_node generics errors (node : Node.t) =
      let errors = List.fold ~f:(verify_instr generics) ~init:errors node.instrs in
      verify_terminator node.last_loc errors node.last
    in
    let errors = verify_variadic_position errors procdesc in
    List.fold ~f:(verify_node generics) ~init:errors procdesc.nodes
  in
  match decl with
  | Global _ | Struct _ | Procdecl _ ->
      errors
  | Proc pdesc ->
      verify_procdesc errors pdesc


let run (module_ : Module.t) env =
  let f = verify_decl ~env in
  List.fold ~f ~init:[] module_.decls
