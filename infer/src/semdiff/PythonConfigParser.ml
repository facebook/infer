(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging
module Ast = PythonSourceAst
open Ast.Node
open PythonCompareDirectRewrite

exception ParseError of {lineno: int option; expected: string}

let get ?lineno node name field =
  let error lineno =
    raise (ParseError {lineno; expected= F.asprintf "field %s of node %s" field name})
  in
  match node with
  | Dict dict when is_type dict name -> (
    match find_field field dict with Some node -> node | None -> error (get_line_number dict) )
  | Dict dict ->
      error (get_line_number dict)
  | _ ->
      error lineno


let list_exn ?lineno node =
  let error lineno = raise (ParseError {lineno; expected= "list"}) in
  match node with List l -> l | _ -> error lineno


let list ~f = function List l -> Some (List.filter_map ~f l) | _ -> None

let find_field = Ast.Node.find_field

let singleton = function List [node] -> Some node | _ -> None

let str = function Str str -> Some str | _ -> None

let name = function
  | Dict dict when is_type dict "Name" ->
      let open IOption.Let_syntax in
      let* id = find_field "id" dict in
      str id
  | _ ->
      None


let constant = function
  | Dict dict when is_type dict "Constant" ->
      let open IOption.Let_syntax in
      let* value = find_field "value" dict in
      str value
  | _ ->
      None


let call = function
  | Dict dict when is_type dict "Call" ->
      let open IOption.Let_syntax in
      let* keywords = find_field "keywords" dict in
      let* args = find_field "args" dict in
      let* func = find_field "func" dict in
      let* funcname = name func in
      Some (funcname, args, keywords)
  | _ ->
      None


let assign map node =
  let open IOption.Let_syntax in
  match node with
  | Dict dict ->
      let* targets = find_field "targets" dict in
      let* target = singleton targets in
      let* value = find_field "value" dict in
      let* target = name target in
      let* funcname, args, _ = call value in
      let* () = if String.equal funcname "var" then Some () else None in
      let* arg = singleton args in
      let* arg = constant arg in
      Some (IString.Map.add target arg map)
  | _ ->
      None


let rec pattern map node =
  let open IOption.Let_syntax in
  match node with
  | Dict dict when is_type dict "Call" ->
      let* funcname, _args, keywords = call node in
      let* args = list keywords ~f:(arg map) in
      Some (Pattern.Node {name= Name.of_string funcname; args})
  | Dict dict when is_type dict "List" ->
      let* elts = find_field "elts" dict in
      let* l = list elts ~f:(pattern map) in
      Some (Pattern.List l)
  | Dict dict when is_type dict "Constant" ->
      let* value = find_field "value" dict in
      Some (Pattern.AstNode value)
  | Dict dict when is_type dict "Name" ->
      let* id = find_field "id" dict >>= str in
      if String.equal id "null" then Some (Pattern.AstNode Null)
      else
        let* var = IString.Map.find_opt id map >>| Var.of_string in
        Some (Pattern.Var var)
  | _ ->
      None


and arg0 map = function
  | Dict dict when is_type dict "keyword" ->
      let open IOption.Let_syntax in
      let* arg = find_field "arg" dict in
      let* key = str arg in
      let* value = find_field "value" dict in
      let* pattern = pattern map value in
      Some (Name.of_string key, pattern)
  | _ ->
      None


and arg map node =
  let res = arg0 map node in
  if Option.is_none res then F.printf "CAN'T PARSE ARG %s@." (Ast.Node.to_str node) ;
  res


let keyword name = function
  | Dict dict when is_type dict "keyword" ->
      let open IOption.Let_syntax in
      let* arg = find_field "arg" dict in
      let* key = str arg in
      let* value = find_field "value" dict in
      if String.equal name key then Some value else None
  | _ ->
      None


let expr map node =
  let open IOption.Let_syntax in
  match node with
  | Dict dict -> (
      let* value = find_field "value" dict in
      let* funcname, args, keywords = call value in
      match (funcname, args, keywords) with
      | "ignore", List [node], List [] ->
          let* node = pattern map node in
          Some (`Ignore node)
      | "rewrite", List [], List ([_; _] as l) ->
          let* lhs = List.find_map l ~f:(keyword "lhs") >>= pattern map in
          let* rhs = List.find_map l ~f:(keyword "rhs") >>= pattern map in
          Some (`Rewrite (lhs, rhs))
      | "accept", List [], List ([_; _] as l) ->
          let* lhs = List.find_map l ~f:(keyword "lhs") >>= pattern map in
          let* rhs = List.find_map l ~f:(keyword "rhs") >>= pattern map in
          Some (`Accept (lhs, rhs))
      | _, _, _ ->
          None )
  | _ ->
      None


let toplevel map node =
  let open IOption.Let_syntax in
  match node with
  | Dict dict when is_type dict "ImportFrom" ->
      Some `Import
  | Dict dict when is_type dict "Assign" ->
      let* map = assign map node in
      Some (`Assign map)
  | Dict dict when is_type dict "Expr" ->
      expr map node
  | _ ->
      None


let parse_module ast =
  let module_ = get ast "Module" "body" in
  let body = list_exn module_ in
  let empty_rules : Rules.t = {ignore= []; rewrite= []; accept= []} in
  let _, {Rules.ignore; rewrite; accept} =
    List.fold body ~init:(IString.Map.empty, empty_rules) ~f:(fun (map, rules) node ->
        match toplevel map node with
        | Some `Import ->
            (map, rules)
        | Some (`Ignore pattern) ->
            (map, {rules with Rules.ignore= pattern :: rules.Rules.ignore})
        | Some (`Rewrite (lhs, rhs)) ->
            (map, {rules with Rules.rewrite= {lhs; rhs} :: rules.Rules.rewrite})
        | Some (`Accept (lhs, rhs)) ->
            (map, {rules with Rules.accept= {lhs; rhs} :: rules.Rules.accept})
        | Some (`Assign map) ->
            (map, rules)
        | None ->
            raise (ParseError {lineno= Ast.Node.get_line_number_of_node node; expected= "assign"}) )
  in
  {Rules.ignore= List.rev ignore; rewrite= List.rev rewrite; accept= List.rev accept}


let parse_string ?filename content =
  let parse = Ast.build_parser () in
  match parse ?filename content with
  | Error error ->
      L.die UserError "%a" PythonSourceAst.pp_error error
  | Ok ast -> (
    try parse_module ast with
    | ParseError {lineno= Some line; expected} ->
        L.die UserError "line %d, %s@." line expected
    | ParseError {expected} ->
        L.die UserError "%s@." expected )


let parse_file filename =
  let content = In_channel.with_file filename ~f:In_channel.input_all in
  parse_string ~filename content
