(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module Node = PythonSourceAst.Node
module StringMap = IString.Map

let is_syntactic_field field_name =
  String.equal field_name "kind"
  || List.exists
       ~f:(fun suffix -> String.is_suffix field_name ~suffix)
       [ "_keyword"
       ; "_left_paren"
       ; "_right_paren"
       ; "_left_brace"
       ; "_right_brace"
       ; "_left_bracket"
       ; "_right_bracket"
       ; "_left_angle"
       ; "_right_angle"
       ; "_semicolon"
       ; "_colon"
       ; "_comma"
       ; "_separator"
       ; "_equal"
       ; "_arrow"
       ; "_less_than_question"
       ; "_ampersand"
       ; "_ellipsis"
       ; "_at"
       ; "_star"
       ; "_backslash"
       ; "_question" ]


let rec first_token_line (json : Yojson.Safe.t) : int option =
  match json with
  | `Assoc fields -> (
    match List.Assoc.find fields ~equal:String.equal "kind" with
    | Some (`String "token") ->
        Option.bind (List.Assoc.find fields ~equal:String.equal "token") ~f:(function
          | `Assoc tf -> (
            match List.Assoc.find tf ~equal:String.equal "line_number" with
            | Some (`Int n) ->
                Some n
            | _ ->
                None )
          | _ ->
              None )
    | _ ->
        List.find_map fields ~f:(fun (_, v) -> first_token_line v) )
  | `List items ->
      List.find_map items ~f:first_token_line
  | _ ->
      None


let rec of_hh_json (json : Yojson.Safe.t) : Node.t =
  match json with
  | `Assoc fields -> (
    match List.Assoc.find fields ~equal:String.equal "kind" with
    | Some (`String "token") ->
        Option.bind (List.Assoc.find fields ~equal:String.equal "token") ~f:(function
          | `Assoc tf -> (
            match List.Assoc.find tf ~equal:String.equal "text" with
            | Some (`String s) ->
                Some (Node.Str s)
            | _ ->
                None )
          | _ ->
              None )
        |> Option.value ~default:Node.Null
    | Some (`String "missing") ->
        Null
    | Some (`String "list") ->
        let items =
          match List.Assoc.find fields ~equal:String.equal "elements" with
          | Some (`List l) ->
              l
          | _ ->
              []
        in
        List (List.filter_map items ~f:unwrap_list_item)
    | Some (`String kind) ->
        let line = first_token_line json in
        let semantic_fields =
          List.filter_map fields ~f:(fun (k, v) ->
              if is_syntactic_field k then None
              else match of_hh_json v with Null -> None | node -> Some (k, node) )
        in
        let all_fields = (Node.type_field_name, Node.Str kind) :: semantic_fields in
        let all_fields =
          match line with
          | Some n ->
              (Node.field_lineno, Node.Int n) :: all_fields
          | None ->
              all_fields
        in
        let dict =
          List.fold all_fields ~init:StringMap.empty ~f:(fun acc (k, v) -> StringMap.add k v acc)
        in
        Dict dict
    | _ ->
        Null )
  | _ ->
      Null


and unwrap_list_item (json : Yojson.Safe.t) : Node.t option =
  match json with
  | `Assoc fields -> (
    match List.Assoc.find fields ~equal:String.equal "kind" with
    | Some (`String "list_item") -> (
      match List.Assoc.find fields ~equal:String.equal "list_item" with
      | Some item -> (
        match of_hh_json item with Null -> None | node -> Some node )
      | None ->
          None )
    | _ -> (
      match of_hh_json json with Null -> None | node -> Some node ) )
  | _ ->
      None


let parse_file filename : Node.t =
  let command =
    Printf.sprintf "hh_parse --full-fidelity-json-parse-tree %s" (Filename.quote filename)
  in
  let ic = Unix.open_process_in command in
  let output = In_channel.input_all ic in
  let _status = Unix.close_process_in ic in
  let json = Yojson.Safe.from_string output in
  of_hh_json json
